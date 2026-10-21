;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ofc-common)
(require 'track-changes)

;; ------------------------------------------------------------------------- ;;
;; settings

;; minimum length of tokens that are indexed
(defvar ofc-token-min-len 4)

;; ------------------------------------------------------------------------- ;;
;; struct definitions

;; `used-freq' is the used frequency of `token' in its life time.
;; `buffer-list' is a list of (buffer . count) of this token.
;; `downcased-token' is `token' downcased mainly for reducing GC overhead, and
;; `char-mask' a 32-bit set of its characters.  Both are derived once at
;; indexing time and used as a cheap necessary condition when matching
;; (see `ofc-token--string-char-mask').
(cl-defstruct ofc-token--token-info-s char-mask downcased-token used-freq buffer-list)

;; `candidate-list' is a list of strings, each of which has the following text properties:
;;   * `ofc-token-info': an instance of `ofc-token--token-info-s'.
;;   * `ofc-match-score': the sorting key of the candidate, the higher the earlier.
;;     Its high bits are `used-freq', so more frequently used tokens come first.
;;     Its low 16 bits are the negated cost of turning the current input into this
;;     token, one point per character the token adds to the input, plus one point
;;     per matched character whose case differs from the input.  That is the edit
;;     distance of the alignment `ofc--fuzzy-compare' found, so among tokens with
;;     the same frequency the smaller edit distance comes first.
;;   * `ofc-matched-regions': a list with the form `((begin1 . end1) (begin2 . end2) ...)`
(cl-defstruct ofc-token--matched-item-s downcased-input candidate-list)

;; ------------------------------------------------------------------------- ;;
;; global variables

;; a hash table containing all tokens:
;;   token => `ofc-token--token-info-s'
(defvar ofc-token--token2info (make-hash-table :test 'equal))

;; a hash table containing all `ofc-token--token-info-s' of a buffer:
;;   buffer => (hash-table token => `ofc-token--token-info-s')
;; the values are shared with `ofc-token--token2info'.
(defvar ofc-token--buffer2tokens (make-hash-table :test 'eq))

;; each element of this stack is an instance of `ofc-token--matched-item-s'.
(defvar ofc-token--matched-stack '())

;; tracker used to update only edited regions on save per-buffer.
(defvar-local ofc-token--change-tracker nil)

;; ------------------------------------------------------------------------- ;;

(defun ofc-token--for-each-token-in-region (begin end callback-func)
  (save-excursion
    (goto-char begin)
    (while (< (point) end)
      (skip-syntax-forward " ") ;; skip over whitespace and other non-symbol characters
      (let ((symbol-begin (point)))
        (when (< symbol-begin end)
          (skip-syntax-forward "w_")
          (let ((symbol-end (point)))
            (if (> symbol-end symbol-begin)
                (when (<= symbol-end end)
                  (funcall callback-func symbol-begin symbol-end))
              (forward-char))))))))

(defun ofc-token--for-each-token-in-buffer (callback-func)
  (ofc-token--for-each-token-in-region (point-min) (point-max) callback-func))

(defun ofc-token--string-char-mask (string)
  "Return a 32-bit set of the characters in STRING.
Every character of a pattern must be present in the token for it to
match, so the mask is a cheap necessary condition before running the
fuzzy scan.  Character code collisions only cause false positives."
  (let ((mask 0)
        (idx 0)
        (len (length string)))
    (while (< idx len)
      (setq mask (logior mask (ash 1 (logand (aref string idx) 31))))
      (setq idx (1+ idx)))
    mask))

(defun ofc-token--load-buffer-tokens-in-region (start stop)
  (unless (or (= start stop)
              (string-match-p "*temp*" (buffer-name)))
    (let* ((buffer (current-buffer))
           (token-table
            (or (gethash buffer ofc-token--buffer2tokens)
                (make-hash-table :test 'equal))))
      (ofc-token--for-each-token-in-region
       start stop
       (lambda (begin end)
         (when (>= (- end begin) ofc-token-min-len)
           (let* ((token (buffer-substring-no-properties begin end))
                  (info (gethash token ofc-token--token2info)))
             (if info
                 (let ((buf-and-occur
                        (assq buffer (ofc-token--token-info-s-buffer-list info))))
                   (if buf-and-occur
                       (cl-incf (cdr buf-and-occur))
                     (push (cons buffer 1)
                           (ofc-token--token-info-s-buffer-list info))))
               (let ((downcased-token (downcase token)))
                 (setq info
                       (make-ofc-token--token-info-s
                        :char-mask (ofc-token--string-char-mask downcased-token)
                        :downcased-token downcased-token
                        :used-freq 0
                        :buffer-list (list (cons buffer 1)))))
               (puthash token info ofc-token--token2info))
             ;; share the same `ofc-token--token-info-s' with `ofc-token--token2info'
             (puthash token info token-table)))))
      (puthash buffer token-table ofc-token--buffer2tokens))))

(defun ofc-token--destroy-buffer-tokens ()
  (let* ((buffer (current-buffer))
         (token-table (gethash buffer ofc-token--buffer2tokens)))
    (when token-table
      (maphash (lambda (token token-info)
                 (let ((buffer-list (ofc-token--token-info-s-buffer-list token-info)))
                   (setq buffer-list (cl-delete-if (lambda (buf-and-occur)
                                                     (eq buffer (car buf-and-occur)))
                                                   buffer-list))
                   (if (null buffer-list)
                       (remhash token ofc-token--token2info)
                     (setf (ofc-token--token-info-s-buffer-list token-info) buffer-list))))
               token-table)
      (remhash buffer ofc-token--buffer2tokens)))
  (setq ofc-token--matched-stack '()))

(defun ofc-token--record-reversed-matched-regions (text-idx reversed-matched-regions)
  "updates the matched regions and returns it, in the reversed form of
 `((beginN . endN) ... (begin1 . end1))`. the caller is responsible for
 reversing them back into ascending order."
  (let ((last-region (car reversed-matched-regions)))
    ;; merge current matched position to previous if they are adjacent
    (if (and last-region
             (= text-idx (cdr last-region)))
        (progn
          (cl-incf (cdr last-region))
          reversed-matched-regions)
      (cons (cons text-idx (+ 1 text-idx)) reversed-matched-regions))))

(defun ofc-token--do-gen-candidate (input downcased-input input-length token token-info)
  "returns a matched candidate-token or nil."
  (let ((downcased-token (ofc-token--token-info-s-downcased-token token-info))
        (token-length (length token))
        (reversed-matched-regions '())
        (case-penalty 0))
    (when (ofc--fuzzy-compare
           downcased-input input-length downcased-token token-length
           (lambda (text-idx pattern-idx)
             ;; the two characters are equal once downcased, so any difference
             ;; left is a case difference, which an edit distance would charge
             ;; as a substitution
             (unless (eq (aref token text-idx) (aref input pattern-idx))
               (cl-incf case-penalty))
             (setq reversed-matched-regions
                   (ofc-token--record-reversed-matched-regions
                    text-idx reversed-matched-regions))))
      (let ((new-candidate-token (substring-no-properties token)))
        (add-text-properties 0 token-length
                             (list :ofc-matched-regions (nreverse reversed-matched-regions)
                                   :ofc-match-score (- (ash (ofc-token--token-info-s-used-freq token-info) 16)
                                                       token-length case-penalty)
                                   :ofc-token-info token-info)
                             new-candidate-token)
        new-candidate-token))))

(defun ofc-token--generate-candidate-list-from-scratch
    (input downcased-input input-length input-mask predicate)
  "creates a list of token candidates."
  (let ((reversed-candidate-list '()))
    (maphash (lambda (token token-info)
               (when (and (= input-mask
                             (logand input-mask
                                     (ofc-token--token-info-s-char-mask token-info)))
                          (funcall predicate token))
                 (let ((new-candidate-token
                        (ofc-token--do-gen-candidate input downcased-input input-length
                                                     token token-info)))
                   (when new-candidate-token
                     (push new-candidate-token reversed-candidate-list)))))
             ofc-token--token2info)
    (nreverse reversed-candidate-list)))

(defun ofc-token--generate-candidate-list-from-another
    (input downcased-input input-length input-mask another-candidate-list predicate)
  (let ((reversed-candidate-list '()))
    (mapc (lambda (candidate-token)
            (let ((token-info (get-text-property 0 :ofc-token-info candidate-token)))
              (when (and (= input-mask
                            (logand input-mask
                                    (ofc-token--token-info-s-char-mask token-info)))
                         (funcall predicate candidate-token))
                (let ((new-candidate-token
                       (ofc-token--do-gen-candidate input downcased-input input-length
                                                    candidate-token token-info)))
                  (when new-candidate-token
                    (push new-candidate-token reversed-candidate-list))))))
          another-candidate-list)
    (nreverse reversed-candidate-list)))

(defun ofc-token--find-matched-item-in-stack (input-length)
  (while (and ofc-token--matched-stack
              (let ((pre-substr (ofc-token--matched-item-s-downcased-input
                                 (car ofc-token--matched-stack))))
                ;; a len-based predicate is enough here
                (> (length pre-substr) input-length)))
    (pop ofc-token--matched-stack))
  (car ofc-token--matched-stack))

(defun ofc-token--sort-candidate-list (candidate-list)
  (sort candidate-list
        :key (lambda (c) (get-text-property 0 :ofc-match-score c))
        :lessp #'>
        :in-place t))

(defun ofc-token--find-candidates (input &optional predicate)
  (let* ((input-length (length input))
         (actual-pred (or predicate (lambda (_) t)))
         (downcased-input (downcase input))
         (input-mask (ofc-token--string-char-mask downcased-input))
         (matched-item (ofc-token--find-matched-item-in-stack input-length))
         (candidate-list '()))
    (if matched-item
        (setq candidate-list
              (ofc-token--generate-candidate-list-from-another
               input downcased-input input-length input-mask
               (ofc-token--matched-item-s-candidate-list matched-item)
               actual-pred))
      (setq candidate-list
            (ofc-token--generate-candidate-list-from-scratch
             input downcased-input input-length input-mask
             actual-pred)))
    (when candidate-list
      ;; `ofc-token--sort-candidate-list' is a destructive sort,
      ;; candidates MUST remain unchanged after return.
      (when (consp (cdr candidate-list)) ;; candidates size > 1
        (setq candidate-list
              (ofc-token--sort-candidate-list candidate-list)))
      (push (make-ofc-token--matched-item-s
             :downcased-input downcased-input
             :candidate-list candidate-list)
            ofc-token--matched-stack)
      candidate-list)))

(defun ofc-token--grab-prefix-length ()
  (let ((end (point))
        (begin (save-excursion (skip-syntax-backward "w_") (point))))
    (when (= end begin)
      (setq ofc-token--matched-stack '()))
    (- end begin)))

(defun ofc-token--grab-suffix-length ()
  (let ((begin (point))
        (end (save-excursion (skip-syntax-forward "w_") (point))))
    (- end begin)))

(defun ofc-token--post-completion (candidate)
  ;; update frequency of the matched token info
  (cl-incf (ofc-token--token-info-s-used-freq
            (get-text-property 0 :ofc-token-info candidate)))
  ;; remove overlapping suffix
  (let ((suffix-length (ofc-token--grab-suffix-length)))
    (when (and (> suffix-length 0)
               (ofc--fuzzy-compare
                (downcase (buffer-substring-no-properties
                           (point) (+ (point) suffix-length)))
                suffix-length
                (ofc-token--token-info-s-downcased-token
                 (get-text-property 0 :ofc-token-info candidate))
                (length candidate)))
      (delete-char suffix-length)))
  (setq ofc-token--matched-stack '()))

(defun ofc-token--destroy-tokens-in-string (str)
  (unless (string-empty-p str)
    (let ((real-buffer (current-buffer))
          (current-syntax (syntax-table)))
      (with-temp-buffer
        (insert str)
        (with-syntax-table current-syntax
          (ofc-token--for-each-token-in-buffer
           (lambda (begin end)
             (when (>= (- end begin) ofc-token-min-len)
               (let* ((token (buffer-substring-no-properties begin end))
                      (token-info (gethash token ofc-token--token2info)))
                 (when token-info
                   (let ((buf-and-occur
                          (assq real-buffer
                                (ofc-token--token-info-s-buffer-list token-info))))
                     (when buf-and-occur
                       (if (> (cdr buf-and-occur) 1)
                           (cl-decf (cdr buf-and-occur))
                         (setf (ofc-token--token-info-s-buffer-list token-info)
                               (delq buf-and-occur
                                     (ofc-token--token-info-s-buffer-list token-info)))
                         (unless (ofc-token--token-info-s-buffer-list token-info)
                           (remhash token ofc-token--token2info))
                         (let ((token-table (gethash real-buffer ofc-token--buffer2tokens)))
                           (when token-table
                             (remhash token token-table))))))))))))))
    (setq ofc-token--matched-stack '())))

(defun ofc-token--update-tokens-in-region (begin end before)
  (unless (or (eq before 'error)
              (string-match-p "*temp*" (buffer-name)))
    (save-restriction
      (widen)
      ;; expand `begin' and `end' to the syntax token boundaries
      (let ((start (save-excursion
                     (goto-char begin)
                     (skip-syntax-backward "w_")
                     (point)))
            (stop (save-excursion
                    (goto-char end)
                    (skip-syntax-forward "w_")
                    (point))))
        (ofc-token--load-buffer-tokens-in-region start stop)
        (ofc-token--destroy-tokens-in-string
         (concat (buffer-substring-no-properties start begin)
                 before
                 (buffer-substring-no-properties end stop)))))))

(defun ofc-token--update-tokens-from-tracker (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (track-changes-fetch ofc-token--change-tracker
                           #'ofc-token--update-tokens-in-region))
    (setq ofc-token--matched-stack '())))

(defun ofc-token--register-change-tracker ()
  (setq ofc-token--change-tracker
        (track-changes-register
         (lambda (_tracker_id)
           (run-with-idle-timer
            0.2 nil
            #'ofc-token--update-tokens-from-tracker
            (current-buffer))))))

(defun ofc-token--unregister-change-tracker ()
  (when ofc-token--change-tracker
    (track-changes-unregister ofc-token--change-tracker)
    (setq ofc-token--change-tracker nil)))

(defun ofc-token--before-buffer-killed ()
  (ofc-token--unregister-change-tracker)
  (ofc-token--destroy-buffer-tokens))

(defun ofc-token--after-buffer-created ()
  (ofc-token--destroy-buffer-tokens)
  (add-hook 'kill-buffer-hook #'ofc-token--before-buffer-killed 0 t)
  (setq ofc-token--matched-stack '())
  (ofc-token--unregister-change-tracker)
  (save-restriction
    (widen)
    (ofc-token--load-buffer-tokens-in-region (point-min) (point-max)))
  (ofc-token--register-change-tracker))

(provide 'ofc-token)
