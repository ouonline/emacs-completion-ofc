;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ofc-common)

;; ------------------------------------------------------------------------- ;;
;; settings

;; minimum length of tokens that are indexed
(defvar ofc-token-min-len 4)

;; ------------------------------------------------------------------------- ;;
;; struct definitions

;; `used-freq' is the used frequency of `token' in its life time.
;; `buffer-list' is a list of buffer(s) where this token occurs.
;; `downcased-token' is `token' downcased mainly for reducing GC overhead, and
;; `char-mask' a 32-bit set of its characters.  Both are derived once at
;; indexing time and used as a cheap necessary condition when matching
;; (see `ofc-token--string-char-mask').
(cl-defstruct ofc-token--token-info-s token used-freq buffer-list downcased-token char-mask)

;; `candidate-list' is a list of strings, each of which has the following text properties:
;;   * `ofc-token-info': an instance of `ofc-token--token-info-s'.
;;   * `ofc-match-score': the cost of turning the current input into this token, one
;;     point per character the token adds to the input, plus one point per matched
;;     character whose case differs from the input.  that is the edit distance of
;;     the alignment `ofc--fuzzy-compare' found, and the lower it is, the earlier
;;     the candidate is sorted.
;;   * `ofc-matched-regions': a list with the form `((begin1 . end1) (begin2 . end2) ...)`
(cl-defstruct ofc-token--matched-item-s downcased-input candidate-list)

;; ------------------------------------------------------------------------- ;;
;; global variables

(define-hash-table-test 'ofc-token--token-hash-strcmp 'string= 'sxhash-equal)

;; a hash table containing all tokens:
;;   token => `ofc-token--token-info-s'
(defvar ofc-token--token-hash (make-hash-table :test 'ofc-token--token-hash-strcmp))

;; a hash table containing all `ofc-token--token-info-s' of a buffer:
;;   buffer => (list `ofc-token--token-info-s' ...)
(defvar ofc-token--buffer2tokens (make-hash-table :test 'eq))

;; each element of this stack is an instance of `ofc-token--matched-item-s'.
(defvar ofc-token--matched-stack '())

;; ------------------------------------------------------------------------- ;;

(defun ofc-token--for-each-token-in-buffer (buffer callback-func)
  (with-current-buffer buffer
    (let ((end (point-max)))
      (save-excursion
        (goto-char (point-min))
        (while (< (point) end)
          (skip-syntax-forward " ") ;; skip over whitespace and other non-symbol characters
          (let ((symbol-start (point)))
            (skip-syntax-forward "w_")
            (let ((symbol-end (point)))
              (if (> symbol-end symbol-start)
                  (funcall callback-func
                           (buffer-substring-no-properties symbol-start symbol-end))
                (when (< symbol-end end)
                  (forward-char))))))))))

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

(defun ofc-token--find-or-insert-token-hash (token buffer)
  "Find or insert an instance of `ofc-token--token-info-s' for TOKEN in
`ofc-token--token-hash', adding BUFFER, and return that instance."
  (let ((token-info (gethash token ofc-token--token-hash)))
    (unless token-info
      (let ((downcased-token (downcase token)))
        (setq token-info (make-ofc-token--token-info-s
                          :token token
                          :used-freq 0
                          :buffer-list '()
                          :downcased-token downcased-token
                          :char-mask (ofc-token--string-char-mask downcased-token))))
      (puthash token token-info ofc-token--token-hash))
    (let ((buffer-list (ofc-token--token-info-s-buffer-list token-info)))
      ;; `buffer` is guranteed unique in `buffer-list` by the caller
      (setf (ofc-token--token-info-s-buffer-list token-info)
            (cons buffer buffer-list)))
    token-info))

(defun ofc-token--remove-token-from-token-hash (token buffer)
  (let ((token-info (gethash token ofc-token--token-hash)))
    (when token-info
      (let ((buffer-list (ofc-token--token-info-s-buffer-list token-info)))
        (setq buffer-list (delete buffer buffer-list))
        (if (eq nil buffer-list)
            (remhash token ofc-token--token-hash)
          (setf (ofc-token--token-info-s-buffer-list token-info) buffer-list))))))

(defun ofc-token--update-buffer-tokens (buffer)
  (unless (string-match-p "*temp*" (buffer-name buffer))
    (let ((new-token-set (make-hash-table :test 'ofc-token--token-hash-strcmp)))
      ;; construct a new-token-set for the modified buffer
      (ofc-token--for-each-token-in-buffer buffer
                                           (lambda (token)
                                             (when (>= (length token) ofc-token-min-len)
                                               (puthash token t new-token-set))))
      (let ((old-token-info-list (gethash buffer ofc-token--buffer2tokens '()))
            (reversed-token-info-list '()))
        (mapc (lambda (token-info)
                (let* ((token (ofc-token--token-info-s-token token-info))
                       (found (gethash token new-token-set)))
                  (if found
                      ;; if a token in the new-token-set exists in the old one, delete it from new-token-set
                      ;; so that it remains unchanged in `ofc-token--token-hash'
                      (progn
                        (remhash token new-token-set)
                        (push token-info reversed-token-info-list))
                    ;; otherwise remove it from `ofc-token--token-hash`
                    (ofc-token--remove-token-from-token-hash token buffer))))
              old-token-info-list)
        (maphash (lambda (token _)
                   (let ((token-info (ofc-token--find-or-insert-token-hash token buffer)))
                     (push token-info reversed-token-info-list)))
                 new-token-set)
        (puthash buffer (nreverse reversed-token-info-list) ofc-token--buffer2tokens)))))

(defun ofc-token--destroy-buffer-tokens (buffer)
  (let ((token-info-list (gethash buffer ofc-token--buffer2tokens)))
    (when token-info-list
      (mapc (lambda (token-info)
              (let ((buffer-list (ofc-token--token-info-s-buffer-list token-info)))
                (setq buffer-list (delete buffer buffer-list))
                (if (eq nil buffer-list)
                    (remhash (ofc-token--token-info-s-token token-info) ofc-token--token-hash)
                  (setf (ofc-token--token-info-s-buffer-list token-info) buffer-list))))
            token-info-list)
      (remhash buffer ofc-token--buffer2tokens))))

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
                   (ofc-token--record-reversed-matched-regions text-idx reversed-matched-regions))))
      (let ((new-candidate-token (substring-no-properties token)))
        (add-text-properties 0 token-length
                             (list :ofc-token-info token-info
                                   :ofc-match-score (+ token-length case-penalty)
                                   :ofc-matched-regions (nreverse reversed-matched-regions))
                             new-candidate-token)
        new-candidate-token))))

(defun ofc-token--generate-candidate-list-from-scratch (input downcased-input input-length input-mask predicate)
  "creates a list of token candidates."
  (let ((reversed-candidate-list '()))
    (maphash (lambda (token token-info)
               (when (and (= input-mask
                             (logand input-mask
                                     (ofc-token--token-info-s-char-mask token-info)))
                          (funcall predicate token))
                 (let ((new-candidate-token (ofc-token--do-gen-candidate input downcased-input input-length
                                                                         token token-info)))
                   (when new-candidate-token
                     (push new-candidate-token reversed-candidate-list)))))
             ofc-token--token-hash)
    (nreverse reversed-candidate-list)))

(defun ofc-token--generate-candidate-list-from-another (input downcased-input input-length input-mask another-candidate-list predicate)
  "creates a new candidate list from another."
  (let ((reversed-candidate-list '()))
    (mapc (lambda (candidate-token)
            (let ((token-info (get-text-property 0 :ofc-token-info candidate-token)))
              (when (and (= input-mask
                            (logand input-mask
                                    (ofc-token--token-info-s-char-mask token-info)))
                         (funcall predicate candidate-token))
                (let ((new-candidate-token (ofc-token--do-gen-candidate input downcased-input input-length
                                                                        candidate-token token-info)))
                  (when new-candidate-token
                    (push new-candidate-token reversed-candidate-list))))))
          another-candidate-list)
    (nreverse reversed-candidate-list)))

(defun ofc-token--find-matched-item-in-stack (downcased-input)
  "pops the cached items that DOWNCASED-INPUT can not be narrowed down from,
 and returns the remaining top one, or nil.

An item is a valid base only if its input is a strict prefix of DOWNCASED-INPUT:
every token matching DOWNCASED-INPUT matches any prefix of it as well, so the
item's candidate list is guaranteed to be a superset of the new one.  Items
failing this test are stale -- point has jumped to another token or to another
buffer without passing through an empty prefix -- and get dropped, which also
keeps the stack bounded by the length of the token being typed."
  (let ((input-length (length downcased-input)))
    (while (and ofc-token--matched-stack
                (let ((pre-substr (ofc-token--matched-item-s-downcased-input
                                   (car ofc-token--matched-stack))))
                  (or (>= (length pre-substr) input-length)
                      (not (string-prefix-p pre-substr downcased-input)))))
      (pop ofc-token--matched-stack))
    (car ofc-token--matched-stack)))

(defun ofc-token--sort-candidate-list (candidate-list)
  "sort by used-frequency, then by `:ofc-match-score'.
the score is token-length + the number of case mismatches, which is the edit
distance of the alignment found by the fuzzy matcher."
  (cl-stable-sort candidate-list
                  (lambda (a b)
                    (let ((freq-a (ofc-token--token-info-s-used-freq
                                   (get-text-property 0 :ofc-token-info a)))
                          (freq-b (ofc-token--token-info-s-used-freq
                                   (get-text-property 0 :ofc-token-info b))))
                      (if (> freq-a freq-b)
                          t
                        (when (= freq-a freq-b)
                          (< (get-text-property 0 :ofc-match-score a)
                             (get-text-property 0 :ofc-match-score b))))))))

(defun ofc-token--find-candidates (input &optional predicate)
  (let* ((input-length (length input))
         (actual-pred (or predicate (lambda (_) t)))
         (downcased-input (downcase input))
         (input-mask (ofc-token--string-char-mask downcased-input))
         (matched-item (ofc-token--find-matched-item-in-stack downcased-input))
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

(defun ofc-token--grab-prefix-range ()
  "returns (begin . end) or nil"
  (let ((end (point))
        (begin (save-excursion (skip-syntax-backward "w_") (point))))
    (if (= end begin)
        (progn
          (setq ofc-token--matched-stack '())
          nil)
      (cons begin end))))

(defun ofc-token--grab-suffix ()
  (let ((suffix (buffer-substring-no-properties
                 (point)
                 (save-excursion (skip-syntax-forward "w_")
                                 (point)))))
    (unless (string-empty-p suffix)
      suffix)))

(defun ofc-token--post-completion (candidate-token)
  ;; update frequency of the matched token info
  (cl-incf (ofc-token--token-info-s-used-freq
            (get-text-property 0 :ofc-token-info candidate-token)))
  ;; remove overlapping suffix
  (let ((suffix (ofc-token--grab-suffix)))
    (when suffix
      (let ((suffix-length (length suffix)))
        (when (ofc--fuzzy-compare (downcase suffix) suffix-length
                                  (downcase candidate-token) (length candidate-token))
          (delete-char suffix-length)))))
  (setq ofc-token--matched-stack '()))

(defun ofc-token--after-buffer-saved ()
  (setq ofc-token--matched-stack '())
  (ofc-token--update-buffer-tokens (current-buffer)))

(defun ofc-token--before-buffer-killed ()
  (ofc-token--destroy-buffer-tokens (current-buffer)))

(defun ofc-token--after-buffer-created ()
  (add-hook 'after-save-hook #'ofc-token--after-buffer-saved 0 t)
  (add-hook 'kill-buffer-hook #'ofc-token--before-buffer-killed 0 t)
  (setq ofc-token--matched-stack '())
  (ofc-token--update-buffer-tokens (current-buffer)))

(provide 'ofc-token)
