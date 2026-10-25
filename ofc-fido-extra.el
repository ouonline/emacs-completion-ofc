;;; ofc-fido-extra.el --- Extra fido-mode configurations -*- lexical-binding: t -*-

;;; Commentary:

;; Extra commands and key bindings for fido-mode.
;;
;; - `ofc-fido-switch-buffer' switches to a buffer or a recent file.  Recent
;;   files are matched by base name but opened by their full path.
;; - "/" in file completion enters the only matching directory, while "/"
;;   after a slash jumps to the root directory.
;; - RET enters the selected directory instead of exiting the minibuffer.
;; - The "./" candidate is hidden from `find-file' completion.

;;; Code:

(require 'cl-lib)
(require 'icomplete)
(require 'recentf)

;; -----------------------------------------------------------------------------
;; File name reading

(defun ofc-fido--read-file-name-predicate (predicate)
  "Return PREDICATE adapted to exclude the \"./\" candidate."
  (lambda (filename)
    (and (or (null predicate)
             (funcall predicate filename))
         (not (string= filename "./")))))

(defun ofc-fido--read-file-name (prompt &optional dir default-filename
                                        mustmatch initial predicate)
  "Read a file name without offering the current-directory candidate.
See `read-file-name' for PROMPT, DIR, DEFAULT-FILENAME, MUSTMATCH,
INITIAL and PREDICATE."
  (read-file-name-default
   prompt dir default-filename mustmatch initial
   (ofc-fido--read-file-name-predicate predicate)))

(setq read-file-name-function #'ofc-fido--read-file-name)
(setq read-file-name-completion-ignore-case t)

;; -----------------------------------------------------------------------------
;; Switch to buffer or recent file

(defconst ofc-fido-switch-buffer--completion-style
  '(o-buffer completion-flex-try-completion completion-flex-all-completions)
  "Completion style used by `ofc-fido-switch-buffer'.")

(defconst ofc-fido-switch-buffer--completion-metadata
  '(metadata (category . o-buffer)
             (display-sort-function . identity)
             (cycle-sort-function . identity)
             (completion--unquote-requote . t))
  "Completion metadata used by `ofc-fido-switch-buffer'.")

(defun ofc-fido-switch-buffer--candidate-file (candidate)
  "Return the file path stored in CANDIDATE, or nil if there is none.
File candidates keep their original path in the `o-buffer-file' text
property."
  (when (stringp candidate)
    (let ((position (text-property-not-all 0 (length candidate)
                                           'o-buffer-file nil candidate)))
      (and position
           (get-text-property position 'o-buffer-file candidate)))))

(defun ofc-fido-switch-buffer--basename-candidate (candidate)
  "Return CANDIDATE as a base name if it is a file candidate."
  (let ((file (ofc-fido-switch-buffer--candidate-file candidate)))
    (if file
        (propertize (file-name-nondirectory file)
                    'o-buffer-file file)
      candidate)))

(defun ofc-fido-switch-buffer--restore-file-candidate (candidate)
  "Restore CANDIDATE's full path after base-name matching."
  (let ((file (ofc-fido-switch-buffer--candidate-file candidate)))
    (if (null file)
        candidate
      (let* ((offset (- (length file) (length candidate)))
             (result (concat (substring file 0 offset) candidate))
             (matches (get-text-property 0 'flex-matches candidate))
             (cost (get-text-property 0 'flex-cost candidate)))
        ;; Flex match positions refer to the base name; move them past the
        ;; directory prefix that is being restored.
        (when matches
          (put-text-property 0 (length result) 'flex-matches
                             (mapcar (lambda (match) (+ match offset))
                                     matches)
                             result))
        (when cost
          (put-text-property 0 1 'flex-cost cost result))
        result))))

(defun ofc-fido-switch-buffer--requote-try-completion (result input candidates)
  "Restore a full path in \\=`try-completion\\=' RESULT.
INPUT and CANDIDATES are the input and transformed candidates used for
matching."
  (cond
   ((eq result t)
    ;; An exact base name is a unique completion, but the minibuffer needs
    ;; the full path to pass `test-completion' when it exits.
    (let ((file (cl-loop for candidate in candidates
                         thereis (and (string= input candidate)
                                      (ofc-fido-switch-buffer--candidate-file
                                       candidate)))))
      (if file
          (cons file (length file))
        result)))
   ((and (consp result)
         (stringp (car result)))
    (let* ((completion (car result))
           (file (ofc-fido-switch-buffer--candidate-file completion)))
      (if (and file
               (string= completion (file-name-nondirectory file)))
          (cons file
                (+ (cdr result)
                   (- (length file) (length completion))))
        result)))
   (t result)))

(defun ofc-fido-switch-buffer--requote-all-completions (result)
  "Restore full paths in \\=`all-completions\\=' RESULT."
  (let ((last (last result)))
    (when (consp last)
      ;; Drop the base-size stored in the last cdr.
      (setcdr last nil))
    (mapcar #'ofc-fido-switch-buffer--restore-file-candidate result)))

(defun ofc-fido-switch-buffer--requote (result n input candidates)
  "Restore full paths in completion RESULT.
N is 1 for \\=`try-completion\\=' and 2 for \\=`all-completions\\='.  INPUT and
CANDIDATES are the input and transformed candidates used for matching."
  (if (= n 1)
      (ofc-fido-switch-buffer--requote-try-completion result input candidates)
    (ofc-fido-switch-buffer--requote-all-completions result)))

(defun ofc-fido-switch-buffer--full-path-input-p (input candidates)
  "Return non-nil if INPUT is one of the full-path file CANDIDATES."
  (cl-some (lambda (candidate)
             (and (equal input candidate)
                  (ofc-fido-switch-buffer--candidate-file candidate)))
           candidates))

(defun ofc-fido-switch-buffer--completion-table (candidates)
  "Return a completion table for CANDIDATES.
File candidates are matched by base name and completed as full paths.

The table uses the `completion--unquote-requote' protocol: the unquoted
form of a file candidate is its base name, while the quoted form is its
full path."
  (lambda (string pred action)
    (cond
     ((eq action 'metadata)
      ofc-fido-switch-buffer--completion-metadata)
     ((eq action 'completion--unquote)
      (if (ofc-fido-switch-buffer--full-path-input-p string candidates)
          ;; Keep an already complete full path working as usual.
          (list string candidates pred (lambda (result _n) result))
        (let ((transformed (mapcar #'ofc-fido-switch-buffer--basename-candidate
                                   candidates)))
          (list string transformed pred
                (lambda (result n)
                  (ofc-fido-switch-buffer--requote
                   result n string transformed))))))
     (t
      (complete-with-action action candidates string pred)))))

(defun ofc-fido-switch-buffer--buffer-names ()
  "Return names of visible buffers except the current one."
  (cl-loop for buffer in (buffer-list)
           for name = (buffer-name buffer)
           unless (or (eq buffer (current-buffer))
                      (string-prefix-p " " name))
           collect name))

(defun ofc-fido-switch-buffer--recent-file-candidates ()
  "Return propertized recent files that are not open in a buffer."
  (cl-loop for file in recentf-list
           unless (find-buffer-visiting file)
           collect (propertize file 'o-buffer-file file)))

(defun ofc-fido-switch-buffer--read-candidate (candidates)
  "Read and return a candidate from CANDIDATES."
  ;; Use flex matching without its score-based sorting, so that buffers stay
  ;; ahead of recent files.
  (let ((completion-styles-alist
         (cons ofc-fido-switch-buffer--completion-style
               completion-styles-alist))
        (completion-category-overrides
         '((o-buffer (styles . (o-buffer))))))
    (completing-read "Switch to: "
                     (ofc-fido-switch-buffer--completion-table candidates)
                     nil t nil nil (car candidates))))

(defun ofc-fido-switch-buffer ()
  "Switch to a buffer or a recent file.
Buffer candidates come first.  Recent files are matched by base name but
opened by their full path."
  (interactive)
  (let* ((buffer-names (ofc-fido-switch-buffer--buffer-names))
         (candidates (append buffer-names
                             (ofc-fido-switch-buffer--recent-file-candidates)))
         (choice (ofc-fido-switch-buffer--read-candidate candidates)))
    (if (member choice buffer-names)
        (switch-to-buffer choice)
      (find-file choice))))

;; -----------------------------------------------------------------------------
;; Fido find-file commands

(defun ofc-fido-find-file--directory-candidate-p (candidate)
  "Return non-nil if CANDIDATE denotes a directory.
File completion marks directory candidates with a trailing slash."
  (and (stringp candidate)
       (string-suffix-p "/" candidate)))

(defun ofc-fido-find-file--unique-directory-completion ()
  "Return (DIRECTORY . BASE-SIZE) if exactly one directory matches.
DIRECTORY includes the trailing slash.  Return nil when there is no
directory candidate or more than one."
  (let* ((beg (icomplete--field-beg))
         (end (icomplete--field-end))
         (completions (completion-all-sorted-completions beg end))
         (base-size (or (cdr (last completions)) 0))
         (directories (cl-loop for candidate in completions
                               when (ofc-fido-find-file--directory-candidate-p
                                     candidate)
                               collect candidate)))
    (when (= (length directories) 1)
      (cons (car directories) base-size))))

(defun ofc-fido-find-file--root-directory ()
  "Return the root directory for the current fido file completion."
  (let* ((input (icomplete--field-string))
         (directory (if (file-name-absolute-p input)
                        input
                      default-directory))
         (remote (file-remote-p directory)))
    (cond
     (remote (concat remote "/"))
     ;; On Windows, "/" expands to the root of the current drive.
     ((memq system-type '(windows-nt ms-dos))
      (expand-file-name "/" directory))
     (t "/"))))

(defun ofc-fido-find-file--replace-with-root ()
  "Replace the completion input with the completion root directory."
  (let ((root (ofc-fido-find-file--root-directory)))
    (delete-region (icomplete--field-beg) (icomplete--field-end))
    (insert root)
    (goto-char (point-max))))

(defun ofc-fido-find-file--handle-slash ()
  "Handle a slash in fido file completion.
Return non-nil if the slash was consumed.  A slash after a slash jumps to
the completion root.  Otherwise complete the only matching directory, if
there is exactly one."
  (when (eq (icomplete--category) 'file)
    (let ((beg (icomplete--field-beg))
          (end (icomplete--field-end)))
      (cond
       ((and (= (point) end)
             (eq (char-before) ?/))
        (ofc-fido-find-file--replace-with-root)
        t)
       (t
        (let ((directory (ofc-fido-find-file--unique-directory-completion)))
          (when directory
            (completion--replace (+ beg (cdr directory))
                                 end
                                 (car directory))
            (completion--done (icomplete--field-string) 'sole)
            t)))))))

(defun ofc-fido-find-file--complete-directory-or-insert-slash ()
  "Complete a directory or insert a slash in fido file completion.
If point follows a slash, jump to the root directory instead.  If the
current completion is not for files, or does not contain exactly one
directory candidate, insert a literal slash."
  (interactive)
  (unless (ofc-fido-find-file--handle-slash)
    (self-insert-command 1)))

(defun ofc-fido-find-file--current-candidate-directory ()
  "Return the current completion candidate when it is a directory."
  (when (eq (icomplete--category) 'file)
    (let* ((beg (icomplete--field-beg))
           (end (icomplete--field-end))
           (current (car (completion-all-sorted-completions beg end)))
           (dir (or (file-name-directory (icomplete--field-string))
                    default-directory)))
      (when (stringp current)
        (let ((probe (expand-file-name (directory-file-name current)
                                       (substitute-env-vars dir))))
          (and (file-directory-p probe)
               probe))))))

(defun ofc-fido-find-file--ret ()
  "Exit minibuffer or enter the selected directory in fido file completion."
  (interactive)
  (if (ofc-fido-find-file--current-candidate-directory)
      (icomplete-force-complete)
    (icomplete-force-complete-and-exit)))

(define-key icomplete-fido-mode-map (kbd "/")
            #'ofc-fido-find-file--complete-directory-or-insert-slash)
(define-key icomplete-fido-mode-map (kbd "RET")
            #'ofc-fido-find-file--ret)
(define-key icomplete-fido-mode-map (kbd "C-m")
            #'ofc-fido-find-file--ret)

(provide 'ofc-fido-extra)

;;; ofc-fido-extra.el ends here
