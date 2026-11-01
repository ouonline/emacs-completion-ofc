;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ofc-common)

;; -----------------------------------------------------------------------------
;; settings

(defconst ofc-path-token-charset "0-9a-zA-Z_/\.~-")
(defconst ofc-path-token-pattern (concat "[" ofc-path-token-charset "]+"))

;; -----------------------------------------------------------------------------
;; global variales

;; each element is `(list dir entry-list)'
(defvar ofc-path--entry-cache '())

;; -----------------------------------------------------------------------------

(defun ofc-path--grab-prefix-info ()
  (when (looking-back ofc-path-token-pattern (line-beginning-position) t)
    (let* ((prefix (match-string-no-properties 0))
           (parent-dir nil)
           (entry nil))
      (when (string-match-p "/" prefix)
        (if (eq (aref prefix 0) ?/)
            ;; absolute path
            (progn
              (setq parent-dir (file-name-directory prefix))
              (setq entry (file-name-nondirectory prefix)))
          ;; relative path
          (if (eq (aref prefix 0) ?~)
              ;; started with `~'
              (let ((prefix-len (length prefix)))
                ;; ~: do nothing
                ;; ~...
                (unless (= prefix-len 1)
                  (if (eq (aref prefix 1) ?/)
                      ;; ~/...: replaced by $HOME/...
                      (let ((real-prefix (concat (getenv "HOME")
                                                 (substring-no-properties prefix 1))))
                        (setq parent-dir (file-name-directory real-prefix))
                        (setq entry (file-name-nondirectory prefix)))
                    ;; ~...: $PWD/~...
                    (let ((relative-parent-dir (file-name-directory prefix)))
                      (setq parent-dir (concat default-directory relative-parent-dir))
                      (setq entry (file-name-nondirectory prefix))))))
            ;; started with non `~'
            (let ((relative-parent-dir (file-name-directory prefix)))
              (setq parent-dir (concat default-directory relative-parent-dir))
              (setq entry (file-name-nondirectory prefix))))))
      (cons parent-dir entry))))

(defun ofc-path--find-entry-list-in-cache (dir)
  (cl-dolist (item ofc-path--entry-cache)
    (when (string= (car item) dir)
      (cl-return (cadr item)))))

(defun ofc-path--filter-entry-list (parent-dir entry-list)
  (let ((result '()))
    (mapc (lambda (entry)
            (unless (or (string= entry ".")
                        (string= entry ".."))
              (let ((full-path (concat parent-dir entry)))
                (if (file-directory-p full-path)
                    (push (concat entry "/") result)
                  (push entry result)))))
          entry-list)
    result))

(defun ofc-path--get-entry-list (parent-dir)
  (let ((entry-list (ofc-path--find-entry-list-in-cache parent-dir)))
    (when (and (not entry-list)
               (file-directory-p parent-dir))
      (let ((current-entry-list (directory-files parent-dir)))
        (when current-entry-list
          (setq entry-list (ofc-path--filter-entry-list
                            parent-dir
                            current-entry-list))
          (when entry-list
            (push (list parent-dir entry-list) ofc-path--entry-cache)))))
    entry-list))

(defun ofc-path--find-candidates (prefix-info)
  (let ((parent-dir (car prefix-info)))
    (when parent-dir
      (let ((downcased-input (downcase (cdr prefix-info)))
            (entry-list (ofc-path--get-entry-list parent-dir)))
        (when entry-list
          (let ((downcased-input-length (length downcased-input))
                (entry-result '()))
            (dolist (entry entry-list)
              (when (ofc--fuzzy-compare downcased-input downcased-input-length
                                        (downcase entry) (length entry))
                (push entry entry-result)))
            entry-result))))))

(defun ofc-path--post-completion ()
  (when (eq (char-before) ?/)
    (delete-char -1))
  (setq ofc-path--entry-cache '()))

(provide 'ofc-path)
