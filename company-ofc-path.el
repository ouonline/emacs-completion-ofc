;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ofc-path)

;; (parent-dir . entry), updated when `prefix' is called
(defvar company-ofc-path--prefix-info '())

(defun company-ofc-path (command &optional arg &rest _ignored)
  (interactive (list 'interactive))
  (cl-case command
    (prefix (let ((info (ofc-path--grab-prefix)))
              (setq company-ofc-path--prefix-info info)
              (cdr info)))
    (candidates (ofc-path--find-candidates company-ofc-path--prefix-info))
    (post-completion (progn
                       (ofc-path--post-completion)
                       (when (string-suffix-p "/" arg)
                         (company-manual-begin))))
    (require-match 'never)
    (sorted t)
    (no-cache t)))

(provide 'company-ofc-path)
