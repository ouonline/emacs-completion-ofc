;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ofc-token)

(defvar company-ofc-token--current-candidates '())

(defun company-ofc-token (command &optional arg &rest _ignored)
  (interactive (list 'interactive))
  (cl-case command
    (init (ofc-token--after-buffer-created))
    (prefix (let ((range (ofc-token--grab-prefix-range)))
              (when range
                (let ((prefix (buffer-substring-no-properties (car range) (cdr range))))
                  (setq company-ofc-token--current-candidates (ofc-token--find-candidates prefix))
                  (when company-ofc-token--current-candidates
                    prefix)))))
    (candidates company-ofc-token--current-candidates)
    (match (get-text-property 0 :matched-region-list arg))
    (annotation (let* ((token-info (get-text-property 0 :token-info arg))
                       (buffer (car (ofc-token--token-info-s-buffer-list token-info))))
                  (concat "[" (buffer-name buffer) "]")))
    (post-completion (ofc-token--post-completion arg))
    (require-match 'never)
    (sorted t) ;; tell company not to sort the result again
    (no-cache t)))

(provide 'company-ofc-token)
