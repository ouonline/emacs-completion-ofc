;; -*- lexical-binding: t -*-

(require 'ofc-token "./ofc-token.el")

(defun capf-ofc-token--completion-table (string predicate _action)
  (ofc-token--find-candidates string predicate))

(defun capf-ofc-token--get-annotation (candidate)
  (let* ((token-info (get-text-property 0 :token-info candidate))
         (buffer (car (ofc-token--token-info-s-buffer-list token-info))))
    (concat "[" (buffer-name buffer) "]")))

(defun capf-ofc-token--exit (candidate status)
  (when (memq status '(finished exact sole))
    (ofc-token--post-completion candidate)))

(defun capf-ofc-token ()
  (let* ((begin (save-excursion (skip-syntax-backward "w_") (point))))
    (list begin (point)
          (lambda (string predicate action)
            (if (eq action 'metadata)
                '(metadata (category . capf-ofc-token))
              (complete-with-action action #'capf-ofc-token--completion-table string predicate)))
          :exclusive 'no
          :category 'capf-ofc-token
          :annotation-function #'capf-ofc-token--get-annotation
          :exit-function #'capf-ofc-token--exit)))

(defun capf-ofc-token--after-change-major-mode ()
  (when (memq #'capf-ofc-token completion-at-point-functions)
    (ofc-token--after-buffer-created)))

(add-hook 'after-change-major-mode-hook
          #'capf-ofc-token--after-change-major-mode)

(defun capf-ofc-token--try-completion (_string _collection _predicate _point)
  (cons "" 0))

(defun capf-ofc-token--all-completions (string collection predicate _point)
  (let ((candidates (all-completions string collection predicate)))
    (when (and candidates
               (consp (cdr candidates)))
      (setq candidates (ofc-token--sort-candidate-list string (length string) candidates)))
    (mapc (lambda (candidate)
            (let ((matched-region-list (get-text-property 0 :matched-region-list candidate)))
              (mapc (lambda (region)
                      (add-text-properties
                       (car region) (cdr region)
                       '(face completions-common-part)
                       candidate))
                    matched-region-list)))
          candidates)))

(add-to-list 'completion-styles-alist
             '(capf-ofc-token
               capf-ofc-token--try-completion
               capf-ofc-token--all-completions
               "capf-ofc-token"))

(add-to-list 'completion-category-overrides
             '(capf-ofc-token
               (styles . (capf-ofc-token))
               (display-sort-function . identity)))

(provide 'capf-ofc-token)
