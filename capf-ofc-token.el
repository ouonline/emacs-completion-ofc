;; -*- lexical-binding: t -*-

(require 'ofc-token)

(defun capf-ofc-token--candidates (string predicate _action)
  (ofc-token--find-candidates string predicate))

(defun capf-ofc-token--get-annotation (candidate)
  (let* ((token-info (get-text-property 0 :ofc-token-info candidate))
         (buffer (car (ofc-token--token-info-s-buffer-list token-info))))
    (concat " [" (buffer-name buffer) "]")))

(defun capf-ofc-token--exit (candidate status)
  (when (memq status '(finished exact sole))
    (ofc-token--post-completion candidate)))

(defun capf-ofc-token--completion-table (string predicate action)
  (cond
   ((eq action 'metadata)
    '(metadata (category . capf-ofc-token)
               (display-sort-function . identity)
               (cycle-sort-function . identity)
               (annotation-function . capf-ofc-token--get-annotation)))
   ;; the whole prefix is a single field, there is no sub-field boundary.
   ;; returning nil here keeps `completion-boundaries' from querying the
   ;; candidates, which it would discard anyway.
   ((eq (car-safe action) 'boundaries) nil)
   ;; `test-completion': STRING is a valid completion only if it is an
   ;; indexed token itself, not merely a prefix matching some candidates.
   ((eq action 'lambda)
    (and (gethash string ofc-token--token-hash)
         (or (null predicate) (funcall predicate string))
         t))
   ;; `try-completion' and `all-completions'
   (t
    (complete-with-action
     action
     #'capf-ofc-token--candidates
     string
     predicate))))

(defun capf-ofc-token ()
  (let ((range (ofc-token--grab-prefix-range)))
    (when range
      (list (car range) (cdr range)
            #'capf-ofc-token--completion-table
            :exclusive 'no
            :exit-function #'capf-ofc-token--exit))))

(defun capf-ofc-token--after-change-major-mode ()
  (when (memq #'capf-ofc-token completion-at-point-functions)
    (ofc-token--after-buffer-created)))

(add-hook 'after-change-major-mode-hook
          #'capf-ofc-token--after-change-major-mode)

(defun capf-ofc-token--try-completion (_string _collection _predicate _point)
  (cons "" 0))

(defun capf-ofc-token--hilit (candidate)
  "Highlight the matched regions of CANDIDATE in place and return it."
  (let ((matched-regions (get-text-property 0 :ofc-matched-regions candidate)))
    (mapc (lambda (region)
            (add-face-text-property
             (car region) (cdr region)
             'completions-common-part
             nil candidate))
          matched-regions))
  candidate)

(defun capf-ofc-token--all-completions (string collection predicate _point)
  (let ((candidates (all-completions string collection predicate)))
    (if completion-lazy-hilit
        ;; the frontend will call this function later, only for candidates
        ;; that are actually displayed
        (setq completion-lazy-hilit-fn #'capf-ofc-token--hilit)
      ;; frontends without lazy highlighting support render eagerly
      (mapc #'capf-ofc-token--hilit candidates))
    candidates))

(add-to-list 'completion-styles-alist
             '(capf-ofc-token
               capf-ofc-token--try-completion
               capf-ofc-token--all-completions
               "capf-ofc-token"))

(add-to-list 'completion-category-overrides
             '(capf-ofc-token
               (styles . (capf-ofc-token))))

(provide 'capf-ofc-token)
