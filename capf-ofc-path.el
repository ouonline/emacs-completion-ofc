;; -*- lexical-binding: t -*-

(require 'ofc-path)

(defun capf-ofc-path ()
  (let ((prefix-info (ofc-path--grab-prefix-info)))
    (when prefix-info
      (list (- (point) (length (cdr prefix-info)))
            (point)
            (lambda (string _candidate _action)
              (ofc-path--find-candidates (cons (car prefix-info) string)))
            :exclusive 'no
            :exit-function (lambda (_candidate _status) (ofc-path--post-completion))))))

(provide 'capf-ofc-path)
