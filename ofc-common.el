;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defun ofc--fuzzy-compare (pattern pattern-length text text-length &optional matched-hook-func)
  "tells if `pattern` is part of `text`.

MATCHED-HOOK-FUNC, when given, is called as (MATCHED-HOOK-FUNC TEXT-IDX
PATTERN-IDX) once per matched character pair, in ascending order of both
indexes."
  (when (<= pattern-length text-length)
    (let ((text-idx 0)
          (pattern-idx 0))
      (while (and (< text-idx text-length)
                  (< pattern-idx pattern-length))
        (when (eq (aref text text-idx) (aref pattern pattern-idx))
          (when matched-hook-func
            (funcall matched-hook-func text-idx pattern-idx))
          (cl-incf pattern-idx))
        (cl-incf text-idx))
      (= pattern-idx pattern-length))))

(provide 'ofc-common)
