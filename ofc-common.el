;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defun ofc--fuzzy-compare (pattern pattern-length text text-length &optional matched-hook-func)
  "tells if `pattern` is part of `text`."
  (when (<= pattern-length text-length)
    (let ((text-idx 0)
          (pattern-idx 0))
      (while (and (< text-idx text-length)
                  (< pattern-idx pattern-length))
        (when (eq (elt text text-idx) (elt pattern pattern-idx))
          (when matched-hook-func
            (funcall matched-hook-func text-idx))
          (cl-incf pattern-idx))
        (cl-incf text-idx))
      (= pattern-idx pattern-length))))

(defun ofc--do-calc-edit-distance (a a-len b b-len)
  (let ((a-seq (number-sequence 1 a-len))
        (b-seq (number-sequence 1 b-len))
        (dis-vec (make-vector (+ 1 b-len) 0)))
    (cl-dolist (i b-seq)
      (aset dis-vec i i))
    (cl-dolist (i a-seq)
      (let ((old (- i 1)))
        (aset dis-vec 0 i)
        (cl-dolist (j b-seq)
          (let ((tmp (aref dis-vec j)))
            (if (eq (elt a (- i 1))
                    (elt b (- j 1)))
                (aset dis-vec j old)
              (aset dis-vec j (+ 1 (min (aref dis-vec j)
                                        (aref dis-vec (- j 1))
                                        old))))
            (setq old tmp)))))
    (aref dis-vec b-len)))

(defun ofc--calc-edit-distance (a a-len b b-len)
  (if (< a-len b-len)
      (ofc--do-calc-edit-distance b b-len a a-len)
    (ofc--do-calc-edit-distance a a-len b b-len)))

(provide 'ofc-common)
