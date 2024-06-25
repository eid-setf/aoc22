(defun most-cals (pathname)
  (with-open-file (str pathname :direction :input)
    (let ((accum (make-array 3 :initial-element 0 :fill-pointer 1 :adjustable t))
          (i 0))
      (do ((line (read-line str nil :eof)
                 (read-line str nil :eof)))
          ((eql line :eof))
        (let ((len (1- (length line)))) ;1- because windows newline
          (cond
            ((zerop len)
             (incf i)
             (vector-push-extend 0 accum))
            (t (incf (aref accum i) (parse-integer line :end len))))))
      (aref (sort accum #'>) 0))))
