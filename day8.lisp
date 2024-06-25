(defun ref (arr i j col?)
  (if col?
      (aref arr j i)
      (aref arr i j)))

(defun (setf ref) (val arr i j col?)
  (if col?
      (setf (aref arr j i) val)
      (setf (aref arr i j) val)))

(defun visible-in-array (arr col?)
  (macrolet ((inner-loop (max start loop-key end)
               `(loop for j from ,start ,loop-key ,end
                      for elt = (ref arr i j col?)
                      with max = (ref arr i ,max col?)
                      count (and (> elt max)
                                 (setf max elt)
                                 (null (ref *visited* i j col?))
                                 (setf (ref *visited* i j col?) t)))))
    (loop with len = (array-dimension arr 0)
          for i from 1 below (1- len)
          sum (progn (+ (inner-loop 0 1 to (- len 2))
                        (inner-loop (1- len) (- len 2) downto 1))))))

(defun visible-trees (pathname)
  (let* ((lines (uiop:read-file-lines pathname))
         (len (length (car lines)))
         (trees (make-array (list len len)))
         (*visited* (make-array (list len len) :initial-element nil)))
    (declare (special *visited*))
    (loop for i from 0 below len
          and line in lines
          do (dotimes (j len)
               (setf (aref trees i j) (digit-char-p (char line j)))))
    (+ (* (1- len) 4)                   ; border
       (visible-in-array trees nil)     ; visible in rows
       (visible-in-array trees t))))    ; visible in columns
