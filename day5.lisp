(defun get-ints (line)
  (let (accum start (end 5))
    (dotimes (i 3)
      (setf start (position-if #'digit-char-p line :start end))
      (setf end (position-if-not #'digit-char-p line :start start))
      (push (parse-integer line :start start :end end) accum))
    (nreverse accum)))

(defun move (n from to)
  (dotimes (i n)
    (push (pop (svref *stacks* (1- from))) (svref *stacks* (1- to)))))

(defun stack (pathname)
  (with-open-file (str pathname :direction :input)
    (do* ((line (read-line str nil :eof)
                (read-line str nil :eof))
          (move? nil)
          (len (1- (length line)))      ;1- because windows newline
          (n (/ (1+ len) 4))
          (*stacks* (make-array n :initial-element nil)))
         ((eql line :eof) (map 'string #'car *stacks*))
      (declare (special *stacks*))
      (cond
        (move? (apply #'move (get-ints line)))
        ((= 0 (1- (length line)))       ;again windows newline
         (setf move? t)
         (map-into *stacks* #'nreverse *stacks*))
        (t (dotimes (i n)
             (let ((eltn (1+ (* i 4))))
               (if (alpha-char-p (char line eltn))
                   (push (char line eltn) (svref *stacks* i))))))))))
