(defun duplicatep (str start end)
  (dotimes (i 3)
    (if (find (char str (+ start i))
              str :start (1+ (+ start i)) :end end)
        (return t))))

(defun start-of-packet (pathname)
  (with-open-file (str pathname :direction :input)
    (let* ((line (read-line str))
           (len (1- (length line))))    ;1- because windows newline
      (do ((i 0 (1+ i)))
          ((> (+ i 4) len))
        (let ((d? (duplicatep line i (+ i 4))))
          (unless d? (return (+ i 4)))))))))

(start-of-packet #p"day6.txt") ; => 1356 (11 bits, #x54C)
