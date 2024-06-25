(defun priority-of (c)
  ;; see the ascii table for why we choose these numbers
  (cond
    ((lower-case-p c) (- (char-code c) 96))
    (t (- (char-code c) 38))))

(defun rucksack (pathname)
  (with-open-file (str pathname :direction :input)
    (do ((sum 0)
         (seen (make-hash-table :size 52)) ;52 is the number of small and capital letters
         (sack (read-line str nil :eof)
               (read-line str nil :eof)))
        ((eql sack :eof) sum)
      (let* ((len (1- (length sack)))           ;1- becauses windows newline
             (mid (/ len 2)))
        (dotimes (i mid)
          (setf (gethash (char sack i) seen) t))
        (loop :for i :from mid :below len
              do (let ((c (char sack i)))
                   (when (gethash c seen)
                     (incf sum (priority-of c))
                     (return))))
        (clrhash seen)))))
