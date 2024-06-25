(defun betweenp (x y)
  (or (<= (car y) (car x) (cdr x) (cdr y))
      (<= (car x) (car y) (cdr y) (cdr x))))

(defun camp-cleanup (pathname)
  (with-open-file (str pathname :direction :input)
    (do ((sum 0)
         (pair (read-line str nil :eof)
               (read-line str nil :eof)))
        ((eql pair :eof) sum)
      (let* ((len (1- (length pair)))   ;1- because windows newline
             (dash (position #\- pair))
             (comma (position #\, pair :start dash))
             (p1 (cons (parse-integer
                        (subseq pair 0 dash))
                       (parse-integer
                        (subseq pair (1+ dash) comma))))
             (dash (position #\- pair :start comma))
             (p2 (cons (parse-integer
                        (subseq pair (1+ comma) dash))
                       (parse-integer
                        (subseq pair (1+ dash) len)))))
        (and (betweenp p1 p2)
             (incf sum))))))
