(defstruct (dir (:print-function print-dir))
  parent name (size 0) (children nil))

(defun print-dir (dir stream depth)
  (declare (ignore depth))
  (format stream "#<~A>" (dir-name dir)))

(defun read-command (cmd len)
  (when (char= (char cmd 2) #\c)
    (let ((name (subseq cmd 5 len)))
      (if (string= name "..")
          (setf *current-dir* (dir-parent *current-dir*))
          (setf *current-dir* (find name (dir-children *current-dir*)
                                    :key #'dir-name :test #'string=))))))

(defun add-child (child-name)
  (let ((child (make-dir :parent *current-dir* :name child-name)))
    (push child (dir-children *current-dir*))
    child))

(defun add-contents (ls-line len)
  (let* ((space (position #\space ls-line))
         (sord (subseq ls-line 0 space))
         (name (subseq ls-line (1+ space) len)))
    (case (char ls-line 0)
      (#\d (add-child name))
      (t (incf (dir-size *current-dir*) (parse-integer sord))))))

(defun set-whole-size (dir)
  (reduce #'+ (dir-children dir)
          :key (lambda (dir)
                 (setf (dir-size dir)
                       (set-whole-size dir)))
          :initial-value (dir-size dir)))

(defun sum-of-dir-sizes (dir)
  (let ((sum 0))
    (labels ((S (dir)
               (if dir
                   (let ((size (dir-size dir)))
                     (when (<= size 100000)
                       (incf sum size))
                     (dolist (x (dir-children dir))
                       (S x))))))
      (S dir))
    sum))

(defun freeable-space (pathname)
  (with-open-file (str pathname :direction :input)
    (read-line str)                     ;first line is always "$ cd /"
    (let ((root (make-dir :parent nil :name "/")))
      (do ((*current-dir* root)
           (line (read-line str nil :eof)
                 (read-line str nil :eof)))   
          ((eql line :eof))
        (declare (special *current-dir*))
        (let ((len (1- (length line)))) ;1- because windows newline
          (case (char line 0)
            (#\$ (read-command line len))
            (t (add-contents line len)))))
      (set-whole-size root)
      (sum-of-dir-sizes root))))