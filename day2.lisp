;;;; -- Utilities --
;;; obj must be in lst
(defun position-circular (obj lst)
  (cond
    ((eql (car lst) obj) 0)
    (t (1+ (position-circular obj (cdr lst))))))
;;;; -- Utilities --

(defvar *plays* (list 'rock 'paper 'scissors))
(setf (cdr (last *plays*)) *plays*)

(defun player-table (char)
  (case char
    (#\X 'rock)
    (#\Y 'paper)
    (#\Z 'scissors)))

(defun enemy-table (char)
  (case char
    (#\A 'rock)
    (#\B 'paper)
    (#\C 'scissors)))

(defun total-score (pathname)
  (with-open-file (str pathname :direction :input)
    (let ((score 0))
      (do ((line (read-line str nil :eof)
                 (read-line str nil :eof)))
          ((eql line :eof) score)
        (let* ((en (enemy-table (char line 0)))
               (pl (player-table (char line 2)))
               (play-score (1+ (position-circular pl *plays*)))) ; 1+ because position-circular value is 0 indexed
          (cond
            ((eql en pl)                ; draw
             (incf score (+ play-score 3)))
            ((eql (cadr (member en *plays*)) pl) ; win
             (incf score (+ play-score 6)))
            (t (incf score play-score)))))))) ; lose
