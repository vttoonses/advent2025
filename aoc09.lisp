(ql:quickload :cl-ppcre)

(defun read-squares (filename)
  (let* ((squares (uiop:read-file-lines filename)))
    (mapcar #'(lambda (square) (mapcar #'parse-integer (ppcre:split "," square))) squares)))

(defun day09 (filename)
  (flet ((area (s1 s2) (* (1+ (abs (- (car s1) (car s2)))) (1+ (abs (- (cadr s1) (cadr s2)))))))
    (let ((squares (read-squares filename)) (largest 0))
      (loop for lcv from 0 below (1- (length squares)) do
        (loop for lcv2 from (1+ lcv) below (length squares) do
          (let* ((p1 (nth lcv squares)) (p2 (nth lcv2 squares)) (tmp (area p1 p2)))
            (if (> tmp largest) (setf largest tmp)))))
      largest)))
