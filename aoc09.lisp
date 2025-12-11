(ql:quickload :cl-ppcre)

(defun read-squares (filename)
  (let* ((squares (uiop:read-file-lines filename)))
    (mapcar #'(lambda (square) (mapcar #'parse-integer (ppcre:split "," square))) squares)))

(defun x (p) (car p))
(defun y (p) (cadr p))
(defun area (s1 s2) (* (1+ (abs (- (x s1) (x s2)))) (1+ (abs (- (y s1) (y s2))))))
(defun compare-two-squares (s1 s2) (if (/= (car s1) (car s2)) (< (car s1) (car s2)) (< (cadr s1) (cadr s2))))

(defun day09 (filename)
  (let ((squares (read-squares filename)) (largest 0))
    (loop for lcv from 0 below (1- (length squares)) do
      (loop for lcv2 from (1+ lcv) below (length squares) do
        (let* ((p1 (nth lcv squares)) (p2 (nth lcv2 squares)) (tmp (area p1 p2)))
          (if (> tmp largest) (setf largest tmp)))))
    largest))

(defun collect-edges (squares)
  (if (>= (length squares) 2) (let ((s1 (car squares)) (s2 (cadr squares))) (cons (sort (list s1 s2) #'compare-two-squares) (collect-edges (cdr squares)))) '()))

(defun collect-areas (squares)
  (flet ((collect-areas-for-square (s1 squares)
          (if squares (let* ((s2 (car squares)) (tmp-rect (sort (list s1 s2) #'compare-two-squares))) (cons (list (area s1 s2) tmp-rect) (collect-areas-for-square s1 (cdr squares)))) '())))
    (if squares 
      (let ((tmp (collect-areas-for-square (car squares) (cdr squares))))
        (append tmp (collect-areas (cdr squares))))
      '())))

(defun no-collisions-p (x1 y1 x2 y2 edges)
  (dolist (edge edges) do
    (let* ((p1 (car edge)) (p2 (cadr edge)) (x3 (x p1)) (y3 (y p1)) (x4 (x p2)) (y4 (y p2)))
      (if (and (> x4 x1) (< x3 x2) (> y4 y1) (< y3 y2)) (return-from no-collisions-p nil))))
  t)

(defun day09.02 (filename)
  (let* (
    (squares (read-squares filename))
    (closed  (append squares (list (car squares))))
    (edges (sort (collect-edges closed) #'> :key #'(lambda (edge) (area (car edge) (cadr edge)))))
    (areas (sort (collect-areas squares) #'> :key #'car)))
      (dolist (a areas) do
        (let* ((points (cadr a)) (p1 (car points)) (p2 (cadr points))
              (x1 (min (x p1) (x p2)))
              (x2 (max (x p1) (x p2)))
              (y1 (min (y p1) (y p2)))
              (y2 (max (y p1) (y p2))))
          (if (no-collisions-p x1 y1 x2 y2 edges) (return-from day09.02 (car a)))))))
