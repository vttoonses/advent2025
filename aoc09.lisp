(ql:quickload :cl-ppcre)

(defun read-squares (filename)
  (let* ((squares (uiop:read-file-lines filename)))
    (mapcar #'(lambda (square) (mapcar #'parse-integer (ppcre:split "," square))) squares)))

(defun x (p) (car p))
(defun y (p) (cadr p))

(defun day09 (filename)
  (flet ((area (s1 s2) (* (1+ (abs (- (x s1) (x s2)))) (1+ (abs (- (y s1) (y s2)))))))
    (let ((squares (read-squares filename)) (largest 0))
      (loop for lcv from 0 below (1- (length squares)) do
        (loop for lcv2 from (1+ lcv) below (length squares) do
          (let* ((p1 (nth lcv squares)) (p2 (nth lcv2 squares)) (tmp (area p1 p2)))
            (if (> tmp largest) (setf largest tmp)))))
      largest)))

(defun row-boundaries (squares)
  (let ((rows '()))
    (loop for square in squares do
      (let* ((pos (position-if #'(lambda (x) (= (car x) (y square))) rows)) (row (if pos (nth pos rows) (list (y square)))))
        (if (null (find (x square) (cdr row))) (setf row (cons (car row) (cons (x square) (cdr row)))))
        (if pos (setf (nth pos rows) row) (setf rows (cons row rows)))))
    (loop for lcv from 0 below (length rows) do
      (let ((row (nth lcv rows))) (setf (nth lcv rows) (cons (car row) (sort (cdr row) #'<)))))
    (sort rows #'< :key #'car)))

(defun define-boundaries (squares)
  (let ((boundaries '()))
    (loop for lcv from 0 below (length squares) do
      (let* ((p1 (nth lcv squares)) (p2idx (if (= lcv (1- (length squares))) 0 (1+ lcv))) (p2 (nth p2idx squares)))
        (if (/= (x p1) (x p2))
          (loop for lcv2 from (min (x p1) (x p2)) to (max (x p1) (x p2)) do (setf boundaries (cons (list lcv2 (y p1)) boundaries)))
          (loop for lcv2 from (min (y p1) (y p2)) to (max (y p1) (y p2)) do (setf boundaries (cons (list (x p1) lcv2) boundaries))))))
    boundaries))

(defun inside-p (pt row-bounds)
  (let ((row (find (y pt) row-bounds :key #'car)) (x-pt (x pt)))
    (cond
      ((null row) nil)
      ((find x-pt (cdr row)) t)
      (t (let ((count 0) (lcv 0) (pnts (cdr row)))
            (loop while (and (< lcv (length pnts)) (> x-pt (nth lcv pnts))) do (incf lcv))
            (loop while (< lcv (length pnts)) do
              (incf count)
              (loop while (and (< lcv (1- (length pnts))) (= (1+ (nth lcv pnts)) (nth (1+ lcv) pnts))) do (incf lcv))
              (incf lcv))
            (oddp count))))))

(defun rect-boundaries (p1 p2)
  (let ((boundaries '()))
    (loop for lcv from (min (x p1) (x p2)) to (max (x p1) (x p2)) do
      (setf boundaries (cons (list lcv (y p1)) (cons (list lcv (y p2)) boundaries))))
    (loop for lcv from (min (y p1) (y p2)) to (max (y p1) (y p2)) do
      (setf boundaries (cons (list (x p1) lcv) (cons (list (x p2) lcv) boundaries))))
    boundaries))

(defun rect-inside-p (rect boundaries)
  (if rect
    (if (not (inside-p (car rect) boundaries)) nil (rect-inside-p (cdr rect) boundaries))
    t))

(defun part2 (filename)
  (flet ((area (s1 s2) (* (1+ (abs (- (x s1) (x s2)))) (1+ (abs (- (y s1) (y s2)))))))
    (let* ((squares (read-squares filename)) (largest 112592935) (bounds (define-boundaries squares)) (row-bounds (row-boundaries bounds)))
      (loop for lcv from 0 below (1- (length squares)) do
        (loop for lcv2 from (1+ lcv) below (length squares) do
          (let* ((p1 (nth lcv squares)) (p2 (nth lcv2 squares)) (rect (rect-boundaries p1 p2)) (tmp (area p1 p2)))
            (if (and (> tmp largest) (rect-inside-p rect row-bounds)) (format t "LARGEST: ~a~%" (setf largest (max largest tmp))))))
      largest))))
