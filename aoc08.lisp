(ql:quickload :cl-ppcre)
(defun square (n) (* n n))

(defun distance (p1 p2)
  (sqrt (+ (square (- (car p1) (car p2)))
           (square (- (cadr p1) (cadr p2)))
           (square (- (caddr p1) (caddr p2))))))

(defun read-jboxes (filename)
  (let* ((lines (uiop:read-file-lines filename)))
    (mapcar #'(lambda (line) (mapcar #'parse-integer (ppcre:split "," line))) lines)))

(defun find-next-shortest-circuit (jbox jboxes circuits current)
  (flet ((find-shortest-for-jbox (jbox jboxes circuits current)
            (if jboxes
              (let* ((next-jbox (car jboxes)) (delta (distance jbox next-jbox)) (circuit (list delta jbox next-jbox)))
                (if (and (not (find circuit circuits :test #'equal)) (or (null current) (< delta (car current))))
                    (setf current (list delta jbox next-jbox)))
                (find-shortest-for-jbox jbox (cdr jboxes) circuits current))
              current)))
    (if jboxes
      (progn
        (setf current (find-shortest-for-jbox jbox jboxes circuits current))
        (find-next-shortest-circuit (car jboxes) (cdr jboxes) circuits current))
      current)))

(defun find-circuits (circuit circuits)
  (let ((b1-idx nil) (b2-idx nil) (jbox1 (car circuit)) (jbox2 (cadr circuit)))
    (loop for lcv from 0 below (length circuits) do
      (let ((cur (nth lcv circuits)))
        (if (find jbox1 cur :test #'equal) (setf b1-idx lcv))
        (if (find jbox2 cur :test #'equal) (setf b2-idx lcv)))
    )
    (list (cons 1 b1-idx) (cons 2 b2-idx))))

(defun distances (jboxes)
  (let ((circuits '()))
    (loop for lcv from 0 below (1- (length jboxes)) do
      (let ((start (nth lcv jboxes)) (other-boxes (subseq jboxes (1+ lcv))))
        (loop for lcv2 from 0 below (length other-boxes) do
          (let ((end (nth lcv2 other-boxes)))
            (setf circuits (cons (list (distance start end) start end) circuits))))))
    (sort circuits #'< :key #'car)))

(defun day08 (filename cycles connections)
  (let* ((jboxes (read-jboxes filename)) (shortest (distances jboxes)) (circuits '()))
    (if (not (zerop cycles)) (setf shortest (subseq shortest 0 cycles)))
    (dolist (delta-circuit shortest) do
      (let* ((circuit (cdr delta-circuit))
             (possibles (find-circuits circuit circuits))
             (p1 (cdr (car possibles)))
             (p2 (cdr (cadr possibles))))
        (if (and (null p1) (null p2))
          (setf circuits (append circuits (list circuit)))
          (if (and (not (null p1)) (null p2))
            (setf (nth p1 circuits) (append (nth p1 circuits) (cdr circuit)))
            (if (and (null p1) (not (null p2)))
              (setf (nth p2 circuits) (append (nth p2 circuits) (list (car circuit))))
              (if (and (not (null p1)) (/= p1 p2))
                (progn
                  (setf (nth p1 circuits) (append (nth p1 circuits) (nth p2 circuits)))
                  (setf circuits (remove (nth p2 circuits) circuits)))))))))
    (reduce #'* (subseq (sort (mapcar #'length circuits) #'>) 0 connections))))

(defun day08p2 (filename)
  (let* ((jboxes (read-jboxes filename)) (c (length jboxes)) (shortest (distances jboxes)) (circuits '()))
    (dolist (delta-circuit shortest) do
      (let* ((circuit (cdr delta-circuit))
             (possibles (find-circuits circuit circuits))
             (p1 (cdr (car possibles)))
             (p2 (cdr (cadr possibles))))
        (if (and (null p1) (null p2))
          (setf circuits (append circuits (list circuit)))
          (if (and (not (null p1)) (null p2))
            (setf (nth p1 circuits) (append (nth p1 circuits) (cdr circuit)))
            (if (and (null p1) (not (null p2)))
              (setf (nth p2 circuits) (append (nth p2 circuits) (list (car circuit))))
              (if (and (not (null p1)) (/= p1 p2))
                (progn
                  (setf (nth p1 circuits) (append (nth p1 circuits) (nth p2 circuits)))
                  (setf circuits (remove (nth p2 circuits) circuits)))))))
        (if (or (and p1 (= (length (nth p1 circuits)) c)) (and p2 (= (length (nth p2 circuits)) c)))
          (let ((x1 (caar circuit)) (x2 (caadr circuit))) (format t "~a * ~a = ~a~%" x1 x2 (* x1 x2)) (return-from day08p2)))))))
