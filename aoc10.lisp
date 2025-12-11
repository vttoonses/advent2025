(ql:quickload :cl-ppcre)

(defun lights (raw)
  (let ((display '()))
    (loop for c across raw do
      (cond
        ((char= c #\#) (setf display (cons 1 display)))
        ((char= c #\.) (setf display (cons 0 display)))
        (t)))
    (reverse display)))

(defun buttons (button-list count)
  (let ((matrix '()) (display '()))
    (dolist (button button-list) do
      (setf display '())
      (dotimes (x count) (setf display (cons 0 display)))
      (let ((circuits (mapcar #'parse-integer (ppcre:split "," (subseq button 1 (1- (length button)))))))
        (dolist (circuit circuits) do
          (setf (nth circuit display) 1))
        (setf matrix (cons display matrix))))
    (reverse matrix)))

(defun parse-schematic (schematic)
  (let* ((parts (ppcre:split "\\s" schematic)) (display (lights (car parts))))
    (list display (buttons (subseq parts 1 (1- (length parts))) (length display)))))

(defun read-schematics (filename)
  (let* ((schematics (uiop:read-file-lines filename)))
    (mapcar #'parse-schematic schematics)))

(defun powerset (lst)
  (if lst (mapcan (lambda (el) (list (cons (car lst) el) el))
                (powerset (cdr lst)))
      '(())))

(defun process-powerset (lst)
  (reverse (remove nil (powerset lst))))

(defun push-buttons (pushes)
  (cond
    ((null pushes) '(0 ()))
    ((= (length pushes) 1) (list 1 (car pushes)))
    (t (let ((results (car pushes)) (ops (cdr pushes)))
      (dolist (op ops) do
        (setf results (mapcar #'(lambda (x y) (if (/= x y) 1 0)) results op)))
      (list (length pushes) results)))))

(defun fewest-pushes (results target)
  (caar (sort (remove-if-not #'(lambda (x) (equal (cadr x) target)) results) #'< :key #'car)))
  
(defun process-machine (lst target)
  (fewest-pushes (mapcar #'push-buttons (process-powerset lst)) target))

(defun day10 (filename)
  (let ((schematics (read-schematics filename)))
    (reduce #'+ (mapcar #'(lambda (lst) (process-machine (cadr lst) (car lst))) schematics))))
