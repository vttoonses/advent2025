(defun add-ops (ops probs)
  (if ops
    (let ((op (car ops)) (prob (car probs)))
      (cond
        ((string= op "+") (setf op '+))
        ((string= op "*") (setf op '*))
        (t (setf op (parse-integer op))))
      (cons (cons op prob) (add-ops (cdr ops) (cdr probs))))))

(defun solve-human-problems (filename)
  (let ((problems '()))
    (with-open-file (in filename :direction :input)
      (when in
        (loop for line = (read-line in nil :eof)
          until (eq line :eof) do
            (let ((ops (tokenizer line)))
              (if problems
                (setf problems (add-ops ops problems))
                (setf problems (mapcar #'(lambda (o) (list (parse-integer o))) ops)))))))
    (reduce #'+ (mapcar #'eval problems))))

; Just so I don't have to update data sizes beween test and runtime data
(defvar *last-row* 0)

(defun get-problem-boundries (problems idx)
  (let ((end (+ idx 1)) (ops-array (elt problems *last-row*)))
    (loop while (and (< end (length ops-array)) (char= (char ops-array end) #\Space))
      do (incf end))
    (if (= end (length ops-array)) end (- end 1))))

(defun solve-problem (problems idx end)
  (let ((problem '()))
    (loop for lcv from idx below end do
      (let ((num ""))
        (loop for lcv2 from 0 below *last-row* do
          (setf num (concatenate 'string num (string (char (elt problems lcv2) lcv)))))
        (setf problem (cons (parse-integer num) problem))))
    (eval (cons (if (char= #\+ (char (elt problems *last-row*) idx)) '+ '*) problem))))

(defun solve-problems (problems idx)
  (let ((last-index (- (length (elt problems *last-row*)) 1)))
    (if (< idx last-index)
      (let* ((offset (get-problem-boundries problems idx)) (solution (solve-problem problems idx offset)))
        (setf idx (+ offset 1))
        (cons solution (solve-problems problems idx)))
      '())))

(defun solve-cephalopod-problems (filename)
  (let ((problems (make-array 5 :fill-pointer 0)))
    (with-open-file (in filename :direction :input)
      (when in
        (loop for line = (read-line in nil :eof)
          until (eq line :eof) do
            (vector-push line problems))))
    (setf *last-row* (- (length problems) 1))
    (reduce #'+ (solve-problems problems 0))))
