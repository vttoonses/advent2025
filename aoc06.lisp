(defun add-ops (ops probs)
  (if ops
    (let ((op (car ops)) (prob (car probs)))
      (cond
        ((string= op "+") (setf op '+))
        ((string= op "*") (setf op '*))
        (t (setf op (parse-integer op))))
      (cons (cons op prob) (add-ops (cdr ops) (cdr probs))))))

(defun get-problems (filename)
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
