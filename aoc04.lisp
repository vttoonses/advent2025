
(defun get-diagram (filename)
  (let ((diagram (make-array 5 :fill-pointer 0 :adjustable t)))
    (with-open-file (in filename :direction :input)
      (when in
        (loop for line = (read-line in nil :eof)
          until (eq line :eof) do
            (vector-push-extend line diagram))))
    diagram))

(defun check-cell (diagram rows cols row col val)
  (if (or (< row 0) (< col 0) (>= row rows) (>= col cols))
    (values nil nil)
    (let ((cell-val (char (elt diagram row) col)))
      (values (char= val cell-val) t))))

(defun count-neighbor (diagram rows cols row col val)
  (multiple-value-bind (match valid) (check-cell diagram rows cols row col val)
    (if (and match valid) 1 0)))

(defun count-neighbors (diagram rows cols val neighbors)
  (if (null neighbors) 0
    (let ((row (first (first neighbors))) (col (second (first neighbors))))
      (+ (count-neighbor diagram rows cols row col val) (count-neighbors diagram rows cols val (rest neighbors))))))

(defun generate-neighbor-list (row col)
  (list (list (- row 1) (- col 1)) (list (- row 1) col) (list (- row 1) (+ col 1))
    (list row (- col 1)) (list row (+ col 1))
    (list (+ row 1) (- col 1)) (list (+ row 1) col) (list (+ row 1) (+ col 1))))

(defun count-accessable (diagram rows cols clean)
  (let ((total 0))
    (loop for row from 0 below rows do
      (loop for col from 0 below cols do
        (if (char= #\@ (char (elt diagram row) col))
          (let* ((neighbors (generate-neighbor-list row col)))
            (if (< (count-neighbors diagram rows cols #\@ neighbors) 4)
              (progn
                (if clean (setf (char (elt diagram row) col) #\.))
                (incf total)))))))
    total))

(defun clean-warehouse (diagram rows cols)
  (let ((pass-total (count-accessable diagram rows cols t)))
    (if (= pass-total 0) 0
        (+ pass-total (clean-warehouse diagram rows cols)))))

(let* ((diagram (get-diagram "aoc04.txt")) (rows (length diagram)) (cols (length (elt diagram 0))))
  (format t "Part 1 Total: ~a~%" (count-accessable diagram rows cols nil)))

(let* ((diagram (get-diagram "aoc04.txt")) (rows (length diagram)) (cols (length (elt diagram 0))))
  (format t "Part 2 Total: ~a~%" (clean-warehouse diagram rows cols)))
