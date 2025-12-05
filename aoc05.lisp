(defun tokenizer (string &key (delimiterp #'(lambda (char) (char= char #\-))))
    (loop :for beg = (position-if-not delimiterp string)
        :then (position-if-not delimiterp string :start (1+ end))
        :for end = (and beg (position-if delimiterp string :start beg))
        :when beg :collect (subseq string beg end)
        :while end))

(defun check-freshness (id ranges)
  (if (null ranges) nil
    (let* ((range (first ranges)) (low (parse-integer (first range))) (high (parse-integer (second range))))
      (if (and (>= id low) (<= id high)) t (check-freshness id (cdr ranges))))))

(defun count-fresh-ingredients (filename)
  (let ((total 0) (check-ingredient nil) (ranges '()))
    (with-open-file (in filename :direction :input)
      (when in
        (loop for line = (read-line in nil :eof)
          until (eq line :eof) do
            (if (zerop (length line)) (setf check-ingredient t)
              (if (null check-ingredient)
                (setf ranges (cons (tokenizer line) ranges))
                (incf total (if (check-freshness (parse-integer line) ranges) 1 0)))))))
    total))

(defun remove-overlap (r1 r2)
  (let ((r1l (first r1)) (r1h (second r1)) (r2l (first r2)) (r2h (second r2)))
    (if (or (> r1l r2h) (< r1h r2l)) r1
      (if (and (>= r1l r2l) (<= r1h r2h)) nil
        (if (>= r1l r2l) (list (+ r2h 1) r1h) (list r1l (- r2l 1)))))))

(defun rip-it (range ranges)
  (if (null ranges) range
    (let ((result (remove-overlap range (first ranges))))
      (if (null result) nil (rip-it result (rest ranges))))))
      
(defun remove-overlaps (range ranges)
  (if (null ranges) (list range)
    (let ((clean-range (rip-it range ranges)))
      (if (null clean-range) (remove-overlaps (first ranges) (rest ranges))
        (cons clean-range (remove-overlaps (first ranges) (rest ranges)))))))

(defun count-possible-ingredients (ranges)
  (if (null ranges) 0
    (let* ((cur (first ranges)) (low (first cur)) (high (second cur)))
      (+ (+ (- high low) 1) (count-possible-ingredients (rest ranges))))))

(defun count-possible-fresh-ingredients (filename)
  (let ((ranges '()))
    (with-open-file (in filename :direction :input)
      (when in
        (loop for line = (read-line in nil :eof)
          until (or (eq line :eof) (zerop (length line))) do
            (let ((toks (tokenizer line)))
              (setf ranges (cons (list (parse-integer (first toks)) (parse-integer (second toks))) ranges))))))
    (let* ((sorted (sort ranges #'(lambda (r1 r2) (< (- (second r1) (first r1)) (- (second r2) (first r2))))))
      (clean-ranges (remove-overlaps (first sorted) (rest sorted))))
        (count-possible-ingredients clean-ranges))))
