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

(format t "Fresh Ingredient Count: ~a~%" (count-fresh-ingredients "working.txt"))
