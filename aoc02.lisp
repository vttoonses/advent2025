; Helper funcs
(defun tokenizer (string &key (delimiterp #'whitespace-p))
    (loop :for beg = (position-if-not delimiterp string)
        :then (position-if-not delimiterp string :start (1+ end))
        :for end = (and beg (position-if delimiterp string :start beg))
        :when beg :collect (subseq string beg end)
        :while end))

(defun whitespace-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return)))

; Part One
(defun check-range (range)
    (let* ((toks (tokenizer range :delimiterp #'(lambda (char) (char= char #\-)))) (beg (car toks)) (end (car (cdr toks))) (total 0))
        (loop for lcv from (parse-integer beg) to (parse-integer end) do
            (let* ((str-num (write-to-string lcv)) (str-len (length str-num)) (half (truncate str-len 2)) (first-half (subseq str-num 0 half)) (second-half (subseq str-num half)))
                (if (string= first-half second-half) (incf total lcv))))
        total))

(defun check-ranges (ranges)
    (if (null ranges) 0 (+ (check-range (car ranges)) (check-ranges (cdr ranges)))))

(with-open-file (in "aoc02.txt" :direction :input)
    (when in
        (loop for line = (read-line in nil :eof)
            until (eq line :eof) do
                (let ((rs (tokenizer line :delimiterp #'(lambda (char) (char= char #\,)))))
                    (format t "Total Part One: ~a~%" (check-ranges rs))))))


; Part Two
(defun check-substrings (num pattern)
    (let ((test (subseq num 0 (length pattern))))
        (if (string/= pattern test)
            nil
            (if (= (length num) (length pattern))
                t
                (check-substrings (subseq num (length pattern)) pattern)))))

(defun target-id (num)
    (let ((half (truncate (length num) 2)))
        (loop for lcv from 1 to half do
            (let ((pattern (subseq num 0 lcv)))
                (if (= (mod (length num) lcv) 0)
                    (if (check-substrings num pattern) (return-from target-id t)))))
        nil))

(defun check-range (range)
    (let* ((toks (tokenizer range :delimiterp #'(lambda (char) (char= char #\-)))) (beg (car toks)) (end (car (cdr toks))) (total 0))
        (loop for lcv from (parse-integer beg) to (parse-integer end) do
            (let* ((str-num (write-to-string lcv)) (result (target-id str-num)))
                (if result (incf total lcv))))
        total))

(defun check-ranges (ranges)
    (if (null ranges) 0 (+ (check-range (car ranges)) (check-ranges (cdr ranges)))))

(with-open-file (in "aoc02.txt" :direction :input)
    (when in
        (loop for line = (read-line in nil :eof)
            until (eq line :eof) do
                (let ((rs (tokenizer line :delimiterp #'(lambda (char) (char= char #\,)))))
                    (format t "Total Part Two: ~a~%" (check-ranges rs))))))
