(defun find-first-largest (str start end)
    (let ((largest start))
        (loop for lcv from start to end do
            (if (char> (char str lcv) (char str largest))
                (setf largest lcv)))
        largest))

(defun build-prefix (str len)
    (if (= len 0)
        ""
        (let ((end (- (length str) len)))
            (if (= end 0) ""
                (let ((idx (find-first-largest str 0 end)))
                    (concatenate 'string (string (char str idx)) (build-prefix (subseq str (+ idx 1)) (- len 1))))))))

(defun build-num (str len)
    (let* ((pfx (build-prefix str len)) (sfx-idx (+ (- (length str) len) (length pfx))))
        (concatenate 'string pfx (subseq str sfx-idx))))

(defun pick-batteries (filename len)
    (with-open-file (in filename :direction :input)
        (when in
            (let ((total 0))
                (loop for line = (read-line in nil :eof)
                    until (eq line :eof) do
                        (incf total (parse-integer (build-num line len))))
                (format t "Total [~a]: ~a~%" len total)))))

(pick-batteries "aoc03.txt" 2)
(pick-batteries "aoc03.txt" 12)
