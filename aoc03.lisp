(defun pick-batteries (current bank)
    (if (= (length bank) 0)
        current
        (let* (
            (cur-str (format nil "~a" current))
            (digit (subseq bank 0 1))
            (tens (subseq cur-str 0 1))
            (ones (subseq cur-str 1))
            (test-1 (concatenate 'string tens digit))
            (test-2 (concatenate 'string ones digit)))
                (pick-batteries (max (parse-integer test-1) (parse-integer test-2) current) (subseq bank 1)))))

(with-open-file (in "aoc03.txt" :direction :input)
    (when in
        (let ((total 0))
            (loop for line = (read-line in nil :eof)
                until (eq line :eof) do
                    (let ((current (parse-integer (subseq line 0 2))))
                        (incf total (pick-batteries current (subseq line 2)))))
            (format t "Total: ~a~%" total))))


; Part two
(defun find-first-largest (str start end)
    (let ((largest start))
        (loop for lcv from start to end do
            (if (char> (char str lcv) (char str largest))
                (setf largest lcv)))
        largest))

(defun build-prefix (str len)
    (format t "~a -- ~a~%" len str)
    (let ((end (- (length str) len)))
        (format t "end ~a~%" end)
        (if (= end 0) ""
            (let ((idx (find-first-largest str 0 end)))
                (concatenate 'string (string (char str idx)) (build-prefix (subseq str (+ idx 1)) len))))))

(defun build-num (str len)
    (let* ((pfx (build-prefix str len)) (sfx-idx (+ (- (length str) len) (length pfx))))
        (format t "~a -- ~a~%" pfx sfx-idx)
        (concatenate 'string pfx (subseq str sfx-idx))))
