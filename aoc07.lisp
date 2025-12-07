(defun count-splits (data-file beams)
  (let ((line (read-line data-file nil :eof)) (current-splits 0))
    (cond
      ((eq line :eof) 0)
      (t
        (loop for lcv from 0 below (length line) do
          (if (and (char= #\^ (char line lcv)) (char= #\| (char beams lcv)))
            (progn
              (incf current-splits)
              (cond
                ((zerop lcv) (setf (char beams (+ 1 lcv)) #\|) (setf (char beams lcv) #\.))
                ((= lcv (- (length line) 1)) (setf (char beams (- lcv 1)) #\|) (setf (char beams lcv) #\.))
                (t (setf (char beams (+ 1 lcv)) #\|) (setf (char beams lcv) #\.) (setf (char beams (- lcv 1)) #\|))))))
        (+ current-splits (count-splits data-file beams))))))

(defun part-one (filename)
  (with-open-file (in filename :direction :input)
      (when in
        (let* ((line (read-line in nil :eof)) (beams (make-string (length line) :initial-element #\.)))
          (setf (char beams (position #\S line)) #\|)
          (count-splits in beams)))))
