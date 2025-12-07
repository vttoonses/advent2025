(defun count-splits (data-file beams)
  (let ((line (read-line data-file nil :eof)) (current-splits 0))
    (cond
      ((eq line :eof) 0)
      (t
        (loop for lcv from 0 below (length line) do
          (if (and (char= #\^ (char line lcv)) (> (elt beams lcv) 0))
            (progn
              (incf current-splits)
              (setf (elt beams (+ 1 lcv)) 1) (setf (elt beams lcv) 0) (setf (elt beams (- lcv 1)) 1))))
        (+ current-splits (count-splits data-file beams))))))

(defun count-timelines (data-file beams)
  (let ((line (read-line data-file nil :eof)))
    (cond
      ((eq line :eof) (reduce #'+ beams))
      (t (loop for lcv from 0 below (length line) do
            (if (and (char= (char line lcv) #\^) (> (elt beams lcv) 0))
              (progn
                (incf (elt beams (- lcv 1)) (elt beams lcv))
                (incf (elt beams (+ lcv 1)) (elt beams lcv))
                (setf (elt beams lcv) 0))))
          (count-timelines data-file beams)))))

(defun day07 (filename count-fn)
  (with-open-file (in filename :direction :input)
      (when in
        (let* ((line (read-line in nil :eof)) (beams (make-array (length line) :initial-element 0)))
          (setf (elt beams (position #\S line)) 1)
          (funcall count-fn in beams)))))
