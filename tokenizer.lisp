(defun whitespace-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return)))

(defun tokenizer (string &key (delimiterp #'whitespace-p))
    (loop :for beg = (position-if-not delimiterp string)
        :then (position-if-not delimiterp string :start (1+ end))
        :for end = (and beg (position-if delimiterp string :start beg))
        :when beg :collect (subseq string beg end)
        :while end))
