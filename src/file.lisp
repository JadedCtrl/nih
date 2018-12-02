(in-package :nih)

;; PATH --> STRING
(defun read-file-string (path)
  "Read all lines from a file into a string."

  (if (file-exists path)
    (let ((encoding (asdf-encodings:detect-file-encoding path)))
      
      (with-open-file (fstream path
			       :direction :input
			       :external-format encoding)
	(line-string
	  (loop
	    :for line = (read-line fstream nil)
	    :while line
	    :collect line))))))


(defun write-file-string (path string &key
			       (if-exists :append)
			       (if-does-not-exist :create))
  "Write a string to a file."

  (let ((encoding :utf-8))

    (if (file-exists path)
      (setq encoding (asdf-encodings:detect-file-encoding path)))

    (with-open-file (fstream path
			     :direction :output
			     :external-format encoding
			     :if-exists if-exists
			     :if-does-not-exist if-does-not-exist)
      (format fstream "~A" string))))
