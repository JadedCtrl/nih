(in-package :nih)

;; DATA --> LIST_OR_SUBLIST
(defun list-or-real (data)
  "If a piece of data's `car` is a list (I.E., if it's a list of a list)
  then return the sub-list. If it's not a list, then just return the top-level
  list.

  This is useful for recursion, so that you can just serve the &rest list to
  a function, instead of having to itemize the &rest list out to reproduce it."

  (if (and
	(eq (type-of (ignore-errors (car data))) 'CONS)
	(eq (length data) 1))
    (car data)
    data))

;; DATA ALT --> DATA_OR_ALT
(defun value-or (data alt)
  "If a piece of data exists, return it. Otherwise, return the alternative.
  literally just `if`, lol"

  (if data
    data
    alt))


;; ARG-LIST &REST KEYS_TO_LOOK_FOR --> LIST_OF_KEY-VALUE_PAIRS
(defun parse-keys (arg-list key-list)
  "Return all searched-for keys fom a list in a list of key-value pairs."

  (let ((stack '())
	(new-list arg-list))
    (loop
      :for key
      :in key-list
      :do
      (if (ignore-errors (position key arg-list))
	(let* ((key-position (position key arg-list))
	       (value-position (+ 1 key-position))
	       (key-value (nth value-position arg-list)))
	  (setq stack (concatenate 'list stack (list key key-value)))
	  (setq new-list (remove key-value (remove key new-list))))))

    (values stack new-list)))


;; LIST --> RANDOM_ITEM
(defun random-item (list)
  "Return a random item from a list."

  (if (not list)
    nil
    (nth (random (length list)) list)))

;; INTEGER LIST --> LIST
(defun random-items (number list)
  "Return an amount of random items from a list."

  (if (not list)
    nil
    (let ((item (random-item list)))
      (concatenate 'list
		   (list item)
		   (if (not (eq number 1))
		     (random-items (- number 1) (remove item list)))))))

;; FILE_PATH --> BOOLEAN
(defun file-exists (path)
  "Return whether or not a file exists."

  (if (ignore-errors (file-author path))
    'T
    nil))


;; STREAM --> STRING_OF_ENTIRE_STREAM
(defun read-line-entire (stream)
  (let* ((cur-line (ignore-errors (read-line stream))))

    (cond
      (cur-line
	(string-combine cur-line
			(read-line-entire stream)
			:seperator (format nil "~%")) )
      ('T ""))))
