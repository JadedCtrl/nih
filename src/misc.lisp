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

  (nth (random (length list)) list))


;; UNIVERSAL-TIME --> ISO8601-FORMAT_TIME
(defun iso-time (universal-time)
  "Return `universal-time` in ISO 8601 format. :)"

  (multiple-value-bind
    (second minute hour date month year)
    (decode-universal-time universal-time)

    (format nil "~A-~A-~A"
	    year
	    (min-string-length month 2 "0")
	    (min-string-length date 2 "0"))))
