(in-package :nih)

;; UNIVERSAL-TIME --> STRING
(defun iso-time (universal-time)
  "Return `universal-time` in ISO 8601 format. :)"

  (multiple-value-bind
    (second minute hour date month year)
    (decode-universal-time universal-time)

    (format nil "~A-~A-~A"
	    year
	    (min-string-length month 2 :prefix "0")
	    (min-string-length date 2 :prefix "0"))))


;; NIL --> STRING
(defun get-iso-time ()
  "Return the ISO 8601 time of immediately, right here, right now."

  (iso-time (get-universal-time)))


;; STRING STRING --> INTEGER
(defun iso-date-distance (iso-date-a iso-date-b)
  "Return the number of days between date-a and date-b."

  (let* ((a (mapcar #'read-from-string (nih:char-split "-" iso-date-a)))
	 (year-a (car a)) (month-a (cadr a)) (day-a (caddr a))

	 (b (mapcar #'read-from-string (nih:char-split "-" iso-date-b)))
	 (year-b (car b)) (month-b (cadr b)) (day-b (caddr b)))

    (+
      (* 365 (- year-b year-a))
      (- (day-number month-b day-b) (day-number month-a day-a)))))


(defvar month-length
  '(31 28 31 30 31 30 31 31 30 31 30 30))


;; INTEGER [INTEGER] --> INTEGER
(defun day-number (month &optional (date 1))
  "Return the day-number (of a year) of a month/day combo."

  (let ((month-days (reduce #'+ (nih:up-to (- month 2) month-length)))
	(date-days date))

    (+ month-days date-days)))

