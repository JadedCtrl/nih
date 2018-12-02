(in-package :nih)

;; CHAR-LIST FUNCTIONS
;; =====================================

;; STRING --> LIST_OF_CHARS
(defun char-list (string)
  "Turn a string into a list of characters."

  (let ((stack '()))
    (loop
      :for char
      :across string
      :do (setq stack
		(append stack (list char))))
    stack))


;; LIST_OF_CHARS --> STRING
(defun char-string (char-list)
  "Convert a list of chars into a string"

  (cond
    ((eq (length char-list) 1)
     (string (car char-list)))
    ('T
     (reduce
       (lambda (char-a char-b)
	 (string-combine (string char-a) (string char-b) :seperator ""))
       char-list))))


;; STRING --> NTH_CHARACTER
(defun char-nth (n string)
  "Return nth line number from a multi-line string."

  (nth n (char-list string)))


;; STRING --> INTEGER
(defun char-length (string)
  "Return the length of a string by character."

  (length (char-list string)))



;; ----------------------------------------


;; REGEX STRING --> LINES_SANS_MATCHES
(defun char-remove (character string)
  "Remove the first instance of a char from a string."

  (let ((switch 0))
    (reduce
      (lambda (char-a char-b)
	(string-combine
	  (if (and (eq switch 0) (eq character char-a))
	    (progn (setq switch 1) "")
	    char-a)
	  char-b))
      (char-list string))))


;; REGEX STRING --> LIST_OF_MATCHING_WORDS
(defun char-remove-all (character string)
  "Remove all instances of a char in a string."

  (char-string (remove character (char-list string))))


;; CHARACTER STRING --> 1ST_POS_OF_CHAR_IN_STRING
(defun char-position (character string)
  "Return the first position of a character in a string."

  (position character (char-list string)))

;; CHARACTER STRING --> LIST_OF_CHAR_POS_IN_STRING
(defun char-positions (character string)
  "Return a list of the positions of a character in a string."

  (positions character (char-list string)))


;; CHARACTER STRING --> LIST_OF_STRINGS
(defun char-split (character string)
  "Split a string into a list of strings, at a set character."

  (regex-split (string character)
	       (mapcar #'string (char-list string))))


;; ----------------------------------------


(create-string-manip char-car #'nih:char-list #'nih:char-string #'car)
(create-string-manip char-caar #'nih:char-list #'nih:char-string #'caar)
(create-string-manip char-cadddrr #'nih:char-list #'nih:char-string #'cadddrr)
(create-string-manip char-cadaar #'nih:char-list #'nih:char-string #'cadaar)
(create-string-manip char-cadr #'nih:char-list #'nih:char-string #'cadr)
(create-string-manip char-caadr #'nih:char-list #'nih:char-string #'caadr)
(create-string-manip char-caaadr #'nih:char-list #'nih:char-string #'caaadr)
(create-string-manip char-caaaar #'nih:char-list #'nih:char-string #'caaaar)
(create-string-manip char-caadar #'nih:char-list #'nih:char-string #'caadar)
(create-string-manip char-caddr #'nih:char-list #'nih:char-string #'caddr)
(create-string-manip char-caaar #'nih:char-list #'nih:char-string #'caaar)

(create-string-manip char-cdr #'nih:char-list #'nih:char-string #'cdr)
(create-string-manip char-cdddar #'nih:char-list #'nih:char-string #'cdddar)
(create-string-manip char-cdar #'nih:char-list #'nih:char-string #'cdar)
(create-string-manip char-cdaaar #'nih:char-list #'nih:char-string #'cdaaar)
(create-string-manip char-cddddr #'nih:char-list #'nih:char-string #'cddddr)
(create-string-manip char-cdddr #'nih:char-list #'nih:char-string #'cdddr)
(create-string-manip char-cdaadr #'nih:char-list #'nih:char-string #'cdaadr)
(create-string-manip char-cdadar #'nih:char-list #'nih:char-string #'cdadar)
(create-string-manip char-cddadr #'nih:char-list #'nih:char-string #'cddadr)
(create-string-manip char-cdaddr #'nih:char-list #'nih:char-string #'cdaddr)
(create-string-manip char-cdadr #'nih:char-list #'nih:char-string #'cdadr)
(create-string-manip char-cddar #'nih:char-list #'nih:char-string #'cddar)
(create-string-manip char-cddr #'nih:char-list #'nih:char-string #'cddr)
(create-string-manip char-cddaar #'nih:char-list #'nih:char-string #'cddaar)
(create-string-manip char-cdaar #'nih:char-list #'nih:char-string #'cdaar)
