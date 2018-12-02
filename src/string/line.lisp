(in-package :nih)

;; LINE-LIST FUNCTIONS
;; =====================================

;; MULTI-LINE_STRING --> LIST_OF_LINES
(defun line-list (string)
  "Return a list of lines from a multi-line string."

  (cl-strings:split string (format nil "~%")))



;; LIST_OF_STRINGS --> MULTI-LINE_STRING
(defun line-string (line-list)
  "Turn a list of lines (string) into a multi-line string."

  (if (not (equal (type-of line-list) 'CONS))
    line-list
    (reduce
      (lambda (line-a line-b)
	(string-combine :seperator (format nil "~%") line-a line-b))
      line-list)))


;; NUMBER MULTI-LINE_STRING --> SINGLE-LINE_STRING
(defun line-nth (n string)
  "Return nth line number from a multi-line string."

  (nth n (line-list string)))


;; STRING --> INTEGER
(defun line-length (string)
  "Return the length of a string in lines."

  (length (line-list string)))



;; ----------------------------------------


;; REGEX STRING --> MATCHING_WORD
(defun line-get (query string)
  "Return a line in a string that matches a regex query."

  (ignore-errors (line-car (line-get-all query string))))

;; REGEX STRING --> LIST_OF_MATCHING_WORDS
(defun line-get-all (query string)
  "Return all lines in a string that match a regex query."

  (line-string (regex-get-all query (line-list string))))


;; REGEX STRING --> LINES_SANS_MATCHES
(defun line-remove (query string)
  "Remove a line from a string that matches a regex query."

  (line-string (regex-remove query (line-list string))))

;; REGEX STRING --> LINES_SANS_MATCHES
(defun line-remove-all (query string)
  "Remove all lines from a string that match a regex query."

  (line-string (regex-remove-all query (line-list string))))


;; LINE STRING --> POS_OF_LINE_IN_STRING
(defun line-position (line string)
  "Return the line-number of a line in a string."

  (position line (line-list string) :test #'equal))

;; LINE STRING --> LIST_OF_LINE_POS_IN_STRING
(defun line-positions (line string)
  "Return a list of the positions of a line in string."

  (positions line (line-list string) :test #'equal))


;; QUERY STRING --> LIST_OF_LINES_SANS_MATCHES
(defun line-split (query string)
    "Split a string into a list, seperated by a set line matching a regex query."

      (regex-split query (line-list string) (string #\Newline)))


;; ----------------------------------------


(create-string-manip line-car #'nih:line-list #'nih:line-string #'car)
(create-string-manip line-caar #'nih:line-list #'nih:line-string #'caar)
(create-string-manip line-cadddrr #'nih:line-list #'nih:line-string #'cadddrr)
(create-string-manip line-cadaar #'nih:line-list #'nih:line-string #'cadaar)
(create-string-manip line-cadr #'nih:line-list #'nih:line-string #'cadr)
(create-string-manip line-caadr #'nih:line-list #'nih:line-string #'caadr)
(create-string-manip line-caaadr #'nih:line-list #'nih:line-string #'caaadr)
(create-string-manip line-caaaar #'nih:line-list #'nih:line-string #'caaaar)
(create-string-manip line-caadar #'nih:line-list #'nih:line-string #'caadar)
(create-string-manip line-caddr #'nih:line-list #'nih:line-string #'caddr)
(create-string-manip line-caaar #'nih:line-list #'nih:line-string #'caaar)

(create-string-manip line-cdr #'nih:line-list #'nih:line-string #'cdr)
(create-string-manip line-cdddar #'nih:line-list #'nih:line-string #'cdddar)
(create-string-manip line-cdar #'nih:line-list #'nih:line-string #'cdar)
(create-string-manip line-cdaaar #'nih:line-list #'nih:line-string #'cdaaar)
(create-string-manip line-cddddr #'nih:line-list #'nih:line-string #'cddddr)
(create-string-manip line-cdddr #'nih:line-list #'nih:line-string #'cdddr)
(create-string-manip line-cdaadr #'nih:line-list #'nih:line-string #'cdaadr)
(create-string-manip line-cdadar #'nih:line-list #'nih:line-string #'cdadar)
(create-string-manip line-cddadr #'nih:line-list #'nih:line-string #'cddadr)
(create-string-manip line-cdaddr #'nih:line-list #'nih:line-string #'cdaddr)
(create-string-manip line-cdadr #'nih:line-list #'nih:line-string #'cdadr)
(create-string-manip line-cddar #'nih:line-list #'nih:line-string #'cddar)
(create-string-manip line-cddr #'nih:line-list #'nih:line-string #'cddr)
(create-string-manip line-cddaar #'nih:line-list #'nih:line-string #'cddaar)
(create-string-manip line-cdaar #'nih:line-list #'nih:line-string #'cdaar)
