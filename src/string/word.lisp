(in-package :nih)

;; WORD-LIST FUNCTIONS
;; =====================================

;; STRING --> LIST_OF_WORDS
(defun word-list (string)
  "Turn a string into a list of words."

  (cl-strings:split string " "))



;; LIST_OF_WORDS --> STRING
(defun word-string (word-list)
  "Convert a list of words into a string"

  (if (not (equal (type-of word-list) 'CONS))
    word-list
    (reduce
      (lambda (word-a word-b)
	(string-combine :seperator " " word-a word-b))
      word-list)))


;; STRING --> NTH_WORD_FROM_STRING
(defun word-nth (n string)
  "Get nth word from a string."

  (word-string (nth n (word-list string))))


;; ----------------------------------------


;; REGEX STRING --> MATCHING_WORD
(defun word-get (query string)
  "Return a word in a string that matches a regex query."

  (word-car (word-get-all query string)))

;; REGEX STRING --> LIST_OF_MATCHING_WORDS
(defun word-get-all (query string)
  "Return all words in a string that match a regex query."

  (word-string (regex-get-all query (word-list string))))


;; REGEX STRING --> LINES_SANS_MATCHES
(defun word-remove (query string)
    "Remove a word from a string that matches a regex query."

      (word-string (regex-remove query (word-list string))))

;; REGEX STRING --> LINES_SANS_MATCHES
(defun word-remove-all (query string)
    "Remove all words from a string that match a regex query."

      (word-string (regex-remove-all query (word-list string))))


;; WORD STRING --> WORD_POS_IN_STRING
(defun word-position (word string)
  "Return the position of a word (relative to other words) in a string."

  (position word (word-list string) :test #'equal))

;; WORD STRING --> LIST_OF_WORD_POS_IN_STRING
(defun word-positions (word string)
  "Return a list of positions of a word (relative to other words) in a string."

  (positions word (word-list string) :test #'equal))


;; ----------------------------------------


(create-string-manip word-car #'nih:word-list #'nih:word-string #'car)
(create-string-manip word-caar #'nih:word-list #'nih:word-string #'caar)
(create-string-manip word-cadddrr #'nih:word-list #'nih:word-string #'cadddrr)
(create-string-manip word-cadaar #'nih:word-list #'nih:word-string #'cadaar)
(create-string-manip word-cadr #'nih:word-list #'nih:word-string #'cadr)
(create-string-manip word-caadr #'nih:word-list #'nih:word-string #'caadr)
(create-string-manip word-caaadr #'nih:word-list #'nih:word-string #'caaadr)
(create-string-manip word-caaaar #'nih:word-list #'nih:word-string #'caaaar)
(create-string-manip word-caadar #'nih:word-list #'nih:word-string #'caadar)
(create-string-manip word-caddr #'nih:word-list #'nih:word-string #'caddr)
(create-string-manip word-caaar #'nih:word-list #'nih:word-string #'caaar)

(create-string-manip word-cdr #'nih:word-list #'nih:word-string #'cdr)
(create-string-manip word-cdddar #'nih:word-list #'nih:word-string #'cdddar)
(create-string-manip word-cdar #'nih:word-list #'nih:word-string #'cdar)
(create-string-manip word-cdaaar #'nih:word-list #'nih:word-string #'cdaaar)
(create-string-manip word-cddddr #'nih:word-list #'nih:word-string #'cddddr)
(create-string-manip word-cdddr #'nih:word-list #'nih:word-string #'cdddr)
(create-string-manip word-cdaadr #'nih:word-list #'nih:word-string #'cdaadr)
(create-string-manip word-cdadar #'nih:word-list #'nih:word-string #'cdadar)
(create-string-manip word-cddadr #'nih:word-list #'nih:word-string #'cddadr)
(create-string-manip word-cdaddr #'nih:word-list #'nih:word-string #'cdaddr)
(create-string-manip word-cdadr #'nih:word-list #'nih:word-string #'cdadr)
(create-string-manip word-cddar #'nih:word-list #'nih:word-string #'cddar)
(create-string-manip word-cddr #'nih:word-list #'nih:word-string #'cddr)
(create-string-manip word-cddaar #'nih:word-list #'nih:word-string #'cddaar)
(create-string-manip word-cdaar #'nih:word-list #'nih:word-string #'cdaar)
