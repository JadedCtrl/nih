(in-package :nih)


;; FUNCTION-NAME #'TO-LIST #'TO-STRING #'MANIPULATION
(defmacro create-string-manip (manip-name list-function string-function manip)
  "Define a function to perform a manipulation (#'manip, i.e. #'car)  on the
  list-form of a particular string (converting to list with #'list-function),
  then converting back into a string (#'string-function) again
  (but with the manipulations made)."

  `(defun ,manip-name (string)
     (funcall ,string-function
	      (funcall ,manip
		       (funcall ,list-function string)))))


;; ======================================== 


;; STRINGS :SEPERATOR --> STRINGS_COMBINED_W_SEPERATOR
(defun string-combine (&rest arg-list)
  "Combine strings into a single string, marked with a seperator.
  :seperator is parsed manually, since mixing &rest and &key makes
  me want to eat my fucking hat. >;c"

  (multiple-value-bind (keys strings)
    (parse-keys arg-list '(:seperator))

    (let ((seperator (value-or (getf keys :seperator) "")))

      (reduce (lambda (word1 word2)
		(format nil "~A~A~A" word1 seperator word2))
	      strings))))


;; STRING_A STRING_B --> SYMBOL_OF_STRING_A+STRING_B
(defun intern-combine (&rest arg-list)
  "Combine two strings into a symbol, marked with a seperator.
  :seperator and :package are parsed manually, since mixing &rest
  and &key is utterly painful! ;_:"

  (multiple-value-bind (keys strings)
    (parse-keys arg-list '(:seperator :package))

    (let* ((seperator (value-or (getf keys :seperator) ""))
	   (package (value-or (getf keys :package) nil))
	   (combined (string-upcase
		       (reduce (lambda (str1 str2)
				 (string-combine  str1 str2
						  :seperator seperator))
			       strings))))

      (if package
	(intern combined (string-upcase package))
	(intern combined)))))


;; ---------------------------------------- 


;; LIST --> LIST_SANS_NIL_WITH_""
(defun nil-string (list)
  "Replace all instances of nil in a list with a blank string."

  (replace-at list nil ""))


;; ---------------------------------------- 


;; STRING PREFIX SUFFIX --> STRING_ONE_OR_TWO_CHARS_LARGER
(defun pad-string (string &key prefix suffix)
  "Increase the character-count of a string by 1; either by
  adding a prefix-substitutor or a suffix-substitutor.
  If both a :prefix and a :suffix are provided, :suffix is
  ignored."

  (flet ((padding (prefix string suffix)
		  (format nil "~A~A~A" prefix string suffix)))

    (cond
      ((and prefix suffix)
       (padding prefix string ""))
      (prefix
	(padding prefix string ""))
      (suffix
	(padding "" string suffix)))))


;; STRING INTEGER --> STRING
(defun max-string-length (string length)
  "Return a string by splitting it into lines, each line being length long."

  (let ((stack "")
	(i 0))
    (loop
      :for char
      :across string
      :do
      (if (eq length i)
	(progn
	  (setq i 0)
	  (setq stack
		(nih:string-combine stack (format nil "~%~A" char))))
	(setq stack
	      (nih:string-combine stack (format nil "~A" char))))
      (setq i (+ 1 i)))
    stack))


;; STRING DESIRED_LENGTH [PREFIX] [SUFFIX] --> STRING_OF_DESIRED_LENGTH
(defun min-string-length (string target-length
				 &key (prefix "") (suffix ""))
  "If a string *must* be a certain length (formatting reasons), use
this function.

Returns a string of `length`, using the `prefix-substitutor` or
`suffix-substitutor` to beef up the character-count if it's too short.

If both `prefix` and `suffix` are defined, `prefix` is used.

Example:
(min-string-length \"9\" 3 \"0\")

\"009\""

(let* ((string (format nil "~A" string))
       (cur-length (length string)))

  (if (eq cur-length target-length)
    string

    (min-string-length
      (if (eq (length suffix) 0)
	(pad-string string :prefix prefix)
	(pad-string string :suffix suffix))
      target-length
      :prefix prefix
      :suffix suffix))))


;; ---------------------------------------- 


;; REGEX STRING --> MATCHING_WORD
(defun regex-get (query strings)
  "Return a string in a list that matches a regex query."

  (car (regex-get-all query strings)))


;; REGEX STRING --> LIST_OF_MATCHING_WORDS
(defun regex-get-all (query strings)
  "Return all strings in a list that match a regex query."

  (let ((stack '())
	(i 0))

    (loop
      :while (< i (length strings))
      :do
      (if (cl-ppcre:scan-to-strings query (nth i strings))
	(setq stack (concatenate 'list stack (list (nth i strings)))))

      (setq i (+ 1 i)))

    stack))


;; REGEX STRING --> ALL_BUT_MATCHES
(defun regex-remove (query strings)
  "Return a list of strings without a matches to a regex query."

  (let ((stack '())
	(switch 0)
	(i 0))

    (loop
      :while (< i (length strings))
      :do
      (cond
	((eq switch 1)
	 (setq stack (concatenate 'list stack (list (nth i strings)))))
	((cl-ppcre:scan-to-strings query (nth i strings))
	 (setq switch 1))
	('T 
	 (setq stack (concatenate 'list stack (list (nth i strings))))))

      (setq i (+ 1 i)))

    stack))

;; REGEX STRING --> ALL_BUT_MATCHES
(defun regex-remove-all (query strings)
  "Return a list of strings without any matches to a regex query."

  (let ((stack '())
	(i 0))

    (loop
      :while (< i (length strings))
      :do
      (if (not (cl-ppcre:scan-to-strings query (nth i strings)))
	(setq stack (concatenate 'list stack (list (nth i strings)))))

      (setq i (+ 1 i)))

    stack))


;; QUERY LIST_OF_STRINGS --> LIST_SANS_QUERY_MATCHES
(defun regex-split (query list &optional (combiner ""))
  "Split a string into a list, seperated by a set item matching a regex query."

  (let ((stack '(""))
	(i 0))

    (loop
      :while (< i (length list))
      :do
      (let ((string (nth i list))
	    (last-string (car (reverse stack)))
	    (stack-sans (reverse (cdr (reverse stack)))))

	(cond
	  ((ppcre:scan-to-strings query string)
	   (setq stack (concatenate 'list stack (list ""))))
	  ('T
	   (setq stack (concatenate 'list stack-sans
				    (list (string-trim combiner
						       (string-combine
							 last-string string
							 :seperator combiner))))))))

      (setq i (+ 1 i)))

    (remove "" stack :test #'equal)))


;; ---------------------------------------- 


;; LIST_OF_SUBLISTS STRING --> SUBLIST_WITH_STRING_AS_CAR
(defun getf-string (list string)
  "Get an item from a list by an identifying string in `car`.
  I.E., if the string is 'apple', the first sublist like this:
  ('apple' 1 2 3)
  will be returned."

  (car (getf-strings list string)))



;; LIST_OF_SUBLISTS STRING --> LIST_OF_SUBLISTS_WITH_STRING_AS_CAR
(defun getf-strings (list string &optional (stack '()))
  "Get items from list by an identifying string in `car`.
  I.E., if the string is 'apple', any sublists like this:
  ('apple' 1 2 3)
  will be returned."

  ;; just recurse through the list, adding each new matching
  ;; item to the `stack`

  (if (and (< 0 (length list)) (listp list))
    (if (ignore-errors
	  ;; the item might not be a list; for our purposes, let's ignore that.
	  (equal
	    (car (car list))    ;; '( ( here ) )
	    string))
      (getf-strings (cdr list) string (concatenate 'list stack (list (car list))))
      (getf-strings (cdr list) string stack))
    stack))


;; LIST_OF_SUBLISTS STRING --> LIST_OF_SUBLISTS_WITH_STRING_AS_CAR
(defun getf-cars (list value &key (stack '()) (test 'eq))
  "Get items from list by an identifying string in `car`.
  I.E., if the string is 'apple', any sublists like this:
  ('apple' 1 2 3)
  will be returned."

  ;; just recurse through the list, adding each new matching
  ;; item to the `stack`

  (if (and (< 0 (length list)) (listp list))
    (if (ignore-errors
	  ;; the item might not be a list; for our purposes, let's ignore that.
	  (funcall test
	    (car (car list))    ;; '( ( here ) )
	    value))
      (getf-cars (cdr list) value
		 :test test
		 :stack (concatenate 'list stack (list (car list))))
      (getf-cars (cdr list) value
		 :stack stack
		 :test test))
    stack))

(defun getf-car (list value &key (test 'eq))
  (car (getf-cars list value :test test)))

;; LIST_OF_SUBLISTS STRING --> LIST_OF_SUBLISTS_WITH_STRING_AS_CAR
(defun getf-cadrs (list value &key (stack '()) (test 'eq))
  "Get items from list by an identifying string in `car`.
  I.E., if the string is 'apple', any sublists like this:
  ('apple' 1 2 3)
  will be returned."

  ;; just recurse through the list, adding each new matching
  ;; item to the `stack`

  (if (and (< 0 (length list)) (listp list))
    (if (ignore-errors
	  ;; the item might not be a list; for our purposes, let's ignore that.
	  (funcall test
	    (cadr (car list))    ;; '( ( here ) )
	    value))
      (getf-cadrs (cdr list) value
		 :test test
		 :stack (concatenate 'list stack (list (car list))))
      (getf-cadrs (cdr list) value
		 :stack stack
		 :test test))
    stack))

(defun getf-cadr (list value &key (test 'eq))
  (car (getf-cadrs list value :test test)))

;; ---------------------------------------- 


;; STRING COLON_VARIABLE_NAME --> COLON_VALUE
(defun get-colon-values (string)
  "Return a the value of a `colon variable`; I.E.,
  a line of a string starting with `:` followed by a variable
  name, a space, then the value of said `colon variable`.

  Here's an example:
  (setq *example-string*
	\"Blah blah blah
	:Date 1999
	Blah blah blah\")

  If you ran
  (get-colon-values *example-string*)

  you would get
  (:DATE \"1999\") in return.

  Mainly useful for multi-line strings, but your use-case might
  involve a `colon variable` in a single-lined string."

  (if (line-get-all "^:" string)
    (let* ((colon-lines (line-get-all "^:.* " string))
	   (colon-list (line-list colon-lines))
	   (cur-line (line-car colon-lines))
	   (variable (word-car cur-line))
	   (value (word-cdr cur-line)))

      (cond
	((eq 1 (length colon-list))
	 (list (read-from-string variable) value))
	('T
	 (concatenate 'list (list (read-from-string variable) value)
		      (get-colon-values (line-cdr colon-lines))))))))


;; STRING COLON_VARIABLE_NAME --> COLON_VALUE
(defun get-colon-value (string variable)
  "Return a value of a `colon variable`."

  (getf (get-colon-values string) (read-from-string variable)))


;; STRING COLON_VARIABLE_NAME --> STRING_WITHOUT_COLON_VARIABLE
(defun remove-colon-values (string)
  "Remove the colon-variable declaration from a string."

  (line-remove-all "^:.*" string))



;; STRING SYMBOL STRING --> STRING
(defun replace-colon-value (string key-string value)
  "Replace a colon variable's value."

  (let ((existent
	  (getf (get-colon-values string) (read-from-string key-string))))
    (if existent
      (line-replace
	(line-position
	  (line-get (string-combine "^" key-string " ") string)
	  string)
	(string-combine key-string value :seperator " ")
	string)
      (string-combine :seperator (string #\Newline)
		      string (string-combine key-string " " value)))))




;; -------------------------------------
;; PRIVATE HELPER FUNCTIONS


;; INTEGER STRING STRING --> STRING
(defun line-replace (position new-line string)
  "Replace nth line with a new one in a string."

  (let* ((line-list (nih:line-split (nih:line-nth position string) string))
	 (modified-list
	   (list (car line-list)
		 new-line
		 (nih:value-or (cadr line-list) ""))))

    (reduce (lambda (a b) (nih:string-combine a b :seperator (format nil "~%")))
	    modified-list)))
