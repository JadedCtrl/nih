;; ----------------------------------------
;; STRING.LISP
;; ----------------------------------------


;; DATA DECLARATIONS
;; ----------------------------------------

(defvar *a-string*
  "Hiya! <3<3<3")

(defvar *multiple-lines*
  "Oh, hi, how're you doing?
  I hope you responded 'fine'.
  Honestly, I can't hear you from here,
  Hopefully your day is OK, though. <3")

(defvar *multiple-lines-list*
  '("Oh, hi, how're you doing?"
    "I hope you responded 'fine'."
    "Honestly, I can't hear you from here,"
    "Hopefully your day is OK, though. <3"))

(defvar *single-multiple-lines-list*
  '("Hiya! <3<3<3"))

(defvar *number-string*
  "9")

(defvar *getf-strings-list*
  '(("one" 1)("two" 2)("three" 3)("four" iv)("four" 4)))

(defvar *colon-valued-string*
  ":Date 1999-01-23
This is a QOTD-format colon-value thing!
:Programme QOTDD
From QOTDD, of course =w=
:Spice tasty")

(defvar *misc-strings-list*
  '("hi" "hiya" "fhi" "die" "love"))

;; ----------------------------------------


(rt:deftest string-combine-i
	    (nih:string-combine "hi")
	    "hi")

(rt:deftest string-combine-ii
	    (nih:string-combine "hi" "dad" :seperator " ")
	    "hi dad")

(rt:deftest string-combine-iii
	    (nih:string-combine "the" "lad" "which" "never" :seperator "-")
	    "the-lad-which-never")

(rt:deftest string-combine-iv
	    (nih:string-combine "she" "who" "never" :seperator "^" "lied")
	    "she^who^never^lied")

(rt:deftest string-combine-v
	    (nih:string-combine "hi" "dad")
	    "hidad")


(rt:deftest intern-combine-i
	    (nih:intern-combine "hi")
	    hi :internal)

(rt:deftest intern-combine-ii
	    (nih:intern-combine "hi" "dad" :seperator " ")
	    |HI DAD| :internal)

(rt:deftest intern-combine-iii
	    (nih:intern-combine :package "nih" "the" "lad" "which"
				"never" :seperator "-")
	    NIH::the-lad-which-never :internal)
;; notice that keys are interspersed with strings

(rt:deftest intern-combine-iv
	    (nih:intern-combine "she" "who" "never" :seperator "^" "lied")
	    she^who^never^lied :internal)

(rt:deftest intern-combine-v
	    (nih:intern-combine "hi" "dad")
	    hidad :internal)


;; ----------------------------------------


(rt:deftest pad-string-i
	    (nih:pad-string *a-string* :prefix "a")
	    "aHiya! <3<3<3")

(rt:deftest pad-string-ii
	    (nih:pad-string *a-string* :prefix "a" :suffix "b")
	    "aHiya! <3<3<3") ;; notice how the :suffix is ignored!

(rt:deftest pad-string-iii
	    (nih:pad-string *a-string* :suffix "b")
	    "Hiya! <3<3<3b")


(rt:deftest min-string-length-i
	    (nih:min-string-length *number-string* 3 :prefix "0")
	    "009")

(rt:deftest min-string-length-ii
	    (nih:min-string-length *number-string* 9 :prefix "9")
	    "999999999")

(rt:deftest min-string-length-iii
	    (nih:min-string-length "bonege" 10 :suffix "e")
	    "bonegeeeee")


;; ----------------------------------------


(rt:deftest regex-get
	    (nih:regex-get ".*hi.*" *misc-strings-list*)
	    "hi")

(rt:deftest regex-get-all
	    (nih:regex-get-all ".*hi.*" *misc-strings-list*)
	    ("hi" "hiya" "fhi"))


(rt:deftest regex-remove
	    (nih:regex-remove ".*hi.*" *misc-strings-list*)
	    ("hiya" "fhi" "die" "love"))

(rt:deftest regex-remove-all
	    (nih:regex-remove-all ".*hi.*" *misc-strings-list*)
	    ("die" "love"))


;; ----------------------------------------


(rt:deftest nil-string
	    (nih:nil-string '(1 2 3 nil "hi" nil 3))
	    (1 2 3 "" "hi" "" 3))


;; ----------------------------------------


(rt:deftest getf-strings-i
	    (nih:getf-strings  *getf-strings-list* "one")
	    (("one" 1)))

(rt:deftest getf-strings-ii
	    (nih:getf-strings  *getf-strings-list* "two")
	    (("two" 2)))

(rt:deftest getf-strings-iii
	    (nih:getf-strings  *getf-strings-list* "four")
	    (("four" iv)("four" 4)))


(rt:deftest getf-string
	    (nih:getf-string  *getf-strings-list* "four")
	    ("four" iv))


;; ----------------------------------------


(rt:deftest get-colon-values-i
	    (nih:get-colon-values *colon-valued-string*)
	    (:DATE "1999-01-23" :PROGRAMME "QOTDD" :SPICE "tasty"))

(rt:deftest get-colon-values-ii
	    (nih:get-colon-values *a-string*)
	    nil)


(rt:deftest remove-colon-values-i
	    (nih:remove-colon-values *colon-valued-string*)
"This is a QOTD-format colon-value thing!
From QOTDD, of course =w=")

(rt:deftest remove-colon-values-ii
	    (nih:remove-colon-values *a-string*)
	    "Hiya! <3<3<3")
