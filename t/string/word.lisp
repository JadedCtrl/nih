;; ----------------------------------------
;; WORD.LISP
;; ----------------------------------------


;; DATA DECLARATIONS
;; ----------------------------------------

(defvar *three-word-string*
  "Hiya! <3<3<3 daddyyyyy")

(defvar *three-word-list*
  '("Hiya!" "<3<3<3" "daddyyyyy"))

(defvar *word-repeat*
  "I like to eat like to eat like to eat babies")


;; ----------------------------------------


(rt:deftest word-list-i
	    (nih:word-list *three-word-string*)
	    ("Hiya!" "<3<3<3" "daddyyyyy"))

(rt:deftest word-list-ii
	    (nih:word-list "hi")
	    ("hi"))


(rt:deftest word-string-i
	    (nih:word-string *three-word-list*)
	    "Hiya! <3<3<3 daddyyyyy")

(rt:deftest word-string-ii
	    (nih:word-string '("hi"))
	    "hi")


(rt:deftest word-nth-i
	    (nih:word-nth 2 *three-word-string*)
	    "daddyyyyy")

(rt:deftest word-nth-ii
	    (nih:word-nth 0 *three-word-string*)
	    "Hiya!")

(rt:deftest word-nth-iii
	    (nih:word-nth 1 *three-word-string*)
	    "<3<3<3")

(rt:deftest word-nth-iv
	    (nih:word-nth 0 "hi")
	    "hi")


;; ----------------------------------------


(rt:deftest word-get
	    (nih:word-get "Hi" *three-word-string*)
	    "Hiya!")


(rt:deftest word-get-all
	    (nih:word-get-all ".*a.*" *three-word-string*)
	    "Hiya! daddyyyyy")


(rt:deftest word-remove
	    (nih:word-remove "a" *three-word-string*)
	    "<3<3<3 daddyyyyy")

(rt:deftest word-remove-all
	    (nih:word-remove-all ".*a.*" *three-word-string*)
	    "<3<3<3")


(rt:deftest word-position
	    (nih:word-position "Hiya!" *three-word-string*)
	    0)

(rt:deftest word-positions
	    (nih:word-positions "like" *word-repeat*)
	    (1 4 7))
