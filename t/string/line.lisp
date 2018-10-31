;; ----------------------------------------
;; LINE.LISP
;; ----------------------------------------


;; DATA DECLARATIONS
;; ----------------------------------------

(defvar *single-line-string*
  "Hiya! <3<3<3 daddyyyyy")

(defvar *single-line-list*
  '("Hiya! <3<3<3 daddyyyyy"))

(defvar *multi-line-string*
  "I like to eat
like to eat
like to eat
babies")

(defvar *multi-line-list*
  '("I like to eat"
    "like to eat"
    "like to eat"
    "babies"))


;; ----------------------------------------


(rt:deftest line-list-i
	    (nih:line-list *multi-line-string*)
	    ("I like to eat" "like to eat" "like to eat" "babies"))

(rt:deftest line-list-ii
	    (nih:line-list *single-line-string*)
	    ("Hiya! <3<3<3 daddyyyyy"))


(rt:deftest line-string-i
	    (nih:line-string *multi-line-list*)
"I like to eat
like to eat
like to eat
babies")

(rt:deftest line-string-ii
	    (nih:line-string *single-line-list*)
	    "Hiya! <3<3<3 daddyyyyy")


(rt:deftest line-nth-i
	    (nih:line-nth 2 *multi-line-string*)
	    "like to eat")

(rt:deftest line-nth-ii
	    (nih:line-nth 0 *multi-line-string*)
	    "I like to eat")

(rt:deftest line-nth-iii
	    (nih:line-nth 3 *multi-line-string*)
	    "babies")


;; ----------------------------------------


(rt:deftest line-get
	    (nih:line-get "eat" *multi-line-string*)
	    "I like to eat")

(rt:deftest line-get-all
	    (nih:line-get-all ".*eat.*" *multi-line-string*)
"I like to eat
like to eat
like to eat")


(rt:deftest line-remove
	    (nih:line-remove "eat" *multi-line-string*)
  "like to eat
like to eat
babies")

(rt:deftest line-remove-all
	    (nih:line-remove-all "eat" *multi-line-string*)
	    "babies")


(rt:deftest line-position-i
	    (nih:line-position "I like to eat" *multi-line-string*)
	    0)

(rt:deftest line-position-ii
	    (nih:line-position "like to eat" *multi-line-string*)
	    1)

(rt:deftest line-position-iii
	    (nih:line-position "babies" *multi-line-string*)
	    3)


(rt:deftest line-positions
	    (nih:line-positions "like to eat" *multi-line-string*)
	    (1 2))
