;; ----------------------------------------
;; WORD.LISP
;; ----------------------------------------


;; DATA DECLARATIONS
;; ----------------------------------------

(defvar *three-words*
  "Hiya! <3<3<3 daddyyyyy")

(defvar *three-words-char-list*
  '(#\H #\i #\y #\a #\!
    #\Space #\< #\3 #\< #\3 #\< #\3
    #\Space #\d #\a #\d #\d #\y #\y #\y #\y #\y))

(defvar *char-repeat*
  "I like to eat like to eat like to eat babies")


;; ----------------------------------------


(rt:deftest char-list
	    (nih:char-list *three-words*)
	    (#\H #\i #\y #\a #\!
	     #\Space #\< #\3 #\< #\3 #\< #\3
	     #\Space #\d #\a #\d #\d #\y #\y #\y #\y #\y))


(rt:deftest char-string
	    (nih:char-string *three-words-char-list*)
	    "Hiya! <3<3<3 daddyyyyy")


(rt:deftest char-nth-i
	    (nih:char-nth 0 *three-words*)
	    #\H)

(rt:deftest char-nth-ii
	    (nih:char-nth 2 *three-words*)
	    #\y)

(rt:deftest char-nth-iii
	    (nih:char-nth 21 *three-words*)
	    #\y)


;; ----------------------------------------


(rt:deftest char-remove-i
	    (nih:char-remove #\h "hi there :)")
	    "i there :)")

(rt:deftest char-remove-ii
	    (nih:char-remove #\h "hi")
	    "i")


(rt:deftest char-remove-all
	    (nih:char-remove-all #\h "hi there :)")
	    "i tere :)")


;; ----------------------------------------


(rt:deftest char-position-i
	    (nih:char-position #\H *three-words*)
	    0)

(rt:deftest char-position-ii
	    (nih:char-position #\y *three-words*)
	    2)


(rt:deftest char-positions
	    (nih:char-positions #\y *three-words*)
	    (2 17 18 19 20 21))
