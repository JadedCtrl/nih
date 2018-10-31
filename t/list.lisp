;; ----------------------------------------
;; STRING.LISP
;; ----------------------------------------


;; DATA DECLARATIONS
;; ----------------------------------------

(defvar *number-list* '(98 37 23 24 14 14 24 52 5325 32 4234))

(defvar *one-number-list* '(1))


;; TESTS
;; ----------------------------------------

(rt:deftest up-to-i
	    (nih:up-to 2 *number-list*)
	    (98 37 23))

(rt:deftest up-to-ii
	    (nih:up-to 0 *one-number-list*)
	    (1))

(rt:deftest up-to-iii
	    (nih:up-to 3 *one-number-list*)
	    (1))

(rt:deftest up-to-iv
	    (nih:up-to 13 *number-list*)
	    (98 37 23 24 14 14 24 52 5325  32 4234))


(rt:deftest up-from-i
	    (nih:up-from 2 *number-list*)
	    (23 24 14 14 24 52 5325 32 4234))

(rt:deftest up-from-ii
	    (nih:up-from 10 *number-list*)
	    (4234))

(rt:deftest up-from-iii
	    (nih:up-from 11 *number-list*)
	    nil)


;; ----------------------------------------


(rt:deftest before-i
	    (nih:before 37 *number-list*)
	    98)

(rt:deftest before-ii
	    (nih:before 98 *number-list*)
	    nil)


(rt:deftest after-i
	    (nih:after 37 *number-list*)
	    23)

(rt:deftest after-ii
	    (nih:after 4234 *number-list*)
	    nil)


;; ----------------------------------------


(rt:deftest following-i
	    (nih:following 37 *number-list*)
	    (23 24 14 14 24 52 5325 32 4234))

(rt:deftest following-ii
	    (nih:following 4234 *number-list*)
	    nil)


(rt:deftest preceding-i
	    (nih:preceding 23 *number-list*)
	    (98 37))

(rt:deftest preceding-ii
	    (nih:preceding 98 *number-list*)
	    nil)


;; ----------------------------------------


(rt:deftest positions
	    (nih:positions 14 *number-list*)
	    (4 5))


;; ----------------------------------------


(rt:deftest replace-at-i
	    (nih:replace-at '(1 2 3 1 4) 1 0)
	    (0 2 3 0 4))

(rt:deftest replace-at-ii
	    (nih:replace-at '("yo" "hi" "hi" "ya") "hi" "ye" :test #'equal)
	    ("yo" "ye" "ye" "ya"))
