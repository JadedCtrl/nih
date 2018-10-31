;; ----------------------------------------
;; STRING.LISP
;; ----------------------------------------


;; DATA DECLARATIONS
;; ----------------------------------------

(defvar *list-in-list* '((1 2 3 4)))

(defvar *keyed-list* '(1 2 :morto "hi" 3 :ploro 4))


;; TESTS
;; ----------------------------------------


(rt:deftest parse-keys-i
	    (nih:parse-keys *keyed-list* '(:morto))
	    (:morto "hi")
	    (1 2 3 :ploro 4))

(rt:deftest parse-keys-ii
	    (nih:parse-keys *keyed-list* '(:ploro))
	    (:ploro 4)
	    (1 2 :morto "hi" 3))

(rt:deftest parse-keys-iii
	    (nih:parse-keys *keyed-list* '(:ploro :morto))
	    (:ploro 4 :morto "hi")
	    (1 2 3))

(rt:deftest parse-keys-iv
	    (nih:parse-keys *list-in-list* '(:ploro :morto))
	    nil
	    ((1 2 3 4)))


;; ----------------------------------------


(rt:deftest value-or-i
	    (nih:value-or nil "")
	    "")

(rt:deftest value-or-ii
	    (nih:value-or 2 "")
	    2)


;; ----------------------------------------


(rt:deftest list-or-real-i
	    (nih:list-or-real *list-in-list*)
	    (1 2 3 4))

(rt:deftest list-or-real-ii
	    (nih:list-or-real *keyed-list*)
	    (1 2 :morto "hi" 3 :ploro 4))
