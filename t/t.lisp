(ql:quickload :rt)
(ql:quickload :cl-strings)
(ql:quickload :cl-ppcre)



(defpackage :nih-testing
  (:use :cl)
  (:export

    :do-all))

(in-package :nih-testing)



(defun do-all ()
  "Execute all tests."

  (rt:do-tests))




(load "src/package.lisp")

(load "src/misc.lisp")
(load "t/misc.lisp")

(load "src/list.lisp")
(load "t/list.lisp")

(load "src/string/string.lisp")
(load "t/string/string.lisp")

(load "src/string/word.lisp")
(load "t/string/word.lisp")

(load "src/string/line.lisp")
(load "t/string/line.lisp")

(load "src/string/char.lisp")
(load "t/string/char.lisp")

(nih-testing:do-all)
