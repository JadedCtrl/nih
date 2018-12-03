(defsystem "nih"
	   :version "0.3"
	   :author "Jaidyn Ann <jadedctrl@teknik.io>"
           :license "LGPLv3"
	   :depends-on ("cl-strings" "asdf-encodings" "cl-ppcre")
	   :components ((:module "src"
			   :components
			   ((:file "package")
			    (:file "misc")
			    (:file "file")
			    (:file "list")
			    (:file "date")
			    (:file "string/string")
			    (:file "string/word")
			    (:file "string/line")
			    (:file "string/char"))))

           :description
           "Library of miscellanous (probably extraneous) functions (mainly string-manipulations).")
