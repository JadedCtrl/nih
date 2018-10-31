(defsystem "nih"
	   :version "0.1"
	   :author "Jaidyn Ann <jadedctrl@teknik.io>"
           :license "CC 0"
	   :depends-on ("cl-strings" "cl-ppcre")
	   :components ((:module "src"
			   :components
			   ((:file "package")
			    (:file "misc")
			    (:file "list")
			    (:file "string/string")
			    (:file "string/word")
			    (:file "string/line")
			    (:file "string/char"))))

           :description
           "Library of miscellanous (probably extraneous) functions (mainly string-manipulations).")
