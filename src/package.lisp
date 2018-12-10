(defpackage :nih
  (:use :cl)
  (:export

    ;; STRING
    ;; =======================
    :apply-convert
    :string-manip
    :create-string-manip
    :manip-name

    :string-combine
    :intern-combine

    :regex-get
    :regex-get-all
    :regex-remove
    :regex-remove-all
    :regex-split

    :nil-string

    :pad-string
    :min-string-length
    :max-string-length

    :getf-string
    :getf-strings

    :getf-car
    :getf-cars
    :getf-cadr
    :getf-cadrs

    :get-colon-values
    :get-colon-value
    :remove-colon-values
    :replace-colon-value


    ;; STRING/WORD
    ;; =======================
    :word-list
    :word-string
    :word-nth

    :word-get
    :word-get-all
    :word-remove
    :word-remove-all
    :word-position
    :word-positions
    :word-split
    :word-length

    :word-car
    :word-caar :word-cadddrr :word-cadaar :word-cadr :word-caadr
    :word-caaadr :word-caaaar :word-caadar :word-caddr :word-caaar
    :word-cdr
    :word-cdddar :word-cdar :word-cdaaar :word-cddddr :word-cdddr
    :word-cdaadr :word-cdadar :word-cddadr :word-cdaddr :word-cdadr
    :word-cddar :word-cddr :word-cddaar :word-cdaar
    :word-last :word-reverse


    ;; STRING/LINE
    ;; =======================
    :line-list
    :line-string
    :line-nth

    :line-get
    :line-get-all
    :line-remove
    :line-remove-all
    :line-position
    :line-positions
    :line-split
    :line-length

    :line-car
    :line-caar :line-cadddrr :line-cadaar :line-cadr :line-caadr
    :line-caaadr :line-caaaar :line-caadar :line-caddr :line-caaar
    :line-cdr
    :line-cdddar :line-cdar :line-cdaaar :line-cddddr :line-cdddr
    :line-cdaadr :line-cdadar :line-cddadr :line-cdaddr :line-cdadr
    :line-cddar :line-cddr :line-cddaar :line-cdaar
    :line-last :line-reverse


    ;; STRING/CHAR
    ;; =======================
    :char-list
    :char-string
    :char-nth

    :char-remove
    :char-remove-all
    :char-position
    :char-positions
    :char-split
    :char-length

    :char-car
    :char-caar :char-cadddrr :char-cadaar :char-cadr :char-caadr
    :char-caaadr :char-caaaar :char-caadar :char-caddr :char-caaar
    :char-cdr
    :char-cdddar :char-cdar :char-cdaaar :char-cddddr :char-cdddr
    :char-cdaadr :char-cdadar :char-cddadr :char-cdaddr :char-cdadr
    :char-cddar :char-cddr :char-cddaar :char-cdaar
    :char-last :char-reverse


    ;; LIST
    ;; =======================
    :up-to
    :up-from

    :before
    :after

    :preceding
    :following

    :positions
    
    :replace-at

    :odds
    :evens

    :property-list-merge


    ;; DATE
    ;; =======================
    :iso-time  ;; see (local-time:format-timestring nil timestamp)
    :get-iso-time
    ;; see (local-time:format-timestring nil (local-time:universal-to-timestamp)
    :iso-date-distance ;; see #'local-time:timestamp-difference
    :day-number ;; see #'local-time:format-timestring
    :week-number ;; see #'local-time:format-timestring


    ;; FILE
    ;; =======================
    :read-file-string ;; see #'alexandria:read-file-into-string
    :write-file-string ;; see #'alexandria:write-string-into-file


    ;; MISC
    ;; =======================
    :random-item
    :random-items
    :iso-time
    :list-or-real
    :value-or
    :file-exists
    :read-line-entire
    :parse-keys))


(in-package :nih)
