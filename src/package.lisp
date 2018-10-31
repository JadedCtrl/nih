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

    :nil-string

    :pad-string
    :min-string-length

    :getf-string
    :getf-strings

    :get-colon-values
    :remove-colon-values


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

    :word-car
    :word-caar :word-cadddrr :word-cadaar :word-cadr :word-caadr
    :word-caaadr :word-caaaar :word-caadar :word-caddr :word-caaar
    :word-cdr
    :word-cdddar :word-cdar :word-cdaaar :word-cddddr :word-cdddr
    :word-cdaadr :word-cdadar :word-cddadr :word-cdaddr :word-cdadr
    :word-cddar :word-cddr :word-cddaar :word-cdaar


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

    :line-car
    :line-caar :line-cadddrr :line-cadaar :line-cadr :line-caadr
    :line-caaadr :line-caaaar :line-caadar :line-caddr :line-caaar
    :line-cdr
    :line-cdddar :line-cdar :line-cdaaar :line-cddddr :line-cdddr
    :line-cdaadr :line-cdadar :line-cddadr :line-cdaddr :line-cdadr
    :line-cddar :line-cddr :line-cddaar :line-cdaar


    ;; STRING/CHAR
    ;; =======================
    :char-list
    :char-string
    :char-nth

    :char-remove
    :char-remove-all
    :char-position
    :char-positions

    :char-car
    :char-caar :char-cadddrr :char-cadaar :char-cadr :char-caadr
    :char-caaadr :char-caaaar :char-caadar :char-caddr :char-caaar
    :char-cdr
    :char-cdddar :char-cdar :char-cdaaar :char-cddddr :char-cdddr
    :char-cdaadr :char-cdadar :char-cddadr :char-cdaddr :char-cdadr
    :char-cddar :char-cddr :char-cddaar :char-cdaar


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


    ;; MISC
    ;; =======================
    :random-item
    :iso-time
    :list-or-real
    :value-or
    :parse-keys))


(in-package :nih)
