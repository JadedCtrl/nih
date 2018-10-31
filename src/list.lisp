(in-package :nih)

;; ITEM LIST --> DATA_AFTER_ITEM
(defun following (item list)
  "Return all items following the first instance of ITEM"

  (cdr (up-from (position item list :test #'equal) list)))


;; ITEM LIST --> DATA_UP_TO_ITEM
(defun preceding (item list)
  "Return all items preceding the first instance of ITEM"

  (reverse
    (cdr (reverse
	   (up-to (position item list :test #'equal) list)))))


;; ----------------------------------------


;; ITEM LIST --> ITEM_BEFORE_ITEM
(defun before (item list)
  "Return the single item before ITEM"

  (car (reverse (preceding item list))))


;; ITEM LIST --> ITEM_AFTER_ITEM
(defun after (item list)
  "Return the single item after ITEM"

  (car (following item list)))


;; ----------------------------------------


;; N LIST --> LIST_FROM_N
(defun up-from (n list &optional (n-stack 0))
  "Return all items after and including the position N in list"

  (if (eq n n-stack)
    list
    (up-from n (cdr list) (+ n-stack 1))))


;; N LIST --> LIST_UP_TO_N
(defun up-to (n list &optional (n-stack 0))
  "Return all items before and including the position N in list"

  (cond
    ((not (car list)) nil)
    ((eq n n-stack) (list (car list)))
    ((concatenate 'list
		  (list (car list))
		  (up-to n (cdr list) (+ n-stack 1))))))


;; ----------------------------------------


;; ITEM LIST --> LIST_OF_ITEM_POS
(defun positions (item list &key (test #'eq))
  "Return a list of positions of an item in a list."

  (let ((i 0)
	(stack '()))
    (loop
      :while (< i (length list))
      :do
      (if (funcall test (nth i list) item)
	(setq stack (concatenate 'list stack (list i))))
      (setq i (+ i 1)))

    stack))


;; ----------------------------------------


;; LIST ITEM REPLACEMENT --> LIST_WITH_REPLACEMENTS
(defun replace-at (list item replacement &key (test #'eq))
  "Replace all instances of an item within a list with a replacement."

  (let ((pos-list (positions item list :test test))
	(stack list)
	(i 0))
    (loop
      :while (< i (length pos-list))
      :do
      (setf (nth (nth i pos-list) stack) replacement)
      (setq i (+ 1 i)))

    stack))
