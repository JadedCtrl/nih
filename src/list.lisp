(in-package :nih)

;; ITEM LIST --> DATA_AFTER_ITEM
(defun following (item list &key (test #'eq))
  "Return all items following the first instance of ITEM"

  (cdr (up-from (position item list :test test) list)))


;; ITEM LIST --> DATA_UP_TO_ITEM
(defun preceding (item list &key (test #'eq))
  "Return all items preceding the first instance of ITEM"

  (reverse
    (cdr (reverse
	   (up-to (position item list :test test) list)))))


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


;; ----------------------------------------


;; LIST --> LIST_OF_ODD-NUMBERED_ITEMS
(defun odds (list)
  "Return a list only containing the odd-numbered items of a list."

  (let ((stack '())
	(i 0))

    (loop
      :while (< i (length list))
      :do
      (if (oddp i)
	(setq stack (concatenate 'list stack (list (nth i list)))))

      (setq i (+ 1 i)))

    stack))


;; LIST --> LIST_OF_ODD-NUMBERED_ITEMS
(defun evens (list)
  "Return a list only containing the even-numbered items of a list."

  (let ((stack '())
	(i 0))

    (loop
      :while (< i (length list))
      :do
      (if (evenp i)
	(setq stack (concatenate 'list stack (list (nth i list)))))

      (setq i (+ 1 i)))

    stack))


;; ----------------------------------------



;; PLIST PLIST --> PLIST
(defun property-list-merge (plist-a plist-b)
  "Merge two property-lists, with plist-a being the canonical one.
  Useful for when you have defaults (in plist-a) and modifications to
  them (in plist-b), especially for configs."

  (let* ((keys (evens plist-a))
	 (pairs (length keys))
	 (stack '())
	 (i 0))

    (loop
      :while (< i pairs)
      :do
      (let* ((key (nth i keys))
	     (a-value (getf plist-a key))
	     (b-value (getf plist-b key)))

	(setq stack
	      (append stack
		      (list key (value-or b-value a-value))))
	(setq i (+ i 1))))
      stack))
