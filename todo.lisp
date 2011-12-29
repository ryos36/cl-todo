#-:asdf (load "/usr/local/lib/common-lisp/asdf/clispfasl/asdf.fasl")

;(pushnew "/home/WWW/lisp-web/cl-clasp/" asdf:*central-registry* :test #'equal)
(asdf:load-system :cl-ppcre :verbose nil)

(defun a-todo-to-lisp ( a-todo )
  (cl-ppcre:register-groups-bind 
	  (section detail estimate)
	  ("(\\++)\\S*([^,]*)(.*)" a-todo)
	  (list 
	    (let ((sum 0 ))
		  (cl-ppcre:do-matches (s e "\\+" section)
				       (incf sum (- e s)))
		  sum)
	    detail 
	    (cl-ppcre:scan-to-strings "[0-9]+" estimate))))

(defun up-stair (layer-list tmp-list t-list)
  (let* ((parents (pop layer-list))
	 (last-parent (pop parents))
	 (last-item (car (last last-parent)))
	 last-child-last-parent)
    (if (listp last-item) 
      (setf last-child-last-parent last-item
	    last-parent (delete-if #'listp last-parent)))

    (format t "upstair <~a> <~a> <~a> <~a>~%" layer-list parents last-parent last-child-last-parent)

    (format t "upstair last-parent <~a> ~%" (append nil (reverse tmp-list)))

    (setf last-parent
	  (append last-parent (append last-child-last-parent (reverse tmp-list))))
    (format t "upstair last-parent <~a> ~%" last-parent)
    (push last-parent parents)
    (if t-list
      (push t-list parents))
    (values layer-list parents)))

(setf layer-list nil)
(setf tmp-list nil)
(push '((top1 2)) layer-list)
(push '(sub2-1 1) tmp-list)
(push '(sub2-2 2) tmp-list)
(push tmp-list layer-list)
(setf tmp-list nil)
(push '(sub3 3) tmp-list)
(format t "~a ~a ~a~%" layer-list tmp-list nil)
(multiple-value-bind (layer-list tmp-list)
		     (up-stair layer-list tmp-list '(sub2-3 3)))
(format t "xxx <~a> <~a> <~a>~%" layer-list tmp-list nil)
(quit)

(defun list-to-layer (alist) 
  (let ((level 1) result tmp-list)
    (mapcar (lambda (a-todo)
	      (let ((t-level (car a-todo))
		    (t-todo (cdr a-todo)))
		(cond ((= level t-level)
		       (push t-todo tmp-list))
		      ((< level t-level)
		       (progn
			 (push tmp-list result)
			 (setf tmp-list nil)
			 (push t-todo tmp-list)))
		      ((> level t-level)
		       (multiple-value-bind (result tmp-list)
		       	(up-stair result tmp-list t-level))))
		(setf level t-level)
	      (format t "~a [~a ~a] ~a ~a~%" level t-level t-todo result tmp-list)
	      ))
	    alist)
    (nreverse result)))

(defun view-one (a-layer-list level)
  (let ((detail (car a-layer-list))
	(estimate (cadr a-layer-list)))
    (dotimes (i level nil) 
      (format t "+"))
    (format t " ~a" detail)
    (if (listp estimate)
      (progn
	(format t "~%")
	(view-layer estimate (+ level)))
      (format t " ~a~%" estimate))))

(defun view-layer (layer-list level)
  (dolist (item layer-list)
    (view-one item level)))

(setf file-name "todo.txt")
(setf layer-data
      (list-to-layer
	(with-open-file (stream file-name)
	  (do ((line (read-line stream nil)
		     (read-line stream nil)) result a-todo)
	    ((null line) (nreverse result))
	    (setf a-todo (a-todo-to-lisp line))
	    ;(print a-todo)
	    (push a-todo result)))))

(format t "~a~%" layer-data 1)
(format t "~a~%" (view-layer layer-data 1))

#|
(setf l (read-line))
(format t "~a~%" l)
(format t "~a~%"
	(cl-ppcre:register-groups-bind 
	  (section detail estimate)
	  ("(\\++) ([^,]*), *([0-9]*)[hH]$" l)
	  (list section detail estimate)))

(format t "~a~%" 
	(eval 
	  (with-input-from-string
	    (in-str l)
	    (read in-str))))
|#
