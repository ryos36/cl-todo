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
	    (let ((str-estimate (cl-ppcre:scan-to-strings "[0-9]+" estimate)))
	      (if (null str-estimate) 0
		(read-from-string str-estimate))))))

;----------------------------------------------------------------
(defun sum-children (children) 
  (reduce (lambda (sum child)
	    (let* ((detail-estimate (car child))
		   (new-children (cadr child))
		   (detail (car detail-estimate))
		   (estimate (cadr detail-estimate)))
	      (incf sum 
		    (if (null new-children)
		      estimate
		      (sum-children new-children)))))
	  children
	  :initial-value 0))

(defun view-one (a-layer-list level)
  (let* ((detail-estimate (car a-layer-list))
	 (children (cadr a-layer-list))
	 (detail (car detail-estimate))
	 (estimate (cadr detail-estimate)))
    (dotimes (i level nil) 
      (format t "+" ))

    (format t " ~a, ~a ~%" detail 
	    (if (null children)
	      estimate
	      (sum-children children)))
    (if (listp children)
      (view-layer children (+ level 1)))))

(defun view-layer (layer-list level)
  (dolist (item layer-list)
    ;(format t "item [~a]~%" item);
    (view-one item level)))

;----------------------------------------------------------------
(defun view-debug-layer (level-layer-list)
  (let ((level (car level-layer-list))
	(layer-list (cdr level-layer-list)))
    (view-layer layer-list level)))

;----------------------------------------------------------------
; '((level ((sub 2) (sub 3))))
; ((3 ((sub3-2 2) ((sub4 3))) ((sub3-3 3) nil)) (1 ((main2 3) nil) ((main3 4) nil)))
(defun have-item-same-level (layer-list level)
  ;(format t "have-item-same-level <~a> <~a>~%" layer-list level)
  (if (null layer-list) nil
    (let ((layer-list-level (caar layer-list)))
      (if (= layer-list-level level)
	(cdr (pop layer-list))))))

(defun layer-list-add-a-todo (layer-list level tmp-list a-todo)
  (let ((t-level (car a-todo))
	(t-todo (cdr a-todo)))
    ;(format t "layer-list-add-a-todo <~a> <~a> <~a> <~a>~%" layer-list level tmp-list a-todo)
    (cond ((= t-level level)
	   (push (list t-todo nil) tmp-list))
	  ((= t-level (- level 1))
	   (setf tmp-list
		 (let ((new-todo (list t-todo tmp-list))
		       (upper-list (have-item-same-level layer-list t-level)))
		   (if upper-list (pop layer-list))
		   (push new-todo upper-list)
		   upper-list)))
	  ((> t-level level)
	   (progn
	     (if tmp-list
	       (push
		 (cons level tmp-list)
		 layer-list))
	     (setf tmp-list (list (list t-todo nil)))))
	  (t (error "format error")))

    ;(format t "----------------------~%")
    ;(format t "<~a> ~a ~a <~a>~%" layer-list t-level level tmp-list)
    ;(view-debug-layer layer-list)
    ;(format t "----------------------~%")

    (values layer-list t-level tmp-list)))

(defun list-to-layer (a-reverse-list)
  (let (layer-list (level 1) tmp-list)
    (dolist (item a-reverse-list layer-list)
      (multiple-value-bind (v-layer-list v-level v-tmp-list)
	(layer-list-add-a-todo layer-list level tmp-list item)
	(setf layer-list v-layer-list
	      level v-level
	      tmp-list v-tmp-list))

      ;(format t "list-to-layer <~a> ~a <~a>~%" layer-list level tmp-list)
      )
    ;(format t "========================~%last <~a> <~a>~%" layer-list tmp-list)
    ;(view-layer tmp-list 1)
    tmp-list
    ))
;----------------------------------------------------------------
(let ((av (argv)))
    (setf last-arg (elt av (- (length av) 1))))

(setf
  todo-file-name
  (if (string= ".todo" (subseq last-arg (- (length last-arg) (length ".todo"))))
    last-arg ))

(when (null todo-file-name) 
  (format t "Usage:~%")
  (format t "      lisp todo.lisp your-todo.todo~%")
  (quit))


(setf layer-data
      (list-to-layer
	(with-open-file (stream todo-file-name)
	  (do ((line (read-line stream nil)
		     (read-line stream nil)) reverse-result a-todo)
	    ((null line) reverse-result)
	    (setf a-todo (a-todo-to-lisp line))
	    ;(print a-todo)
	    (push a-todo reverse-result)
	    reverse-result))))

(view-layer layer-data 1)
;----------------------------------------------------------------
