#-:asdf (load "/usr/local/lib/common-lisp/asdf/clispfasl/asdf.fasl")
#|
(asdf:oos 'asdf:load-op 'cl-ppcre)
(asdf:oos 'asdf:load-op 'cl-who)
(princ
(multiple-value-bind (sec min hour d m y)
  (get-decoded-time)

  (concatenate 'string (princ-to-string y) "-" (princ-to-string m) "-" (princ-to-string d))))
|#

(asdf:oos 'asdf:load-op 'date-calc)
(defun today-deadline (n)
  (let (new-n dow)
    (multiple-value-bind (y m d)
      (date-calc:today)
      (setf dow (date-calc:day-of-week y m d))
      (setf new-n (+ n dow))
      (setf new-n 
	    (multiple-value-bind (new-n-s new-n-a)
	      (floor new-n 5)
	      (+ new-n-a (* new-n-s 7))))
      (decf new-n dow)
      (if (= dow 5) (incf new-n))
      (multiple-value-bind (y0 m0 d0)
	(date-calc:add-delta-days y m d new-n)
	(format nil "~a/~a/~a -> ~a/~a/~a" y m d y0 m0 d0)))))

;(format t "~a~%" (today-deadline 10))

;----------------------------------------------------------------
(let ((av (argv)))
    (setf last-arg (elt av (- (length av) 1))))

(setf add-num (parse-integer last-arg :junk-allowed t))

(if add-num
  (format t "~a~%" (today-deadline add-num)))

