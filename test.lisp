(defpackage :rs-test
  (:use :cl rs))

(in-package :rs-test)

(load-rs-prelude)

;; Simply writes R script
;; If you want to execute it, use xr instead.
(format
 t "~A"
 (rs
   (<- x 10)
   ;; Parentheses are handled automatically
   (* 2 (- 3 4))
   (~ y (+ x1 x2 x3))

   |...|
   ;; You can represent capital letters using escape characters | |.
   ;; However, the following four symbols are treated specially
   true   +true+
   false  +false+
   nan    |NaN|
   na     +na+
   
   (|fooBarBaz| :+key1+ 10 :|keY2| empty)
   
   (<- x "hello 'nalssee'")

   ;; logical operators, and, and1, or, or1, xor
   (and a (> 3 x) (- 3 4) (or1 (== -10 x) 3))
   (xor a b)
   ;; nothing special about dots inside a symbol
   (data.frame :x1 (c 1 2 3) :x2 (c 3 4 5))
   ;; function definition and calls
   (<- addit (lambda (x) (lambda (y) (+ x y))))
   ;; the following is somewhat different from CL
   ((addit 10) 20)
   ((lambda (x) (+ x 10)) 20)

   ;; referencing with 'ref' and 'ref1'
   (<- (ref x (> x 10)) 10)
   (<- (ref1 x (> x 10)) 10)
   
   ;; progn 
   (progn
     (+ x y)
     z)
   ;; while special form
   (while (< x 10)
     (print x)
     (<- x (+ x 1)))
   ;; for loop special form
   (for (i in (seq 1 10))
     (print i))))

;;; You can use macros.

(format t " ~%~%# Macros test~%")

(format
 t
 "~A"
 (rs
   (defvar x 10)
   ;; indentation may look strange, but it like it more ^^;;
   (defun fib (n)
     (cond ((== n 0) 0)
	   ((== n 1) 1)
	   (t (+ (fib (- n 1)) (fib (- n 2))))))
  
   (defun my-sqrt (x)
     (labels ((good-enough-p (guess)
		(< (abs (- (* guess guess) x)) 0.001))
	      (improve (guess)
		(average guess (/ x guess)))
	      (average (x y)
		(/ (+ x y) 2.0))
	      (sqrt-iter (guess)
		(if (good-enough-p guess)
		    guess
		    (sqrt-iter (improve guess)))))
       (sqrt-iter 1.0)))
  
   (let ((x 10)
	 (y 20))
     (+ x y))

   (let* ((x 10)
	  (y (+ x 2)))
     (* x y))
   ))


;; backquote
(format
 t
 "~A"
 (rs
   "# Backquote is needed"
   "# for S3 "
   (defun (bq values<-) (x values)
     (if (!= (length values) (length x))
	 (stop "Invalid replacement length"))
     (vcoords (xcoords x) (ycoords x) values))))

