(in-package :rs)

(defparameter *rmacros* (make-hash-table))

(defconstant +upcase-indicator+ #\+)
(defconstant +camel-case-indicator+ #\-)

(defun handle-capital-character (x)
  "+abc+ => ABC,  +ab-c raises an error, don't mix + and -
   foo-bar => fooBar
   abc12 => abc12
   |fooBar| => fooBar
  "
  (let ((result (string x)))
    (cond ((and (char= (char result 0) +upcase-indicator+)
		(char= (char result (1- (length result))) +upcase-indicator+)
		(loop for a across result thereis (alpha-char-p a)))
	   ;; starts with +upcase-indicator+ and there is at least one alphabet
	   ;; if it starts with +upcase-indicator+ it must not contain a
	   ;; +camel-case-indicator+
	   (if (loop for a across result thereis (char= a +camel-case-indicator+))
	       (error "~S, ~S mixed in a symbol: ~A"
		      +upcase-indicator+ +camel-case-indicator+ x)
	       (string-upcase (subseq result 1 (1- (length result))))))
	  ((and (loop for a across result thereis (char= a +camel-case-indicator+))
		(loop for a across result thereis (alpha-char-p a)))
	   ;; camel case indicator included
	   (ppcre:regex-replace-all "-." (string-downcase result)
				    #'(lambda (match &rest xs)
					(declare (ignore xs))
					(string-upcase (char match 1)))
				    :simple-calls t))
	  ((loop for a across result never (lower-case-p a))
	   ;; no lower case
	   (string-downcase result))
	  (t result))))

(defun rmacro (symbol)
  (and (symbolp symbol)
       (gethash symbol *rmacros*)))

(defmacro def-rmacro (name params &body body)
  `(setf (gethash ',name *rmacros*)
	 (lambda ,params ,@body)))

(defun rmacro-expand (x)
  (if (and (listp x) (rmacro (first x)))
      (rmacro-expand
       (apply (rmacro (first x)) (rest x)))
      x))

(defstruct row (indent 0) string)

;; make a row list
(defun rowl (&rest strs)
  (mapcar #'(lambda (s)
	      (make-row :string s))
	  strs))

(defparameter *indent-base* 4)

(defun infix-p (x)
  (member x (mapcar #'(lambda (x) (intern (string x)))
		    '(+ - * / ** ^ < == > <= >= != $ @ %/% %% %*% ~ %in%))))

(defun logical-p (x)
  (or (eql x (intern "AND"))
      (eql x (intern "AND1"))
      (eql x (intern "OR"))
      (eql x (intern "OR1"))
      (eql x (intern "NOT"))))

(defun function-p (x)
  (eql x (intern "LAMBDA")))

;; This is basically the same as compile-body just add one more empty line
(defun compile-toplevel-exps (exps)
  (mapcan (lambda (e)
	    (if (stringp e)
		;; You can put in comments by inserting strings
		(rowl e)
		(compile-expr e)))
	  exps))

(defmacro rs (&body es)
  `(assemble-rows (compile-toplevel-exps ',es)))

(defun extend-indent (r1 n)
  (setf (row-indent r1)
	(+ (row-indent r1) n))
  r1)

(defun attach-row (r1 r2)
  (make-row :indent (row-indent r1)
	    :string (concatenate 'string
				 (row-string r1)
				 (make-string (row-indent r2)
					      :initial-element #\space)
				 (row-string r2))))

(defun beside1 (xs ys)
  (cond ((null xs) ys)
	((null ys) xs)
	(t (append (butlast xs)
	      (let* ((x1 (first (last xs)))
		     (n (+ (row-indent x1) (length (row-string x1)))))
		(cons
		 (attach-row x1 (first ys))
		 (mapcar (lambda (y1)
			   (extend-indent y1 n))
			 (rest ys))))))))

(defun beside (&rest xss)
  (reduce #'beside1 xss))

;; 
(defun tailbite1 (xs ys)
  (cond ((null xs) ys)
	((null ys) xs)
	(t (append (butlast xs)
		   (cons (attach-row (first (last xs)) (first ys))
			 (cdr ys))))))

(defun tailbite (&rest xss)
  (reduce #'tailbite1 xss))

(defun slide (xs n)
  (mapcar (lambda (x)
	    (extend-indent x n)) xs))

(defun assemble-rows (xs)
  (format nil "~{~A~^~%~}"
	  (mapcar (lambda (x)
		    (concatenate 'string (make-string (row-indent x)
						      :initial-element #\space)
				 (row-string x)))
		  xs)))

(defun compile-body (exps)
  (mapcan #'compile-expr exps))

(defun compile-expr (exp)
  (match exp
    ((cons f rest)
     (cond ((function-p f)
	    (let ((body (compile-body (cdr rest))))
	      (if (null (cdr body))
		  ;; single row body
		  (beside
		   (rowl "function ")
		   (compile-args (car rest)) (rowl " {")
		   body (rowl "}"))
		  (append
		   (beside
		    (rowl "function ")
		    (compile-args (car rest))
		    (rowl " {"))
		   (slide body *indent-base*)
		   (rowl "}")))))
	   
	   ((eql f (intern "<-"))
	    (assert (= (length rest) 2))
	    (let* ((val (second rest)))
	      (if (and (listp val) (not (null val)) (function-p (first val)))
		  ;; when value expression is a function expr
		  (tailbite
		   (compile-expr (first rest))
		   (rowl " <- ")
		   (compile-expr (second rest)))
		  (beside (compile-expr (first rest))
			  (rowl " <- ")
			  (compile-expr (second rest))))))
	   
	   ;; backquote 
	   ((eql f (intern "BQ"))
	    (assert (= (length rest) 1))
	    (rowl (format nil "`~A`" (handle-capital-character (first rest)))))

	   ((rmacro f)
	    (compile-expr (rmacro-expand (cons f rest))))

	   ;; square parenthesis
	   ((eql f (intern "REF")) (beside (enclose-paren-if #'listp (car rest))
					   (compile-args (cdr rest) "[")))
	   ;; x[[1]]
	   ((eql f (intern "REF1")) (beside (enclose-paren-if #'listp (car rest))
					    (compile-args (cdr rest) "[[")))
	   
	   ;; Logical operators
	   ((logical-p f) (compile-logic f rest))
	   
	   ((eql f (intern "PROGN")) (compile-body rest))
	    
	   ;; There can be more
	   ((eql f (intern "COND")) (compile-cond rest))
	   ((eql f (intern "IF")) (compile-if rest))
	   ((eql f (intern "FOR")) (compile-for rest))
	   ((eql f (intern "WHILE")) (compile-while rest))
	   ;; 
	   ((consp f)
	    ;; Just enclose the first element with parenthesis
	    (beside (rowl "(")
		    (compile-expr f)
		    (rowl ")")
		    (compile-args rest)))
	   
	   (t
	    ;; Now then it's a function call
	    (if (infix-p f)
		(compile-infix f rest)
		(beside (rowl (handle-capital-character f))
			(compile-args rest ))))))
     
    (e (typecase e
	 ;; You are going to need this!!!
	 (symbol (cond ((eql e (intern "EMPTY")) (rowl ""))
		       ((eql e (intern "NA")) (rowl "NA"))
		       ((eql e (intern "NAN")) (rowl "NaN"))
		       ;; ((eql e (intern "***")) (rowl "..."))
		       ((eql e (intern "TRUE")) (rowl "TRUE"))
		       ((eql e (intern "FALSE")) (rowl "FALSE"))
		       
		       ;; all symbols are downcased
		       (t (rowl (handle-capital-character e)))))
	 (number (rowl (format nil "~A" e)))
	 (string (rowl (format nil "\"~A\"" e)))))))

(defun compile-while (xs)
  (match xs
    ((cons pred body)
     (append
      (beside (rowl "while (")
	      (compile-expr pred)
	      (rowl ") {"))
      (slide (compile-body body) *indent-base*)
      (rowl "}")))))

(defun compile-for (xs)
  (match xs
    ((cons (list i in1 v) es)
     (if (eql in1 (intern "IN"))
	 (append
	  (beside (rowl "for (" )
		  (rowl (handle-capital-character i))
		  (rowl " in ")
		  (compile-expr v)
		  (rowl ") {"))
	  (slide (compile-body es) *indent-base*)
	  (rowl "}"))
	 (error "Unknown identifier ~A" in1)))
    (_ (error "Syntax Error 'for': ~%~A" xs))))

(defun compile-cond (cs)
  (if (null (cdr cs))
      ;; 1 clause conditional, not necessarily the last
      (destructuring-bind (pred &rest exps) (car cs)
	(if (eql pred t)
	    (append (rowl "{")
		    (slide (compile-body exps) *indent-base*)
		    (rowl "}"))
	    (append (beside (rowl "if (")
			    (compile-expr pred)
			    (rowl ") {"))
		    (slide (compile-body exps) *indent-base*)
		    (rowl "}"))))
      
      ;; at least 2 clauses
      (destructuring-bind ((pred &rest exps) &rest cs) cs
	(tailbite
	 (append (beside (rowl "if (")
			 (compile-expr pred)
			 (rowl ") {"))
		 (slide (compile-body exps) *indent-base*)
		 (rowl "}"))
	 (rowl " else ")
	 (compile-cond cs)))))

(defun compile-if (exps)
  (cond ((or (< (length exps) 2)
	     (> (length exps) 3))
	 (error "if syntax error ~{~A ~}" exps))
	((null (cddr exps))
	 ;; no alternative 
	 (append (beside (rowl "if (")
			 (compile-expr (first exps))
			 (rowl ") {"))
		 (slide (compile-expr (second exps)) *indent-base*)
		 (rowl "}")))
	(t (append (beside (rowl "if (")
			   (compile-expr (first exps))
			   (rowl ") {"))
		   (slide (compile-expr (second exps)) *indent-base*)
		   (rowl "} else {")
		   (slide (compile-expr (third exps)) *indent-base*)
		   (rowl "}")))))

(defun matching-paren (x)
  (cond ((equal x "(") ")")
	((equal x "[") "]")
	((equal x "{") "}")
	((equal x "[[") "]]")
	(t (error "Unknown parenthesis ~A" x))))

(defun compile-args (args &optional (paren "("))
  (labels ((iter (args result)
	     (match args
	       (() (beside
		    (rowl paren)
		    result
		    (rowl (matching-paren paren))))
	       ((list* e0 e1 es)
		(if (keywordp e0)
		    (if (null es)
			(iter es (beside
				  result
				  (beside (rowl (handle-capital-character e0))
					  (rowl " = ")
					  (compile-expr e1))))
			(iter es (beside
				  result
				  (beside (rowl (handle-capital-character e0))
					  (rowl " = ")
					  (compile-expr e1)
					  (rowl ", ")))))
		    (iter (cons e1 es)
			  (beside
			   result
			   (beside (compile-expr e0)
				   (rowl ", "))))))
	       ((list e) (iter '()
			       (beside result
				       (compile-expr e)))))))

    (iter args '())))

(defun compile-logic (f exps)
  (flet ((build-logical-expr (op)
	   (apply #'beside
		  (intersperse
		   (rowl op)
		   (mapcar #'(lambda (exp)
			       (enclose-paren-if #'infix-or-logical? exp))
			   exps)))))
    (cond ((eql f (intern "NOT"))
	   (if (not (eql (length exps) 1))
	       (error "NOT used with multiple args ~{~A ~}" exps)
	       (beside (rowl "! ") (enclose-paren-if #'infix-or-logical? (car exps)))))
	  ;; 
	  ((< (length exps) 2)
	   (error "~A requires at least 2 exps, but ~{~A ~}" f exps))
	  (t (build-logical-expr
	      (cond ((eql f (intern "AND"))  " && ")
		    ((eql f (intern "AND1")) " & ")
		    ((eql f (intern "OR"))   " || ")
		    ((eql f (intern "OR1"))  " | ")))))))


(defun intersperse (k xs)
  "returns a (x1 k x2 k x3 k ... xn), got it?"
  (rest (mapcan #'(lambda (x) (list k x)) xs)))

(defun compile-infix (f args)
  (cond ((and (eql f (intern "-"))
	      (= (length args) 1))
	 ;; (- x) => -x
	 (beside (rowl "-") (compile-expr (first args))))
	((< (length args) 2)
	 (error "Infix operator called with a single arg: ~A ~A" f (first args)))
	((null (cddr args))
	 (beside (enclose-paren-if #'infix-or-logical? (first args))
		 ;; infix operators do not contain alphabets afaik
		 ;; I'm just being cautious.
		 (rowl (concatenate 'string " " (handle-capital-character f) " "))
		 (enclose-paren-if #'infix-or-logical? (second args))))
	(t
	 (beside (enclose-paren-if #'infix-or-logical? (first args))
		 (rowl (concatenate 'string " " (handle-capital-character f) " "))
		 (compile-infix f (cdr args))))))

;; actually should've been named "compile-enclose-paren-if"
(defun enclose-paren-if (condition expr)
  "If the condition is met, compile-expr it and
   enclose with parenthesis, or just compile-expr it"
  (if (funcall condition expr)
      (beside (rowl "(") (compile-expr expr) (rowl ")"))
      (compile-expr expr)))

(defun infix-or-logical? (expr)
  (and (listp expr)
       (not (null expr))
       (or (infix-p (first expr))
	   (logical-p (first expr)))))

(defmacro load-rs-prelude ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((prelude-file (make-pathname
			  :name "prelude"
			  :type "lisp"
			  :defaults (asdf:system-source-directory "rs"))))
       (load prelude-file)
       (format t "~&~A loaded~%" prelude-file))))

