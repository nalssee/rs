;; Add more macros later
(def-rmacro let (bindings &rest body)
  `((lambda ,(mapcar #'first bindings) ,@body)
    ,@(mapcar #'second bindings)))

(def-rmacro let* (bindings &rest body)
  (if (null bindings)
      `(progn ,@body)
      `(let (,(first bindings))
	 (let* ,(rest bindings) ,@body))))

(def-rmacro defun (name params &rest body)
  `(<- ,name (lambda ,params ,@body)))

(def-rmacro defvar (name value)
  `(<- ,name ,value))

(def-rmacro setf (&rest xs)
  (if (null (cddr xs))
      `(<- ,(first xs) ,(second xs))
      `(progn
	 (<- ,(first xs) ,(second xs))
	 (setf ,@(cddr xs)))))

(def-rmacro labels (defs &rest body)
  `(progn ,@(loop for def in defs collect
		 `(<- ,(first def) (lambda (,@(second def))
				     ,@(cddr def))))
	  ,@body))

;; flet and labels are the same here!!
(def-rmacro flet (defs &rest body)
  `(progn ,@(loop for def in defs collect
		 `(<- ,(first def) (lambda (,@(second def))
				     ,@(cddr def))))
	  ,@body))



