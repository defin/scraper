(in-package page-scraper)

;; Bugs:
;; can't nest loops cause all iteration vars get unbound on any iteration of any loop
;; -have to keep track of which environment each loop owns and unbind just hte right vars..
(defparameter *report-stream* t)
(defparameter *trace-match-var-bindings* nil)
(defparameter *trace-iteration-var-bindings* nil)

(defvar *maximum-loop-iterations* 1000)

(defstruct (pvars) scraper-sym bindings-sym bound (iteration-var-list nil) (search nil))

(defmacro binding-pvars ((scraper-sym bindings-sym iteration-var-list search) &body body)
  "Need to always call this before any match compiling can be done."
  `(let ((pvars (make-pvars :scraper-sym ,scraper-sym
			    :bindings-sym ,bindings-sym
			    :bound nil
			    :iteration-var-list ,iteration-var-list
			    :search ,search)))
     (declare (special pvars))
     ,@body))

(defmacro with-pattern-compilation-environment (() &body body)
  `(let ()
     (declare (special pvars))
     (with-accessors ((scraper-sym pvars-scraper-sym)
		      (bindings-sym pvars-bindings-sym) 
		      (bound pvars-bound)
		      (iteration-var-list pvars-iteration-var-list) 
		      (search pvars-search)) pvars
       ,@body)))

(defun match-var-p (x)
  (and (symbolp x)
       (char= #\? (char (symbol-name x) 0))))

(defun iteration-var-p (x)
  (and (symbolp x)
       (char= #\@ (char (symbol-name x) 0))))

(defun decompose-variable-name (var)
  (let* ((var-name (symbol-name var))
	 (dotpos (position #\. var-name :test #'char=)))
    (if dotpos
	(let ((slot-name (intern (subseq var-name (1+ dotpos))))
	      (base-name (intern (subseq var-name 0 dotpos))))
	  (values t base-name slot-name))
	(values nil))))

(defun bind-match-var (match-var)
  "Bind a match variable to the current chunk.  Has no effect on position."
  (with-pattern-compilation-environment ()
    (multiple-value-bind (success base-name slot-name)
	(decompose-variable-name match-var)
      (if success
	  (if (find base-name bound :test #'string-equal)
	      (error "Cannot bind ~A, it has already been bound." base-name)
	      (progn (push match-var bound)
		     (push base-name bound)
		     `(progn (push (cons ',match-var (chunk-slot current-chunk ',slot-name)) ,bindings-sym)
		             (setf ,match-var (chunk-slot current-chunk ',slot-name))
		             (push (cons ',base-name current-chunk) ,bindings-sym)
                             (when *trace-match-var-bindings*
			       (format *report-stream* "~&** Bound ~S to ~S.~%" ',base-name current-chunk))
		             (setf ,base-name current-chunk))))
	  (if (find match-var bound :test #'string-equal)
	      (error "Cannot bind ~A, it has already been bound." match-var)
	      (progn (push match-var bound)
		     `(progn (push (cons ',match-var current-chunk) ,bindings-sym)
		       (when *trace-match-var-bindings*
			 (format *report-stream* "~&** Bound ~S to ~S.~%" ',match-var current-chunk))
		       (setf ,match-var current-chunk))))))))

(defun bind-iteration-var (iteration-var &optional (value 'current-chunk))
  "Bind a iteration variable to some value, defaulting to the current chunk.  Has no effect on position."
  (with-pattern-compilation-environment ()
    (multiple-value-bind (success base-name slot-name)
	(decompose-variable-name iteration-var)
      (if success
	  (progn (push iteration-var bound)
		 (push base-name bound)
		 `(progn (push (cons ',iteration-var (chunk-slot ,value ',slot-name)) ,bindings-sym)
		   (setf ,iteration-var (chunk-slot ,value ',slot-name))
		   (push (cons ',base-name ,value) ,bindings-sym)
		   (when *trace-iteration-var-bindings*
		     (format *report-stream* "~&** Bound ~S to ~S.~%" ',base-name ,value))
		   (setf ,base-name ,value)))
	  (progn (push iteration-var bound)
		 `(progn (push (cons ',iteration-var ,value) ,bindings-sym)
		   (when *trace-iteration-var-bindings*
		     (format *report-stream* "~&** Bound ~S to ~S.~%" ',iteration-var ,value))
		   (setf ,iteration-var ,value)))))))

(defun iteration-unbinding-forms (list)
  (mapcar #'(lambda (v)
	      `(progn (when *trace-iteration-var-bindings*
			(format *report-stream* "~&** Unbinding ~S.~%" ',v))
		(setf ,v nil)))
	  list))

(defun mappatterns (list)
  (mapcar #'(lambda (x)
	      (transform-pattern x nil))
	  list))

(defun write-code-for-pattern (pattern vars iteration-vars &key (search t))
  (let* ((scraper-sym (gensym "SCRAPER-"))
	 (bindings-sym (gensym "BINDINGS-")))
    (binding-pvars (scraper-sym bindings-sym iteration-vars search)
      (with-pattern-compilation-environment ()
	`#'(lambda (,scraper-sym &aux ,@vars)
	     (declare (ignorable ,@vars))
	     (let ((,bindings-sym)
		   (landmarks))
	       (with-accessors ((current-chunk scraper-current-chunk)
				(previous-chunk previous-chunk))
		   ,scraper-sym
		 (flet ((match-bind (var)
			  (bind-match-var var))
			(set-landmark (l)
			  (push l landmarks))
			(current-landmark ()
			  (first landmarks)))
		   (if ,(transform-pattern pattern nil)
		       ,bindings-sym
		       nil)))))))))

(defun collect-vars-and-accessors (list match-sym)
  (let ((bind))
    (loop for pattern in list
	  do (cond ((listp pattern)
		    (mapcar #'(lambda (x) (push x bind)) (collect-vars-and-accessors pattern match-sym)))
;		   ((keywordp pattern) pattern)
		   ((or (match-var-p pattern)
			(iteration-var-p pattern))
		    (multiple-value-bind (success base-name slot-name)
			(decompose-variable-name pattern)
		      (declare (ignore slot-name))
		      (if success
			  (progn (push `(,base-name (cdr (assoc ',base-name ,match-sym))) bind)
				 (push `(,pattern (cdr (assoc ',pattern ,match-sym))) bind))
			  (push `(,pattern  (cdr (assoc ',pattern ,match-sym))) bind))))))
    (reverse bind)))

(defmacro with-matched-pattern ((scraper &rest options) pattern &body body)
  (let* ((match-sym (gensym "MATCH-"))
	 (vars-and-vals (collect-vars-and-accessors pattern match-sym))
	 (all-vars (mapcar #'car vars-and-vals))
	 (match-vars-and-vals (remove-if #'iteration-var-p vars-and-vals :key #'car))
	 (iteration-vars-and-vals (remove-if #'match-var-p vars-and-vals :key #'car))
	 (match-vars (mapcar #'car match-vars-and-vals))
	 (match-vals (mapcar #'cadr match-vars-and-vals))
	 (iteration-vars (remove-duplicates (mapcar #'car iteration-vars-and-vals)))
	 (code (write-code-for-pattern pattern all-vars iteration-vars
				       :search (cond ((not (not (find :regexp options))) :regexp)
						     ((not (not (find :search options))) t)
						     (t nil)))))
    `(let* ((,match-sym (funcall ,code ,scraper))
	    (?match-succeeded (not (not ,match-sym))))
       (funcall #'(lambda (,@match-vars)
		    (declare (ignorable ,@match-vars))
		    ,@body)
	        ,@match-vals))))

(defmacro expand-pattern (pattern)
  "Expand PATTERN into its lisp transformation.  Only used for debugging."
  `(binding-pvars ('scraper 'bindings nil t)
     (with-pattern-compilation-environment ()
       (transform-pattern ',pattern nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(expand-pattern with-matched-pattern current-chunk previous-chunk ?match-succeeded loop-value loop-iteration)))
