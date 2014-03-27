(in-package page-scraper)

;;;; Transformation methods

; !TODO split this off into a seperate file

;; always match the next chunk if its a start tag
(defmethod transform-pattern ((pattern symbol) (op (eql :start-tag)))
  (with-pattern-compilation-environment ()
    `(typep (next-chunk ,scraper-sym) 'dtd-base:start-tag)))

;; always match the next chunk if its an end-tag
(defmethod transform-pattern ((pattern symbol) (op (eql :end-tag)))
  (with-pattern-compilation-environment ()
    `(typep (next-chunk ,scraper-sym) 'dtd-base:end-tag)))

;; always match the next chunk if its an empty-element-tag
(defmethod transform-pattern ((pattern symbol) (op (eql :empty-element-tag)))
  (with-pattern-compilation-environment ()
    `(typep (next-chunk ,scraper-sym) 'dtd-base:empty-element-tag)))

;; always match the next chunk if its a tag
(defmethod transform-pattern ((pattern symbol) (op (eql :tag)))
  (with-pattern-compilation-environment ()
    `(typep (next-chunk ,scraper-sym) '(or dtd-base:start-tag dtd-base:end-tag))))

;; always match the next chunk if its some content
(defmethod transform-pattern ((pattern symbol) (op (eql :content)))
  (with-pattern-compilation-environment ()
    `(typep (next-chunk ,scraper-sym) 'dtd-base:content)))

;; always match the next chunk
(defmethod transform-pattern ((pattern symbol) (op (eql :next)))
  (with-pattern-compilation-environment ()
    `(next-chunk ,scraper-sym)))

;; always true
(defmethod transform-pattern ((pattern symbol) (op (eql :t)))
  (with-pattern-compilation-environment ()
    (transform-pattern pattern :pass)))

;; always true.  antonym of FAIL.
(defmethod transform-pattern ((pattern symbol) (op (eql :pass)))
  (with-pattern-compilation-environment ()
    t))

;; always fail (ends inner match), has no effect on position
(defmethod transform-pattern ((pattern symbol) (op (eql :nil)))
  (with-pattern-compilation-environment ()
    (transform-pattern pattern :fail)))

;; always fail (ends inner match), has no effect on position
(defmethod transform-pattern ((pattern symbol) (op (eql :fail)))
  (with-pattern-compilation-environment ()
    nil))

(defmethod transform-pattern ((pattern list) (op (eql :setf)))
  (with-pattern-compilation-environment ()
    (transform-pattern pattern :assign)))

(defmethod transform-pattern ((pattern list) (op (eql :assign)))
  (with-pattern-compilation-environment ()
    (multiple-value-bind (success)
	(decompose-variable-name (second pattern))
      (if success
	  (error "Cannot ~S, because ~S specifies a slot access." pattern (second pattern))
	  (if (find (second pattern) bound :test #'string-equal)
	      (error "Cannot assign ~A, it has already been assigned." (second pattern))
	      (let ((value (gensym "VALUE-")))
		(if (match-var-p (second pattern))
		    (progn (push (second pattern) bound)
			   `(let ((,value ,(transform-pattern (third pattern) nil)))
			      (push (cons ',(second pattern) ,value) ,bindings-sym)
			      (setf ,(second pattern) ,value)))
		    `(let ((,value ,(transform-pattern (third pattern) nil)))
		       (setf ,(second pattern) ,value)))))))))

(defmethod transform-pattern ((pattern list) (op (eql :push)))
  (with-pattern-compilation-environment ()
;    (if (match-var-p (third pattern))
;	(error "Cannot PUSH onto match variable.")
    `(push ,(transform-pattern (second pattern) nil) ,(third pattern))))

(defmethod transform-pattern ((pattern list) (op (eql :pair)))
  (with-pattern-compilation-environment ()
    `(cons ,(second pattern) ,(third pattern))))

(defmethod transform-pattern ((pattern list) (op (eql :value)))
  (with-pattern-compilation-environment ()
    (if (or (and (not (match-var-p (second pattern)))
		 (not (iteration-var-p (second pattern))))
	    (find (second pattern) bound :test #'equalp))
	(second pattern)
	(error "Cannot take the value of ~A here, it has not yet been bound." (second pattern)))))

(defmethod transform-pattern ((pattern list) (op (eql :slot)))
  (with-pattern-compilation-environment ()
    (if (or (match-var-p (second pattern))
	    (iteration-var-p (second pattern))
	    (eq (second pattern) 'current-chunk))
	(if (or (eq (second pattern) 'current-chunk)
		(find (second pattern) bound :test #'equalp))
	    `(chunk-slot ,(second pattern) ',(third pattern))
	    (error "Cannot take the value of ~A here, it has not yet been bound." (second pattern)))
	(error "~S does not specify a match or iteration variable, cannot access slot." (second pattern)))))

(defmethod transform-pattern ((pattern list) (op (eql :match-as-chunk)))
  (with-pattern-compilation-environment ()
    (let* ((regex (second pattern))
	   (options (nthcdr 2 pattern))
	   (regex (concatenate 'string
			       ".*" regex
			       (if (member :with-terminating-whitespace options)
				   "\\W"
				   ""))))
      `(progn (set-raw-mode ,scraper-sym ,regex)
	      (next-chunk ,scraper-sym)))))

(defmethod transform-pattern ((pattern list) (op (eql :oneof)))
  (with-pattern-compilation-environment ()
    `(chunk-match ,scraper-sym ',(rest pattern) (next-chunk ,scraper-sym) :search ,search)))

(defmethod transform-search-pattern ((pattern list) (op (eql :oneof)))
  ;; passing a list in to chunk-match does an OR test, so just chop off the "or" here
  (with-pattern-compilation-environment ()
    `(skip-to-chunk ,scraper-sym ',(rest pattern) :search ,search)))

(defmethod transform-search-pattern ((pattern t) (op t))
  (error "It is meaningless to (SEARCH ~A)." pattern))

(defmethod transform-pattern ((pattern list) (op (eql :progn)))
  (with-pattern-compilation-environment ()
    (transform-pattern pattern :group)))

(defmethod transform-pattern ((pattern list) (op (eql :group)))
  (with-pattern-compilation-environment ()
    `(progn ,@(mappatterns (rest pattern)))))

(defmethod transform-pattern ((pattern list) (op (eql :prog1)))
  (with-pattern-compilation-environment ()
    (transform-pattern pattern :group1)))

(defmethod transform-pattern ((pattern list) (op (eql :group1)))
  (with-pattern-compilation-environment ()
    `(prog1 ,@(mappatterns (rest pattern)))))

(defmethod transform-pattern ((pattern list) (op (eql :landmark)))
  (with-pattern-compilation-environment ()
    (let ((markname (cadr pattern)))
      (if (keywordp markname)
	  `(and (set-landmark ,markname)
		,@(mappatterns (cddr pattern)))
	  `(and ,@(mappatterns (rest pattern)))))))

(defmethod transform-pattern ((pattern list) (op (eql :data)))
  (with-pattern-compilation-environment ()
    `(and ,@(mappatterns (rest pattern)))))

(defmethod transform-pattern ((pattern list) (op (eql :or)))
  (with-pattern-compilation-environment ()
    `(or ,@(mappatterns (rest pattern)))))

(defmethod transform-pattern ((pattern list) (op (eql :and)))
  (with-pattern-compilation-environment ()
    `(and ,@(mappatterns (rest pattern)))))

(defmethod transform-pattern ((pattern list) (op (eql :not)))
  (with-pattern-compilation-environment ()
    `(not ,(transform-pattern (second pattern) nil))))

(defmethod transform-pattern ((pattern list) (op (eql :if)))
  (with-pattern-compilation-environment ()
    `(if ,(transform-pattern (second pattern) nil)
         ,(transform-pattern (third pattern) nil)
         ,(transform-pattern (fourth pattern) nil))))

(defmethod transform-pattern ((pattern list) (op (eql :cond)))
  (with-pattern-compilation-environment ()
    (let ((clauses (loop for clause in (rest pattern)
			 collect (append (list (transform-pattern (first clause) nil))
					 (mappatterns (rest clause))))))
      `(cond ,@clauses))))

(defmethod transform-pattern ((pattern list) (op (eql :cond-every)))
  (with-pattern-compilation-environment ()
    (let ((clauses (loop for clause in (rest pattern)
			 collect (append (list (transform-pattern (first clause) nil))
					 (mappatterns (rest clause))))))
      `(cond-every ,@clauses))))

(defmethod transform-pattern ((pattern list) (op (eql :chunkcase)))
  (with-pattern-compilation-environment ()
    (let* ((form (second pattern))
	   (clauses (loop for clause in (cddr pattern)
			  collect (append (list `(chunk-match ,(first clause) ,form :search ,search))
					  (mappatterns (rest clause))))))
      `(cond ,@clauses))))

(defmethod transform-pattern ((pattern list) (op (eql :when)))
  (with-pattern-compilation-environment ()
    `(when ,(transform-pattern (second pattern) nil)
       ,(transform-pattern (caddr pattern) nil))))

(defmethod transform-pattern ((pattern list) (op (eql :unless)))
  (with-pattern-compilation-environment ()
    `(unless ,(transform-pattern (second pattern) nil)
       ,(transform-pattern (caddr pattern) nil))))

;; keep matching a pattern repeatedly until it doesnt match anymore, resetting all iteration vars each iteration.  always returns t
(defmethod transform-pattern ((pattern list) (op (eql :while)))
  (with-pattern-compilation-environment ()
    `(loop with loop-value = t
	   with loop-value-tail = (list nil)
	   for loop-iteration from 1 to *maximum-loop-iterations*
	   do (progn ,@(iteration-unbinding-forms iteration-var-list))
           while (and ,@(mappatterns (rest pattern)))
           finally (return loop-value))))

;; keep attempting to match a pattern repeatedly until it succeeds matching, resetting all iteration vars each iteration.  always returns t
(defmethod transform-pattern ((pattern list) (op (eql :until)))
  (with-pattern-compilation-environment ()
    `(loop with loop-value = t
	   with loop-value-tail = (list nil)
	   for loop-iteration from 1 to *maximum-loop-iterations*
	   do (progn ,@(iteration-unbinding-forms iteration-var-list))
	   until (progn ,@(mappatterns (rest pattern)))
           finally (return loop-value))))

(defmethod transform-pattern ((pattern list) (op (eql :dotimes)))
  (with-pattern-compilation-environment ()
    `(loop with loop-value = t
	   with loop-value-tail = (list nil)
	   for loop-iteration from 1 to *maximum-loop-iterations*
	   for ,(first (second pattern)) from 1 to ,(second (second pattern))
           do (progn ,@(iteration-unbinding-forms iteration-var-list))
              (progn ,@(mappatterns (cddr pattern)))
	   finally (return loop-value))))

(defmethod transform-pattern ((pattern list) (op (eql :collect)))
  (with-pattern-compilation-environment ()
    `(let ((.value. ,(transform-pattern (second pattern) nil)))
       (if (cdr loop-value-tail)
	   (setf (cdr (cdr loop-value-tail)) (list .value.)
		 (cdr loop-value-tail) (cdr (cdr loop-value-tail)))
	   (setf (cdr loop-value-tail) (list .value.)
		 (car loop-value-tail) (cdr loop-value-tail)))
       ;; if loop-value isn't pointing at the collection, fix it.
       (unless (and (listp loop-value)
		    (not (null loop-value)))
	 (setf loop-value (car loop-value-tail)))
       t)))

;; for testing
(let ((loop-value nil)
      (loop-value-tail (cons nil nil)))
  (defun testc (x)
    (if (cdr loop-value-tail)
	(setf (cdr (cdr loop-value-tail)) (list x)
	      (cdr loop-value-tail) (cdr (cdr loop-value-tail)))
	(setf (cdr loop-value-tail) (list x)
	      (car loop-value-tail) (cdr loop-value-tail)
	      loop-value (car loop-value-tail))))
  (defun testv ()
    (values loop-value loop-value-tail)))

(defmethod transform-pattern ((pattern list) (op (eql :content=)))
  (with-pattern-compilation-environment ()
    `(string= (content-text ,(second pattern)) ,(third pattern))))

(defmethod transform-pattern ((pattern list) (op (eql :content-match)))
  (with-pattern-compilation-environment ()
    `(pull::string-match (content-text ,(second pattern)) ,(third pattern) :search t :case-fold t)))

(defmethod transform-pattern ((pattern list) (op (eql :chunk-match)))
  (with-pattern-compilation-environment ()
    `(chunk-match ,(second pattern) ,(third pattern) :search t :case-fold t)))

(defmethod transform-pattern ((pattern list) (op (eql :report)))
  (with-pattern-compilation-environment ()
    `(progn (format *report-stream* "~&~A~%" ,(second pattern))
            t)))

(defmethod transform-pattern ((pattern list) (op (eql :freport)))
  (with-pattern-compilation-environment ()
    `(progn (format *report-stream* ,(second pattern) ,@(cddr pattern))
            t)))

(defmethod transform-pattern ((pattern list) (op (eql :nth)))
  (with-pattern-compilation-environment ()
    `(skip-to-nth ,scraper-sym ,(second pattern) ,(third pattern))))

(defmethod transform-pattern ((pattern list) (op (eql :typep)))
  (with-pattern-compilation-environment ()
    `(typep ,(second pattern) ',(third pattern))))

(defmethod transform-pattern ((pattern list) (op (eql :path)))
  (with-pattern-compilation-environment ()
    `(pull::path-match ,(apply #'vector (rest pattern)) (pull::parser-path (scraper-parser ,scraper-sym)))))

(defmethod transform-search-pattern ((pattern list) (for (eql :path)))
  (with-pattern-compilation-environment ()
    `(skip-to-path ,scraper-sym ,(apply #'vector (rest pattern)))))

(defmethod transform-pattern ((pattern list) (op (eql :relative-path)))
  (with-pattern-compilation-environment ()
    `(pull::relative-path-match ,(rest pattern) (pull::parser-path (scraper-parser ,scraper-sym)))))

(defmethod transform-search-pattern ((pattern list) (for (eql :relative-path)))
  (with-pattern-compilation-environment ()
    `(skip-to-relative-path ,scraper-sym ,(apply #'vector (cddr pattern)) :search ,search)))

(defmethod transform-search-pattern ((pattern list) (for (eql :mark-position)))
  (with-pattern-compilation-environment ()
    (let ((marker-symbol (second pattern)))
      `(set-position-marker ,scraper-sym ,marker-symbol))))

;; execute arbitrary lisp code which is made to return t or nil
(defmethod transform-pattern ((pattern list) (op (eql :lisp)))
  (with-pattern-compilation-environment ()
    `(not (not ,(car (rest pattern))))))

(defmethod transform-pattern ((pattern symbol) (op null))
  (with-pattern-compilation-environment ()
    (cond ((match-var-p pattern)
	   (bind-match-var pattern))
	  ((iteration-var-p pattern)
	   (bind-iteration-var pattern))
	  ((keywordp pattern)
	   ; Self-matching
	   pattern)
	  (t (transform-pattern pattern (intern (symbol-name pattern) 'keyword))))))

(defmethod transform-pattern ((pattern list) (op null))
  (with-pattern-compilation-environment ()
    (transform-pattern pattern (intern (symbol-name (first pattern)) 'keyword))))

(defmethod transform-pattern ((pattern integer) (op null))
  (with-pattern-compilation-environment ()
    `(skip-to-nth ,scraper-sym ,pattern)))

(defmethod transform-pattern ((pattern string) (op null))
  (with-pattern-compilation-environment ()
    `(chunk-match ,pattern (next-chunk ,scraper-sym) :search ,search)))

(defmethod transform-search-pattern ((pattern string) (op null))
  (with-pattern-compilation-environment ()
    `(skip-to-chunk ,scraper-sym ,pattern :search ,search)))

(defmethod transform-pattern ((pattern dtd-base:chunk) (op null))
  (with-pattern-compilation-environment ()
    `(chunk-match ,pattern (next-chunk ,scraper-sym) :search ,search)))

(defmethod transform-search-pattern ((pattern dtd-base:chunk) (op null))
  (with-pattern-compilation-environment ()
    `(skip-to-chunk ,scraper-sym ,pattern :search ,search)))

;; tell the compiler to turn the its argument pattern into a search if it makes sense to do so
;; this should accept any number of chunks to search for and only return when its found them all in-order.
(defmethod transform-pattern ((pattern list) (op (eql :search)))
  (with-pattern-compilation-environment ()
    (transform-search-pattern (second pattern)
			      (if (listp (second pattern))
				  (intern (symbol-name (first (second pattern))) 'keyword)
				  nil))))

(defmethod transform-pattern ((pattern t) (op t))
  (error "~S is not a valid match expression." pattern))

