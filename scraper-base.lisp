(in-package scraper-base)

(defclass scraper ()
  ((info :accessor scraper-info
	 :initarg :info)
   (parser :accessor scraper-parser
	   :initarg :parser)
   (state :accessor scraper-state
	  :initarg :state)
   (position-markers :accessor scraper-position-markers
		     :initform nil)))

(defclass info () ())

(defmacro define-info (name superclasses &rest slots)
  (let ((massaged-slots (mapcar #'(lambda (slot) 
				    (let ((slot-name (concatenate 'string "INFO-" (symbol-name slot))))
				      `(,slot :accessor ,(intern slot-name (symbol-package slot)))))
				slots))
	(fun (concatenate 'string (symbol-name name) "-SLOTS")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,(intern fun (symbol-package name)) ()
	 ',slots)
       (defclass ,name (,@superclasses) (,@massaged-slots))
       ',name)))

(defmacro with-scraped-slots ((object class) &body body)
  (let ((fun (concatenate 'string (symbol-name class) "-SLOTS")))
    `(with-slots ,(funcall (intern fun (symbol-package class))) ,object
       ,@body)))

(defmethod abandon-scraping ((scraper scraper))
  t)

(defmacro define-info-finder (for-slot &body body)
  `(defmethod find-info ((scraper ,(intern (package-name (symbol-package for-slot))
					   (symbol-package for-slot)))
			 (slot (eql ',for-slot)))
     (with-scraped-slots ((scraper-info scraper)
			  ,(construct-info-class-name (intern (package-name (symbol-package for-slot)))))
       ,@body)))

(defmethod find-info ((scraper scraper) (slot t))
  nil)

(defmethod scraper-slot-list ((scraper scraper))
  (funcall (symbol-function (fintern "~A-INFO-SLOTS" (type-of scraper) (type-of scraper)))))

(defmethod scrape ((scraper scraper))
  (unwind-protect
       (block scrape-for-info
	 (dolist (slot (scraper-slot-list scraper))
	   (let ((info (find-info scraper slot)))
	     (setf (slot-value (scraper-info scraper) slot) info)
	     (setf (slot-value (scraper-info scraper) slot) (massage-slot scraper slot))
	     (case (scraper-state scraper)
	       (:abandon (return-from scrape-for-info nil))
	       (t nil)))))
    (abandon-scraping scraper))
  (sanity-check scraper)
  (scraper-info scraper))

(defmacro define-sanity-checker (for-slot &body body)
  `(defmethod check-sanity ((scraper ,(intern (package-name (symbol-package for-slot))
					      (symbol-package for-slot)))
			    (slot (eql ',for-slot)))
     (let ((,for-slot (slot-value (scraper-info scraper) ',for-slot)))
       ,@body)))

(defmethod check-sanity ((scraper scraper) (slot t))
  nil)

(defmethod sanity-check ((scraper scraper))
  (dolist (slot (scraper-slot-list scraper))
    (unless (check-sanity scraper slot) ;(slot-value (scraper-info scraper) slot))
      (error "~S page info slot ~A failed sanity check." scraper slot))))

(defmacro define-slot-massager (for-slot &body body)
  `(defmethod massage-slot ((scraper ,(intern (package-name (symbol-package for-slot))
					      (symbol-package for-slot)))
			    (slot (eql ',for-slot)))
     (let ((,for-slot (slot-value (scraper-info scraper) ',for-slot)))
       ,@body)))

(defmethod massage-slot ((scraper scraper) (slot t))
  (slot-value (scraper-info scraper) slot))

(defun construct-info-class-name (base)
  (intern (concatenate 'string (string base) "-INFO") (symbol-package base)))

(defun construct-parser-class-name (base)
  (intern (concatenate 'string (string base) "-PARSER") (symbol-package base)))

(defmacro define-scraper-internal ((name &key scraper-class parser-class info-class info-slots) &body body)
  (let ((parser-name (construct-parser-class-name name))
	(info-name (construct-info-class-name name)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (defclass ,name (,scraper-class) ())
	 (defclass ,parser-name (,parser-class) ())
	 (define-info ,info-name (,info-class)
	   ,@info-slots)
	 (let ((info-name ',info-name)
	       (parser-name ',parser-name))
	   ,@body)
	 (list ',name ',parser-name ',info-name))))

(defun time-scraper (s)
  (time (scrape s)))

(defmacro define-summarizer (name &key finder massager checker)
  (let ((finder-def (if finder `(define-info-finder ,name ,@finder) nil))
	(massager-def (if massager `(define-slot-massager ,name ,@massager) nil))
	(checker-def (if checker `(define-sanity-checker ,name ,@checker) nil)))
    `(progn ,finder-def
            ,massager-def
	    ,checker-def)))

(defmethod save-info ((info info) (mode symbol))
  (error "I don't know how to save-info for ~S." info))

(defmethod save-info :after ((info info) (mode symbol))
  t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(scraper make-scraper attribute define-info with-scraped-slots abandon-scraping scrape define-info-finder find-info scraper-parser
	    scraper slot make-scraper-internal info scraper-info save-info scraper-position-markers
	    massage-slot define-slot-massager sanity-check check-sanity define-sanity-checker time-scraper scraper-state
	    define-scraper-internal define-summarizer)))


