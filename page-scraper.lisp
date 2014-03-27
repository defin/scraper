(in-package page-scraper)

(defparameter *scraper-buffer-resource* :500k-string-buffer)

(defclass page-scraper (scraper)
  ((url :accessor scraper-url
	:initarg :url)
   (current-chunk :accessor scraper-current-chunk
		  :initform nil)
   (buffer :accessor scraper-buffer
	   :initform (r:claim-resourced-object *scraper-buffer-resource*))))

(defclass page-info (info) ())

(defparameter *report-parser-chunks* nil)
(defparameter *report-parser-path* nil)
 
(defun make-page-scraper-internal (page-url class page-info-class parser-class &optional (file-buffer nil))
  (let ((scraper (make-instance class)))
    (with-accessors ((page-info scraper-info)
		     (url scraper-url)
		     (stream scraper-stream)
		     (parser scraper-parser)
		     (state scraper-state)
		     (buffer scraper-buffer)) scraper
      (setf url page-url)
      (setf page-info (make-instance page-info-class))
      (setf parser (make-html-pull-parser parser-class (if file-buffer
							   file-buffer
							   (typecase url
							     (string (let ((webi (make-instance 'http-client:web-interactor
												:service :local
												:return-headers-p nil
												:buffer buffer)))
								       (http-client:web-request webi page-url)))
							     (pathname (file->string url))
							     (stream url)))
					  :report-chunk-p *report-parser-chunks* :report-path-p *report-parser-path*))
      (setf state :ready))
    scraper))

(defmethod save-buffer ((scraper page-scraper) pathname)
  (with-open-file (f pathname :direction :output :if-does-not-exist :create :if-exists :overwrite)
    (write-sequence (scraper-buffer scraper) f)))

(defmethod abandon-scraping ((scraper page-scraper))
  (pull:abandon-parsing (scraper-parser scraper))
  (r:release-resourced-object *scraper-buffer-resource* (scraper-buffer scraper)))

(defmethod scrape :around ((scraper page-scraper))
  (handler-case
      (call-next-method)
    (pull::end-of-document-reached ()
      nil))
  (scraper-info scraper))

(defmacro define-page-scraper ((name) &rest info-slots)
  `(scraper-base:define-scraper-internal
     (,name
      :scraper-class page-scraper
      :parser-class pull:html-pull-parser
      :info-class page-info
      :info-slots ,info-slots)
     (defmethod make-page-scraper ((url t) (class (eql ',name)) &optional (file-buffer nil))
       (make-page-scraper-internal url ',name scraper-base::info-name scraper-base::parser-name file-buffer))))

(defmethod make-page-scraper ((url t) (class t) &optional (file-buffer nil))
  (declare (ignore file-buffer))
  (error "I don't know how to make a page-scraper for url ~A of class ~S." url class))

(defun time-scrape (s)
  (time (scrape s)))

(defmethod next-chunk ((scraper page-scraper))
  (let* ((chunk1 (next-chunk (scraper-parser scraper)))
	 (chunk2 (copy-chunk chunk1)))
;    (etrace :chunk (chunk1 chunk2))
    (setf (scraper-current-chunk scraper) chunk2)))

(defmethod previous-chunk ((scraper page-scraper))
  ;; use COPY instead of COPY-CHUNK here because COPY-CHUNK calls RELEASE-CHUNK, which is unnecessary
  (copy (pull::parser-previous-chunk (scraper-parser scraper))))

(defmethod skip-to-chunk ((scraper page-scraper) (chunk t) &key (search nil))
  (setf (scraper-current-chunk scraper)
	(copy-chunk (skip-to-chunk (scraper-parser scraper) chunk :search search))))

(defmethod skip-to-path ((scraper page-scraper) (path t))
  (setf (scraper-current-chunk scraper)
	(copy-chunk (skip-to-path (scraper-parser scraper) path))))

(defmethod skip-to-nth ((scraper page-scraper) (n integer) &optional (chunk-type t))
  (setf (scraper-current-chunk scraper)
	(copy-chunk (skip-to-nth (scraper-parser scraper) n chunk-type))))

(defmethod next-row ((scraper page-scraper))
  (setf (scraper-current-chunk scraper)
	(copy-chunk (skip-to-chunk (scraper-parser scraper) :tr))))

(defmethod nth-row ((scraper page-scraper) (count integer))
  (setf (scraper-current-chunk scraper)
	(copy-chunk (skip-to-nth (scraper-parser scraper) count :tr))))

(defmethod next-column ((scraper page-scraper))
  (setf (scraper-current-chunk scraper)
	(copy-chunk (skip-to-chunk (scraper-parser scraper) :td))))

(defmethod nth-column ((scraper page-scraper) (count integer))
  (setf (scraper-current-chunk scraper)
	(copy-chunk (skip-to-nth (scraper-parser scraper) count :td))))

(defmethod next-table ((scraper page-scraper))
  (setf (scraper-current-chunk scraper)
	(copy-chunk (skip-to-chunk (scraper-parser scraper) :table))))

(defmethod nth-table ((scraper page-scraper) (count integer))
  (setf (scraper-current-chunk scraper)
	(copy-chunk (skip-to-nth (scraper-parser scraper) count :table))))

(defmethod set-base-path ((scraper page-scraper) &optional (base nil))
  (set-base-path (scraper-parser scraper) base))

(defmethod skip-to-relative-path ((scraper page-scraper) (to-path vector))
  (skip-to-relative-path (scraper-parser scraper) to-path))

(defmethod skip-chain ((scraper page-scraper) (chain list))
  (skip-chain (scraper-parser scraper) chain))

(defun matchtrace (&key match-bindings iteration-bindings parser-path parser-chunks methods)
  (if match-bindings
      (setf page-scraper::*trace-match-var-bindings* t)
      (setf page-scraper::*trace-match-var-bindings* nil))
  (if iteration-bindings
      (setf page-scraper::*trace-iteration-var-bindings* t)
      (setf page-scraper::*trace-iteration-var-bindings* nil))
  (if parser-path
      (setf page-scraper::*report-parser-path* t)
      (setf page-scraper::*report-parser-path* nil))
  (if parser-chunks
      (setf page-scraper::*report-parser-chunks* t)
      (setf page-scraper::*report-parser-chunks* nil))
  (if methods
      (trace (next-chunk) (skip-to-chunk) (skip-to-nth) (skip-to-path) (set-base-path) (skip-to-relative-path) (skip-chain)
	     (set-position-marker) (set-raw-mode))
      (untrace next-chunk skip-to-chunk skip-to-nth skip-to-path set-base-path skip-to-relative-path skip-chain
	       set-position-marker set-raw-mode))
  t)

(defmethod set-raw-mode ((scraper page-scraper) (end-regex t))
  (let ((tokenizer (pull::parser-tokenizer (scraper-parser scraper))))
    (toke::set-raw-mode tokenizer end-regex)))

(defmethod set-position-marker ((scraper page-scraper) (marker-symbol symbol))
  (let* ((parser (scraper-parser scraper))
	 (tokenizer (pull::parser-tokenizer scraper))
	 (input-source (toke::tokenizer-input tokenizer))
	 (position (input-source-stream-position input-source)))
    (push (cons marker-symbol position) (scraper-position-markers scraper))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(page-scraper make-page-scraper save-buffer
	    list-cookies attribute define-page-info abandon-scraping scrape define-info-finder find-info
	    scraper slot make-page-scraper-internal skip-to-nth next-chunk skip-to-chunk skip-to-path skip-to-nth next-row
	    skip-n-rows next-column skip-n-columns next-table skip-n-tables scraper-url page-info scraper-info save-info
	    nth-row nth-column nth-table save-page define-page-scraper chunk-match set-base-path skip-to-relative-path skip-chain
	    define-slot-massager sanity-check check-sanity define-sanity-checker time-scrape scraper-state matchtrace)))


