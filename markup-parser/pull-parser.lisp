(in-package html-pull-parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 0) (safety 3) (compilation-speed 0) (debug 3) (debug-info 3))))

(defclass html-pull-parser ()
  ((tokenizer :accessor parser-tokenizer
	      :documentation "Tokenizer parser is reading from.")
   (dtd :initform (find-dtd :html-transitional-401)
	:accessor parser-dtd
	:initarg :dtd
	:documentation "Which DTD this parser uses.")
   (rules :initform (make-hash-table) ;; NOTE Can this hash table be optimized.. like :test 'eq maybe.. ?
	  :accessor parser-rules
	  :documentation "Table of rules for this parser.")
   (path :accessor parser-path
	 :initform (claim-resourced-object 'object-array)
	 :documentation "Stack of open elements.  Location in markup tree.")
   (base-path :accessor parser-base-path
	      :initform #()
	      :documentation "Slot for saving away a base path to resolve relative paths with.")
   (state :accessor parser-state
	  :initform nil
	  :documentation "Internal state of the parser.")
   (saved :initform nil
	  :accessor parser-saved
	  :documentation "Slot that holds various saved information for use while building a chunk.")
   (chunk :initform nil
	  :accessor parser-chunk
	  :documentation "Tag or content that the parser is working on building.")
   (completed-chunk :initform nil
		    :accessor parser-completed-chunk
		    :documentation "Last chunk completed parsing.  This becomes PREVIOUS-CHUNK on next call to parser.")
   (previous-chunk :initform nil
		   :accessor parser-previous-chunk
		   :documentation "Previous chunk parsed by parser, for use by callers of the parser.")
   (report-stream :accessor parser-report-stream
		  :initform *standard-output*
		  :initarg :report-stream
		  :documentation "Stream for parser to report information on.")
   (report-chunk-p :accessor parser-report-chunk-p
		   :initform nil
		   :initarg :report-chunk-p
		   :documentation "Whether or not parser should report chunks as it parses them.")
   (report-path-p :accessor parser-report-path-p
		   :initform nil
		   :initarg :report-path-p
		   :documentation "Whether or not parser should report the current path whenever it changes.")
   (depth :accessor parser-depth
	  :initform 0
	  :documentation "Current depth in markup tree.")
   (indentation-factor :accessor parser-indentation-factor
		       :initform 2
		       :initarg :indentation-factor
		       :documentation "Multiplier to determine what indentation to use for a particular depth of output.")
   (token-count :initform 0
		:accessor parser-token-count
		:documentation "Number of tokens processed by parser.")
   (chunk-count :initform 0
		:accessor parser-chunk-count
		:documentation "Number of chunks produced by parser.")))

(defun make-html-pull-parser (class stream &rest initargs)
  (let ((parser (apply #'make-instance class initargs)))
    (initialize-parser parser stream)
    parser))

;;; Parser states

(defclass-multiton parser-state () ())
(defclass-multiton start (parser-state) ())
(defclass-multiton waiting-for-attribute-value (parser-state) ())
(defclass-multiton waiting-for-attribute-name (parser-state) ())
(defclass-multiton waiting-for-attribute-equals (parser-state) ())
(defclass-multiton waiting-for-tag-name (parser-state) ())
(defclass-multiton waiting-for-tag-close (parser-state) ())
(defclass-multiton waiting-for-comment (parser-state) ())
(defclass-multiton waiting-for-markup-declaration (parser-state) ())
(defclass-multiton waiting-for-processing-instruction (parser-state) ())

(defmethod state ((parser html-pull-parser) &optional (new-value nil))
  (if new-value
      (setf (parser-state parser) new-value)
      (parser-state parser)))

(defmethod saved ((parser html-pull-parser))
  (with-accessors ((saved parser-saved)) parser
    (prog1 saved
           (setf saved nil))))

(defmethod (setf saved) ((new-value t) (parser html-pull-parser))
  (with-accessors ((saved parser-saved)) parser
    (if saved
	(error "Attempt to overwrite saved value ~A with ~A." saved new-value)
	(setf saved new-value))))

;;; Resourcing

(defmethod release-chunk-buffers ((chunk start-tag))
  (map-element-known-attributes #'(lambda (slot-name)
				    (when (slot-boundp chunk slot-name)
				      (toke:release-token-buffer (slot-value chunk slot-name) '(released-by release-chunk-buffers start-tag))))
				chunk)
  (map-element-unknown-attributes #'(lambda (attribute)
				      (when (slot-boundp attribute 'dtd-base::value)
					(toke:release-token-buffer (unknown-attribute-value attribute)
								   '(released-by release-chunk-buffers start-tag unknown-attribute))))
				  chunk))

(defmethod share-chunk-buffers ((chunk start-tag))
  (map-element-known-attributes #'(lambda (slot-name)
				    (when (slot-boundp chunk slot-name)
				      (toke:share-token-buffer (slot-value chunk slot-name) '(shared-by share-chunk-buffers start-tag))))
				chunk)
  (map-element-unknown-attributes #'(lambda (attribute)
				      (when (slot-boundp attribute 'dtd-base::value)
					(toke:share-token-buffer (unknown-attribute-value attribute)
								 '(shared-by share-chunk-buffers start-tag unknown-attribute))))
				  chunk))

;(defmethod release-chunk-buffers ((chunk empty-element-tag))
;  (release-chunk (empty-element-tag-saved-start-tag chunk)))

;(defmethod share-chunk-buffers ((chunk empty-element-tag))
;  (share-chunk (empty-element-tag-saved-start-tag chunk)))

(defmethod release-chunk-buffers ((chunk unknown-start-tag))
  (map-element-unknown-attributes #'(lambda (attribute)
				      (when (slot-boundp attribute 'dtd-base::value)
					(toke:release-token-buffer (unknown-attribute-value attribute) 
								   '(released-by release-chunk-buffers unknown-start-tag))))
				  chunk))

(defmethod share-chunk-buffers ((chunk unknown-start-tag))
  (map-element-unknown-attributes #'(lambda (attribute)
				      (when (slot-boundp attribute 'dtd-base::value)
					(toke:share-token-buffer (unknown-attribute-value attribute)
								 '(shared-by share-chunk-buffers unknown-start-tag))))
				  chunk))

(defmethod release-chunk-buffers ((chunk unknown-end-tag)))

(defmethod share-chunk-buffers ((chunk unknown-end-tag)))

(defmethod release-chunk-buffers ((chunk content))
  (when (slot-boundp chunk 'dtd-base::text)
    (toke:release-token-buffer (content-text chunk) '(released-by release-chunk-buffers content))))

(defmethod share-chunk-buffers ((chunk content))
  (when (slot-boundp chunk 'dtd-base::text)
    (toke:share-token-buffer (content-text chunk) '(shared-by share-chunk-buffers content))))

(defmethod release-chunk-buffers ((chunk start-of-document)))

(defmethod share-chunk-buffers ((chunk start-of-document)))

(defmethod release-chunk-buffers ((chunk end-of-document)))

(defmethod share-chunk-buffers ((chunk end-of-document)))

(defmethod release-chunk-buffers ((chunk end-tag)))

(defmethod share-chunk-buffers ((chunk end-tag)))

(defmethod release-chunk-buffers ((chunk markup-declaration))
  (toke:release-token-buffer (markup-declaration-data chunk) '(released-by release-chunk-buffers markup-declaration)))

(defmethod share-chunk-buffers ((chunk markup-declaration))
  (toke:share-token-buffer (markup-declaration-data chunk) '(shared-by release-chunk-buffers markup-declaration)))

(defmethod release-chunk-buffers ((chunk markup-comment))
  (let ((comments (markup-declaration-comments chunk)))
    (loop for i from 0 below (fill-pointer comments)
	  do (toke:release-token-buffer (aref comments i) '(released-by release-chunk-buffers markup-comment)))
    (release-resourced-object 'object-array comments)))

(defmethod share-chunk-buffers ((chunk markup-comment))
  (let ((comments (markup-declaration-comments chunk)))
    (loop for i from 0 below (fill-pointer comments)
	  do (toke:share-token-buffer (aref comments i) '(shared-by share-chunk-buffers markup-comment)))))

(defmethod release-chunk-buffers ((chunk processing-instruction))
  )

(defmethod share-chunk-buffers ((chunk processing-instruction))
  )

(defun release-chunk (chunk)
  (release-chunk-buffers chunk)
  (release-resourced-object (type-of chunk) chunk)
  t)

(defun share-chunk (chunk)
  (share-chunk-buffers chunk)
  (share-resourced-object (type-of chunk) chunk)
  chunk)

;;; Rules

(defparameter *rules* nil)

(defmethod initialize-rules ((parser html-pull-parser))
  (let ((*package* (find-package 'pull)))
    (initialize-rule-table (parser-rules parser)))
  t)

(defmethod rule-compliance ((parser html-pull-parser) (rule symbol))
  (with-accessors ((rules parser-rules)) parser
    (rule-state rules rule)))

(defmethod (setf rule-compliance) ((new-value symbol) (parser html-pull-parser) (rule symbol))
  (with-accessors ((rules parser-rules)) parser
    (setf (rule-state rules rule) new-value)))

(define-rule attribute-can-only-be-set-once :relaxed)
(define-rule empty-element-tags-only-on-appropriate-elements :relaxed)


;;; Token handlers

(defmethod set-attribute ((parser html-pull-parser) (chunk element) (attribute-name string) (attribute-value string))
;  (push '(seen-by set-attribute) (rutils::resourced-object-annotations toke::*token-buffer-resource* attribute-name))
  (let ((name (resolve-name attribute-name (parser-dtd parser))))
    (if (eq attribute-name attribute-value) ; dont think this is the right test
	nil
	(toke:release-token-buffer attribute-name '(released-by set-attribute)))
    (if (slot-exists-p chunk name)
	(if (slot-boundp chunk name)
	    (case (rule-compliance parser 'attribute-can-only-be-set-once)
	      (:relaxed (toke:release-token-buffer (slot-value chunk name) '(released-by set-attribute))
			(setf (slot-value chunk name) attribute-value))
	      (:enforced (error "A given attribute may only have one value assigned to it.")))
	    (progn (setf (slot-value chunk name) attribute-value)))
	(vector-push-extend (claim-resourced-object 'unknown-attribute :name name :value attribute-value)
			    (element-unknown-attributes chunk))))
  attribute-value)

(defmethod handle-token ((parser html-pull-parser) (token toke:attribute-equals))
  (state parser waiting-for-attribute-value)
  :more)

(defmethod handle-token ((parser html-pull-parser) (token toke:attribute-name))
  (with-accessors ((saved saved) (chunk parser-chunk) (dtd parser-dtd)) parser
;    (push '(seen-by handle-token attribute-name) (rutils::resourced-object-annotations toke::*token-buffer-resource* (toke:token-data token)))
    (ecase (type-of (state parser))
      (waiting-for-attribute-equals 
       ;; attributes without values are flags, set attribute to its own name
       (let ((s saved))
	 (set-attribute parser chunk s s))
       (setf saved (toke:token-data token))
       (state parser waiting-for-attribute-equals))
      (waiting-for-attribute-name (setf saved (toke:token-data token))
				  (state parser waiting-for-attribute-equals))))
  :more)

(defmethod handle-token ((parser html-pull-parser) (token toke:attribute-value))
  (with-accessors ((saved saved) (dtd parser-dtd) (chunk parser-chunk)) parser
;    (push '(seen-by handle-token attribute-value) (rutils::resourced-object-annotations toke::*token-buffer-resource* (toke:token-data token)))
    (set-attribute parser chunk saved (toke:token-data token))
    (state parser waiting-for-attribute-name))
  :more)

(defmethod handle-token ((parser html-pull-parser) (token toke:comment))
  (with-accessors ((chunk parser-chunk)) parser
;    (push '(seen-by handle-token comment) (rutils::resourced-object-annotations toke::*token-buffer-resource* (toke:token-data token)))
    (when (eq (state parser) waiting-for-markup-declaration)
      (release-chunk chunk)
      (setf chunk (claim-resourced-object 'markup-comment))) ; need a way to use change-class with resources!
    (if (typep chunk 'markup-comment)
	(vector-push-extend (toke:token-data token) (markup-declaration-comments chunk))
	(toke:release-token-buffer (toke:token-data token) '(released-by handle-token comment)))
    (state parser waiting-for-attribute-name))
  :more)

(defmethod handle-token ((parser html-pull-parser) (token toke:content))
  (with-accessors ((chunk parser-chunk)) parser
;    (push '(seen-by handle-token content) (rutils::resourced-object-annotations toke::*token-buffer-resource* (toke:token-data token)))
    (let* ((extra-data (toke::token-extra-data token))
	   (content (claim-resourced-object 'content
					    :text (toke:token-data token)
					    :mode (toke:token-mode token)
					    :extra-data extra-data)))
      (setf (toke::token-extra-data token) nil)
;      (etrace :quux (extra-data))
;      (describe content)
      (when (slot-boundp content 'dtd-base::text)
	(setf chunk content))
;      (etrace :quux1 (chunk))
      ))
  :done)

(define-condition end-of-document-reached (condition)
  ()
  (:documentation "The end of the document has been reached."))

(defmethod handle-token ((parser html-pull-parser) (token toke:end-of-document))
  (with-accessors ((chunk parser-chunk)) parser  
    (setf chunk nil)
    (error 'end-of-document-reached))
  :done)

(defmethod handle-token ((parser html-pull-parser) (token toke:end-tag-open))
  (with-accessors ((saved saved)) parser
    (setf saved token)
    (state parser waiting-for-tag-name))
  :more)

(defmethod handle-token ((parser html-pull-parser) (token toke:markup-declaration))
;  (push '(seen-by handle-token markup-declaration) (rutils::resourced-object-annotations toke::*token-buffer-resource* (toke:token-data token)))
  (with-accessors ((chunk parser-chunk)) parser
    (setf (markup-declaration-data chunk) (toke:token-data token)))
  :more) ; fixme

(defmethod handle-token ((parser html-pull-parser) (token toke:markup-declaration-open))
  (with-accessors ((chunk parser-chunk)) parser
    (state parser waiting-for-markup-declaration)
    (setf chunk (claim-resourced-object 'markup-declaration)))
  :more) ; fixme

;;; !BUG <?xml> parses as CONTENT!  lose.  ok for now i guess though.

(defmethod handle-token ((parser html-pull-parser) (token toke:processing-instruction))
  (with-accessors ((chunk parser-chunk)) parser
    (setf (processing-instruction-data chunk) (toke:token-data token)))
  :more) ; fixme

(defmethod handle-token ((parser html-pull-parser) (token toke:processing-instruction-open))
  (with-accessors ((chunk parser-chunk)) parser
    (state parser waiting-for-processing-instruction)
    (setf chunk (claim-resourced-object 'processing-instruction)))
  :more) ; fixme

(defmethod handle-token ((parser html-pull-parser) (token toke:empty-element-tag-close)) ; this doesnt handle unknown empty element tags
  (with-accessors ((chunk parser-chunk) (saved saved)) parser
    (if (or (typep chunk 'dtd-base::end-tag-optional-element)
	    (typep chunk 'dtd-base::end-tag-forbidden-element))
	(let ((new (claim-resourced-object (empty-element-tag-name (type-of chunk)))))
	  ;; ripped from shallow-copy, merge functionality of copying into an object of a different class there
	  (loop for slot-name in (effective-slot-names chunk) 
	    do (when (slot-boundp chunk slot-name)
		 (let ((value (slot-value chunk slot-name)))
		   (setf (slot-value new slot-name) value))))
	  ;; saved-start-tag has been expunged.  pretty sure its useless!
	  ;;	  (setf (empty-element-tag-saved-start-tag new) chunk)
	  (setf chunk new))
	(case (rule-compliance parser 'empty-element-tags-only-on-appropriate-elements)
	  (:relaxed nil)
	  (:enforced (error "Empty-element tags may only occur in elements that do not require an end tag."))))
    (let ((s saved))
      (when s
	(set-attribute parser chunk s s)))
    (state parser nil))
  :done)

(defmethod handle-token ((parser html-pull-parser) (token toke:start-of-document))
  (with-accessors ((chunk parser-chunk)) parser  
    (setf chunk (claim-resourced-object 'start-of-document)))
  :done)

(defmethod handle-token ((parser html-pull-parser) (token toke:start-tag-open))
  (with-accessors ((saved saved)) parser
    (setf saved token)
    (state parser waiting-for-tag-name))
  :more)

(defmethod handle-token ((parser html-pull-parser) (token toke:tag-close))
  (with-accessors ((chunk parser-chunk) (dtd parser-dtd) (saved saved)) parser
    (let ((s saved))
      (when s
	(set-attribute parser chunk s s)
      (state parser nil))))
  :done)

(defmethod handle-token ((parser html-pull-parser) (token toke:tag-name))
  (with-accessors ((saved saved) (chunk parser-chunk) (dtd parser-dtd)) parser
;    (push '(seen-by handle-token tag-name) (rutils::resourced-object-annotations toke::*token-buffer-resource* (toke:token-data token)))
    (let ((data (toke:token-data token))
	  (s saved))
      (ecase (type-of s)
	(toke:start-tag-open (setf chunk (make-start-tag (resolve-name data dtd)))
			     (state parser waiting-for-attribute-name))
	(toke:end-tag-open (setf chunk (make-end-tag (end-tag-name (resolve-name data dtd))))
			   (state parser waiting-for-tag-close)))
      (toke:release-token-buffer data '(released-by handle-token tag-name))))
  :more)


;;; Interface

(defvar *debug-parser* nil)
(defvar *print-short-paths* t)

(defun report-path (parser)
  (format t "~&!path=~S~%" 
	  (if *print-short-paths*
	      (loop named short
		    with short-path = (make-array (length (parser-path parser)))
		    for i from 0 below (length (parser-path parser))
		    do (setf (aref short-path i) (intern (symbol-name (type-of (aref (parser-path parser) i))) 'keyword))
		    finally (return-from short short-path))
	      (parser-path parser))))

(defun report-chunk (parser chunk)
  (format (parser-report-stream parser)
	  "~&~6,'0D:~6,'0D(~2,'0D)| ~0,1,v@A" 
	  (toke:tokenizer-line-number (parser-tokenizer parser))
	  (parser-chunk-count parser)
	  (parser-depth parser)
	  (* (parser-depth parser) (parser-indentation-factor parser))
	  chunk))

(defun string-match (string1 string2 &key (search nil) (case-fold t))
  (not (not (case search
	      (nil (if case-fold
		       (string-equal string1 string2)
		       (string= string1 string2)))
	      (:regexp (excl:match-regexp string1 string2 :return t :case-fold case-fold))
	      (t (search string1 string2 :test (if case-fold #'string-equal #'string=)))))))

(defgeneric chunk-match (template chunk &key (search nil) (case-fold t))
  (:documentation
   "Match a chunk against a template.
If TEMPLATE is T, match is always successful.
If TEMPLATE is a list, return T if any of the elements of TEMPLATE satisfy chunk-match.
If TEMPLATE and CHUNK are both of type UNKNOWN-ELEMENT, return T if their ELEMENT-NAME slots satisfy string-match,
   otherwise return NIL.
If TEMPLATE is of type symbol, return T if the symbol-name of TEMPLATE and the symbol-name of the type of CHUNK satisfy string-match,
   otherwise return NIL.
If TEMPLATE is of type string and CHUNK is of type content, return T if TEMPLATE and the content-text of CHUNK satisfy string-match,
   otherwise return NIL.
If TEMPLATE is of type content and CHUNK is of type content, return T if if the content-text of TEMPLATE and the content-text of CHUNK satisfy
   string-match, otherwise return NIL; unless content-text of TEMPLATE is unbound in which case always return T.
If TEMPLATE is of type end-tag and CHUNK is of type end-tag, return T if their types are eq.
"))

(defmethod chunk-match ((template list) (chunk t) &key (search nil) (case-fold t))
  (some #'(lambda (temp)
	    (chunk-match temp chunk :search search :case-fold case-fold))
	template))

;  (loop named match-list  ; can't this be replaced with a call to SOME, as above?
;	with match = nil
;	for x in template
;	until match
;	when (chunk-match x chunk :search search)
;	  do (setf match t)
;	finally (return-from match-list match)))

(defmethod chunk-match ((template (eql t)) (chunk t) &key (search nil) (case-fold t))
  (declare (ignore search case-fold))
  t)

(defmethod chunk-match ((template dtd-base::unknown-element) (chunk t) &key (search nil) (case-fold t))
  (declare (ignore search case-fold))
  nil)

(defmethod chunk-match ((template dtd-base::unknown-element) (chunk dtd-base::unknown-element) &key (search nil) (case-fold t))
  (string-match (symbol-name (dtd-base::element-name template)) (symbol-name (dtd-base::element-name chunk))
		:search search :case-fold case-fold))
;; what about the attributes of the unknown element??
;; (chunk-match [nobr foo=5] [nobr foo=6]) => T should be NIL

(defmethod chunk-match ((template symbol) (chunk t) &key (search nil) (case-fold t))
  (string-match (symbol-name template) (symbol-name (type-of chunk))
		:search search :case-fold case-fold))

(defmethod chunk-match ((template string) (chunk dtd-base::content) &key (search nil) (case-fold t))
  (string-match template (content-text chunk)
		:search search :case-fold case-fold))

(defmethod chunk-match ((template dtd-base::content) (chunk dtd-base::content) &key (search nil) (case-fold t))
  (if (slot-boundp template 'dtd-base::text)
      (string-match (content-text template) (content-text chunk)
		    :search search :case-fold case-fold)
      t))

(defmethod chunk-match ((template dtd-base::end-tag) (chunk dtd-base::end-tag) &key (search nil) (case-fold t))
  (declare (ignore search case-fold))
  (and (eq (type-of template) (type-of chunk))))

(defmethod chunk-match ((template t) (chunk t) &key (search nil) (case-fold t))
  (if (eq (type-of template) (type-of chunk))
      (let ((attributes (mop:class-direct-slots (find-class (type-of template))))
	    (template-unknown-attributes (and (slot-boundp template 'dtd-base::.unknown-attributes.)
					      (element-unknown-attributes template)))
	    (chunk-unknown-attributes (and (slot-boundp template 'dtd-base::.unknown-attributes.)
					   (element-unknown-attributes chunk))))
	(and (every #'(lambda (attribute)
			(let* ((slot-name (mop:slot-definition-name attribute))	; need some better iterator that knows about unknown attributes
			       (template-slot-value-boundp (slot-boundp template slot-name))
			       (chunk-slot-value-boundp (slot-boundp chunk slot-name)))
			  (or (not template-slot-value-boundp)
			      (and template-slot-value-boundp
				   chunk-slot-value-boundp
				   (string-match (slot-value template slot-name) (slot-value chunk slot-name)
						 :search search :case-fold case-fold)))))
		    attributes)
	     (if (zerop (length template-unknown-attributes))
		 t
		 (every #'(lambda (attribute)
			    (let* ((slot-name (unknown-attribute-name attribute))
				   (template-slot-value (unknown-attribute-value attribute))
				   (chunk-slot-value-pos (position slot-name chunk-unknown-attributes
								   :test #'string-equal
								   :key #'(lambda (x)
									    (symbol-name (unknown-attribute-name x))))))
			      (and chunk-slot-value-pos
				   (string-match template-slot-value
						 (unknown-attribute-value (aref chunk-unknown-attributes chunk-slot-value-pos))))))
			template-unknown-attributes))))
      nil))

;	    (match nil)) ; old code for above
;	(dolist (s attributes)
;	  (let* ((slot-name (mop:slot-definition-name s)) ; need some better iterator that knows about unknown attributes
;		 (template-slot-value-boundp (slot-boundp template slot-name))
;		 (chunk-slot-value-boundp (slot-boundp chunk slot-name)))
;	    (when (and template-slot-value-boundp
;		       chunk-slot-value-boundp
;		       (string-match (slot-value template slot-name) (slot-value chunk slot-name)
;				     :search search :case-fold case-fold))
;	      (setq match t))))
;	match)

(defun path-match (template path)
  (if (= (length path) (length template))
      (loop named match-path
	    for path-index from 0 below (length path)
	    for template-index from 0 below (length template)
	    do (if (chunk-match (aref template template-index) (aref path path-index))
		   nil
		   (return-from match-path nil))
	    finally (return-from match-path t))
      nil))

(defmethod path-push ((parser html-pull-parser) (chunk t))
  (with-accessors ((path parser-path) (depth parser-depth) (report-stream parser-report-stream)
		   (report-chunk-p parser-report-chunk-p) (report-path-p parser-report-path-p) (indentation-factor parser-indentation-factor))
      parser
    (when report-chunk-p (report-chunk parser chunk))
    (typecase chunk
      (end-tag-forbidden-element nil)
      (empty-element-tag nil)
      (start-tag (vector-push-extend (share-chunk chunk) path)
		 (incf depth)
		 (when report-path-p
		   (report-path parser)))
      (t nil))
    path))

;; SPEC HTML sec. 3.2.1: (SGML sec. 7.5.1) an end tag closes, back to the matching start tag, all unclosed intervening start tags with omitted end tags
;; - if an end tag doesnt have a matching start tag, just ignore it, don't pop

;; BUT.. when we are closing all intervening start tags with omitted
;; end tags, keep a special memory of them so that later if we ever
;; see an end tag for one of them (such as in the case of swapped end
;; tags .. <TR><TD></TR></TD> .. just silently take it off of that
;; list instead of modifying the path further.. unless of course we've
;; seen the corresponding start tag in the meantime, which should
;; automatically take the unclosed end tag off the special memory

(defmethod path-pop-to-match ((parser html-pull-parser) (chunk t))
  (with-accessors ((path parser-path) (depth parser-depth) (report-stream parser-report-stream)
		   (report-chunk-p parser-report-chunk-p) (report-path-p parser-report-path-p) (indentation-factor parser-indentation-factor))
      parser
    (let ((matching-start (if (typep chunk 'dtd-base:unknown-end-tag)
			      (make-instance 'dtd-base:unknown-start-tag
					     :name (start-tag-name (dtd-base:element-name chunk)))
			      (let ((type (type-of chunk)))
				(if (end-tag-name-p type)
				    (start-tag-name type)))))
	  (match-position nil))
      (loop for i from (1- (fill-pointer path)) downto 0
	    until (and (chunk-match matching-start (aref path i))
		       (setf match-position i)))
      (when match-position
	(loop for i from (1- (fill-pointer path)) downto match-position
	      do (decf depth)
	         (release-chunk (aref path i)))
	(setf (fill-pointer path) match-position)))
    (when report-chunk-p (report-chunk parser chunk))
    (when report-path-p (report-path parser))
    path))

(defmacro build-chunk (&key when-done)
  `(loop named build
	 with tokenizer = (parser-tokenizer parser)
	 for status = (handle-token parser (toke:next-token tokenizer))
	 do (incf (parser-token-count parser))
	    (when (eq *debug-parser* :token)
	      (report-parser-status parser *standard-output*))
	    (ecase status
	      (:more nil)
	      (:done (incf (parser-chunk-count parser))
		     (when (eq *debug-parser* :done)
		       (report-parser-status parser *standard-output*))
		     (let ((chunk (parser-chunk parser)))
		       (setf (parser-chunk parser) nil)
;		       (when (parser-previous-chunk parser)
;			 (release-chunk (parser-previous-chunk parser)))
;		       (when (parser-completed-chunk parser)
;			 (setf (parser-previous-chunk parser) (parser-completed-chunk parser)))
;		       (setf (parser-completed-chunk parser) (share-chunk chunk))
		       (typecase chunk
			 (end-tag (path-pop-to-match parser chunk))
			 (t (path-push parser chunk)))
		       (with-accessors ((path parser-path)) parser
			 ,when-done))))))

(defmethod report-totals ((parser html-pull-parser) (stream stream))
  (format stream "~&~D lines containing ~D chunks made from ~D tokens totalling ~D characters"
	  (toke:tokenizer-line-number (parser-tokenizer parser))
	  (parser-chunk-count parser)
	  (parser-token-count parser)
	  (toke:tokenizer-position (parser-tokenizer parser))))

(defmethod report-parser-status ((parser html-pull-parser) (stream stream))
  (format stream "~&!- state=~A, saved=~A, chunk=~A"
	  (parser-state parser) (parser-saved parser) (parser-chunk parser)))

(defmethod initialize-parser ((parser html-pull-parser) (source t))
  (let ((tokenizer (toke:tokenizer-open (make-instance 'toke:html-tokenizer) source)))
    (include-character-entity-set (toke:tokenizer-character-entity-table tokenizer)
				  (dtd-character-entity-table (parser-dtd parser)))
    (initialize-rules parser)
    (setf (parser-tokenizer parser) tokenizer)
    (state parser start))
  t)

(defmethod abandon-parsing ((parser html-pull-parser))
  (setf (parser-previous-chunk parser) nil)
  (path-pop-to-match parser (aref (parser-path parser) 0))
  (release-resourced-object 'object-array (parser-path parser))
  (toke:abandon-tokenizing (parser-tokenizer parser)))

(defmethod copy-chunk ((chunk t))
  (let ((copy (copy chunk)))
    (release-chunk chunk)
    copy))

(defun tag (string)
  (let ((parser (make-instance 'html-pull-parser)))
    (with-input-from-string (s string)
      (initialize-parser parser s)
      (release-chunk (next-chunk parser)) ; START-OF-DOCUMENT
      (prog1 (copy-chunk (next-chunk parser))
	     (abandon-parsing parser)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(html-pull-parser make-html-pull-parser initialize-parser release-chunk-buffers release-chunk next-chunk parser-chunk
	    abandon-parsing report-totals report-parser-status
	    end-of-document-reached chunk-match set-base-path tag copy-chunk)))

; #<HTML-TOKENIZER:ATTRIBUTE-NAME with data ""> should never be returned by the tokenizer! .. or CONTENT or MARKUP-DECLARATION or anything else with empty data
;(untrace)
