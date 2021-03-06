;;;; HTML Tokenizer (K) 2004

;; SPEC SGML (see [ISO8879], section 7.6.1) specifies that a line break immediately following a start tag must be ignored, as must a line break immediately before an end tag. This applies to all HTML elements without exception.

(in-package html-tokenizer)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 0) (safety 3) (compilation-speed 0) (debug 3) (debug-info 3))))

;;; Tokenizer classes

(defclass html-tokenizer () 
  ((context :initarg :context
	    :accessor tokenizer-context
	    :documentation "Current context of tokenizer.")
   (previous-context :initform nil
		     :accessor tokenizer-previous-context
		     :documentation "Previous context of tokenizer.")
   (input :initarg :input
	  :accessor tokenizer-input
	  :documentation "Input source for tokenizer.")
   (rules :initform (make-hash-table)
	  :accessor tokenizer-rules
	  :documentation "Table of syntax rules for this tokenizer, which parts of the syntax it will enforce.")
   (mode :initform nil
	 :accessor tokenizer-mode
	 :documentation "For handling elements with special needs such as PRE, and SCRIPT and STYLE elements according to HTML 4.01 sec. 6.2.
Also for Raw input mode (used by PSPEL's MATCH-AS-CHUNK)")
   (mode-end-regex :initform nil
		   :accessor tokenizer-mode-end-regex
		   :documentation "When in a special mode, the mode ends when this regex is matched.")
   (folds-case-p :initform t
		 :accessor tokenizer-folds-case-p
		 :documentation "Whether the tokenizer folds the case of name/id type tokens.")
   (character-entity-table :initform (make-character-entity-set-table)
			   :accessor tokenizer-character-entity-table
			   :documentation "Table for resolving character entity references.")
   (token :accessor tokenizer-token
	  :initform nil
	  :documentation "Current token being built by the tokenizer.")
   (previous-token :accessor tokenizer-previous-token
		   :documentation "Previous token built by the tokenizer.")
   (start-position :accessor tokenizer-start-position
		   :initform 0
		   :documentation "Character position at which the current token started."))
  (:documentation "An HTML tokenizer object."))

(define-condition tokenizer-condition (condition)
  ((tokenizer :initarg :tokenizer
	      :reader tokenizer-condition-tokenizer)))

(define-condition invalid-context-for-token (tokenizer-condition)
  ()
  (:documentation "A token was encountered in an illegal context."))

(define-condition illegal-syntax (tokenizer-condition)
  ((character :initarg :character
	      :reader illegal-syntax-character)
   (tokenizer :initarg :tokenizer
	      :reader illegal-syntax-tokenizer)
   (problem :initarg :problem
	    :initform "No specific problem specified."
	    :reader illegal-syntax-problem))
  (:report (lambda (condition stream)
	     (let* ((tokenizer (illegal-syntax-tokenizer condition))
		    (input (tokenizer-input tokenizer)))
	       (format stream "Illegal syntax on line ~D near column ~D (~S at position ~D) for context ~A~:[, in ~A mode~;~]:~%"
		       (input-source-line-number input)
		       (compute-column-number input)
		       (illegal-syntax-character condition)
		       (input-source-stream-position input)
		       (symbol-name (type-of (tokenizer-context tokenizer)))
		       (if (tokenizer-mode tokenizer)
			   nil
			   t)
		       (tokenizer-mode tokenizer))
	       (format stream "  Problem: ~A~%" (illegal-syntax-problem condition))
	       (print-position-context-string input stream 46 23)))))

;;; Contexts controlling tokenizer behavior

(defclass-multiton tokenizer-context ()
  ()
  (:documentation "Base class for tokenizer contexts."))

(defclass-multiton attribute-equals-context (tokenizer-context)
  ()
  (:documentation "Context for processing the '=' seperating attribute names and attribute values."))

(defclass-multiton attribute-name-context (tokenizer-context)
  ()
  (:documentation "Context for processing the name of an attribute inside a tag."))

(defclass-multiton attribute-value-context (tokenizer-context)
  ()
  (:documentation "Context for processing the value of an attribute (after a '=' inside a tag)."))  ; canonicalize to a string

(defclass-multiton comment-context (tokenizer-context)
  ()
  (:documentation "Context for processing a tag comment."))

(defclass-multiton content-context (tokenizer-context)
  ()
  (:documentation "Context for processing non-tag document content."))

(defclass-multiton end-of-document-context (tokenizer-context)
  ()
  (:documentation "Final context for tokenizer (EOF)."))

(defclass-multiton markup-declaration-context (tokenizer-context)
  ()
  (:documentation "Context for processing markup declarations."))

(defclass-multiton markup-declaration-marked-section-context (tokenizer-context)
  ()
  (:documentation "Context for processing marked sections."))

(defclass-multiton processing-instruction-context (tokenizer-context)
  ()
  (:documentation "Context for processing processing instructions."))

(defclass-multiton start-of-document-context (tokenizer-context)
  ()
  (:documentation "Initial context for tokenizer."))

(defclass-multiton tag-close-context (tokenizer-context)
  ()
  (:documentation "Context for processing the end of a tag."))

(defclass-multiton tag-name-context (tokenizer-context)
  ()
  (:documentation "Context for processing the name of a tag."))

(defclass-multiton tag-open-context (tokenizer-context)
  ()
  (:documentation "Context for deciding what type of tag is starting."))

;;; Token buffers

(defparameter *token-buffer-length* 1023)

(defun make-token-buffer-array (&rest ignore)
  (declare (ignore ignore))
  (make-array *token-buffer-length* :element-type 'character :adjustable t :fill-pointer 0))

(defresource (:token-buffer)
  :constructor #'make-token-buffer-array
  :deinitializer #'reset-buffer)

(defparameter *token-buffer-resource* nil)

(defun claim-token-buffer (annotation)
  #-resource-annotations (declare (ignore annotation))
  (let ((ob (claim-resourced-object *token-buffer-resource*)))
    #+resource-annotations (push annotation (resourced-object-annotations *token-buffer-resource* ob))
    ob))

(defun share-token-buffer (buf annotation)
  #-resource-annotations (declare (ignore annotation))
  #+resource-annotations (push annotation (resourced-object-annotations *token-buffer-resource* buf))
  (share-resourced-object *token-buffer-resource* buf))

(defun release-token-buffer (buf annotation)
  #-resource-annotations (declare (ignore annotation))
  #+resource-annotations (push annotation (resourced-object-annotations *token-buffer-resource* buf))
  (release-resourced-object *token-buffer-resource* buf))

;;; HTML token classes

(defclass-multiton html-token ()
  ()
  (:documentation "Base class for tokens."))

(defclass-multiton html-syntax-token (html-token)
  ()
  (:documentation "Base class for tokens representing HTML syntax."))

(defmethod print-object ((object html-syntax-token) (stream stream))
  (print-unreadable-object (object stream :type t :identity nil)))

(defclass-multiton non-html-syntax-token (html-token)
  ((data :accessor token-data
	 :type string
	 :initform "String data for the token read.")
   (extra-data :accessor token-extra-data
	       :type t
	       :initform nil
	       :documentation "Any extra data that may be interesting to send along, in any format."))
  (:documentation "Base class for tokens representing non-syntax HTML."))

(defmethod print-object ((object non-html-syntax-token) (stream stream))
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "with data ~S" 
	    (token-data object))))

(defmethod initialize-multiton ((multiton non-html-syntax-token) annotation)
  (with-accessors ((data token-data)) multiton
    (setf data (claim-token-buffer annotation)))
  multiton)

(defclass-multiton attribute-equals (html-syntax-token)
  ()
  (:documentation "Token representing '=', only valid in attribute-equals-context.
'=' is treated as ordinary text in content-context."))

(defclass-multiton attribute-name (non-html-syntax-token)
  ()
  (:documentation "A token representing the name of an attribute within a tag."))

(defclass-multiton attribute-value (non-html-syntax-token)
  ()
  (:documentation "A token representing the value of an attribute within a tag."))

(defclass-multiton comment (non-html-syntax-token)
  ()
  (:documentation "A token representing a comment within a tag."))

;(defclass-multiton comment-marker (html-syntax-token)
;  ()
;  (:documentation "Token representing '--', only valid in attribute-name-context or comment-context.
;There is always one '--' at the start of a comment and one '--' at the end of a comment.
;'--' is treated as ordinary text in content-context."))

(defclass-multiton content (non-html-syntax-token)
  ((complete-p :accessor token-complete-p
	       :initform nil)
   (mode :accessor token-mode
	 :initform nil))
  (:documentation "A token representing some non-tag data within the document content."))

(defmethod print-object ((object content) (stream stream))
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "with data ~S~:[~;, in ~A mode~]" 
	    (token-data object) (token-mode object) (token-mode object))))

(defclass-multiton end-of-document (html-syntax-token)
  ()
  (:documentation "Token representing document EOF, only valid in content-context.
It is an error to call NEXT-TOKEN after this token has been returned."))

(defclass-multiton end-tag-open (html-syntax-token)
  ()
  (:documentation "Token representing '</', only valid in tag-open-context."))

(defclass-multiton markup-declaration (non-html-syntax-token)
  ()
  (:documentation "A token representing the contents of a markup declaration."))

(defclass-multiton markup-declaration-open (html-syntax-token)
  ()
  (:documentation "Token representing '<!', only valid in tag-open-context."))

(defclass-multiton processing-instruction (non-html-syntax-token)
  ()
  (:documentation "A token representing the contents of a processing instruction."))

(defclass-multiton processing-instruction-open (html-syntax-token) ; processing instruction
  ()
  (:documentation "Token representing '<?', only valid in tag-open-context."))

(defclass-multiton empty-element-tag-close (html-syntax-token)
  ()
  (:documentation "Token representing '/>', only valid in attribute-name-context."))

(defclass-multiton start-of-document (html-syntax-token)
  ()
  (:documentation "Token representing document beginning, only valid in start-of-document-context."))

(defclass-multiton start-tag-open (html-syntax-token)
  ()
  (:documentation "Token representing '<', only valid in tag-open-context."))

(defclass-multiton tag-close (html-syntax-token)
  ()
  (:documentation "Token representing '>', only valid in attribute-name-context."))

(defclass-multiton tag-name (non-html-syntax-token)
  ()
  (:documentation "A token representing the name of a tag."))

;;; Syntax Rules

(defparameter *rules* nil)

(defmethod initialize-rules ((tokenizer html-tokenizer))
  (let ((*package* (find-package 'toke)))
    (initialize-rule-table (tokenizer-rules tokenizer)))
  t)

(define-rule document-starts-with-tag :relaxed)
(define-rule raw-end-tag :relaxed)

(define-rule cdata-unknown-character-reference :relaxed)
(define-rule cdata-character-reference-termination :relaxed)
(define-rule content-unknown-character-reference :relaxed)
(define-rule content-character-reference-termination :relaxed)
(define-rule character-reference-misplaced-sharpsign :relaxed)
(define-rule character-reference-terminated-by-end-of-cdata :relaxed) ; "foo&blah", "foo&"
(define-rule zero-length-character-reference :relaxed) ; &;
(define-rule nonnumeric-numeric-character-reference :relaxed)

(define-rule equals-must-seperate-attribute-name-and-value :relaxed) ; foo"23"
(define-rule whitespace-between-attribute-name-and-equals :relaxed) ; foo =bar
(define-rule whitespace-between-equals-and-attribute-value :relaxed) ; foo= bar
(define-rule \,-in-name-type :relaxed) ; <foo bar="baz" , "quux">
(define-rule %-in-name-type :relaxed) ; foo%bar=baz%ugh   </%doc>
(define-rule less-than-must-open-tags :relaxed) ; <&nbsp;foo
(define-rule tag-close-needed-to-end-tag :relaxed) ; <foo<bar>

(define-rule comma-in-unquoted-attribute-value :relaxed) ; foo=bar,baz
(define-rule sharpsign-in-unquoted-attribute-value :relaxed) ; foo=#bar
(define-rule slash-in-unquoted-attribute-value :relaxed) ; <a href=/foo/bar>
(define-rule question-mark-in-unquoted-attribute-value :relaxed) ; <a href=/foo/bar?>
(define-rule ampersand-in-unquoted-attribute-value :relaxed) ; <a href=/foo/bar=foo>
(define-rule equals-in-unquoted-attribute-value :relaxed) ; <a href=/foo/foo=blah>
(define-rule star-in-unquoted-attribute-value :relaxed) ; <a href=/foo=*>



(defparameter *unquoted-attribute-value-rules-table* 
  (let ((table (make-hash-table :test 'eq)))
    (dolist (r '((#\, . comma-in-unquoted-attribute-value)
		 (#\# . sharpsign-in-unquoted-attribute-value)
		 (#\/ . slash-in-unquoted-attribute-value)
		 (#\? . question-mark-in-unquoted-attribute-value)
		 (#\& . ampersand-in-unquoted-attribute-value)
		 (#\= . equals-in-unquoted-attribute-value)
		 (#\* . star-in-unquoted-attribute-value)))
      (setf (gethash (car r) table) (cdr r)))
    table))


;;; Tokenizer code

(defmethod initialize-tokenizer ((tokenizer html-tokenizer))
  (setf *token-buffer-resource* (find-resource :token-buffer))
  (initialize-rules tokenizer)
  (setf (tokenizer-character-entity-table tokenizer) (make-character-entity-set-table))
  (include-character-entity-set (tokenizer-character-entity-table tokenizer) :predefined)
  t)

(defmethod tokenizer-open ((tokenizer html-tokenizer) (source t))
  (initialize-tokenizer tokenizer)
  (setf (tokenizer-input tokenizer) (input-source-open source))
  (setf (tokenizer-context tokenizer) start-of-document-context)
  tokenizer)

(defmethod tokenizer-position ((tokenizer html-tokenizer))
  (input-source-stream-position (tokenizer-input tokenizer)))

(defmethod tokenizer-line-number ((tokenizer html-tokenizer))
  (input-source-line-number (tokenizer-input tokenizer)))

(defmethod abandon-tokenizing ((tokenizer html-tokenizer))
  (input-source-close (tokenizer-input tokenizer)))

(defmethod rule-compliance ((tokenizer html-tokenizer) (rule symbol))
  (with-accessors ((rules tokenizer-rules)) tokenizer
    (rule-state rules rule)))

(defmethod (setf rule-compliance) ((new-value symbol) (tokenizer html-tokenizer) (rule symbol))
  (with-accessors ((rules tokenizer-rules)) tokenizer
    (setf (rule-state rules rule) new-value)))

(defmethod unknown-character-entity-handler ((tokenizer html-tokenizer) (entity string))
  ;; default to returning unknown entity strings as-is
  entity)

(defmethod process-html-character-reference ((tokenizer html-tokenizer))
  (with-accessors ((input tokenizer-input) (context tokenizer-context)) tokenizer
    (let ((temp-buffer (claim-token-buffer '(claimed-by process-html-character-reference)))
	  (numericp nil)
	  (hexp nil)
	  (new-context context)
	  (reference-terminating-character))
      (macrolet ((store (char)
		   `(vector-push-extend ,char temp-buffer))
		 (return-context (context)
		   `(return-from toke-entity (setf new-context ,context)))
		 (syntax-error (reason)
		   `(error 'illegal-syntax :character char :tokenizer tokenizer :problem ,reason)))
	(loop named toke-entity
	      for char = (read-next-character input)
	      as position from 1
	      do (selector char char=
		   (#\# (if (not (= 1 position))
			    (case (rule-compliance tokenizer 'character-reference-misplaced-sharpsign)
			      (:relaxed (store char) (return-from toke-entity nil))
			      (:enforced (syntax-error "Sharpsign is not allowed in character references except as the first character.")))
			    (let ((nextchar (read-next-character input)))
			      (setf numericp t)
			      (if (char-equal nextchar #\X)
				  (setf hexp t)
				  (rewind-input input 1)))))
		   (#\; (progn (setf reference-terminating-character #\;)
			       (return-from toke-entity nil)))
		   ;; SPEC Character entities dont have to be terminated in some cases according to HTML 4.01 sec. 5.3
		   (#\< (if (eq context content-context)
			    (progn (setf reference-terminating-character #\<)
				   (return-context tag-open-context))
			    (syntax-error "< is not allowed in attribute value character references.")))
		   (#\Newline (progn (setf reference-terminating-character #\Newline)
				     (return-from toke-entity nil)))
		   (t (if (alphanumericp char)
			  (store char)
			  (selector char char=
			    ((#\" #\')
			     (case (rule-compliance tokenizer 'character-reference-terminated-by-end-of-cdata)
			       (:relaxed (rewind-input input 1)
					 (return-from toke-entity nil))
			       (:enforced (syntax-error "Character references must be properly terminated before end of CDATA."))))
			    (t
			     (if (eq context attribute-value-context)
				 (case (rule-compliance tokenizer 'cdata-character-reference-termination)
				   (:relaxed (store char) (return-from toke-entity nil))
				   (:enforced (syntax-error "Character references in attribute values must be terminated by ; or Newline."))) ;
				 (case (rule-compliance tokenizer 'content-character-reference-termination)
				   (:relaxed (store char) (return-from toke-entity nil))
				   (:enforced (syntax-error "Character references in content must be terminated by ; < or Newline."))))))))))
	(if (zerop (length temp-buffer))
	    (case (rule-compliance tokenizer 'zero-length-character-reference)
	      (:relaxed (translate-html-character-reference tokenizer temp-buffer context new-context numericp hexp reference-terminating-character))
	      (:enforced (let ((char #\Null))
			   (syntax-error "Zero-length character references are not allowed."))))
	    (translate-html-character-reference tokenizer temp-buffer context new-context numericp hexp reference-terminating-character))))))

(defun translate-html-character-reference (tokenizer temp-buffer context new-context numericp hexp reference-terminating-character)
  (macrolet ((syntax-error (reason)
	       `(error 'illegal-syntax :character nil :tokenizer tokenizer :problem ,reason)))
    (let ((save-temp-buffer nil)
	  (translation nil)
	  (code (if numericp
		    (if hexp
			(ignore-errors (parse-integer temp-buffer :radix 16))
			(ignore-errors (parse-integer temp-buffer)))
		    (character-entity-code (tokenizer-character-entity-table tokenizer) temp-buffer))))
      (flet ((handle-unknown-character-reference (rule)
	       (case (rule-compliance tokenizer rule)
		 (:relaxed (when hexp
			     (insert-character temp-buffer #\x))
			   (when numericp
			     (insert-character temp-buffer #\#))
			   (insert-character temp-buffer #\&)
			   (when reference-terminating-character
			     (vector-push-extend reference-terminating-character temp-buffer))
			   (setf save-temp-buffer t)
			   (unknown-character-entity-handler tokenizer temp-buffer))
		 (:enforced (error 'unknown-character-entity temp-buffer)))))
	(let ((which-rule (if (eq context attribute-value-context)
			      'cdata-unknown-character-reference
			      'content-unknown-character-reference)))
	  (cond ((eq code nil)
		 (case (rule-compliance tokenizer 'nonnumeric-numeric-character-reference)
		   (:relaxed (setf translation (handle-unknown-character-reference which-rule)))
		   (:enforced
		    (syntax-error "Numeric character references must contain only digits in the correct base."))))
		((eq code :no-such-entity)
		 (setf translation (handle-unknown-character-reference which-rule)))
		((integerp code)
		 (setf translation (code-char code))))))
      (unless save-temp-buffer
	(release-token-buffer temp-buffer '(released-by translate-html-character-reference)))
      (values translation new-context))))

(defmethod process-name ((tokenizer html-tokenizer) &key
			 (allow-nonalphanumeric-start nil)
			 (allow-whitespace-start nil)
			 (for-attribute-name nil)
			 (for-attribute-value nil)
			 (for-tag-name nil))
  (with-accessors ((token tokenizer-token) (input tokenizer-input)) tokenizer
    (let ((new-context attribute-name-context)
	  (data (token-data token)))
      (macrolet ((store (char)
			`(setf seen-non-whitespace (vector-push-extend (if (tokenizer-folds-case-p tokenizer) (char-upcase ,char) ,char) data)))
		 (return-context (context) `(return-from tag-toke (setf new-context ,context)))
		 (syntax-error (reason) `(error 'illegal-syntax :character char :tokenizer tokenizer :problem ,reason)))
	(loop named tag-toke
	      for char = (read-next-character input)
	      as index from 0
	      with seen-non-whitespace = nil
	      do (cond ((alphanumericp char)
			(store char))
		       ((whitespacep char)
			(cond ((and (not seen-non-whitespace) allow-whitespace-start)
			       nil)
			      ((and seen-non-whitespace (or for-tag-name for-attribute-value))
			       (return-context attribute-name-context))
			      ((and seen-non-whitespace for-attribute-name)
			       (case (rule-compliance tokenizer 'whitespace-between-attribute-name-and-equals)
				 (:relaxed (loop for char = (read-next-character input)
						 until (not (whitespacep char))
						 finally (progn (rewind-input input 1)
								(if (member char '(#\= #\" #\') :test #'char=)
								    (return-context attribute-equals-context)
								    (return-context attribute-name-context)))))
				 (:enforced (return-context attribute-name-context))))))
		       ((char= char #\<)
			(case (rule-compliance tokenizer 'tag-close-needed-to-end-tag)
			  (:relaxed (rewind-input input 1)
				    (insert-next-character input #\>)
				    (return-context tag-close-context))
			  (:enforced (syntax-error "Tag delimiters must be balanced.  Cannot open another tag here."))))
		       ((char= char #\>)
			(rewind-input input 1)
			(return-context tag-close-context))
		       ((and (char= char #\/) (not for-attribute-value))
			(let ((nextchar (read-next-character input)))
			  (if (char= nextchar #\>)
			      (progn (rewind-input input 2)
				     (return-context tag-close-context))
			      (syntax-error "/ is not allowed to occur in the body of a tag."))))
		       ((and (zerop index) (not allow-nonalphanumeric-start))
			(if (char= char #\%)
			    (case (rule-compliance tokenizer '%-in-name-type)
			      (:relaxed (store char))
			      (:enforced (syntax-error "% cannot start tokens of type NAME.")))
			    (syntax-error "Names must start with an alphanumeric character.")))
		       ((member char +legal-nonalphanumerics-for-id-and-name-tokens+ :test #'char=)
			(store char))
		       (for-attribute-value
			(let ((rule (gethash char *unquoted-attribute-value-rules-table* :char-not-handled)))
			  (unless :char-not-handled
			    (case (rule-compliance tokenizer rule)
			      (:relaxed (store char))
			      (:enforced (syntax-error (format nil "~A is not allowed in unquoted attribute values." char)))))))
		       ((and for-attribute-name 
			     (member char '(#\= #\" #\') :test #'char=))
			(rewind-input input 1)
			(return-context attribute-equals-context))
		       ((char= char #\%)
			(case (rule-compliance tokenizer '%-in-name-type)
			  (:relaxed (store char))
			  (:enforced (syntax-error "% is not allowed in tokens of type NAME."))))
		       (t (syntax-error (format nil "~A is not allowed to occur in the body of a tag." char))))))
      new-context)))

;;need to add a SEARCH option or some other fcn to set up a fencepost past which searching will not proceed...

;;searching for /SCRIPT breaks up content 
(defmethod process-raw-content ((tokenizer html-tokenizer))
  (with-accessors ((token tokenizer-token)
		   (input tokenizer-input)
		   (mode tokenizer-mode)
		   (mode-end-regex tokenizer-mode-end-regex))
      tokenizer
;    (when mode-end-regex
;      (etrace :regex (mode-end-regex)))
    (with-accessors ((data token-data))
	token
      (let ((new-context attribute-name-context)
;	    (current-position (input-source-stream-position input))
	    (end-match (when mode-end-regex
			 (multiple-value-list (input-source-match-re input mode-end-regex)))))
	(if (and end-match
		 (eq (first end-match) t))
	    (destructuring-bind (success whole &rest submatches)
		end-match
	      (when success
					;	      (etrace :foo (submatches))
		(destructuring-bind (whole-start . whole-end)
		    whole
;		  (etrace :whole (whole-start whole-end))
		  (awhen (or (and submatches
				  (mapcar (lambda (sub)
					    (destructuring-bind (start . end)
						sub
;					      (etrace :sub (start end))
					      (input-source-substring input start end)))
					  submatches))
			     (and (not submatches)
				  (input-source-substring input whole-start whole-end)))
;		    (etrace :foo1 (it))
		    (loop for char across (input-source-substring input whole-start whole-end)
			  do (vector-push-extend char data))
		    (setf new-context content-context)
		    (forward-to-position input whole-end) ; should i be using read-next-character? M-. this stuff
;		    (setf current-position (input-source-stream-position input))
;		    (etrace :fnord (current-position new-context data))
		    ))))
	    ;; dike this entire section out, set mode-end-regex to </script> or </style> appropriately and use that code.. it will work much better!
	    ;; just be sure to respect the 'raw-end-tag rule (use </ as a match if its enforced)
	    ;; will also then be able to dike out the function process-raw-content-end-tag!
	    (when (or (eq mode :script)
		      (eq mode :style))
	      (loop named toke-data
		    for char = (read-next-character input)
		    do (selector char char=
			 ;; SPEC HTML 4.01 sec. 6.14: script data that is element content may not contain character references.
			 (#\< (let ((char1 (read-next-character input)))
				(selector char1 char=
				  ;; handle <\/ properly, pass through </ to data
				  (#\\ (vector-push-extend char data)) 
				  ;; SPEC HTML 4.01 sec. B.3.2: the data begins immediately after the element start tag and ends
				  ;; at the first ETAGO ("</") delimiter followed by a name start character ([a-zA-Z]).
				  ;; - but ebay screws this up, so we have to check for </SCRIPT> or </STYLE>
				  (#\/ (case (rule-compliance tokenizer 'raw-end-tag)
					 (:relaxed (if (process-raw-content-end-tag tokenizer char char1)
						       ;; found matching script/style end tag, end
						       (progn
							 (setf new-context content-context)
							 (return-from toke-data t))
						       ;; wasn't matching end tag, push #\/ and continue
						       (vector-push-extend char data)))
					 (:enforced (let ((char2 (read-next-character input)))
						      (if (alpha-char-p char2)
							  (progn (rewind-input input 3)
								 (setf new-context content-context)
								 (return-from toke-data t))
							  (error 'illegal-syntax :character char2 :tokenizer tokenizer))))))
				  ;; pass through < followed by anything else unharmed
				  (t (vector-push-extend char data)
				     (vector-push-extend char1 data)))))
			 (t (vector-push-extend char data))))))
	(setf mode nil
	      mode-end-regex nil)
	new-context))))

(defmethod process-raw-content-end-tag ((tokenizer html-tokenizer) (char0 character) (char1 character))
  (with-accessors ((mode tokenizer-mode)) tokenizer
    (let* ((test-buffer (claim-token-buffer '(claimed-by process-raw-content-end-tag)))
	   (mode-string (case mode
			  (:script "SCRIPT")
			  (:style "STYLE")))
	   (length (length mode-string)))
      (loop repeat length
	    do (vector-push-extend (read-next-character (tokenizer-input tokenizer)) test-buffer))
      (if (string-equal test-buffer mode-string)
	  (progn (rewind-input (tokenizer-input tokenizer) length)
		 (rewind-input (tokenizer-input tokenizer) 2)
		 (setf mode nil)
		 (release-token-buffer test-buffer '(released-by process-raw-content-end-tag))
		 t)
	  (progn (release-token-buffer test-buffer '(released-by process-raw-content-end-tag))
		 nil)))))

(defmethod process-pcdata ((tokenizer html-tokenizer))
  (let ((new-context nil)
	(data (token-data (tokenizer-token tokenizer))))
    (loop named toke-data
	  for char = (read-next-character (tokenizer-input tokenizer))
	  with seen-non-whitespace-p = nil
	  do (if (and (not seen-non-whitespace-p)
		      (not (eq (tokenizer-mode tokenizer) :pre))
		      (whitespacep char))
		 ;; ignore leading whitespace
		 nil
		 (progn (setf seen-non-whitespace-p t)
			(selector char char=
			  (#\< (let ((nextchar (read-next-character (tokenizer-input tokenizer))))
				 (if (or (alpha-char-p nextchar)
					 (member nextchar '(#\/ #\! #\?) :test #'char=))
				     (progn (rewind-input (tokenizer-input tokenizer) 1)
					    (setf new-context tag-open-context)
					    (return-from toke-data t))
				     (case (rule-compliance tokenizer 'less-than-must-open-tags)
				       (:relaxed (rewind-input (tokenizer-input tokenizer) 1)
						 (vector-push-extend char data))
				       (:enforced (error 'illegal-syntax :character nextchar :tokenizer tokenizer
							 :problem (format nil "'~A' is not allowed to start a tag name." nextchar)))))))
			  (#\& (multiple-value-bind (ref new-context-1)
				   (process-html-character-reference tokenizer)
				 (etypecase ref
				   (string (loop for i from 0 to (1- (length ref))
						 for r = (aref ref i)
						 do (vector-push-extend r data))
					   (release-token-buffer ref '(released-by process-pcdata)))
				   (character (vector-push-extend ref data)))
				 (setf new-context new-context-1)
				 (unless (eq new-context content-context)
				   (return-from toke-data t))))
			  (t (vector-push-extend char data))))))
    ;; ignore trailing whitespace
    (loop named ignore-trailing-whitespace
	  for i from (1- (length data)) downto 0
	  as char = (aref data i)
	  with seen-non-whitespace-p = nil
	  do (if (and (not seen-non-whitespace-p)
		      (not (eq (tokenizer-mode tokenizer) :pre))
		      (whitespacep char))
		 nil
		 (progn (setf seen-non-whitespace-p t)
			(setf (fill-pointer data) (1+ i))
			(return-from ignore-trailing-whitespace t)))
	  finally (when (and (zerop i)
			     (not seen-non-whitespace-p))
		    (setf (fill-pointer data) 0)))
    new-context))

(defmethod process-cdata ((tokenizer html-tokenizer) (delimiter character))
  ;; SPEC HTML 4.01 sec. 3.2.2: Single quote marks can be included within the attribute value
  ;; SPEC when the value is delimited by double quote marks, and vice versa.
  ;; SPEC HTML 4.01 sec. 6.2: Replace character entities with characters, Ignore line feeds, Replace each carriage return or tab with a single space.
  ;; SPEC User agents may ignore leading and trailing white space in CDATA attribute values.
  (with-accessors ((token tokenizer-token)) tokenizer
    (let ((data (token-data token)))
      (loop named value-toke
	    for char = (read-next-character (tokenizer-input tokenizer))
	    do (selector char char=
		 (delimiter (return-from value-toke t))
		 (#\& (let ((ref (process-html-character-reference tokenizer)))
			(etypecase ref
			  (string (loop for i from 0 to (1- (length ref))
					for r = (aref ref i)
					do (vector-push-extend r data))
				  (release-token-buffer ref '(released-by process-cdata)))
			  (character (vector-push-extend ref data)))))
		 (#\^J)
		 (#\^M (vector-push-extend #\Space data))
		 (#\Tab (vector-push-extend #\Space data))
		 (t (vector-push-extend char data)))))))

(defmethod next-token-in-context ((tokenizer html-tokenizer) (context attribute-equals-context))
  (with-accessors ((token tokenizer-token)) tokenizer  
    (setf token attribute-equals)
    (let ((char (read-next-character (tokenizer-input tokenizer))))
      (cond ((char= char #\=)
	     (case (rule-compliance tokenizer 'whitespace-between-equals-and-attribute-value)
	       (:relaxed (loop for char = (read-next-character (tokenizer-input tokenizer))
			       until (not (whitespacep char))
			       finally (rewind-input (tokenizer-input tokenizer) 1)))
	       (:enforced (error 'illegal-syntax :character char :tokenizer tokenizer
				 :problem "No whitespace is allowed between = and the attribute value.")))
	     t)
	    ((or (char= char #\") (char= char #\'))
	     (case (rule-compliance tokenizer 'equals-must-seperate-attribute-name-and-value)
	       (:relaxed (rewind-input (tokenizer-input tokenizer) 1))
	       (:enforced (error 'illegal-syntax :character char :tokenizer tokenizer
				 :problem "Equals must seperate attribute name and value.")))
	     t)
	    (t (error 'illegal-syntax :character char :tokenizer tokenizer
		      :problem "Equals must seperate attribute name and value.")))))
  attribute-value-context)

(defmethod next-token-in-context ((tokenizer html-tokenizer) (context attribute-name-context))
  (with-accessors ((token tokenizer-token)) tokenizer  
    (let ((firstchar (read-next-character (tokenizer-input tokenizer))))
      (when (char= firstchar #\<)
	(case (rule-compliance tokenizer 'tag-close-needed-to-end-tag)
	  (:relaxed (rewind-input (tokenizer-input tokenizer) 1)
		    (insert-next-character (tokenizer-input tokenizer) #\>)
		    (setf firstchar (read-next-character (tokenizer-input tokenizer))))
	  (:enforced (error 'illegal-syntax :character firstchar :tokenizer tokenizer
			    :problem "Tag delimiters must be balanced.  Cannot open another tag here."))))
      (if (member firstchar '(#\> #\/) :test #'char=)
	  ;; eliminate null attribute-name
	  (progn (rewind-input (tokenizer-input tokenizer) 1)
		 (next-token-in-context tokenizer tag-close-context))
	  (progn (setf token (initialize-multiton attribute-name '(claimed-by next-token-in-context attribute-name-context)))
		 (rewind-input (tokenizer-input tokenizer) 1)
		 (let ((new-context (process-name tokenizer :for-attribute-name t :allow-whitespace-start t)))
		   (if (zerop (fill-pointer (token-data token)))
		       (progn (release-token-buffer (token-data token) '(released-by next-token-in-context attribute-name-context))
			      (next-token-in-context tokenizer tag-close-context))
		       new-context)))))))

(defmethod next-token-in-context ((tokenizer html-tokenizer) (context attribute-value-context))
  (with-accessors ((token tokenizer-token)) tokenizer
    (setf token (initialize-multiton attribute-value '(claimed-by next-token-in-context attribute-value-context)))
;									      ,(input-source-line-number (tokenizer-input tokenizer))
;									      ,(tokenizer-previous-context tokenizer)
;									      ,(tokenizer-previous-token tokenizer))))
    (let ((new-context attribute-name-context)
	  (char (read-next-character (tokenizer-input tokenizer))))
      (selector char char= 
	(#\" (process-cdata tokenizer #\")
	     (setf new-context attribute-name-context))
	(#\' (process-cdata tokenizer #\')
	     (setf new-context attribute-name-context))
	;; SPEC HTML 4.01 sec. 3.2.2: In certain cases, authors may specify the value of an attribute without any quotation
	;; SPEC marks. The attribute value may only contain letters, digits, hyphens, periods, underscores, and colons.
	(t (rewind-input (tokenizer-input tokenizer) 1)
	   (setf new-context (process-name tokenizer :for-attribute-value t :allow-nonalphanumeric-start t))))
      new-context)))

(defmethod next-token-in-context ((tokenizer html-tokenizer) (context comment-context))
  (with-accessors ((token tokenizer-token)) tokenizer
    (with-accessors ((data token-data))
	(setf token (initialize-multiton comment '(claimed-by next-token-in-context comment-context)))
      (let ((new-context comment-context))
	(loop named toke-data
	      for char = (read-next-character (tokenizer-input tokenizer))
	      do (selector char char=
		   (#\- (let ((nextchar (read-next-character (tokenizer-input tokenizer))))
			  (selector nextchar char= 
			    (#\- ;; treat the preceeding space as part of the comment delimiter
			     (string-right-trim '(#\space) data) 
			     (setf new-context markup-declaration-context)
			     (return-from toke-data t))
			    (t (vector-push-extend char data)
			       (vector-push-extend nextchar data)))))
		   (t (vector-push-extend char data))))
	new-context))))

(defmethod next-token-in-context ((tokenizer html-tokenizer) (context content-context))
  (with-accessors ((token tokenizer-token)
		   (mode tokenizer-mode)) tokenizer
    (setf token (initialize-multiton content '(claimed-by next-token-in-context content-context)))
    (setf (token-mode token) mode)
    (if (or (eq mode :script) 
	    (eq mode :style)
	    (eq mode :raw))
	(process-raw-content tokenizer)
;	(let ((firstchar (read-next-character tokenizer)))
;	  (if (char= firstchar #\<)
;	      ;; eliminate null content
;	      (progn ;(unread-previous-character tokenizer firstchar)
;		     (next-token-in-context tokenizer tag-open-context))
;	(progn (unread-previous-character tokenizer firstchar)
;              (process-pcdata tokenizer))))
	(let ((new-context (process-pcdata tokenizer)))
	  (if (string= (token-data token) "")
	      ;; eliminate null content
	      (progn (release-token-buffer (token-data token) '(released-by next-token-in-context content-context))
		     (next-token-in-context tokenizer new-context))
	      new-context)))))

(defmethod next-token-in-context ((tokenizer html-tokenizer) (context end-of-document-context))
  (setf (tokenizer-token tokenizer) end-of-document)
  end-of-document-context)

(defmethod next-token-in-context ((tokenizer html-tokenizer) (context markup-declaration-context))
  (with-accessors ((token tokenizer-token)) tokenizer  
    (setf token (initialize-multiton markup-declaration '(claimed-by next-token-in-context markup-declaration-context)))
    (let ((new-context content-context)
	  (data (token-data token)))
      (loop named toke-data
	    for char = (read-next-character (tokenizer-input tokenizer))
	    do (selector char char=
		 (#\- (let ((nextchar (read-next-character (tokenizer-input tokenizer))))
			(if (char= nextchar #\-)
			    (progn (setf new-context comment-context)
				   (release-token-buffer data '(released-by next-token-in-context markup-declaration-context))
				   (return-from toke-data t))
			    (progn (vector-push-extend char data)
				   (vector-push-extend nextchar data)))))
		 (#\> (setf new-context tag-close-context)
		      (rewind-input (tokenizer-input tokenizer) 1)
		      (return-from toke-data t))
		 (#\[ (setf new-context markup-declaration-marked-section-context)
		      (return-from toke-data t))
		 (t (vector-push-extend char data))))
      new-context)))

(defmethod next-token-in-context ((tokenizer html-tokenizer) (context markup-declaration-marked-section-context))
  ;; just ignore marked sections
  (loop named toke-data
	for char = (read-next-character (tokenizer-input tokenizer))
	with depth = 1
	do (selector char char=
	     (#\[ (incf depth))
	     (#\] (decf depth)
	          (when (zerop depth)
		    (return-from toke-data t)))))
  tag-close-context)

(defmethod next-token-in-context ((tokenizer html-tokenizer) (context processing-instruction-context))
  (with-accessors ((token tokenizer-token)) tokenizer  
    (setf token (initialize-multiton processing-instruction '(claimed-by next-token-in-context processing-instruction-context)))
    (let ((new-context content-context)
	  (data (token-data token)))
      (loop named toke-data
	    for char = (read-next-character (tokenizer-input tokenizer))
	    do (selector char char=
		 (#\> (return-from toke-data t))
		 (t (vector-push-extend char data))))
      new-context)))

(defmethod next-token-in-context ((tokenizer html-tokenizer) (context start-of-document-context))
  (with-accessors ((token tokenizer-token)) tokenizer    
    (let ((new-context nil)
	  (char (read-next-character (tokenizer-input tokenizer))))
      (selector char char=
	(#\< (setf new-context tag-open-context)
	     (setf token start-of-document))
	(t (if (whitespacep char)
	       (progn (loop for char = (read-next-character (tokenizer-input tokenizer))
			    until (not (whitespacep char))
			    finally (rewind-input (tokenizer-input tokenizer) 1))
		      (setf new-context (next-token-in-context tokenizer start-of-document-context)))
	       (error 'illegal-syntax :character char :tokenizer tokenizer))))
      new-context)))

(defmethod next-token-in-context ((tokenizer html-tokenizer) (context tag-close-context))
  (with-accessors ((token tokenizer-token)) tokenizer
    (let ((new-context attribute-name-context)
	  (char (read-next-character (tokenizer-input tokenizer))))
      (selector char char=
	(#\> (setf token tag-close)
	     (setf new-context content-context))
	(#\/ ;; read next char and make sure its a >
	     (let ((nextchar (read-next-character (tokenizer-input tokenizer))))
	       (if (char= nextchar #\>)
		   (progn (setf token empty-element-tag-close)
			  (setf new-context content-context))
		   (error 'illegal-syntax :character char :tokenizer tokenizer))))
	(t (error 'illegal-syntax :character char :tokenizer tokenizer)))
      new-context)))

(defmethod next-token-in-context ((tokenizer html-tokenizer) (context tag-name-context))
  (with-accessors ((token tokenizer-token)
		   (previous-token tokenizer-previous-token)
		   (mode tokenizer-mode)) tokenizer
    (setf token (initialize-multiton tag-name '(claimed-by next-token-in-context tag-name-context)))
    (let ((new-context attribute-name-context)
	  (data (token-data token)))
      (setf new-context (process-name tokenizer :for-attribute-name t))
      ;; SPEC SCRIPT and STYLE elements require special tokenizing according to HTML 4.01 sec. 6.2
      (cond ((string= data "SCRIPT")
	     (unless (eq previous-token end-tag-open)
	       (setf mode :script)))
	    ((string= data "STYLE")
	     (unless (eq previous-token end-tag-open)
	       (setf mode :style))))
      new-context)))

(defmethod next-token-in-context ((tokenizer html-tokenizer) (context tag-open-context))
  (with-accessors ((token tokenizer-token)) tokenizer
    (let ((char (read-next-character (tokenizer-input tokenizer)))
	  (new-context tag-name-context))
      (selector char char= 
	(#\/ (setf token end-tag-open))
	(#\! (setf token markup-declaration-open
		   new-context markup-declaration-context))
	(#\? (setf token processing-instruction-open
		   new-context processing-instruction-context))
	(t (setf token start-tag-open)
	   (rewind-input (tokenizer-input tokenizer) 1)))
      new-context)))

(defmethod next-token ((tokenizer html-tokenizer))
  (with-accessors ((context tokenizer-context)
		   (previous-context tokenizer-previous-context)
		   (token tokenizer-token)
		   (input tokenizer-input)
		   (previous-token tokenizer-previous-token)) tokenizer
    (handler-case (setf previous-context context
			previous-token token
			(tokenizer-start-position tokenizer) (input-source-stream-position input)
			context (next-token-in-context tokenizer context))
      (end-of-file ()
	(setf context end-of-document-context)))
    token))

(defmethod set-raw-mode ((tokenizer html-tokenizer) (end-regex t)) ; dont constrain this too tightly cause it might be a compiled regex object
  (with-slots (mode mode-end-regex)
      tokenizer
    (setf mode :raw
	  mode-end-regex end-regex)))

;(toke-test)
;(print tag-name)
;(gc t)
;(gc)
;(room t)
;(trace (next-token-in-context))
;(trace (process-html-character-reference))
;(untrace)
;(print *token-buffer-resource*)

(defun dummy (string)
  (let ((toke (make-instance 'html-tokenizer)))
    (with-input-from-string (s string)
      (initialize-tokenizer toke)
      (tokenizer-open toke s)
      (include-character-entity-set (tokenizer-character-entity-table toke) "-//W3C//ENTITIES Latin 1//EN//HTML")
      (include-character-entity-set (tokenizer-character-entity-table toke) "-//W3C//ENTITIES Symbols//EN//HTML")
      (include-character-entity-set (tokenizer-character-entity-table toke) "-//W3C//ENTITIES Special//EN//HTML")
      (loop for tk = (next-token toke)
	    until (eql tk end-of-document)
	    do (print tk)))))

; testing
(defun cdata (string)
  (let ((toke (make-instance 'html-tokenizer)))
    (with-input-from-string (s (concatenate 'string "<>" string)) ; yucky.. add a rule to allow a document not to start with a tag
      (initialize-tokenizer toke)
      (tokenizer-open toke s)
      (include-character-entity-set (tokenizer-character-entity-table toke) "-//W3C//ENTITIES Latin 1//EN//HTML")
      (include-character-entity-set (tokenizer-character-entity-table toke) "-//W3C//ENTITIES Symbols//EN//HTML")
      (include-character-entity-set (tokenizer-character-entity-table toke) "-//W3C//ENTITIES Special//EN//HTML")
      (next-token toke) ; START-OF-DOCUMENT
      (next-token toke) ; <
      (next-token toke) ; 
      (next-token toke) ; >
      (let* ((tok (next-token toke))
	     (result (copy (token-data tok))))
	(release-token-buffer (token-data tok) '(released-by cdata))
	(abandon-tokenizing toke)
	result))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(next-token share-token-buffer release-token-buffer token-data token-complete-p token-mode
		       html-tokenizer tokenizer-condition invalid-context-for-token illegal-syntax html-token html-syntax-token
		       non-html-syntax-token tokenizer-character-entity-table tokenizer-line-number abandon-tokenizing
		       attribute-equals attribute-name attribute-value comment content end-of-document end-tag-open 
		       markup-declaration markup-declaration-open processing-instruction processing-instruction-open 
		       empty-element-tag-close start-of-document start-tag-open tag-close tag-name tokenizer-open tokenizer-position cdata)))

