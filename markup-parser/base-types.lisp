;;; HTML base types (HTML 4.01 sec. 6.2)

(in-package html-base)

(deftype pcdata ()
  ;; #PCDATA - parsed character data; text occurring in a context in which markup and entity references may occur.
  '(satisfies stringp))

(deftype cdata ()
  '(satisfies stringp))

(defconstant +legal-nonalphanumerics-for-id-and-name-tokens+ '(#\- #\_ #\: #\.))

(defun id-or-name-p (x)
  ;; ID and NAME tokens must begin with a letter, and may be followed by any number of 
  ;; letters, digits, hyphens, underscores, colons, and periods.
  (if (and (stringp x)
	   (alpha-char-p (aref x 0))
	   (loop named valid
		 for i from 1 to (1- (length x))
		 as char = (aref x i)
		 when (or (not (alphanumericp char))
			  (not (member char +legal-nonalphanumerics-for-id-and-name-tokens+ :test #'char=)))
		     do (return-from valid nil)
		   finally (return-from valid t)))
	t
        nil))

(deftype id () '(satisfies id-or-name-p))

(deftype name () '(satisfies id-or-name-p))

(deftype idref () '(satisfies id-or-name-p))

(defun idrefs-p (x)
  ;; IDREFS is a space-separated list of tokens
  (if (and (stringp x)
	   (alpha-char-p (aref x 0))
	   (loop named valid
		 for i from 1 to (1- (length x))
		 as char = (aref x i)
		 when (or (not (alphanumericp char))
			  (not (member char +legal-nonalphanumerics-for-id-and-name-tokens+ :test #'char=))
			  (not (char= char #\Space)))
		 do (return-from valid nil)
		 finally (return-from valid t)))
      t
      nil))

(deftype idrefs () '(satisfies idrefs-p))

(defun html-number-p (x) x t) ; -TODO 

(deftype html-number () '(satisfies html-number-p))

;;;; Character Entity accessors
;; HTML 4.01 sec. 5.3.2: Character entity references are case-sensitive.

(defvar *character-entity-set-handle-table* (make-hash-table))
(defvar *character-entity-set-identifier-table* (make-hash-table :test 'string=))

(defun make-character-entity-set-table () 
  (make-hash-table :test 'string=))

(defclass character-entity-set ()
  ((handle :accessor character-entity-set-handle
	   :initarg :handle)
   (public-identifier :accessor character-entity-set-public-identifier
		      :initarg :public-identifier)
   (table :accessor character-entity-set-table
	  :initform (make-character-entity-set-table))))

(defmethod print-object ((object character-entity-set) (stream stream))
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~A" (character-entity-set-public-identifier object))))

(defun register-character-entity-set (handle public-identifier)
  (let ((set (make-instance 'character-entity-set :handle handle :public-identifier public-identifier)))
    (setf (gethash public-identifier *character-entity-set-identifier-table*) set)
    (setf (gethash handle *character-entity-set-handle-table*) set)
    set))

(defun find-character-entity-set (handle-or-identifier)
  (etypecase handle-or-identifier
    (string (gethash handle-or-identifier *character-entity-set-identifier-table* :character-entity-set-not-found))
    (symbol (gethash handle-or-identifier *character-entity-set-handle-table* :character-entity-set-not-found))))

(defmacro define-character-entity (set-handle name code)
 `(eval-when (:load-toplevel :execute)
    (setf (gethash ,name (character-entity-set-table (find-character-entity-set ,set-handle))) ,code)
    ',name))

(defun character-entity-code (table symbol)
  (gethash symbol table :no-such-entity))

(defun include-character-entity-set (base-set new-set)
  (let ((base-table (etypecase base-set
		      (cl:hash-table base-set)
		      (character-entity-set (character-entity-set-table base-set))
		      (string (character-entity-set-table (find-character-entity-set base-set)))
		      (symbol (character-entity-set-table (find-character-entity-set base-set)))))
	(new-table (etypecase new-set
		      (cl:hash-table new-set)
		      (character-entity-set (character-entity-set-table new-set))
		      (string (character-entity-set-table (find-character-entity-set new-set)))
		      (symbol (character-entity-set-table (find-character-entity-set new-set))))))
    (maphash #'(lambda (key value)
		 (setf (gethash key base-table) value))
	     new-table)))

;; Predefined Character Entities

(register-character-entity-set :predefined "-//DEFAULT//ENTITIES Predefined Character Entities//EN")

(define-character-entity :predefined "quot" 34)
(define-character-entity :predefined "amp"  38)
(define-character-entity :predefined "lt"   60)
(define-character-entity :predefined "gt"   62)
;(define-character-entity :predefined "apos" )

;;; Rules

(defmacro define-rule (name default)
  (let ((default-symbol-name (fintern "*~A-RULE-DEFAULT*" name))
	(rules (intern "*RULES*")))
    `(eval-when (:load-toplevel :execute)
       (defparameter ,default-symbol-name ,default)
       (push ',name ,rules) ; actually this should replace a rule thats already there..
       ',name)))

(defun rule-state (rule-table rule)
  (let ((state (gethash rule rule-table :unknown-rule)))
    (if (eq state :unknown-rule)
	(error "Rule ~A is not known in rule-table ~A." rule rule-table)
        state)))

(defun (setf rule-state) (new-value rule-table rule)
  (if (or (eq :enforced new-value)
	  (eq :relaxed new-value))
      (setf (gethash rule rule-table) new-value)
      (error "Rule ~A is not known in rule-table ~A." rule rule-table)))

(defun initialize-rule-table (table)
  (let ((rules (symbol-value (intern "*RULES*"))))
    (dolist (r rules)
      (setf (rule-state table r) (symbol-value (fintern "*~A-RULE-DEFAULT*" r))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(pcdata cdata id name idref idrefs html-number +legal-nonalphanumerics-for-id-and-name-tokens+
		   character-entity-set register-character-entity-set find-character-entity-set include-character-entity-set
		   make-character-entity-set-table define-character-entity character-entity-code
		   define-rule rule-state initialize-rule-table)))




