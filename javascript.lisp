(in-package page-scraper)

(defstruct (string-escape (:constructor make-string-escape (escape-string substitution documentation)))
  escape-string escaped-escape-string substitution documentation)

(defparameter *javascript-string-escapes*
  `( ,(make-string-escape "\\b" #\Backspace "Backspace.")
     ,(make-string-escape "\\f" #\Page "Form feed.")
     ,(make-string-escape "\\n" #\Newline "Newline.")
     ,(make-string-escape "\\O" #\Null "ASCII NUL character.")
     ,(make-string-escape "\\r" #\Return "Carriage return.")
     ,(make-string-escape "\\t" #\Tab "Horizontal tab.")
     ,(make-string-escape "\\v" #\Tab "Vertical tab.")
     ,(make-string-escape "\\'" #\' "Single quote.")
     ,(make-string-escape "\\\"" #\" "Double quote.")
     ,(make-string-escape "\\\\\\" #\\ "Backslash.")
     ,(make-string-escape "\\([0-7]{3})" 'decode-octal-character-escape
			  "The Latin-1 character specified by the three octal digits between 0 and 377.")
     ,(make-string-escape "\\x([0-9A-Fa-f]{2})" 'decode-hex-character-escape
			  "The Latin-1 character specified by the two hexadecimal digits dd between 00 and FF.")
     ,(make-string-escape "\\u([0-9A-Fa-f]{4})" 'decode-hex-character-escape
			  "The Unicode character specified by the four hexadecimal digits dddd.") )
  "Table of substitutions for string escapes.  Substitution can either be a character object
or a symbol naming a function, which will be called with the submatch as its single argument.")

(defparameter *javascript-string-escape-regex*
  (excl:compile-re `(:alternation ,@(loop for s-e in *javascript-string-escapes*
					  for escaped = (concatenate 'string "\\"
								     (string-escape-escape-string s-e))
					  do (setf (string-escape-escaped-escape-string s-e) escaped)
					  collect (excl:parse-re escaped)))))

(defun lookup-javascript-escape-substitution (match)
  (and match
       (find match *javascript-string-escapes*
	     :test #'(lambda (m o)
		       (excl:match-re (string-escape-escaped-escape-string o) m)))))

(defun decode-octal-character-escape (digits)
  (code-char (parse-integer digits :radix 8)))

(defun decode-hex-character-escape (digits)
  (code-char (parse-integer digits :radix 16)))

(defun compute-escape-substitution (matches)
  (let* ((m (first matches))
	 (string-escape (lookup-javascript-escape-substitution m))
	 (substitution (string-escape-substitution string-escape)))
    (cond ((or (characterp substitution)
	       (stringp substitution))
	   (list substitution))
	  ((null substitution)
	   nil)
	  ((symbolp substitution)
	   (list (funcall (symbol-function substitution)
			  (nth-value 2 (match-re (string-escape-escaped-escape-string string-escape) m))))))))

(defun unescape-javascript-string (string)
  "Substitute JavaScript escapes with appropriate text."
  (excl:replace-re string
		   *javascript-string-escape-regex*
		   #'compute-escape-substitution))
