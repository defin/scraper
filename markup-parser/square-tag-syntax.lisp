(in-package pull)

(defun |[-READER| (stream &optional arg)
  (declare (ignore arg))
  (let* ((chars (loop for char = (read-char stream)
		      until (char= char #\])
		      collect char))
	 (tag (concatenate 'string '(#\<) chars '(#\>))))
    (tag tag)))

(defvar *original-readtable* nil)

(defun squaretag-install ()
  (unless *original-readtable*
    (setf *original-readtable* *readtable*))
  (setf *readtable* (let ((rt (copy-readtable *original-readtable*)))
		      (set-macro-character #\[ #'|[-READER| rt)
		      rt))
  t)

(defun squaretag-uninstall ()
  (setf *readtable* *original-readtable*)
  t)

(defmacro enable-squaretag-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (squaretag-install)))

(defmacro disable-squaretag-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (squaretag-uninstall)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(squaretag-install squaretag-uninstall)))

