(in-package dtd-base)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((symbols ()))
    (do-symbols (s (find-package 'dtd-base))
      (when (or (search "ELEMENT-" (symbol-name s) :test #'string=)
		(search "CONTENT-" (symbol-name s) :test #'string=))
	(push s symbols)))
    (export symbols (find-package 'dtd-base))))
