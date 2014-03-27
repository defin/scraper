(defpackage null-dtd
  (:use)
  (:documentation "Empty DTD."))

(in-package null-dtd)

(dtd-base:register-dtd :null "-//PREDEFINED//DTD Null//EN")

(dtd-base:include-character-entity-set (dtd-base:dtd-character-entity-table (dtd-base:find-dtd :null))
				       "-//W3C//ENTITIES Latin 1//EN//HTML")

(dtd-base:include-character-entity-set (dtd-base:dtd-character-entity-table (dtd-base:find-dtd :null))
				       "-//W3C//ENTITIES Symbols//EN//HTML")

(dtd-base:include-character-entity-set (dtd-base:dtd-character-entity-table (dtd-base:find-dtd :null))
				       "-//W3C//ENTITIES Special//EN//HTML")

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:export (cl:let ((symbols ()))
	       (cl:do-symbols (s (cl:find-package 'null-dtd))
		 (cl:push s symbols))
	       symbols)))
