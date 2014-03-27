(in-package cl-user)

(defpackage markup-base
  (:documentation "Basic data types for XML/HTML/XHTML markup.")
  (:use cl excl racehorse rutils)
  (:nicknames html-base)
  (:import-from "EXCL" "GC")
  (:export gc))

(defpackage markup-tokenizer
  (:documentation "Tokenizer for XML/HTML/XHTML syntax.")
  (:nicknames toke html-tokenizer)
  (:use cl excl racehorse rutils html-base))

(defpackage dtd-base
  (:documentation "Basic DTD definitions.")
  (:use cl excl racehorse rutils html-base))

(defpackage markup-pull-parser
  (:documentation "Pull Parser for XML/HTML/XHTML.")
  (:nicknames pull html-pull-parser)
  (:use cl excl racehorse rutils html-base dtd-base))


