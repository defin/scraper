; even though these functions are embedded in the parser, they logically belong with the scraper system.

(in-package pull)

(defmethod next-chunk ((parser html-pull-parser))
  (build-chunk :when-done (return-from build chunk)))

(defmethod skip-to-chunk ((parser html-pull-parser) (to-chunk t) &key (search nil))
  (build-chunk :when-done (if (chunk-match to-chunk chunk :search search)
			      (return-from build chunk)
			      (release-chunk chunk))))

(defmethod skip-to-path ((parser html-pull-parser) (to-path t))
  (build-chunk :when-done (if (path-match to-path path)
			      (return-from build chunk)
			      (release-chunk chunk))))

(defmethod skip-to-nth ((parser html-pull-parser) (n integer) &optional (chunk-type t))
  (let ((chunk-n 0))
    (build-chunk :when-done (if (and (chunk-match chunk-type chunk)
				     (= (incf chunk-n) n))
				(return-from build chunk)
				(release-chunk chunk)))))

(defmethod set-base-path ((parser html-pull-parser) &optional (base nil))
  (with-accessors ((path parser-path) (base-path parser-base-path)) parser
    (if base
	(setf base-path base)
	(setf base-path path))))

(defmethod skip-to-relative-path ((parser html-pull-parser) (to-path vector))
  (let ((full-path (concatenate 'vector (parser-base-path parser) to-path))) ; use object-array resource!
    (skip-to-path parser full-path)))

(defmethod skip-chain ((parser html-pull-parser) (chain list))
  (build-chunk :when-done (if (and (chunk-match (pop chain) chunk)
				   (zerop (length chain)))
			      (return-from build chunk)
			      (release-chunk chunk))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(skip-to-chunk skip-to-path skip-to-nth skip-to-relative-path skip-chain)))

