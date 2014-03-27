(in-package scraper-base)

(defmethod scrape-and-transform ((scraper scraper))
  (scrape scraper)
  (transform scraper))

;; need to define a matcher on urls that base don a regex of th eurl decides what class scraper should be used to scrape it
;; then we can write a proxy easily.
(defmethod transform ((scraper) (function-list list))
  ; each function in the FUNCTION-LIST can consume as many of the position-markers for the scraper as it likes.. but only in-order
  ; generally, each function will be responsible for one :insert, :delete, or :replace transformation.
  (loop for (tf ) in function-list
	do (funcall function scraper)))

;	position-marker in (scraper-position-markers scraper)


(defmethod compute-transformed-text ((start-marker symbol) (info scraper-info) (source-text string))
  ; given the marker name, scraped info, and the source text, return the transformed text to output
)

(defmethod transformation ((scraper scraper) (tf (eql :insert))
			   &key insertion-function
			   &allow-other-keys)
  (let* ((start-marker (pop (scraper-position-markers scraper)))
	 (transformed-text (compute-transformed-text start-marker (scraper-info scraper) "")))
    transformed-text

)
  )

(defmethod transformation ((scraper scraper) (transform (eql :delete))
			   &key start-marker end-marker
			   &allow-other-keys)
  )


(defmethod transformation ((scraper scraper) (tf (eql :replace))
			   &key start-marker end-marker insertion-function
			   &allow-other-keys)
  )
