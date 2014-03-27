(in-package cl-user)

(defpackage scraper-base
  (:documentation "Base code for scraping objects from data sources.")
  (:use cl excl rutils))

(defpackage page-scraper
  (:documentation "Web Page Scraper.")
  (:use cl excl rutils dtd-base scraper-base html-base html-pull-parser))


