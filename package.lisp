;;;; package.lisp

(defpackage #:binoxxo-engine
  (:use #:cl) (:import-from :alexandria :copy-array) (:export #:lösen #:erstelle-rätsel))

(defpackage #:tk-binoxxo
  (:use #:cl #:binoxxo-engine))
