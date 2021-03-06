;;;; package.lisp

(defpackage #:vk-samples
  (:use #:cl))

(defpackage #:vk-samples/samples
  (:use #:cl)
  (:export
   #:*api-version*
   #:01-init-instance))
