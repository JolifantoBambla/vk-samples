;;;; package.lisp

(defpackage #:vk-samples
  (:use #:cl))

(defpackage #:vk-samples/samples
  (:use #:cl)
  (:export
   #:*api-version*
   #:create-instance
   #:01-init-instance
   #:02-enumerate-devices))
