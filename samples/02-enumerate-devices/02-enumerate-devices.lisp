;;;; 02-enumerate-devices.lisp

(in-package #:vk-samples/02-enumerate-devices)

(defun 02-enumerate-devices (&optional (app-name "02-enumerate-devices"))
  ;; with-instance is located in utils - check 01-init-instance to see how to create and destroy an instance
  (with-instance (instance app-name)
    (format t "Found ~a devices!~%"
            (length (vk:enumerate-physical-devices instance)))))
