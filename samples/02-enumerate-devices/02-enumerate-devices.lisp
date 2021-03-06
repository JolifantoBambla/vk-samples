;;;; 02-enumerate-devices.lisp

(in-package #:vk-samples/samples)

(defun 02-enumerate-devices (&optional (app-name "01-init-instance") (engine-name "vk"))
  (let* ((instance (create-instance app-name))
         ;; enumerate the physical devices
         (physical-devices (vk:enumerate-physical-devices instance)))

    (format t "Found ~a devices!~%"
            (length physical-devices))

    (vk:destroy-instance instance)))
