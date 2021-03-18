;;;; 09-init-descriptor-sets.lisp

(in-package #:vk-samples/09-init-descriptor-sets)

(defun 09-init-descriptor-sets (&optional (app-name "09-init-descriptor-sets"))
  ;; with-instance-and-device is located in utils - check 03-init-device to see how to create and destroy a device
  (with-instance-and-device (instance device physical-device :app-name app-name :window-extensions nil)))
