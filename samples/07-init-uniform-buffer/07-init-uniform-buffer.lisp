;;;; 07-init-uniform-buffer.lisp

(in-package #:vk-samples/07-init-uniform-buffer)

(defun 07-init-uniform-buffer (&optional (app-name "07-init-uniform-buffer"))
  ;; with-instance-and-device is located in utils - check 03-init-device to see how to create and destroy a device
  (with-instance-and-device (instance device physical-device :app-name app-name :window-extensions nil)))
