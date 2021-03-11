;;;; 04-init-command-buffer

(in-package #:vk-samples/04-init-command-buffer)

(defun 04-init-command-buffer (&optional (app-name "04-init-command-buffer"))
  ;; with-instance-and-device is located in utils - check 03-init-device to see how to create and destroy a device
  (with-debug-instance-and-device (instance device :app-name app-name)
    (format t "Hey, I'm not implemented yet :(~%")))
