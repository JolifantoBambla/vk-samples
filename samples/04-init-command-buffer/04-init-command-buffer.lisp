;;;; 04-init-command-buffer

(in-package #:vk-samples/04-init-command-buffer)

(defun 04-init-command-buffer (&optional (app-name "04-init-command-buffer") (engine-name "vk"))
  ;; with-instance-and-device is located in utils - check 03-init-device to see how to create and destroy a device
  (with-instance-and-device (instance device app-name engine-name)
    (format t "Hey, I'm not implemented yet :(~%")))
