;;;; vk-samples.lisp

(in-package #:vk-samples)

(defun run-all-samples ()
  (format t "Running 01-init-device!~%")
  (01-init-instance)
  (format t "~%~%Running 02-enumerate-devices!~%")
  (02-enumerate-devices)
  (format t "~%~%Running 03-init-device!~%")
  (03-init-device)
  (format t "~%~%Running 04-init-command-buffer!~%")
  (04-init-command-buffer)
  (format t "~%~%Running create-debug-utils-messenger!~%")
  (create-debug-utils-messenger)
  (format t "~%~%Running create-debug-utils-messenger-next!~%")
  (create-debug-utils-messenger-next))