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
  (format t "~%~%Running 05-init-swapchain!~%")
  (05-init-swapchain)
  (format t "~%~%Running 06-init-depth-buffer!~%")
  (06-init-depth-buffer)
  (format t "~%~%Running 07-init-uniform-buffer!~%")
  (07-init-uniform-buffer)
  (format t "~%~%Running 08-init-pipeline-layout!~%")
  (08-init-pipeline-layout)
  (format t "~%~%Running 09-init-descriptor-sets!~%")
  (09-init-descriptor-sets)
  (format t "~%~%Running 10-init-render-pass!~%")
  (10-init-render-pass)
  (format t "~%~%Running 11-init-shaders!~%")
  (11-init-shaders)
  (format t "~%~%Running 12-init-frame-buffers!~%")
  (12-init-frame-buffers)
  (format t "~%~%Running 13-init-vertex-buffer!~%")
  (13-init-vertex-buffer)
  (format t "~%~%Running create-debug-utils-messenger!~%")
  (create-debug-utils-messenger)
  (format t "~%~%Running create-debug-utils-messenger-next!~%")
  (create-debug-utils-messenger-next))
