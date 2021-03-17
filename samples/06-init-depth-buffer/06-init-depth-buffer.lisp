;;;; 06-init-depth-buffer.lisp

(in-package #:vk-samples/06-init-depth-buffer)

(defun 06-init-depth-buffer (&optional (app-name "05-init-swapchain") (window-width 500) (window-height 500))
  ;; WITH-GFX in located in utils - check out the previous examples to see how to create an instance, a device and a surface
  (with-gfx (instance device physical-device surface :app-name app-name :window-width window-width :window-height window-height)
    ))
