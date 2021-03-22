;;;; 12-init-frame-buffers.lisp

(in-package #:vk-samples/12-init-frame-buffers)

(defun 12-init-frame-buffers (&optional (app-name "12-init-frame-buffers") (window-width 500) (window-height 500))
  ;; WITH-GFX in located in utils - check out the previous examples to see how to create an instance, a device and a surface
  (with-gfx (instance
             device
             physical-device
             surface
             swapchain
             swapchain-extent
             swapchain-images
             swapchain-image-views
             depth-image
             depth-image-view
             graphics-index
             present-index
             :app-name app-name
             :window-width window-width
             :window-height window-height)))
