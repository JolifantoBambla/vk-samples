;;;; 12-init-frame-buffers.lisp

(in-package #:vk-samples/12-init-frame-buffers)

(defun 12-init-frame-buffers (&optional (app-name "12-init-frame-buffers") (window-width 500) (window-height 500))
  ;; WITH-GFX-BASE in located in utils - check out the previous examples to see how to create an instance, a device and a surface
  (with-gfx-base (instance
                  device
                  physical-device
                  surface
                  :app-name app-name
                  :window-width window-width
                  :window-height window-height
                  :enable-swapchain-p t)
    (multiple-value-bind (graphics-index present-index)
        (find-graphics-and-present-queue-family-indices physical-device surface)
      (with-swapchain (swapchain
                       swapchain-extent
                       swapchain-images
                       swapchain-image-views
                       color-format
                       device
                       physical-device
                       surface
                       graphics-index
                       present-index
                       window-width
                       window-height)
        (let ((depth-format :d16-unorm))
          (with-depth-buffer (depth-image
                              depth-image-view
                              device
                              physical-device
                              depth-format
                              swapchain-extent)
            (with-render-pass (render-pass
                               device
                               color-format
                               depth-format)
              (let ((framebuffers (loop for swapchain-image-view in swapchain-image-views
                                        for framebuffer-create-info = (make-instance 'vk:framebuffer-create-info
                                                                                     :render-pass render-pass
                                                                                     :attachments (list
                                                                                                   swapchain-image-view
                                                                                                   depth-image-view)
                                                                                     :width (vk:width swapchain-extent)
                                                                                     :height (vk:height swapchain-extent)
                                                                                     :layers 1)
                                        collect (vk:create-framebuffer device framebuffer-create-info))))
                (unwind-protect
                     (format t "Created ~a framebuffer(s)!" (length framebuffers))
                  (loop for framebuffer in framebuffers
                        do (vk:destroy-framebuffer device framebuffer)))))))))))
