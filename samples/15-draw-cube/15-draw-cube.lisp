;;;; 15-draw-cube.lisp

(in-package #:vk-samples/15-draw-cube)

(defun 15-draw-cube (&optional (show-cube-seconds 1) (app-name "15-draw-cube") (window-width 500) (window-height 600))
  ;; WITH-GFX is a bit convoluted, but it contains everything from the previous samples
  (with-gfx (instance
             device
             physical-device
             surface
             swapchain
             swapchain-extent
             swapchain-images
             swapchain-image-views
             swapchain-image-format
             depth-image
             depth-image-view
             render-pass
             framebuffers
             command-pool
             graphics-queue
             graphics-index
             present-index
             :app-name app-name
             :window-width window-width
             :window-height window-height
             :log-levels (:info :warning :error))
    (with-simple-descriptor-set-layout (descriptor-set-layout
                                        device)
      (with-simple-pipeline-layout (pipeline-layout
                                    device
                                    descriptor-set-layout)
        (with-simple-graphics-pipeline (graphics-pipeline
                                        device
                                        pipeline-layout
                                        render-pass
                                        swapchain-extent
                                        "vertex-shader.spv"
                                        "fragment-shader.spv")
          (multiple-value-bind (mvpc size-of-mvpc) (make-mvpc)
            (with-uniform-buffer (uniform-buffer
                                  uniform-buffer-memory
                                  uniform-buffer-memory-requirements
                                  device
                                  physical-device
                                  size-of-mvpc
                                  :float
                                  :initial-contents mvpc)
              (with-simple-descriptor-set (descriptor-set
                                           descriptor-pool
                                           device
                                           descriptor-set-layout
                                           uniform-buffer
                                           size-of-mvpc)
                (multiple-value-bind (cube-data size-of-cube) (make-colored-cube-data)
                  ;; todo: with-vertex-buffer
                  ;; render pass
                  ;; present
                  (format t "stub for 15-draw-cube!~%"))))))))))
