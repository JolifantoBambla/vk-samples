;;;; 15-draw-cube.lisp

(in-package #:vk-samples/15-draw-cube)

(defun 15-draw-cube (&optional (show-cube-seconds 1) (app-name "15-draw-cube") (window-width 500) (window-height 500))
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
          (format t "stub for 15-draw-cube!~%"))))))
