;;;; compile-shaders-from-repl.lisp

(in-package #:vk-samples/compile-shaders-from-repl)

(defmacro with-repl-compiled-shader-module ((shader-module device code) &body body)
  `(let ((,shader-module
           (vk:create-shader-module ,device
                                    (make-instance 'vk:shader-module-create-info
                                                   :code ,code))))
     (unwind-protect
          (progn ,@body)
       (vk:destroy-shader-module ,device ,shader-module))))

(defun compile-shaders-from-repl (&optional (show-cube-seconds 1) (app-name "compile-shaders-from-repl") (window-width 500) (window-height 500))
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
        (with-repl-compiled-shader-module (vertex-shader-module
                                           device
                                           (shaderc:compile-to-spv (make-simple-vertex-shader)
                                                                   :vertex-shader))
          (with-repl-compiled-shader-module (fragment-shader-module
                                             device
                                             (shaderc:compile-to-spv (make-simple-fragment-shader)
                                                                     :fragment-shader))
             (with-simple-graphics-pipeline (graphics-pipeline
                                        device
                                        pipeline-layout
                                        render-pass
                                        swapchain-extent
                                        vertex-shader-module
                                        fragment-shader-module)
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
                       (with-vertex-buffer (vertex-buffer
                                            vertex-buffer-memory
                                            vertex-buffer-memory-requirements
                                            device
                                            physical-device
                                            cube-data
                                            size-of-cube)
                         (with-command-buffer (command-buffer
                                               device
                                               command-pool)
                           (with-semaphore (image-acquired-semaphore
                                            device)
                             (let* ((next-image-index (vk:acquire-next-image-khr device
                                                                                 swapchain
                                                                                 *fence-timeout*
                                                                                 image-acquired-semaphore))
                                    (render-pass-begin-info (make-default-render-pass-begin-info render-pass
                                                                                                 (nth next-image-index framebuffers)
                                                                                                 swapchain-extent)))
                               ;; first we have to record all commands needed for rendering our cube in a command buffer
                               (record-command-buffer (command-buffer)
                                                      (vk:cmd-begin-render-pass command-buffer
                                                                                render-pass-begin-info
                                                                                :inline)
                                                      (vk:cmd-bind-pipeline command-buffer
                                                                            :graphics
                                                                            graphics-pipeline)
                                                      (vk:cmd-bind-descriptor-sets command-buffer
                                                                                   :graphics
                                                                                   pipeline-layout
                                                                                   0 ;; the first descriptor set in the list of descriptor sets that we want to bind
                                                                                   (list descriptor-set)
                                                                                   nil) ;; a list of dynamic offsets
                                                      (vk:cmd-bind-vertex-buffers command-buffer
                                                                                  0 ;; first binding
                                                                                  (list vertex-buffer)
                                                                                  '(0)) ;; offsets
                                                      (vk:cmd-set-viewport command-buffer
                                                                           0
                                                                           (list
                                                                            (make-instance 'vk:viewport
                                                                                           :x 0.0
                                                                                           :y 0.0
                                                                                           :width (float (vk:width swapchain-extent))
                                                                                           :height (float (vk:height swapchain-extent))
                                                                                           :min-depth 0.0
                                                                                           :max-depth 1.0)))
                                                      (vk:cmd-set-scissor command-buffer
                                                                          0
                                                                          (list
                                                                           (make-instance 'vk:rect-2d
                                                                                          :offset (make-instance 'vk:offset-2d
                                                                                                                 :x 0
                                                                                                                 :y 0)
                                                                                          :extent swapchain-extent)))
                                                      (vk:cmd-draw command-buffer
                                                                   (* 12 3) ;; we have 6 faces, 2 triangles per face and 3 vertices per triangle
                                                                   1 ;; we want to render one instance
                                                                   0 ;; we want to start with the first vertex in our vertex buffer
                                                                   0) ;; we want to start with the first instance
                                                      (vk:cmd-end-render-pass command-buffer))
                               ;; now we can submit the command buffer to the graphics pipeline
                               (with-fence (fence
                                            device)
                                 (vk:queue-submit graphics-queue
                                                  (list
                                                   (make-instance 'vk:submit-info
                                                                  :wait-semaphores (list image-acquired-semaphore)
                                                                  :wait-dst-stage-mask '(:color-attachment-output)
                                                                  :command-buffers (list command-buffer)))
                                                  fence)
                                 (loop while (eq :timeout (vk:wait-for-fences device (list fence) t *fence-timeout*)))
                                 ;; as soon as the graphics queue has finished rendering our cube, we can tell the
                                 ;; present queue to show the image that has been generated in the graphics queue
                                 ;; on our window
                                 (let* ((present-queue (if (= graphics-index present-index)
                                                           graphics-queue
                                                           (vk:get-device-queue device
                                                                                present-index
                                                                                0)))
                                        (present-result (vk:queue-present-khr present-queue
                                                                              (make-instance 'vk:present-info-khr
                                                                                             :swapchains (list swapchain)
                                                                                             :image-indices (list next-image-index)))))
                                   (unless (eq :success present-result)
                                     (if (eq :suboptimal-khr present-result)
                                         (format t "vk:queue-present-khr returned ~a~%" present-result)
                                         (error "vk:queue-present-khr returned unexpected value: ~a~%" present-result)))
                                   ;; finally we can show our cube for a couple of seconds and lean back - our work here is done
                                   (sleep show-cube-seconds)
                                   (vk:device-wait-idle device))))))))))))))))))
