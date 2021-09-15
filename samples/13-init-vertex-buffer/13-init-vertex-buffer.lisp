;;;; 13-init-vertex-buffer.lisp

(in-package #:vk-samples/13-init-vertex-buffer)

(defun 13-init-vertex-buffer (&optional (app-name "13-init-vertex-buffer") (window-width 500) (window-height 500))
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
             :window-height window-height)
    (with-command-buffer (command-buffer
                          device
                          command-pool)
      (let* ((colored-cube-data (make-colored-cube-data))
             (vertex-buffer (vk:create-buffer device
                                              (vk:make-buffer-create-info
                                               :size (* (cffi:foreign-type-size :float)
                                                        (length colored-cube-data))
                                               :usage :vertex-buffer
                                               :sharing-mode :exclusive))))
        (unwind-protect
             ;; the process of allocating memory and copying the data to the device is basically
             ;; the same as in 07-INIT-UNIFORM-BUFFER
             (let ((memory-requirements (vk:get-buffer-memory-requirements device vertex-buffer)))
               (with-allocated-memory (memory
                                       device
                                       (vk:make-memory-allocate-info
                                        :allocation-size (vk:size memory-requirements)
                                        :memory-type-index (find-type-index physical-device
                                                                            memory-requirements)))
                 (copy-to-device device
                                 memory
                                 colored-cube-data
                                 :float)
                 ;; we copied the data, but we still have to bind it to our vertex buffer!
                 (vk:bind-buffer-memory device
                                        vertex-buffer
                                        memory
                                        0)
                 (let ((image-acquired-semaphore (vk:create-semaphore device
                                                                      (vk:make-semaphore-create-info))))
                   (unwind-protect
                        (multiple-value-bind (next-image-index result)
                            (vk:acquire-next-image-khr device
                                                       swapchain
                                                       *fence-timeout*
                                                       image-acquired-semaphore)
                          (assert (and (< next-image-index (length framebuffers))
                                       (eq result :success))
                                  () "Acquired next image index: ~a VkResult: ~a" next-image-index result)
                          (let* ((clear-color-value (vk:make-clear-color-value
                                                     :float-32 (make-array 4
                                                                           :initial-contents '(0.2 0.2 0.2 0.2))))
                                 (clear-depth-stencil (vk:make-clear-depth-stencil-value
                                                       :depth 1.0
                                                       :stencil 0))
                                 (clear-values (list
                                                (vk:make-clear-value
                                                 :color clear-color-value)
                                                (vk:make-clear-value
                                                 :depth-stencil clear-depth-stencil)))
                                 (render-pass-begin-info (vk:make-render-pass-begin-info
                                                          :render-pass render-pass
                                                          :framebuffer (nth next-image-index framebuffers)
                                                          :render-area (vk:make-rect-2d
                                                                        :offset (vk:make-offset-2d
                                                                                 :x 0
                                                                                 :y 0)
                                                                        :extent swapchain-extent)
                                                          :clear-values clear-values)))
                            ;; now we can record some commands in our command buffer!
                            (vk:begin-command-buffer command-buffer
                                                     (vk:make-command-buffer-begin-info))
                            (vk:cmd-begin-render-pass command-buffer
                                                      render-pass-begin-info
                                                      :inline)
                            (vk:cmd-bind-vertex-buffers command-buffer
                                                        0 ;; first binding
                                                        (list vertex-buffer)
                                                        '(0)) ;; offsets
                            (vk:cmd-end-render-pass command-buffer)
                            (vk:end-command-buffer command-buffer)

                            ;; finally, we submit the command buffer and wait for the queue to finish processing it
                            (let ((fence (vk:create-fence device
                                                          (vk:make-fence-create-info))))
                              (unwind-protect
                                   (progn
                                     (vk:queue-submit graphics-queue
                                                      (list
                                                       (vk:make-submit-info
                                                        :command-buffers (list command-buffer)))
                                                      fence)
                                     (loop while (eq :timeout
                                                     (vk:wait-for-fences device
                                                                         (list fence)
                                                                         t ;; wait for all fences given to the the function
                                                                         *fence-timeout*))
                                           do (format t "Still waiting for our commands to finish!")))
                                (vk:destroy-fence device fence)))))
                     (vk:destroy-semaphore device image-acquired-semaphore)))))
          (vk:destroy-buffer device vertex-buffer))))))
