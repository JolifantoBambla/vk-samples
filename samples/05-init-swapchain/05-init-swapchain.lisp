;;;; 05-init-swapchain.lisp

(in-package #:vk-samples/05-init-swapchain)

(defun 05-init-swapchain (&optional (app-name "05-init-swapchain") (window-width 64) (window-height 64))
  ;; graphics calls on OS X must occur in the main thread
  ;; todo: wrap this (trivial-main-thread:with-body-in-main-thread ()
  
  ;; to create a swapchain we first need a window. we'll use glfw3 for the window creation
  (glfw:with-init-window (:title app-name :width window-width :height window-height :client-api :no-api)
    ;; Vulkan is a platform agnostic API, which means that it can not interface directly with the window system on its own.
    ;; We need to enable the WSI (Window System Integration) extensions to establish a connection between our instance and
    ;; the window system.
    ;; The required WSI extensions are different for each platform. Luckily glfw has us covered and we can query the names
    ;; of all required extensions.
    (let* ((required-extensions (glfw:get-required-instance-extensions))
           ;; we also enable the debug utils extension to get debug output - this is completely optional
           ;; check out the samples create-debug-utils-messenger(-next) to see how this works
           (extension-names (push vk:+ext-debug-utils-extension-name+ required-extensions))
           (instance (vk:create-instance (make-instance 'vk:instance-create-info
                                                       ;; we create a debug-utils-messenger together with the instance (optional)
                                                       :next (make-default-debug-utils-messenger-create-info)
                                                       :application-info (make-default-application-info app-name)
                                                       ;; we enable the validation layer to get validation messages (optional)
                                                       :enabled-layer-names (list *vk-validation-layer-name*)
                                                       :enabled-extension-names extension-names))))
      (unwind-protect
           ;; first we'll create a surface - the Vulkan API does not provide a function for this though
           ;; so we have to create this via glfw
           (let* ((surface (glfw:create-window-surface instance glfw:*window* vk:*default-allocator*)))
             (unwind-protect
                  (let* ((physical-device (first (vk:enumerate-physical-devices instance)))
                         (queue-family-properties (vk:get-physical-device-queue-family-properties physical-device))
                         (graphics-queue-family-index (position-if (lambda (q)
                                                                     (member :graphics (vk:queue-flags q)))
                                                                   queue-family-properties))
                         ;; if we want to present images on our window surface we need a queue family which supports presentation
                         ;; most of the time a graphics queue family will support presentation as well, but this is not necessarily the case
                         ;; our quest to find a suitable queue family begins here
                         (present-queue-family-index (when (vk:get-physical-device-surface-support-khr physical-device surface graphics-queue-family-index)
                                                       graphics-queue-family-index)))
                    ;; if our graphics queue family doesn't support presentation, we'll try to find a queue family which supports both
                    (unless present-queue-family-index
                      (loop for q in queue-familiy-properties
                            for i from 0
                            when (and (member :graphics (vk:queue-flags q))
                                      (vk:get-physical-device-surface-support-khr physical-device surface i))
                            do (setf graphics-queue-family-index i)
                               (setf present-queue-family-index i)
                               (return)))
                    ;; if we didn't find a queue family which supports graphics and presentation, we'll try to find a family which supports presentation
                    (unless present-queue-family-index
                      (loop for q in queue-family-properties
                            for i from 0
                            when (vk:get-physical-device-surface-support-khr physical-device surface i)
                            do (setf present-queue-family-index i)
                               (return)))
                    ;; if there is really no queue family that supports presenting to a window surface we'll have to accept defeat...
                    (unless present-queue-family-index
                      (error "Could not find a present queue family"))
                    ;; if both queue family indices are the same we can simply create just one queue, otherwise we need to create 2 - one in each family
                    (let* ((queue-create-infos (if (= graphics-queue-family-index present-queue-family-index)
                                                   (list
                                                    (make-instance
                                                     'vk:device-queue-create-info
                                                     :queue-family-index graphics-queue-family-index
                                                     :queue-priorities '(0.0)))
                                                   (list
                                                    (make-instance
                                                     'vk:device-queue-create-info
                                                     :queue-family-index graphics-queue-family-index
                                                     :queue-priorities '(0.0))
                                                    (make-instance
                                                     'vk:device-queue-create-info
                                                     :queue-family-index present-queue-family-index
                                                     :queue-priorities '(0.0)))))
                           (device (vk:create-device physical-device
                                                     (make-instance 'vk:device-create-info
                                                                    :queue-create-infos queue-create-infos
                                                                    ;; we need to enable the swapchain extension on the device!
                                                                    :enabled-extension-names (list vk:+khr-swapchain-extension-name+)))))
                      (unwind-protect
                           (let* ((formats (vk:get-physical-device-surface-formats-khr physical-device surface))
                                  ;; we'll just take the first format or B8G8R8A8
                                  (image-format (if (eq (first formats) :undefined)
                                                    :b8g8r8a8-unorm
                                                    (vk:format (first formats))))
                                  ;; the surface capabilities holds information about what sizes and formats the surface actually supports
                                  (surface-capabilities (vk:get-physical-device-surface-capabilities-khr physical-device surface))
                                  ;; #xFFFFFFFF is a special value, meaning that the surface size is undefined
                                  ;; in that case we'll use the width and height that we want (clamped to the possible range)
                                  (swapchain-extent (if (= (vk:width (vk:current-extent surface-capabilities)) #xFFFFFFFF)
                                                        (make-instance 'vk:extent-2d
                                                                       :width (alexandria:clamp window-width
                                                                                                (vk:width (vk:min-image-extent surface-capabilities))
                                                                                                (vk:width (vk:max-image-extent surface-capabilities)))
                                                                       :height (alexandria:clamp window-height
                                                                                                 (vk:height (vk:min-image-extent surface-capabilities))
                                                                                                 (vk:height (vk:max-image-extent surface-capabilities))))
                                                        (vk:current-extent surface-capabilities)))
                                  (swapchain-create-info (make-instance 'vk:swapchain-create-info-khr
                                                                        :surface surface
                                                                        ;; at least min-image-count swapchain images will be created
                                                                        ;; we just set it to the minimum number that is supported by our surface
                                                                        :min-image-count (vk:min-image-count surface-capabilities)
                                                                        :image-format image-format
                                                                        :image-color-space :srgb-nonlinear-khr
                                                                        :image-extent swapchain-extent
                                                                        :image-array-layers 1
                                                                        :image-usage :color-attachment
                                                                        ;; if we use two separate queues we'll need to share the images between them
                                                                        :image-sharing-mode (if (= graphics-queue-family-index present-queue-family-index)
                                                                                                :exclusive
                                                                                                :concurrent)
                                                                        :queue-family-indices (if (= graphics-queue-family-index present-queue-family-index)
                                                                                                  (list graphics-queue-family-index)
                                                                                                  (list graphics-queue-family-index
                                                                                                        present-queue-family-index))
                                                                        :pre-transform (if (member :identity (vk:supported-transforms surface-capabilities))
                                                                                           :identity
                                                                                           (vk:current-transform surface-capabilities))
                                                                        :composite-alpha (cond
                                                                                           ((member :pre-multiplied (vk:supported-composite-alpha surface-capabilities))
                                                                                            :pre-multiplied)
                                                                                           ((member :post-multiplied (vk:supported-composite-alpha surface-capabilities))
                                                                                            :post-multiplied)
                                                                                           ((member :inherit (vk:supported-composite-alpha surface-capabilities))
                                                                                            :inherit)
                                                                                           (t :opaque))
                                                                        ;; FIFO is always guaranteed by the spec to be supported
                                                                        :present-mode :fifo-khr
                                                                        :clipped t))
                                  (swapchain (vk:create-swapchain-khr device swapchain-create-info)))
                             (unwind-protect
                                  ;; swapchain images are created during swapchain creation
                                  (let* ((swapchain-images (vk:get-swapchain-images-khr device swapchain))
                                         (component-mapping (make-instance 'vk:component-mapping
                                                                           :r :r
                                                                           :b :b
                                                                           :g :b
                                                                           :a :a))
                                         (subresource-range (make-instance 'vk:image-subresource-range
                                                                           :aspect-mask :color
                                                                           :base-mip-level 0
                                                                           :level-count 1
                                                                           :base-array-layer 0
                                                                           :layer-count 1))
                                         (image-views (loop for image in swapchain-images
                                                            collect (vk:create-image-view device
                                                                                          (make-instance 'vk:image-view-create-info
                                                                                                         :image image
                                                                                                         :view-type :2d
                                                                                                         :format image-format
                                                                                                         :components component-mapping
                                                                                                         :subresource-range subresource-range)))))
                                    (format t "Created ~a image view(s) ready to use on the created swapchain!"
                                            (length image-views))
                                    ;; destroy the image views again
                                    (loop for image-view in image-views
                                          do (vk:destroy-image-view device image-view)))
                               ;; destroy the swapchain
                               (vk:destroy-swapchain-khr device swapchain)))
                        ;; never forget to destroy the device!
                        (vk:destroy-device device))))
               ;; TODO: document that vk*KHR stuff is loaded by default, so we don't need to initialize an extension loader
               ;; even though we didn't create the surface using the Vulkan API, we must destroy it via VK:DESTROY-SURFACE-KHR
               (vk:destroy-surface-khr instance surface)))
        ;; finally destroy the instance
        (vk:destroy-instance instance)))))
