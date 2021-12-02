;;;; ray-tracing

(in-package #:vk-samples/ray-tracing)

(defparameter *queued-frames* 2)

(defclass per-frame-data ()
  ((command-pool
    :initarg :command-pool
    :reader command-pool)
   (command-buffer
    :initarg :command-buffer
    :reader command-buffer)
   (fence
    :initarg :fence
    :reader fence)
   (present-complete-semaphore
    :initarg :present-complete-semaphore
    :reader present-complete-semaphore)
   (render-complete-semaphore
    :initarg :render-complete-semaphore
    :reader render-complete-semaphore)))

(defclass swapchain-data ()
  ((color-format
    :initarg :color-format
    :reader color-format)
   (swapchain
    :initarg :swapchain
    :reader swapchain)
   (images
    :initarg :images
    :reader images)
   (image-views
    :initarg :image-views
    :reader image-views)))

(defgeneric clear-handle-data (device handle-data)
  (:method (device (handle-data per-frame-data))
    (vk:free-command-buffers device (command-pool handle-data) (list (command-buffer handle-data)))
    (vk:destroy-command-pool device (command-pool handle-data))
    (vk:destroy-fence device (fence handle-data))
    (vk:destroy-semaphore device (present-complete-semaphore handle-data))
    (vk:destroy-semaphore device (render-complete-semaphore handle-data)))
  (:method (device (handle-data swapchain-data))
    (loop for image-view in (image-views handle-data)
          do (vk:destroy-image-view device image-view))
    (vk:destroy-swapchain-khr device (swapchain handle-data))))

(defun prepare-per-frame-data (device graphics-and-present-queue-indices)
  (loop for i from 0 below *queued-frames*
        collect (let ((command-pool (vk:create-command-pool device
                                                            (vk:make-command-pool-create-info
                                                             :flags '(:reset-command-buffer)
                                                             :queue-family-index (first graphics-and-present-queue-indices)))))
                  (make-instance 'per-frame-data
                                 :command-pool command-pool
                                 :command-buffer (first (vk:allocate-command-buffers device
                                                                                     (vk:make-command-buffer-allocate-info
                                                                                      :command-pool command-pool
                                                                                      :level :primary
                                                                                      :command-buffer-count 1)))
                                 :fence (vk:create-fence device
                                                         (vk:make-fence-create-info
                                                          :flags '(:signaled)))
                                 :present-complete-semaphore (vk:create-semaphore device
                                                                                  (vk:make-semaphore-create-info))
                                 :render-complete-semaphore (vk:create-semaphore device
                                                                                 (vk:make-semaphore-create-info))))))

(defun make-swapchain-data (physical-device
                            device
                            surface
                            extent
                            usage
                            old-swapchain
                            graphics-queue-family-index
                            present-queue-family-index)
  (let* ((surface-format (pick-color-format physical-device surface))
         (color-format (vk:format surface-format))
         (surface-capabilities (vk:get-physical-device-surface-capabilities-khr physical-device surface))
         (swapchain-extent (determine-swapchain-extent physical-device surface (vk:width extent) (vk:height extent)))
         (swapchain (vk:create-swapchain-khr device
                                             (vk:make-swapchain-create-info-khr
                                              :surface surface
                                              :min-image-count (vk:min-image-count surface-capabilities)
                                              :image-format color-format
                                              :image-color-space (vk:color-space surface-format)
                                              :image-extent swapchain-extent
                                              :image-array-layers 1
                                              :image-usage usage
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
                                              :present-mode (let ((present-mode :fifo-khr))
                                                              (or (loop for mode in (vk:get-physical-device-surface-present-modes-khr physical-device
                                                                                                                                      surface)
                                                                        when (eq mode :mailbox-khr) return mode
                                                                        when (eq mode :immediate) do (setf present-mode mode))
                                                                  present-mode))
                                              :clipped t
                                              :old-swapchain old-swapchain)))
         (images (vk:get-swapchain-images-khr device swapchain)))
    (make-instance 'swapchain-data
                   :color-format color-format
                   :swapchain swapchain
                   :images images
                   :image-views (loop for image in images
                                      collect (vk:create-image-view device
                                                                    (vk:make-image-view-create-info
                                                                     :image image
                                                                     :view-type :2d
                                                                     :format color-format
                                                                     :components (vk:make-component-mapping
                                                                                  :r :r
                                                                                  :g :g
                                                                                  :b :b
                                                                                  :a :a)
                                                                     :subresource-range (vk:make-image-subresource-range
                                                                                         :aspect-mask :color
                                                                                         :base-mip-level 0
                                                                                         :level-count 1
                                                                                         :base-array-layer 0
                                                                                         :layer-count 1)))))))

(defun make-descriptor-pool (device pool-sizes)
  (let ((max-sets (reduce #'+ (map 'list #'vk:descriptor-count pool-sizes))))
    (vk:create-descriptor-pool device
                               (vk:make-descriptor-pool-create-info
                                :flags '(:free-descriptor-set)
                                :max-sets max-sets
                                :pool-sizes pool-sizes))))

(defun supports-extensions-p (physical-device extensions)
  (let ((extension-names (map 'list
                              #'vk:extension-name
                              (vk:enumerate-device-extension-properties physical-device))))
    (= (length extensions)
       (length (intersection extensions extension-names :test #'string=)))))

(defun ray-tracing (&optional (app-name "ray-tracing")
                              (window-width 1280)
                              (window-height 720))
  (glfw:with-init-window (:title app-name
                          :width window-width
                          :height window-height
                          :client-api :no-api)
    (with-instance (instance
                    :app-name app-name
                    :window-extensions t)
      (with-surface (surface
                     instance)
        (let* ((device-extensions (list vk:+khr-swapchain-extension-name+
                                        vk:+khr-ray-tracing-pipeline-extension-name+
                                        vk:+khr-acceleration-structure-extension-name+ ;; this is not enabled in the 
                                        vk:+khr-deferred-host-operations-extension-name+ ;; this is not enabled 
                                        vk:+khr-get-memory-requirements-2-extension-name+))
               (physical-device (or (find-if (lambda (physical-device)
                                               (supports-extensions-p physical-device
                                                                      device-extensions))
                                             (vk:enumerate-physical-devices instance))
                                    (error "Could not find a device supporting all required extensions.")))
               (supported-features (vk:get-physical-device-features-2 physical-device
                                                                      (vk:make-physical-device-features-2
                                                                       :next (vk:make-physical-device-descriptor-indexing-features))))
               (queue-create-infos (make-default-queue-create-infos physical-device surface))
               (graphics-and-present-queue-indices (map 'list #'vk:queue-family-index queue-create-infos)))
          (vk-utils:with-device (device
                                 physical-device
                                 (vk:make-device-create-info
                                  :next (vk:next supported-features)
                                  :queue-create-infos queue-create-infos
                                  :enabled-extension-names device-extensions
                                  :enabled-features (vk:features supported-features)))
            (let ((vk:*default-extension-loader* (vk:make-extension-loader :instance instance
                                                                           :device device)))
              (let* ((per-frame-data (prepare-per-frame-data device graphics-and-present-queue-indices))
                     (graphics-queue (vk:get-device-queue device (first graphics-and-present-queue-indices) 0))
                     (present-queue (vk:get-device-queue device
                                                         (or (second graphics-and-present-queue-indices)
                                                             (first graphics-and-present-queue-indices))
                                                         0))
                     (pool-sizes (list (vk:make-descriptor-pool-size
                                        :type :combined-image-sampler
                                        :descriptor-count 1000)
                                       (vk:make-descriptor-pool-size
                                        :type :uniform-buffer
                                        :descriptor-count 1000)
                                       (vk:make-descriptor-pool-size
                                        :type :storage-buffer
                                        :descriptor-count 1000)))
                     (descriptor-pool (make-descriptor-pool device pool-sizes))
                     (swapchain-data (make-swapchain-data physical-device
                                                          device
                                                          surface
                                                          (vk:make-extent-2d
                                                           :width window-width
                                                           :height window-height)
                                                          '(:color-attachment :storage)
                                                          nil
                                                          (first graphics-and-present-queue-indices)
                                                          (or (second graphics-and-present-queue-indices)
                                                              (first graphics-and-present-queue-indices))))
                     (surface-format (color-format swapchain-data)))
                (unwind-protect
                     (progn)
                  (clear-handle-data device swapchain-data)
                  (vk:destroy-descriptor-pool device descriptor-pool)
                  (loop for frame-data in per-frame-data
                        do (clear-handle-data device frame-data)))))))))))
