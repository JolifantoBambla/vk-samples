;;;; utils.lisp

(in-package #:vk-samples/utils)

(defparameter *vk-validation-layer-name* "VK_LAYER_KHRONOS_validation")

(defparameter *api-version* vk:+header-version-complete+)

(defparameter *fence-timeout* 100000000)

(defun push-flag (flags flag)
  (union
   (if (listp flags) flags (list flags))
   (list flag)))

(defmacro with-allocated-memory ((memory device allocate-info) &body body)
  `(vk-utils:with-memory (,memory ,device ,allocate-info)
     (progn ,@body)))

(defmacro with-mapped-memory ((p-data device memory offset size) &body body)
  `(cffi:with-foreign-object (,p-data :pointer)
     (vk:map-memory ,device ,memory ,offset ,size ,p-data)
     (unwind-protect
          (progn ,@body)
       (vk:unmap-memory ,device ,memory))))

(defun copy-to-device (device memory data data-type &optional (offset 0))
  "Copies data to device memory.
DEVICE - a VkDevice handle
MEMORY - a VkDeviceMemory handle
DATA - lisp data to copy
DATA-TYPE - a foreign CFFI type corresponding to DATA's type."
  (let* ((data-count (cond
                       ((listp data) (length data))
                       ((arrayp data) (array-total-size data))
                       (t 1)))
         (data-size (* (cffi:foreign-type-size data-type) data-count)))
    (with-mapped-memory (p-mapped device memory offset data-size)
      (cffi:with-foreign-object (p-data data-type data-count)
        (dotimes (i data-count)
          (setf (cffi:mem-aref p-data data-type i)
                (cond
                  ((arrayp data) (aref data i))
                  ((listp data) (nth i data))
                  (t data))))
        (vk-utils:memcpy (cffi:mem-aref p-mapped :pointer)
                         p-data
                         data-size)))))

(defun find-type-index (physical-device memory-requirements &optional (requirements '(:host-visible :host-coherent)))
  (loop with memory-properties = (vk:get-physical-device-memory-properties physical-device)
        with type-bits = (vk:memory-type-bits memory-requirements)
        with requirements-mask = (cffi:foreign-bitfield-value '%vk:memory-property-flags
                                                              requirements)
        for i from 0 below (vk:memory-type-count memory-properties)
        for property-flags = (cffi:foreign-bitfield-value '%vk:memory-property-flags
                                                          (vk:property-flags (nth i (vk:memory-types memory-properties))))
        if (and (not (= (logand type-bits 1) 0))
                (= (logand property-flags requirements-mask)
                   requirements-mask))
          return i
        else
          do (setf type-bits (ash type-bits -1))))

(defun pick-color-format (physical-device surface)
  (let ((surface-formats (vk:get-physical-device-surface-formats-khr physical-device surface)))
    (if (and (= (length surface-formats) 1)
             (eq (first surface-formats) :undefined))
        (progn
          (setf (vk:format (first surface-formats)) :b8g8r8a8-unorm)
          (setf (vk:color-space (first surface-formats)) :srgb-nonlinear-khr)
          (first surface-formats))
        (find-if (lambda (f)
                   (and (member (vk:format f) '(:b8g8r8a8-unorm :r8g8b8a8-unorm :b8g8r8-unorm :r8g8b8-unorm))
                        (eq (vk:color-space f) :srgb-nonlinear-khr)))
                 surface-formats))))

(defun pick-depth-format (physical-device)
  (or (loop for f in '(:d32-sfloat :d32-sfloat-s8-uint :d23-unorm-s8-uint)
            when (member :depth-stencil-attachment (vk:optimal-tiling-features (vk:get-physical-device-format-properties physical-device f)))
            return f)
      (error "failed to find supported depth format")))

(defun create-render-pass (device color-format depth-format &optional (load-op :clear) (color-final-layout :present-src-khr))
  (assert (not (eq color-format :undefined))
          (color-format) "Color format must not be :undefined")
  (let ((attachment-descriptions nil))
    (unless (eq depth-format :undefined)
      (push (vk:make-attachment-description
           :format depth-format
           :samples :1
           :load-op load-op
           :store-op :dont-care
           :stencil-load-op :dont-care
           :stencil-store-op :dont-care
           :initial-layout :undefined
           :final-layout :depth-stencil-attachment-optimal)
            attachment-descriptions))
    (push (vk:make-attachment-description
           :format color-format
           :samples :1
           :load-op load-op
           :store-op :store
           :stencil-load-op :dont-care
           :stencil-store-op :dont-care
           :initial-layout :undefined
           :final-layout color-final-layout)
          attachment-descriptions)
    (vk:create-render-pass device
                           (vk:make-render-pass-create-info
                            :attachments attachment-descriptions
                            :subpasses (list (vk:make-subpass-description
                                              :pipeline-bind-point :graphics
                                              :color-attachments (list (vk:make-attachment-reference
                                                                        :layout :color-attachment-optimal))
                                              :depth-stencil-attachment (list (vk:make-attachment-reference
                                                                               :attachment 1
                                                                               :layout :depth-stencil-attachment-optimal))))))))

(defmacro define-debug-utils-messenger-callback (name logger &optional (user-data-type nil))
  (let ((log-level (gensym))
        (message-type (gensym))
        (callback-data (gensym))
        (user-data (gensym)))
    `(cffi:defcallback ,name %vk:bool32 ((,log-level %vk:debug-utils-message-severity-flag-bits-ext)
                                         (,message-type %vk:debug-utils-message-type-flags-ext)
				         (,callback-data %vk:debug-utils-messenger-callback-data-ext)
				         (,user-data :pointer))
       (,logger ,log-level
                ,message-type
                ,callback-data
                ,(if user-data-type
                     `(cffi:mem-aref ,user-data ,user-data-type)
                     `,user-data))
       nil)))

(define-debug-utils-messenger-callback default-debug-utils-log-callback
    (lambda (log-level message-type message &rest rest)
      (format t "[~a] ~a: ~a~%"
              log-level message-type (vk:message message))))

(defun make-default-application-info (app-name)
  (vk:make-application-info
   :application-name app-name
   :application-version 1
   :engine-name "vk"
   :engine-version 1
   :api-version *api-version*))

(defun make-default-debug-utils-messenger-create-info (&key (log-levels '(:warning :error)) (message-types '(:validation)))
  (vk:make-debug-utils-messenger-create-info-ext
   :message-type message-types
   :message-severity log-levels
   :pfn-user-callback (cffi:get-callback 'default-debug-utils-log-callback)
   :user-data (cffi:null-pointer)))

(defun make-default-render-pass-begin-info (render-pass framebuffer extent &key (clear-color #(0.2 0.2 0.2 0.2)) (clear-depth 1.0) (clear-stencil 0))
  (vk:make-render-pass-begin-info
   :render-pass render-pass
   :framebuffer framebuffer
   :render-area (vk:make-rect-2d
                 :offset (vk:make-offset-2d
                          :x 0
                          :y 0)
                 :extent extent)
   :clear-values (list
                  (vk:make-clear-value
                   :color (vk:make-clear-color-value
                           :float-32 clear-color))
                  (vk:make-clear-value
                   :depth-stencil (vk:make-clear-depth-stencil-value
                                   :depth clear-depth
                                   :stencil clear-stencil)))))

(defmacro with-debug-utils-messenger ((messenger instance create-info) &body body)
  (let ((ext-loader (gensym "EXT-LOADER")))
    `(let ((,ext-loader (vk:make-extension-loader :instance ,instance)))
       (vk-utils:with-debug-utils-messenger-ext (,messenger
                                                 ,instance
                                                 ,create-info
                                                 :extension-loader ,ext-loader)
         (progn ,@body)))))

(defmacro with-instance ((instance
                          &key
                            (app-name "sample-app")
                            (window-extensions t)
                            (log-levels '(:warning :error))
                            (message-types '(:validation))
                            (create-debug-messengerp t))
                         &body body)
  (let ((extension-names (gensym "EXT-NAMES"))
        (layer-names (gensym "LAYER-NAMES"))
        (message-type (gensym "MESSAGE-TYPE"))
        (message-severity (gensym "MESSAGE-SEVERITY"))
        (messenger-create-info (gensym "MESSENGER-CREATE-INFO"))
        (messenger (gensym "MESSENGER")))
    `(let ((,layer-names nil)
           (,extension-names nil)
           (,message-severity '(,@log-levels))
           (,message-type '(,@message-types)))
       (when ,window-extensions
         (setf ,extension-names (nconc ,extension-names (glfw:get-required-instance-extensions))))
       (when ,message-severity
         (push vk:+ext-debug-utils-extension-name+ ,extension-names))
       (when (and ,message-type
                  (member :validation ,message-type))
         (push *vk-validation-layer-name* ,layer-names))
       (let ((,messenger-create-info (when ,create-debug-messengerp
                                       (make-default-debug-utils-messenger-create-info
                                        :log-levels ,message-severity
                                        :message-types ,message-type))))
         (vk-utils:with-instance (,instance
                                  (vk:make-instance-create-info
                                   :next ,messenger-create-info
                                   :application-info (make-default-application-info ,app-name)
                                   :enabled-layer-names ,layer-names
                                   :enabled-extension-names ,extension-names))
           ,(if create-debug-messengerp
                `(with-debug-utils-messenger (,messenger ,instance ,messenger-create-info)
                   (progn ,@body))
                `(progn ,@body)))))))

(defun find-graphics-queue-family-index (physical-device)
  (position-if
   (lambda (q)
     (member :graphics (vk:queue-flags q)))
   (vk:get-physical-device-queue-family-properties physical-device)))

(defun find-graphics-and-present-queue-family-indices (physical-device surface)
  (let* ((queue-family-properties (vk:get-physical-device-queue-family-properties physical-device))
         (graphics-queue-family-index (find-graphics-queue-family-index physical-device))
         (present-queue-family-index (when (vk:get-physical-device-surface-support-khr physical-device graphics-queue-family-index surface)
                                       graphics-queue-family-index)))
    (unless present-queue-family-index
      (loop for q in queue-family-properties
            for i from 0
            when (and (member :graphics (vk:queue-flags q))
                      (vk:get-physical-device-surface-support-khr physical-device i surface))
            do (setf graphics-queue-family-index i)
               (setf present-queue-family-index i)
               (return)))
    (unless present-queue-family-index
      (loop for q in queue-family-properties
            for i from 0
            when (vk:get-physical-device-surface-support-khr physical-device i surface)
            do (setf present-queue-family-index i)
               (return)))
    (unless present-queue-family-index
      (error "Could not find a present queue family"))
    (values graphics-queue-family-index
            present-queue-family-index)))

(defun make-default-queue-create-infos (physical-device &optional (surface nil))
  (if surface
      (multiple-value-bind (gfx present)
          (find-graphics-and-present-queue-family-indices physical-device surface)
        (if (= gfx present)
            (list
             (vk:make-device-queue-create-info
              :queue-family-index gfx
              :queue-priorities '(0.0)))
            (list
             (vk:make-device-queue-create-info
              :queue-family-index gfx
              :queue-priorities '(0.0))
             (vk:make-device-queue-create-info
              :queue-family-index present
              :queue-priorities '(0.0)))))
      (list
       (vk:make-device-queue-create-info
        :queue-family-index (find-graphics-queue-family-index physical-device)
        :queue-priorities '(0.0)))))

(defmacro with-device ((device instance &optional (physical-device (gensym "PHYSICAL-DEVICE")) (surface nil) (enable-swapchain-p nil)) &body body)
  `(let ((,physical-device (first (vk:enumerate-physical-devices ,instance))))
     (vk-utils:with-device (,device
                            ,physical-device
                            (vk:make-device-create-info
                             :queue-create-infos (make-default-queue-create-infos ,physical-device ,surface)
                             :enabled-extension-names ,(if enable-swapchain-p
                                                           `(list vk:+khr-swapchain-extension-name+)
                                                           `nil)))
       (progn ,@body))))

(defmacro with-instance-and-device ((instance device physical-device &key (app-name "sample") (window-extensions t) (log-levels '(:warning :error)) (message-types '(:validation)) (surface nil) (enable-swapchain-p nil)) &body body)
  `(with-instance (,instance
                   :app-name,app-name
                   :window-extensions ,window-extensions
                   :log-levels ,log-levels
                   :message-types ,message-types)
     ,(if surface
          `(with-surface (,surface ,instance)
             (with-device (,device ,instance ,physical-device ,surface ,enable-swapchain-p)
               (progn ,@body)))
          `(with-device (,device ,instance ,physical-device)
             (progn ,@body)))))

(defmacro with-surface ((surface instance) &body body)
  `(let ((,surface (vk:make-surface-khr-wrapper
                    (glfw:create-window-surface (vk:raw-handle ,instance) glfw:*window* vk:*default-allocator*))))
     (unwind-protect
          (progn ,@body)
       (vk:destroy-surface-khr ,instance ,surface))))

(defmacro with-gfx-base ((instance device physical-device surface
                          &key
                            (app-name "sample")
                            (window-width 500)
                            (window-height 500)
                            (log-levels '(:warning :error))
                            (message-types '(:validation))
                            (enable-swapchain-p nil))
                            &body body)
  `(glfw:with-init-window (:title ,app-name
                           :width ,window-width
                           :height ,window-height
                           :client-api :no-api)
     (with-instance-and-device (,instance
                                ,device
                                ,physical-device
                                :surface ,surface
                                :app-name ,app-name
                                :log-levels (,@log-levels)
                                :message-types (,@message-types)
                                :enable-swapchain-p ,enable-swapchain-p)
       (progn ,@body))))

(defun determine-swapchain-extent (physical-device surface window-width window-height)
  (let ((surface-capabilities (vk:get-physical-device-surface-capabilities-khr physical-device surface)))
    (if (= (vk:width (vk:current-extent surface-capabilities)) #xFFFFFFFF)
        (vk:make-extent-2d
         :width (alexandria:clamp window-width
                                  (vk:width (vk:min-image-extent surface-capabilities))
                                  (vk:width (vk:max-image-extent surface-capabilities)))
         :height (alexandria:clamp window-height
                                   (vk:height (vk:min-image-extent surface-capabilities))
                                   (vk:height (vk:max-image-extent surface-capabilities))))
        (vk:current-extent surface-capabilities))))

(defmacro with-swapchain-image-views ((swapchain-image-views swapchain-images device image-format) &body body)
  (let ((img (gensym))
        (img-view (gensym)))
    `(let* ((,swapchain-image-views
              (loop for ,img in ,swapchain-images collect
                    (vk:create-image-view
                     ,device
                     (vk:make-image-view-create-info
                      :image ,img
                      :view-type :2d
                      :format ,image-format
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
                                          :layer-count 1))))))
       (unwind-protect
            (progn ,@body)
         (loop for ,img-view in ,swapchain-image-views
               do (vk:destroy-image-view ,device ,img-view))))))

(defmacro with-swapchain ((swapchain
                           swapchain-extent
                           swapchain-images
                           swapchain-image-views
                           swapchain-image-format
                           device
                           physical-device
                           surface
                           graphics-index
                           present-index
                           window-width
                           window-height)
                          &body body)
  (let ((surface-capabilities (gensym)))
    `(let ((,swapchain-image-format (vk:format (pick-color-format ,physical-device ,surface)))
           (,surface-capabilities (vk:get-physical-device-surface-capabilities-khr ,physical-device ,surface))
           (,swapchain-extent (determine-swapchain-extent ,physical-device
                                                          ,surface
                                                          ,window-width
                                                          ,window-height)))
       (vk-utils:with-swapchain-khr (,swapchain
                                     ,device
                                     (vk:make-swapchain-create-info-khr
                                      :surface ,surface
                                      :min-image-count (vk:min-image-count ,surface-capabilities)
                                      :image-format ,swapchain-image-format
                                      :image-color-space :srgb-nonlinear-khr
                                      :image-extent ,swapchain-extent
                                      :image-array-layers 1
                                      :image-usage '(:color-attachment :transfer-src)
                                      :image-sharing-mode (if (= ,graphics-index ,present-index)
                                                              :exclusive
                                                              :concurrent)
                                      :queue-family-indices (if (= ,graphics-index ,present-index)
                                                                (list ,graphics-index)
                                                                (list ,graphics-index
                                                                      ,present-index))
                                      :pre-transform (if (member :identity (vk:supported-transforms ,surface-capabilities))
                                                         :identity
                                                         (vk:current-transform ,surface-capabilities))
                                      :composite-alpha (cond
                                                         ((member :pre-multiplied (vk:supported-composite-alpha ,surface-capabilities))
                                                          :pre-multiplied)
                                                         ((member :post-multiplied (vk:supported-composite-alpha ,surface-capabilities))
                                                          :post-multiplied)
                                                         ((member :inherit (vk:supported-composite-alpha ,surface-capabilities))
                                                          :inherit)
                                                         (t :opaque))
                                      :present-mode :fifo-khr
                                      :clipped t))
         (let ((,swapchain-images (vk:get-swapchain-images-khr ,device ,swapchain)))
           (with-swapchain-image-views (,swapchain-image-views ,swapchain-images ,device ,swapchain-image-format)
             (progn ,@body)))))))

(defmacro with-depth-buffer ((depth-image depth-image-view device physical-device depth-format extent) &body body)
  (let ((format-properties (gensym))
        (memory-requirements (gensym))
        (memory (gensym)))
    `(let ((,format-properties (vk:get-physical-device-format-properties ,physical-device ,depth-format)))
       (vk-utils:with-image (,depth-image
                             ,device
                             (vk:make-image-create-info
                              :image-type :2d
                              :format ,depth-format
                              :extent (vk:make-extent-3d
                                       :width (vk:width ,extent)
                                       :height (vk:height ,extent)
                                       :depth 1)
                              :mip-levels 1
                              :array-layers 1
                              :samples :1
                              :tiling (cond
                                        ((member :depth-stencil-attachment (vk:linear-tiling-features ,format-properties))
                                         :linear)
                                        ((member :depth-stencil-attachment (vk:optimal-tiling-features ,format-properties))
                                         :optimal)
                                        (t (error "The physical device does not support DEPTH-STENCIL-ATTACHMENT for D16-UNORM")))
                              :sharing-mode :exclusive
                              :usage :depth-stencil-attachment
                              :initial-layout :undefined))
         (let ((,memory-requirements (vk:get-image-memory-requirements ,device ,depth-image)))
           (with-allocated-memory (,memory
                                   ,device
                                   (vk:make-memory-allocate-info
                                    :allocation-size (vk:size ,memory-requirements)
                                    :memory-type-index (find-type-index ,physical-device
                                                                        ,memory-requirements
                                                                        :device-local)))
             (vk:bind-image-memory ,device
                                   ,depth-image
                                   ,memory
                                   0) ;; offset
             (vk-utils:with-image-view (,depth-image-view
                                        ,device
                                        (vk:make-image-view-create-info
                                         :image ,depth-image
                                         :view-type :2d
                                         :format ,depth-format
                                         :components (vk:make-component-mapping
                                                      :r :r
                                                      :g :g
                                                      :b :b
                                                      :a :a)
                                         :subresource-range (vk:make-image-subresource-range
                                                             :aspect-mask :depth
                                                             :base-mip-level 0
                                                             :level-count 1
                                                             :base-array-layer 0
                                                             :layer-count 1)))
               (progn ,@body))))))))

(defmacro with-render-pass ((render-pass device color-format depth-format) &body body)
  `(vk-utils:with-render-pass (,render-pass
                               ,device
                               (vk:make-render-pass-create-info
                                :attachments (list
                                              (vk:make-attachment-description
                                               :format ,color-format
                                               :samples :1
                                               :load-op :clear
                                               :store-op :store
                                               :stencil-load-op :dont-care
                                               :stencil-store-op :dont-care
                                               :initial-layout :undefined
                                               :final-layout :present-src-khr)
                                              (vk:make-attachment-description
                                               :format ,depth-format
                                               :samples :1
                                               :load-op :clear
                                               :store-op :dont-care
                                               :stencil-load-op :dont-care
                                               :stencil-store-op :dont-care
                                               :initial-layout :undefined
                                               :final-layout :depth-stencil-attachment-optimal))
                                :subpasses (list
                                            (vk:make-subpass-description
                                             :pipeline-bind-point :graphics
                                             :color-attachments (list
                                                                 (vk:make-attachment-reference
                                                                  :attachment 0
                                                                  :layout :color-attachment-optimal))
                                             :depth-stencil-attachment (list
                                                                        (vk:make-attachment-reference
                                                                         :attachment 1
                                                                         :layout :depth-stencil-attachment-optimal))))))
     (progn ,@body)))

(defmacro with-shader-module ((shader-module device shader-file-name) &body body)
  `(vk-utils:with-shader-module (,shader-module
                                 ,device
                                 (vk:make-shader-module-create-info
                                  :code (vk-utils:read-shader-source
                                         (merge-pathnames
                                          ,shader-file-name
                                          (asdf:system-relative-pathname
                                           'vk-samples
                                           (make-pathname :directory '(:relative "shaders")))))))
     (progn ,@body)))

(defmacro with-compiled-shader-module ((shader-module device code) &body body)
  `(vk-utils:with-shader-module (,shader-module
                                 ,device
                                 (vk:make-shader-module-create-info
                                  :code ,code))
     (progn ,@body)))

(defun make-framebuffers (device render-pass swapchain-image-views depth-image-view swapchain-extent)
  (loop for swapchain-image-view in swapchain-image-views
        collect (vk:create-framebuffer device
                                       (vk:make-framebuffer-create-info
                                        :render-pass render-pass
                                        :attachments (list swapchain-image-view depth-image-view)
                                        :width (vk:width swapchain-extent)
                                        :height (vk:height swapchain-extent)
                                        :layers 1))))

(defmacro with-framebuffers ((framebuffers device render-pass swapchain-image-views depth-image-view swapchain-extent) &body body)
  (let ((swapchain-image-view (gensym "SWAP-CHAIN-IMAGE-VIEW"))
        (framebuffer (gensym "FRAME-BUFFER")))
    `(let ((,framebuffers (make-framebuffers ,device ,render-pass ,swapchain-image-views ,depth-image-view ,swapchain-extent)))
       (unwind-protect
            (progn ,@body)
         (loop for ,framebuffer in ,framebuffers
               do (vk:destroy-framebuffer ,device ,framebuffer))))))

(defmacro with-command-pool ((command-pool device queue-family-index) &body body)
  `(vk-utils:with-command-pool (,command-pool
                                ,device
                                (vk:make-command-pool-create-info
                                 :queue-family-index ,queue-family-index))
     (progn ,@body)))

(defmacro with-command-buffer ((command-buffer device command-pool &key (level :primary)) &body body)
  (let ((command-buffers (gensym "COMMAND-BUFFERS")))
    `(vk-utils:with-command-buffers (,command-buffers
                                     ,device
                                     (vk:make-command-buffer-allocate-info
                                      :command-pool ,command-pool
                                      :level ,level
                                      :command-buffer-count 1))
       (let ((,command-buffer (first ,command-buffers)))
         (progn ,@body)))))

(defmacro record-command-buffer ((command-buffer) &body body)
  `(progn
     (vk:begin-command-buffer ,command-buffer
                              (vk:make-command-buffer-begin-info))
     (progn ,@body)
     (vk:end-command-buffer ,command-buffer)))

(defmacro with-semaphore ((semaphore device) &body body)
  `(vk-utils:with-semaphore (,semaphore
                             ,device
                             (vk:make-semaphore-create-info))
     (progn ,@body)))

(defmacro with-fence ((fence device) &body body)
  `(vk-utils:with-fence (,fence
                         ,device
                         (vk:make-fence-create-info))
     (progn ,@body)))

(defmacro with-gfx ((instance
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
                     &key
                       (app-name "sample")
                       (window-width 500)
                       (window-height 500)
                       (log-levels '(:warning :error))
                       (message-types '(:validation))
                       (depth-format :d16-unorm))
                    &body body)
  `(with-gfx-base (,instance
                   ,device
                   ,physical-device
                   ,surface
                   :app-name ,app-name
                   :window-width ,window-width
                   :window-height ,window-height
                   :log-levels (,@log-levels)
                   :message-types (,@message-types)
                   :enable-swapchain-p t)
     (multiple-value-bind (,graphics-index ,present-index)
         (find-graphics-and-present-queue-family-indices ,physical-device ,surface)
       (with-swapchain (,swapchain
                        ,swapchain-extent
                        ,swapchain-images
                        ,swapchain-image-views
                        ,swapchain-image-format
                        ,device
                        ,physical-device
                        ,surface
                        ,graphics-index
                        ,present-index
                        ,window-width
                        ,window-height)
         (with-depth-buffer (,depth-image
                             ,depth-image-view
                             ,device
                             ,physical-device
                             ,depth-format
                             ,swapchain-extent)
           (with-render-pass (,render-pass
                              ,device
                              ,swapchain-image-format
                              ,depth-format)
             (with-framebuffers (,framebuffers
                                 ,device
                                 ,render-pass
                                 ,swapchain-image-views
                                 ,depth-image-view
                                 ,swapchain-extent)
               (with-command-pool (,command-pool
                                   ,device
                                   ,graphics-index)
                 (let ((,graphics-queue (vk:get-device-queue ,device
                                                             ,graphics-index
                                                             0)))
                   (progn ,@body))))))))))

(defmacro with-uniform-buffer ((buffer buffer-memory memory-requirements device physical-device size type &key (initial-contents nil)) &body body)
  `(vk-utils:with-buffer (,buffer
                          ,device
                          (vk:make-buffer-create-info
                           :usage :uniform-buffer
                           :sharing-mode :exclusive
                           :size ,size))
     (let ((,memory-requirements (vk:get-buffer-memory-requirements ,device ,buffer)))
       (with-allocated-memory (,buffer-memory
                               ,device
                               (vk:make-memory-allocate-info
                                :allocation-size (vk:size ,memory-requirements)
                                :memory-type-index (find-type-index ,physical-device
                                                                    ,memory-requirements)))
         ,(when initial-contents
            `(copy-to-device ,device
                             ,buffer-memory
                             ,initial-contents
                             ,type))
         (vk:bind-buffer-memory ,device
                                ,buffer
                                ,buffer-memory
                                0)
         (progn ,@body)))))

(defmacro with-vertex-buffer ((buffer buffer-memory memory-requirements device physical-device contents size &optional (type :float)) &body body)
  `(vk-utils:with-buffer (,buffer
                          ,device
                          (vk:make-buffer-create-info
                           :usage :vertex-buffer
                           :sharing-mode :exclusive
                           :size ,size))
     (let ((,memory-requirements (vk:get-buffer-memory-requirements ,device ,buffer)))
       (with-allocated-memory (,buffer-memory
                               ,device
                               (vk:make-memory-allocate-info
                                :allocation-size (vk:size ,memory-requirements)
                                :memory-type-index (find-type-index ,physical-device
                                                                    ,memory-requirements)))
         (copy-to-device ,device
                         ,buffer-memory
                         ,contents
                         ,type)
         (vk:bind-buffer-memory ,device
                                ,buffer
                                ,buffer-memory
                                0)
         (progn ,@body)))))

(defmacro with-simple-descriptor-set-layout ((descriptor-set-layout device) &body body)
  `(vk-utils:with-descriptor-set-layout (,descriptor-set-layout
                                         ,device
                                         (vk:make-descriptor-set-layout-create-info
                                          :bindings (list
                                                     (vk:make-descriptor-set-layout-binding
                                                      :binding 0
                                                      :descriptor-type :uniform-buffer
                                                      :descriptor-count 1
                                                      :stage-flags :vertex))))
     (progn ,@body)))

(defmacro with-simple-pipeline-layout ((pipeline-layout device descriptor-set-layout) &body body)
  `(vk-utils:with-pipeline-layout (,pipeline-layout
                                   ,device
                                   (vk:make-pipeline-layout-create-info
                                    :set-layouts (list ,descriptor-set-layout)))
     (progn ,@body)))

(defmacro with-simple-graphics-pipeline ((graphics-pipeline
                                          device
                                          pipeline-layout
                                          render-pass
                                          swapchain-extent
                                          vertex-shader-module
                                          fragment-shader-module)
                                         &body body)
  (let ((graphics-pipelines (gensym "GRAPHICS-PIPELINES")))
    `(vk-utils:with-graphics-pipelines (,graphics-pipelines
                                        ,device
                                        (list
                                         (make-instance
                                          'vk:graphics-pipeline-create-info
                                          :stages (list
                                                   (make-instance
                                                    'vk:pipeline-shader-stage-create-info
                                                    :stage :vertex
                                                    :module ,vertex-shader-module
                                                    :name "main")
                                                   (make-instance
                                                    'vk:pipeline-shader-stage-create-info
                                                    :stage :fragment
                                                    :module ,fragment-shader-module
                                                    :name "main"))
                                          :vertex-input-state (make-instance
                                                               'vk:pipeline-vertex-input-state-create-info
                                                               :vertex-binding-descriptions (list
                                                                                             (make-instance
                                                                                              'vk:vertex-input-binding-description
                                                                                              :binding 0
                                                                                              :stride (* (cffi:foreign-type-size :float) 8)
                                                                                              :input-rate :vertex))
                                                               :vertex-attribute-descriptions (list
                                                                                               (make-instance
                                                                                                'vk:vertex-input-attribute-description
                                                                                                :location 0
                                                                                                :binding 0
                                                                                                :format :r32g32b32a32-sfloat
                                                                                                :offset 0)
                                                                                               (make-instance
                                                                                                'vk:vertex-input-attribute-description
                                                                                                :location 1
                                                                                                :binding 0
                                                                                                :format :r32g32b32a32-sfloat
                                                                                                :offset 16)))
                                          :input-assembly-state (make-instance
                                                                 'vk:pipeline-input-assembly-state-create-info
                                                                 :topology :triangle-list
                                                                 :primitive-restart-enable nil)
                                          :viewport-state (make-instance
                                                           'vk:pipeline-viewport-state-create-info
                                                           :viewports (list
                                                                       (make-instance
                                                                        'vk:viewport
                                                                        :x 0.0
                                                                        :y 0.0
                                                                        :width (float (vk:width ,swapchain-extent))
                                                                        :height (float (vk:height ,swapchain-extent))
                                                                        :min-depth 0.0
                                                                        :max-depth 1.0))
                                                           :scissors (list
                                                                      (make-instance
                                                                       'vk:rect-2d
                                                                       :offset (vk:make-offset-2d
                                                                                :x 0
                                                                                :y 0)
                                                                       :extent ,swapchain-extent)))
                                          :rasterization-state (make-instance
                                                                'vk:pipeline-rasterization-state-create-info
                                                                :depth-clamp-enable nil
                                                                :rasterizer-discard-enable nil
                                                                :polygon-mode :fill
                                                                :cull-mode :back
                                                                :front-face :clockwise
                                                                :depth-bias-enable nil
                                                                :depth-bias-constant-factor 0.0
                                                                :depth-bias-clamp 0.0
                                                                :depth-bias-slope-factor 0.0
                                                                :line-width 1.0)
                                          :multisample-state (make-instance
                                                              'vk:pipeline-multisample-state-create-info
                                                              :rasterization-samples :1
                                                              :min-sample-shading 0.0
                                                              :sample-mask nil
                                                              :sample-shading-enable nil
                                                              :alpha-to-coverage-enable nil
                                                              :alpha-to-one-enable nil)
                                          :depth-stencil-state (make-instance
                                                                'vk:pipeline-depth-stencil-state-create-info
                                                                :depth-test-enable t
                                                                :depth-write-enable t
                                                                :depth-compare-op :less-or-equal
                                                                :depth-bounds-test-enable nil
                                                                :stencil-test-enable nil
                                                                :front (make-instance
                                                                        'vk:stencil-op-state
                                                                        :fail-op :keep
                                                                        :pass-op :keep
                                                                        :depth-fail-op :keep
                                                                        :compare-op :always
                                                                        :compare-mask 0
                                                                        :write-mask 0
                                                                        :reference 0)
                                                                :back (make-instance
                                                                       'vk:stencil-op-state
                                                                       :fail-op :keep
                                                                       :pass-op :keep
                                                                       :depth-fail-op :keep
                                                                       :compare-op :always
                                                                       :compare-mask 0
                                                                       :write-mask 0
                                                                       :reference 0)
                                                                :min-depth-bounds 0.0
                                                                :max-depth-bounds 0.0)
                                          :color-blend-state (make-instance
                                                              'vk:pipeline-color-blend-state-create-info
                                                              :logic-op-enable nil
                                                              :logic-op :no-op
                                                              :attachments (list
                                                                            (make-instance
                                                                             'vk:pipeline-color-blend-attachment-state
                                                                             :blend-enable nil
                                                                             :src-color-blend-factor :zero
                                                                             :dst-color-blend-factor :zero
                                                                             :color-blend-op :add
                                                                             :src-alpha-blend-factor :zero
                                                                             :dst-alpha-blend-factor :zero
                                                                             :alpha-blend-op :add
                                                                             :color-write-mask '(:r :g :b :a)))
                                                              :blend-constants  #(1.0 1.0 1.0 1.0))
                                          :dynamic-state (make-instance
                                                          'vk:pipeline-dynamic-state-create-info
                                                          :dynamic-states '(:viewport :scissor))
                                          :layout ,pipeline-layout
                                          :render-pass ,render-pass
                                          :subpass 0)))
       (let ((,graphics-pipeline (first ,graphics-pipelines)))
         (progn ,@body)))))
  

(defmacro with-simple-descriptor-pool ((descriptor-pool device) &body body)
  `(vk-utils:with-descriptor-pool (,descriptor-pool
                                   ,device
                                   (vk:make-descriptor-pool-create-info
                                    :flags :free-descriptor-set
                                    :max-sets 1
                                    :pool-sizes (list (vk:make-descriptor-pool-size
                                                       :type :uniform-buffer
                                                       :descriptor-count 1))))
     (progn ,@body)))

(defmacro with-simple-descriptor-set ((descriptor-set descriptor-pool device descriptor-set-layout uniform-buffer size) &body body)
  (let ((descriptor-sets (gensym "DESCRIPTOR-SETS")))
    `(with-simple-descriptor-pool (,descriptor-pool
                                   ,device)
       (vk-utils:with-descriptor-sets (,descriptor-sets
                                       ,device
                                       (vk:make-descriptor-set-allocate-info
                                        :descriptor-pool ,descriptor-pool
                                        :set-layouts (list ,descriptor-set-layout)))
         (let ((,descriptor-set (first ,descriptor-sets)))
           (progn
             (vk:update-descriptor-sets ,device
                                        (list
                                         (vk:make-write-descriptor-set
                                          :dst-set ,descriptor-set
                                          :dst-binding 0
                                          :dst-array-element 0
                                          :descriptor-type :uniform-buffer
                                          :buffer-info (list  (vk:make-descriptor-buffer-info
                                                               :buffer ,uniform-buffer
                                                               :offset 0
                                                               :range ,size))))
                                        nil)
             ,@body))))))

(defun create-graphics-pipeline (device
                                 pipeline-cache
                                 vertex-shader-data
                                 fragment-shader-data
                                 vertex-stride
                                 vertex-input-attribute-offset
                                 front-face
                                 depth-buffered
                                 pipeline-layout
                                 render-pass)
  (let* ((vertex-input-attribute-descriptions (when (< 0 vertex-stride)
                                                (loop for offset in vertex-input-attribute-offset
                                                      for i from 0
                                                      collect (vk:make-vertex-input-attribute-description
                                                               :location i
                                                               :binding 0
                                                               :format (first offset)
                                                               :offset (second offset)))))
         (vertex-input-binding-description (vk:make-vertex-input-binding-description
                                            :binding 0
                                            :stride vertex-stride
                                            :input-rate :vertex))
         (stencil-op-state (vk:make-stencil-op-state
                            :fail-op :keep
                            :pass-op :keep
                            :depth-fail-op :keep
                            :compare-op :always
                            :compare-mask 0
                            :write-mask 0
                            :reference 0)))
    (first
     (vk:create-graphics-pipelines
      device
      (list
       (vk:make-graphics-pipeline-create-info
        :stages (list (vk:make-pipeline-shader-stage-create-info
                       :stage :vertex
                       :module (first vertex-shader-data)
                       :name "main"
                       :specialization-info (second vertex-shader-data))
                      (vk:make-pipeline-shader-stage-create-info
                       :stage :fragment
                       :module (first fragment-shader-data)
                       :name "main"
                       :specialization-info (second fragment-shader-data)))
        :vertex-input-state (when (< 0 vertex-stride)
                              (vk:make-pipeline-vertex-input-state-create-info
                               :vertex-binding-descriptions (list vertex-input-binding-description)
                               :vertex-attribute-descriptions vertex-input-attribute-descriptions))
        :input-assembly-state (vk:make-pipeline-input-assembly-state-create-info
                               :topology :triangle-list
                               :primitive-restart-enable nil)
        :viewport-state (vk:make-pipeline-viewport-state-create-info
                         ;; we'll set both viewport and scissor state as dynamic, so these members are ignored
                         ;; however, we'll still need to set something here for the viewportCount and scissorCount members to be set
                         :viewports (list (vk:make-viewport))
                         :scissors (list (vk:make-rect-2d)))
        :rasterization-state (vk:make-pipeline-rasterization-state-create-info
                              :depth-clamp-enable nil
                              :rasterizer-discard-enable nil
                              :polygon-mode :fill
                              :cull-mode :back
                              :front-face front-face
                              :depth-bias-enable nil
                              :depth-bias-constant-factor 0.0
                              :depth-bias-clamp 0.0
                              :depth-bias-slope-factor 0.0
                              :line-width 1.0)
        :multisample-state (vk:make-pipeline-multisample-state-create-info
                            :rasterization-samples :1
                            :min-sample-shading 0.0
                            :sample-mask nil
                            :sample-shading-enable nil
                            :alpha-to-coverage-enable nil
                            :alpha-to-one-enable nil)
        :depth-stencil-state (vk:make-pipeline-depth-stencil-state-create-info
                              :depth-test-enable depth-buffered
                              :depth-write-enable depth-buffered
                              :depth-compare-op :less-or-equal
                              :depth-bounds-test-enable nil
                              :stencil-test-enable nil
                              :front stencil-op-state
                              :back stencil-op-state
                              :min-depth-bounds 0.0
                              :max-depth-bounds 0.0)
        :color-blend-state (vk:make-pipeline-color-blend-state-create-info
                            :logic-op-enable nil
                            :logic-op :no-op
                            :attachments (list
                                          (make-instance
                                           'vk:pipeline-color-blend-attachment-state
                                           :blend-enable nil
                                           :src-color-blend-factor :zero
                                           :dst-color-blend-factor :zero
                                           :color-blend-op :add
                                           :src-alpha-blend-factor :zero
                                           :dst-alpha-blend-factor :zero
                                           :alpha-blend-op :add
                                           :color-write-mask '(:r :g :b :a)))
                            :blend-constants  #(1.0 1.0 1.0 1.0))
        :dynamic-state (vk:make-pipeline-dynamic-state-create-info
                        :dynamic-states '(:viewport :scissor))
        :layout pipeline-layout
        :render-pass render-pass
        :subpass 0))))))

(defun update-descriptor-sets (device
                               descriptor-set
                               buffer-data
                               texture-data
                               &optional
                                 (binding-offset 0))
  (let* ((buffer-write-descriptor-sets
           (loop for bd in (if (listp buffer-data) buffer-data (list buffer-data))
                 for dst-binding from binding-offset
                 collect (vk:make-write-descriptor-set
                          :dst-set descriptor-set
                          :dst-binding dst-binding
                          :dst-array-element 0
                          :descriptor-type (first bd)
                          :buffer-info (list
                                        (vk:make-descriptor-buffer-info
                                         :buffer (second bd)
                                         :offset 0
                                         :range vk:+whole-size+))
                          :texel-buffer-view (third bd))))
         (texture-write-descriptor-sets
           (list (vk:make-write-descriptor-set
                  :dst-set descriptor-set
                  :dst-binding (+ binding-offset (length buffer-write-descriptor-sets))
                  :dst-array-element 0
                  :descriptor-type :combined-image-sampler
                  :image-info (loop for td in (if (listp texture-data) texture-data (list texture-data))
                                    collect (vk:make-descriptor-image-info
                                             :sampler (sampler td)
                                             :image-view (image-view (image-data td))
                                             :image-layout :shader-read-only-optimal)))))
         (write-descriptor-sets (concatenate 'list
                                             buffer-write-descriptor-sets
                                             texture-write-descriptor-sets)))
    (vk:update-descriptor-sets device
                               write-descriptor-sets
                               nil)))

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

(defclass image-data ()
  ((img-format
    :initarg :format
    :reader img-format)
   (image
    :initarg :image
    :reader image)
   (device-memory
    :initarg :device-memory
    :reader device-memory)
   (image-view
    :initarg :image-view
    :reader image-view)))

(defclass buffer-data ()
  ((buffer
    :initarg :buffer
    :reader buffer)
   (device-memory
    :initarg :device-memory
    :reader device-memory)))

(defclass texture-data ()
  ((tex-format
    :initarg :format
    :reader tex-format)
   (extent
    :initarg :extent
    :reader extent)
   (needs-staging-p
    :initarg :needs-staging-p
    :reader needs-staging-p)
   (staging-buffer-data
    :initarg :staging-buffer-data
    :reader staging-buffer-data)
   (image-data
    :initarg :image-data
    :reader image-data)
   (sampler
    :initarg :sampler
    :reader sampler)))

(defgeneric clear-handle-data (device handle-data)
  (:method (device (handle-data swapchain-data))
    (loop for image-view in (image-views handle-data)
          do (vk:destroy-image-view device image-view))
    (vk:destroy-swapchain-khr device (swapchain handle-data)))
  (:method (device (handle-data buffer-data))
    (vk:free-memory device (device-memory handle-data))
    (vk:destroy-buffer device (buffer handle-data)))
  (:method (device (handle-data image-data))
    (vk:destroy-image-view device (image-view handle-data))
    (vk:free-memory device (device-memory handle-data))
    (vk:destroy-image device (image handle-data)))
  (:method (device (handle-data texture-data))
    (when (needs-staging-p handle-data)
      (clear-handle-data device (staging-buffer-data handle-data)))
    (clear-handle-data device (image-data handle-data))
    (vk:destroy-sampler device (sampler handle-data))))

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

(defun allocate-device-memory (device physical-device memory-requirements memory-property-flags &optional (next nil))
  (vk:allocate-memory device
                      (vk:make-memory-allocate-info
                       :next next
                       :allocation-size (vk:size memory-requirements)
                       :memory-type-index (find-type-index physical-device memory-requirements memory-property-flags))))

(defun make-image-data (physical-device device img-format extent tiling usage initial-layout memory-properties aspect-mask)
  (let* ((image (vk:create-image device (vk:make-image-create-info
                                         :image-type :2d
                                         :format img-format
                                         :extent (vk:make-extent-3d
                                                  :width (vk:width extent)
                                                  :height (vk:height extent)
                                                  :depth 1)
                                         :mip-levels 1
                                         :array-layers 1
                                         :samples :1
                                         :tiling tiling
                                         :usage (if (listp usage)
                                                    (concatenate 'list usage '(:sampled))
                                                    (list usage :sampled))
                                         :sharing-mode :exclusive
                                         :initial-layout initial-layout)))
         (device-memory (allocate-device-memory device
                                                physical-device
                                                (vk:get-image-memory-requirements device image)
                                                memory-properties)))
    (vk:bind-image-memory device image device-memory 0)
    (make-instance 'image-data
                   :format img-format
                   :image image
                   :device-memory device-memory
                   :image-view (vk:create-image-view device
                                                     (vk:make-image-view-create-info
                                                      :image image
                                                      :view-type :2d
                                                      :format img-format
                                                      :components (vk:make-component-mapping
                                                                   :r :r
                                                                   :g :g
                                                                   :b :b
                                                                   :a :a)
                                                      :subresource-range (vk:make-image-subresource-range
                                                                          :aspect-mask aspect-mask
                                                                          :level-count 1
                                                                          :layer-count 1))))))

(defun make-depth-buffer-data (physical-device device depth-format extent)
  (make-image-data physical-device
                   device
                   depth-format
                   extent
                   :optimal
                   :depth-stencil-attachment
                   :undefined
                   :device-local
                   :depth))

(defun make-buffer-data (physical-device
                         device
                         size
                         usage
                         &optional (property-flags '(:host-visible :host-coherent)))
  (let* ((buffer (vk:create-buffer device
                                   (vk:make-buffer-create-info
                                    :size size
                                    :usage usage
                                    :sharing-mode :exclusive)))
         (device-memory-next (when (member :shader-device-address (if (listp usage) usage (list usage)))
                               (vk:make-memory-allocate-flags-info
                                :flags '(:device-address))))
         (device-memory (allocate-device-memory device
                                                physical-device
                                                (vk:get-buffer-memory-requirements device buffer)
                                                property-flags
                                                device-memory-next)))
    (vk:bind-buffer-memory device buffer device-memory 0)
    (make-instance 'buffer-data
                   :buffer buffer
                   :device-memory device-memory)))

(defun copy-to-buffer (buffer-data
                       physical-device
                       device
                       command-pool
                       queue
                       data
                       data-type)
  (let* ((data-count (cond
                       ((listp data) (length data))
                       ((arrayp data) (array-total-size data))
                       (t 1)))
         (data-size (* (cffi:foreign-type-size data-type)
                       data-count))
         (staging-buffer (make-buffer-data physical-device
                                           device
                                           data-size
                                           '(:transfer-src))))
    (copy-to-device device
                    (device-memory staging-buffer)
                    data
                    data-type)
    (one-time-submit device
                     command-pool
                     queue
                     (lambda (command-buffer)
                       (vk:cmd-copy-buffer command-buffer
                                           (buffer staging-buffer)
                                           (buffer buffer-data)
                                           (list (vk:make-buffer-copy
                                                  :src-offset 0
                                                  :dst-offset 0
                                                  :size data-size)))))
    (clear-handle-data device staging-buffer)))

(defun make-texture-data (physical-device
                          device
                          &optional
                            (extent (vk:make-extent-2d :width 256 :height 256))
                            usage-flags
                            format-feature-flags
                            enable-anisotropy-p
                            force-staging-p)
  (let* ((tex-format :r8g8b8a8-unorm)
         (format-properties (vk:get-physical-device-format-properties physical-device tex-format))
         (feature-flags (push-flag format-feature-flags
                                   :sampled-image))
         (needs-staging-p (or force-staging-p
                              (some (lambda (flag)
                                      (not (member flag (vk:linear-tiling-features format-properties))))
                                    feature-flags)))
         (image-tiling (if needs-staging-p
                           :optimal
                           :linear))
         (initial-layout (if needs-staging-p
                             :undefined
                             :preinitialized))
         (staging-buffer-data (when needs-staging-p
                                (make-buffer-data physical-device
                                                  device
                                                  (* (vk:width extent)
                                                     (vk:height extent)
                                                     4)
                                                  :transfer-src)))
         (image-data (make-image-data physical-device
                                      device
                                      tex-format
                                      extent
                                      image-tiling
                                      (push-flag (if needs-staging-p
                                                     (push-flag usage-flags :transfer-dst)
                                                     usage-flags)
                                                 :sampled)
                                      initial-layout
                                      ;;'(:host-coherent :host-visible)
                                      (unless needs-staging-p '(:host-coherent :host-visible))
                                      :color))
         (sampler (vk:create-sampler device
                                     (vk:make-sampler-create-info
                                      :mag-filter :linear
                                      :min-filter :linear
                                      :mipmap-mode :linear
                                      :address-mode-u :repeat
                                      :address-mode-v :repeat
                                      :address-mode-w :repeat
                                      :anisotropy-enable enable-anisotropy-p
                                      :max-anisotropy 16.0
                                      :compare-op :never
                                      :border-color :float-opaque-black))))
    (make-instance 'texture-data
                   :format tex-format
                   :needs-staging-p needs-staging-p
                   :staging-buffer-data staging-buffer-data
                   :extent extent
                   :image-data image-data
                   :sampler sampler)))

(defun set-image-layout (command-buffer image img-format old-image-layout new-image-layout)
  (let* ((source-access-mask (cond
                               ((eq :transfer-dst-optimal old-image-layout)
                                '(:transfer-write))
                               ((eq :preinitialized old-image-layout)
                                '(:host-write))
                               ((not (or (eq :general old-image-layout)
                                         (eq :undefined old-image-layout)))
                                (error "illegal OLD-IMAGE-LAYOUT: ~s" old-image-layout))))
         (source-stage (cond
                         ((member old-image-layout '(:general :preinitialized))
                          '(:host))
                         ((eq :transfer-dst-optimal old-image-layout)
                          '(:transfer))
                         ((eq :undefined old-image-layout)
                          '(:top-of-pipe))
                         (t (error "illegal OLD-IMAGE-LAYOUT: ~s" old-image-layout))))
         (destination-access-mask (cond
                                    ((eq :color-attachment-optimal new-image-layout)
                                     '(:color-attachment-write))
                                    ((eq :depth-stencil-attachment-optimal new-image-layout)
                                     '(:depth-stencil-attachment-read :depth-stencil-attachment-write))
                                    ((eq :shader-read-only-optimal new-image-layout)
                                     '(:shader-read))
                                    ((eq :transfer-src-optimal new-image-layout)
                                     '(:transfer-read))
                                    ((eq :transfer-dst-optimal new-image-layout)
                                     '(:transfer-write))
                                    ((not (member new-image-layout '(:general :present-src-khr)))
                                     (error "illegal NEW-IMAGE-LAYOUT: ~s" new-image-layout))))
         (destination-stage (cond
                              ((eq :color-attachment-optimal new-image-layout)
                               '(:color-attachment-output))
                              ((eq :depth-stencil-attachment-optimal new-image-layout)
                               '(:early-fragment-tests))
                              ((eq :general new-image-layout)
                               '(:host))
                              ((eq :present-src-khr new-image-layout)
                               '(:bottom-of-pipe))
                              ((eq :shader-read-only-optimal new-image-layout)
                               '(:fragment-shader))
                              ((member new-image-layout '(:transfer-dst-optimal :transfer-src-optimal))
                               '(:transfer))
                              (t (error "illegal NEW-IMAGE-LAYOUT: ~s" new-image-layout))))
         (aspect-mask (if (eq :depth-stencil-attachment-optimal new-image-layout)
                          (if (member img-format '(:d32-sfloat-s8-uint :d24-unorm-s8-uint))
                              '(:stencil :depth)
                              '(:depth))
                          '(:color)))
         (image-subresource-range (vk:make-image-subresource-range
                                   :aspect-mask aspect-mask
                                   :level-count 1
                                   :layer-count 1))
         (image-memory-barrier (vk:make-image-memory-barrier
                                :src-access-mask source-access-mask
                                :dst-access-mask destination-access-mask
                                :old-layout old-image-layout
                                :new-layout new-image-layout
                                :src-queue-family-index vk:+queue-family-ignored+
                                :dst-queue-family-index vk:+queue-family-ignored+
                                :image image
                                :subresource-range image-subresource-range)))
    (vk:cmd-pipeline-barrier command-buffer
                             nil
                             nil
                             (list image-memory-barrier)
                             source-stage
                             destination-stage)))

(defun set-texture-image (texture-data device command-buffer image-generator)
  (with-slots (needs-staging-p
               staging-buffer-data
               image-data
               extent)
      texture-data
    (copy-to-device device
                    (device-memory (if needs-staging-p
                                       staging-buffer-data
                                       image-data))
                    (funcall image-generator extent)
                    :uint8)
    (with-slots (image img-format) image-data
      (if needs-staging-p
          (with-slots ((width vk:width)
                       (height vk:height))
              extent
            (set-image-layout command-buffer
                              image
                              img-format
                              :undefined
                              :transfer-dst-optimal)
            (vk:cmd-copy-buffer-to-image command-buffer
                                         (buffer staging-buffer-data)
                                         image
                                         :transfer-dst-optimal
                                         (list
                                          (vk:make-buffer-image-copy
                                           :buffer-offset 0
                                           :buffer-row-length width
                                           :buffer-image-height height
                                           :image-subresource (vk:make-image-subresource-layers
                                                               :aspect-mask :color
                                                               :mip-level 0
                                                               :base-array-layer 0
                                                               :layer-count 1)
                                           :image-offset (vk:make-offset-3d
                                                          :x 0
                                                          :y 0
                                                          :z 0)
                                           :image-extent (vk:make-extent-3d
                                                          :width width
                                                          :height height
                                                          :depth 1))))
            (set-image-layout command-buffer
                              image
                              img-format
                              :transfer-dst-optimal
                              :shader-read-only-optimal))
          (set-image-layout command-buffer
                            image
                            img-format
                            :preinitialized
                            :shader-read-only-optimal)))))

(defun one-time-submit (device command-pool queue func)
  (let ((command-buffer (first (vk:allocate-command-buffers device
                                                            (vk:make-command-buffer-allocate-info
                                                             :command-pool command-pool
                                                             :level :primary
                                                             :command-buffer-count 1)))))
    (vk:begin-command-buffer command-buffer
                             (vk:make-command-buffer-begin-info
                              :flags '(:one-time-submit)))
    (funcall func command-buffer)
    (vk:end-command-buffer command-buffer)
    (vk:queue-submit queue
                     (list (vk:make-submit-info
                            :command-buffers (list command-buffer))))
    (vk:queue-wait-idle queue)))

(defun checkerboard-image-generator  (&optional (rgb0 #(0 0 0)) (rgb1 #(255 255 255)))
  (lambda (extent)
    (with-slots ((width vk:width)
                 (height vk:height))
        extent
      (let ((data (make-array (* width height 4) :element-type '(unsigned-byte 8) :initial-element 255)))
        (loop for row from 0 below height
              do (loop for column from 0 below width
                       for idx = (* 4 (+ column (* row width))) 
                       for rgb = (if (= 0
                                        (logxor (min (logand row #x10) 1)
                                                (min (logand column #x10) 1)))
                                     rgb1
                                     rgb0)
                       do
                       (setf (elt data idx) (elt rgb 0))
                       (setf (elt data (+ idx 1)) (elt rgb 1))
                       (setf (elt data (+ idx 2)) (elt rgb 2))))
        data))))
