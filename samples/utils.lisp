;;;; utils.lisp

(in-package #:vk-samples/utils)

(defparameter *vk-validation-layer-name* "VK_LAYER_KHRONOS_validation")

(defparameter *api-version* (vk:make-api-version 1 2 153))

;; todo: memcpy should be in VK-UTILS
(cffi:defcfun ("memcpy" memcpy) :pointer
  (dest :pointer)
  (src :pointer)
  (count %vk:size-t))

(defmacro with-allocated-memory ((memory device allocate-info) &body body)
  `(let ((,memory (vk:allocate-memory ,device ,allocate-info)))
     (unwind-protect
          (progn ,@body)
       (vk:free-memory ,device ,memory))))

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
        (memcpy (cffi:mem-aref p-mapped :pointer)
                p-data
                data-size)))))

(defun find-type-index (physical-device memory-requirements)
  (loop with memory-properties = (vk:get-physical-device-memory-properties physical-device)
        with type-bits = (vk:memory-type-bits memory-requirements)
        with requirements-mask = (cffi:foreign-bitfield-value '%vk:memory-property-flags
                                                              '(:host-visible :host-coherent))
        for i from 0 below (vk:memory-type-count memory-properties)
        for property-flags = (cffi:foreign-bitfield-value '%vk:memory-property-flags
                                                          (vk:property-flags (nth i (vk:memory-types memory-properties))))
        if (and (logand type-bits 1)
                (= (logand property-flags requirements-mask)
                   requirements-mask))
          return i
        else
          do (setf type-bits (ash type-bits -1))))

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
  (make-instance 'vk:application-info
                 :application-name app-name
                 :application-version 1
                 :engine-name "vk"
                 :engine-version 1
                 :api-version *api-version*))

(defun make-default-debug-utils-messenger-create-info (&key (log-levels '(:warning :error)) (message-types '(:validation)))
  (make-instance 'vk:debug-utils-messenger-create-info-ext
                 :message-type message-types
                 :message-severity log-levels
                 :pfn-user-callback (cffi:get-callback 'default-debug-utils-log-callback)
                 :user-data (cffi:null-pointer)))

(defmacro with-instance ((instance &key (app-name "sample-app") (window-extensions t) (log-levels '(:warning :error)) (message-types '(:validation))) &body body)
  (let ((extension-names (gensym "EXT-NAMES"))
        (layer-names (gensym "LAYER-NAMES"))
        (message-type (gensym "MESSAGE-TYPE"))
        (message-severity (gensym "MESSAGE-SEVERITY")))
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
       (let ((,instance (vk:create-instance (make-instance 'vk:instance-create-info
                                                           :next (when ,message-severity
                                                                   (make-default-debug-utils-messenger-create-info
                                                                    :log-levels ,message-severity
                                                                    :message-types ,message-type))
                                                           :application-info (make-default-application-info ,app-name)
                                                           :enabled-layer-names ,layer-names
                                                           :enabled-extension-names ,extension-names))))
         (unwind-protect
              (progn ,@body)
           (vk:destroy-instance ,instance))))))

(defun find-graphics-queue-family-index (physical-device)
  (position-if
   (lambda (q)
     (member :graphics (vk:queue-flags q)))
   (vk:get-physical-device-queue-family-properties physical-device)))

(defun find-graphics-and-present-queue-family-indices (physical-device surface)
  (let* ((graphics-queue-family-index (find-graphics-queue-family-index physical-device))
         (present-queue-family-index (when (vk:get-physical-device-surface-support-khr physical-device surface graphics-queue-family-index)
                                       graphics-queue-family-index)))
    (unless present-queue-family-index
      (loop for q in queue-familiy-properties
            for i from 0
            when (and (member :graphics (vk:queue-flags q))
                      (vk:get-physical-device-surface-support-khr physical-device surface i))
            do (setf graphics-queue-family-index i)
               (setf present-queue-family-index i)
               (return)))
    (unless present-queue-family-index
      (loop for q in queue-family-properties
            for i from 0
            when (vk:get-physical-device-surface-support-khr physical-device surface i)
            do (setf present-queue-family-index i)
               (return)))
    (unless present-queue-family-index
      (error "Could not find a present queue family"))
    (values graphics-queue-family-index
            present-queue-family-index)))

(defmacro with-device ((device instance &optional (physical-device (gensym "PHYSICAL-DEVICE")) (surface nil)) &body body)
  (let ((gfx (gensym "GRAPHICS-FAMILY"))
        (present (gensym "PRESENT-FAMILY")))
    `(let* ((,physical-device (first (vk:enumerate-physical-devices ,instance)))
            (,device
              (vk:create-device
               ,physical-device
               (make-instance
                'vk:device-create-info
                :queue-create-infos ,(if surface
                                         `(multiple-value-bind (,gfx ,present)
                                              (find-graphics-and-present-queue-family-indices ,physical-device ,surface)
                                            (if (= ,gfx ,present)
                                                (make-instance
                                                 'vk:device-queue-create-info
                                                 :queue-family-index ,gfx
                                                 :queue-priorities '(0.0)))
                                            (list
                                             (make-instance
                                              'vk:device-queue-create-info
                                              :queue-family-index ,gfx
                                              :queue-priorities '(0.0))
                                             (make-instance
                                              'vk:device-queue-create-info
                                              :queue-family-index ,present
                                              :queue-priorities '(0.0))))
                                         `(list
                                           (make-instance
                                            'vk:device-queue-create-info
                                            :queue-family-index (find-graphics-queue-family-index ,physical-device)
                                            :queue-priorities '(0.0))))))))
       (unwind-protect
            (progn ,@body)
         (vk:destroy-device ,device)))))

(defmacro with-instance-and-device ((instance device physical-device &key (app-name "sample") (window-extensions t) (log-levels '(:warning :error)) (message-types '(:validation)) (surface nil)) &body body)
  `(with-instance (,instance
                   :app-name,app-name
                   :window-extensions ,window-extensions
                   :log-levels ,log-levels
                   :message-types ,message-types)
     ,(if surface
          `(with-surface (,surface ,instance)
             (with-device (,device ,instance ,physical-device ,surface)
               (progn ,@body)))
          `(with-device (,device ,instance ,physical-device)
             (progn ,@body)))))

(defmacro with-surface ((surface instance) &body body)
  `(let ((,surface (glfw:create-window-surface ,instance glfw:*window* vk:*default-allocator*)))
     (unwind-protect
          (progn ,@body)
       (vk:destroy-surface-khr ,instance ,surface))))

(defmacro with-gfx ((instance device physical-device surface
                     &key
                       (app-name "sample")
                       (window-width 500)
                       (window-height 500)
                       (log-levels '(:warning :error))
                       (message-types '(:validation)))
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
                                :message-types (,@message-types))
       (progn ,@body))))

(defmacro with-uniform-buffer ((buffer buffer-memory memory-requirements device physical-device size type &key (initial-contents nil)) &body body)
  `(let ((,buffer (vk:create-buffer ,device
                                    (make-instance 'vk:buffer-create-info
                                                   :usage :uniform-buffer
                                                   :sharing-mode :exclusive
                                                   :size ,size))))
     (unwind-protect
          (let ((,memory-requirements (vk:get-buffer-memory-requirements ,device ,buffer)))
            (with-allocated-memory (,buffer-memory
                                    ,device
                                    (make-instance 'vk:memory-allocate-info
                                                   :allocation-size (vk:size ,memory-requirements)
                                                   :memory-type-index (find-type-index ,physical-device
                                                                                       ,memory-requirements)))
              ,(when initial-contents
                 `(copy-to-device ,device
                                  ,buffer-memory
                                  ,initial-contents
                                  ,type))
              (progn ,@body)))
       (vk:destroy-buffer ,device ,buffer))))

(defmacro with-simple-descriptor-set-layout ((descriptor-set-layout device) &body body)
  `(let ((,descriptor-set-layout (vk:create-descriptor-set-layout
                                  ,device
                                  (make-instance 'vk:descriptor-set-layout-create-info
                                                 :bindings (list
                                                            (make-instance 'vk:descriptor-set-layout-binding
                                                                           :binding 0
                                                                           :descriptor-type :uniform-buffer
                                                                           :descriptor-count 1
                                                                           :stage-flags :vertex))))))
     (unwind-protect
          (progn ,@body)
       (vk:destroy-descriptor-set-layout ,device ,descriptor-set-layout))))
