;;;; utils.lisp

(in-package #:vk-samples/utils)

(defparameter *vk-validation-layer-name* "VK_LAYER_KHRONOS_validation")

(defparameter *api-version* (vk:make-api-version 1 2 153))

;;; BEGIN: WINDOW UTILS
;; todo: open issue / pull-request over at cl-glfw3 to add the functions I need

(cffi:defcfun ("glfwGetRequiredInstanceExtensions" %glfw-get-required-instance-extensions) :pointer
  (count :pointer))

(cffi:defcfun ("glfwCreateWindowSurface" %glfw-create-window-surface) %vk:result
  (instance %vk:instance)
  (window :pointer)
  (allocator :pointer)
  (surface-khr %vk:surface-khr))

(defun glfw-get-required-instance-extensions ()
  (cffi:with-foreign-object (count :uint32)
    (let* ((result (%glfw-get-required-instance-extensions count))
           (extension-count (cffi:mem-aref count :uint32)))
      (loop for i from 0 below extension-count
            collect (cffi:mem-aref result :string i)))))

(defun glfw-create-window-surface (instance &optional (window glfw:*window*) (allocator vk:*default-allocator*))
  (cffi:with-foreign-object (surface-khr '%vk:surface-khr)
    (let ((result (%glfw-create-window-surface instance window allocator surface-khr)))
      (values (cffi:mem-aref surface-khr '%vk:surface-khr) result))))

;;; END: WINDOW UTILS

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
        (layer-names (gensym "LAYER-NAMES")))
    `(let ((,layer-names nil)
           (,extension-names nil))
       (when ,window-extensions
         (setf ,extension-names (nconc extension-names (glfw-get-required-instance-extensions))))
       (when ,log-levels
         (push vk:+ext-debug-utils-extension-name+ ,extension-names))
       (when (and ,message-types
                  (member :validation ,message-types))
         (push *vk-validation-layer-name* ,layer-names))
       (let ((,instance (vk:create-instance (make-instance 'vk:instance-create-info
                                                           :next (when ,log-levels
                                                                   (make-default-debug-utils-messenger-create-info
                                                                    :log-levels ,log-levels
                                                                    :message-types ,message-types))
                                                           :application-info (make-default-application-info ,app-name)
                                                           :enabled-layer-names ,layer-names
                                                           :enabled-extension-names ,extension-names))))
         (unwind-protect
              (progn ,@body)
           (vk:destroy-instance ,instance))))))

(defmacro with-debug-instance ((instance &key (app-name "sample-app") (layer-names nil) (extension-names nil) (log-levels '(:warning :error)) (message-types '(:validation))) &body body)
  (unless (member *vk-validation-layer-name* layer-names :test #'string=)
    (push *vk-validation-layer-name* layer-names))
  (unless (member vk:+ext-debug-utils-extension-name+ extension-names :test #'string=)
    (push vk:+ext-debug-utils-extension-name+ extension-names))
  `(let ((,instance (vk:create-instance (make-instance 'vk:instance-create-info
                                                       :next (make-instance 'vk:debug-utils-messenger-create-info-ext
                                                                          :message-type '(,@message-types)
                                                                          :message-severity '(,@log-levels)
                                                                          :pfn-user-callback (cffi:get-callback 'default-debug-utils-log-callback)
                                                                          :user-data (cffi:null-pointer))
                                                       :application-info (make-instance 'vk:application-info
                                                                                        :application-name ,app-name
                                                                                        :application-version 1
                                                                                        :engine-name "vk"
                                                                                        :engine-version 1
                                                                                        :api-version *api-version*)
                                                       :enabled-layer-names '(,@layer-names)
                                                       :enabled-extension-names '(,@extension-names)))))
     (unwind-protect
          (progn ,@body)
       (vk:destroy-instance ,instance))))

(defun find-graphics-queue-family-index (physical-device)
  (position-if
   (lambda (q)
     (member :graphics (vk:queue-flags q)))
   (vk:get-physical-device-queue-family-properties physical-device)))

(defmacro with-device ((device instance &optional (physical-device (gensym "PHYSICAL-DEVICE"))) &body body)
  `(let* ((,physical-device (first (vk:enumerate-physical-devices ,instance)))
          (,device
            (vk:create-device
             ,physical-device
             (make-instance
              'vk:device-create-info
              :queue-create-infos (list
                                   (make-instance
                                    'vk:device-queue-create-info
                                    :queue-family-index (find-graphics-queue-family-index ,physical-device)
                                    :queue-priorities '(0.0)))))))
     (unwind-protect
          (progn ,@body)
       (vk:destroy-device ,device))))

(defmacro with-instance-and-device ((instance device physical-device &key (app-name "sample") (window-extensions t) (log-levels '(:warning :error)) (message-types '(:validation))) &body body)
  `(with-instance (,instance
                   :app-name,app-name
                   :window-extensions ,window-extensions
                   :log-levels ,log-levels
                   :message-types ,message-types)
     (with-device (,device ,instance ,physical-device)
       (progn ,@body))))

(defmacro with-debug-instance-and-device ((instance device physical-device &key (app-name "sample-app") (layer-names nil) (extension-names nil) (log-levels '(:warning :error)) (message-types '(:validation))) &body body)
  `(with-debug-instance (,instance
                         :app-name ,app-name
                         :layer-names ,layer-names
                         :extension-names ,extension-names
                         :log-levels ,log-levels
                         :message-types ,message-types)
     (with-device (,device ,instance ,physical-device)
       (progn ,@body))))
