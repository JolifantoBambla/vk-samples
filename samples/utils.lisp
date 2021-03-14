;;;; utils.lisp

(in-package #:vk-samples/utils)

(defparameter *vk-validation-layer-name* "VK_LAYER_KHRONOS_validation")

(defparameter *api-version* (vk:make-api-version 1 2 153))

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

