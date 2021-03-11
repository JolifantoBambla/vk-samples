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

(defmacro with-instance ((instance &optional (app-name "sample-app") (layer-names nil) (extension-names nil)) &body body)
  `(let ((,instance (vk:create-instance (make-instance 'vk:instance-create-info
                                                       :application-info (make-instance 'vk:application-info
                                                                                        :application-name ,app-name
                                                                                        :application-version 1
                                                                                        :engine-name "vk"
                                                                                        :engine-version 1
                                                                                        :api-version *api-version*)
                                                       :enabled-layer-names ,layer-names
                                                       :enabled-extension-names ,extension-names))))
     (unwind-protect
          (progn ,@body)
       (vk:destroy-instance ,instance))))

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

(defmacro with-device ((device instance) &body body)
  (let ((physical-device (gensym)))
    `(let* ((,physical-device (first (vk:enumerate-physical-devices ,instance)))
            (,device
              (vk:create-device
               ,physical-device
               (make-instance
                'vk:device-create-info
                :queue-create-infos (list
                                     (make-instance
                                      'vk:device-queue-create-info
                                      :queue-family-index (position-if
                                                           (lambda (q)
                                                             (member :graphics (vk:queue-flags q)))
                                                           (vk:get-physical-device-queue-family-properties ,physical-device))
                                      :queue-priorities '(0.0)))))))
       (unwind-protect
            (progn ,@body)
         (vk:destroy-device ,device)))))

(defmacro with-instance-and-device ((instance device &optional (app-name "sample")) &body body)
  `(with-instance (,instance ,app-name)
     (with-device (,device ,instance)
       (progn ,@body))))

(defmacro with-debug-instance-and-device ((instance device &key (app-name "sample-app") (layer-names nil) (extension-names nil) (log-levels '(:warning :error)) (message-types '(:validation))) &body body)
  `(with-debug-instance (,instance
                         :app-name ,app-name
                         :layer-names ,layer-names
                         :extension-names ,extension-names
                         :log-levels ,log-levels
                         :message-types ,message-types)
     (with-device (,device ,instance)
       (progn ,@body))))
