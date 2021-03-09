;;;; utils.lisp

(in-package #:vk-samples/utils)

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

(defmacro with-instance ((instance &optional (app-name "sample-app") (engine-name "vk") (layer-names nil) (extension-names nil)) &body body)
  `(let ((,instance (vk:create-instance (make-instance 'vk:instance-create-info
                                                       :application-info (make-instance 'vk:application-info
                                                                                        :application-name ,app-name
                                                                                        :application-version 1
                                                                                        :engine-name ,engine-name
                                                                                        :engine-version 1
                                                                                        :api-version *api-version*)
                                                       :enabled-layer-names ,layer-names
                                                       :enabled-extension-names ,extension-names))))
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

(defmacro with-instance-and-device ((instance device &optional (app-name "sample-app") (engine-name "vk") (layer-names nil) (extension-names nil)) &body body)
  `(with-instance (,instance ,app-name ,engine-name ,layer-names ,extension-names)
     (with-device (,device ,instance)
       (progn ,@body))))
