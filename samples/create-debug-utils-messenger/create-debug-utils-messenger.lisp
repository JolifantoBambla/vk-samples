;;;; create-debug-utils-messenger.lisp

(in-package #:vk-samples/create-debug-utils-messenger)

;; this function will actually log 
(defun debug-utils-messenger (flags message-types callback-data user-data)
  (format t "[~a] ~a: ~a; user-data: ~a~%"
          flags message-types (vk:message callback-data) user-data))

(cffi:defcallback debug-messenger-callback %vk:bool32 ((flags %vk:debug-utils-message-severity-flag-bits-ext)
                                                       (message-types %vk:debug-utils-message-type-flags-ext)
					               (callback-data %vk:debug-utils-messenger-callback-data-ext)
					               (user-data :pointer))
  (debug-utils-messenger flags
                         message-types
                         callback-data
                         ;; the user data will be passed as a raw pointer in this example, the rest is translated automatically
                         user-data)
  ;; a debug callback MUST return VkFalse! (NIL will be translated to VkFalse automatically)
  nil)

(defun create-debug-utils-messenger (&optional (app-name "create-debug-utils-messenger"))
  (assert (find-if (lambda (p)
                     (string= (vk:extension-name p) vk:+ext-debug-utils-extension-name+))
                   (vk:enumerate-instance-extension-properties))
          ()
          "Could not find the ~a extension." vk:+ext-debug-utils-extension-name+)
  (with-instance (instance app-name nil (list vk:+ext-debug-utils-extension-name+))
    ;; supply the default extension loader with our instance
    (setf vk:*default-extension-loader* (vk:make-extension-loader :instance instance))
    ;; set up the create info
    (let* ((create-info (make-instance 'vk:debug-utils-messenger-create-info-ext
                                       :message-type '(:general :performance :validation)
                                       :message-severity '(:info :warning :error)
                                       :pfn-user-callback (cffi:get-callback 'debug-messenger-callback)
                                       :user-data (cffi:null-pointer)))
           ;; create the debug utils messenger
           (messenger (vk:create-debug-utils-messenger-ext instance create-info)))
      ;; create and destroy a device to actually get some debug messages
      (with-device (device instance))
      ;; destroy the messenger - must be destroyed before the instance is destroyed!
      (vk:destroy-debug-utils-messenger-ext instance messenger))))
