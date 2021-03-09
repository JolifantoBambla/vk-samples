;;;; create-debug-utils-messenger.lisp

(in-package #:vk-samples/create-debug-utils-messenger)

(cffi:defcallback debug-messenger-callback %vk:bool32 ((flags %vk:debug-utils-message-severity-flag-bits-ext)
                                                       (message-types %vk:debug-utils-message-type-flags-ext)
					               (callback-data %vk:debug-utils-messenger-callback-data-ext)
					               (user-data :pointer))
  (debug-utils-messenger flags message-types callback-data user-data))

(defun debug-utils-messenger (flags message-types callback-data user-data)
  (format t "flags: ~a; message-types: ~a; callback-data: ~a; user-data: ~a~%"
          flags message-types callback-data user-data)
  nil)

(defun create-debug-utils-messenger (&optional (app-name "create-debug-utils-messenger") (engine-name "vk"))
  (assert (find-if (lambda (p)
                     (string= (vk:extension-name p) vk:+ext-debug-utils-extension-name+))
                   (vk:enumerate-instance-extension-properties))
          ()
          "Could not find the ~a extension." vk:+ext-debug-utils-extension-name+)
  (with-instance (instance app-name engine-name nil (list vk:+ext-debug-utils-extension-name+
                                                          vk:+ext-debug-report-extension-name+))
    
    ;; todo: this should use the extension loader mechanism instead
    ;; todo: document that order of parameters is not necessarily the same as in C API because some of them might be optional (e.g. instance in get-isntance-proc-addr)
    (let ((pfn-create-debug-utils-messenger (vk:get-instance-proc-addr "vkCreateDebugUtilsMessengerEXT" instance))
          (pfn-destroy-debug-utils-messenger (vk:get-instance-proc-addr "vkDestroyDebugUtilsMessengerEXT" instance)))
      (assert pfn-create-debug-utils-messenger
              ()
              "Could not get function pointer for vkCreateDebugUtilsMessengerEXT!")
      (assert pfn-destroy-debug-utils-messenger
              ()
              "Could not get function pointer for vkDestroyDebugUtilsMessengerEXT"))
    
    (setf vk:*default-extension-loader* (vk:make-extension-loader :instance instance))
    (let* ((create-info (make-instance 'vk:debug-utils-messenger-create-info-ext
                                       :message-type '(:general :performance :validation)
                                       :message-severity '(:warning :error)
                                       :pfn-user-callback (cffi:get-callback 'debug-messenger-callback)
                                       :user-data (cffi:null-pointer)))
           (messenger (vk:create-debug-utils-messenger-ext instance create-info)))
      (vk:destroy-debug-utils-messenger-ext instance messenger))
    (format t "created instance~%")))
