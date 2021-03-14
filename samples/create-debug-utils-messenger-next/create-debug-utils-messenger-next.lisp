;;;; create-debug-utils-messenger-next.lisp

(in-package #:vk-samples/create-debug-utils-messenger-next)

(defun create-debug-utils-messenger-next (&optional (app-name "create-debug-utils-messenger-next"))
  (assert (find-if (lambda (p)
                     (string= (vk:layer-name p) *vk-validation-layer-name*))
                   (vk:enumerate-instance-layer-properties))
          ()
          "Could not find the ~a layer." *vk-validation-layer-name*)
  (assert (find-if (lambda (p)
                     (string= (vk:extension-name p) vk:+ext-debug-utils-extension-name+))
                   (vk:enumerate-instance-extension-properties))
          ()
          "Could not find the ~a extension." vk:+ext-debug-utils-extension-name+)
  ;; when we extend the vk:instance-create-info with a vk:debug-utils-messenger-create-info-ext the debug utils messenger is created & destroyed
  ;; alongside the instance implicitly
  ;; this way we also get debug information for the instance creation and destruction
  (let ((instance (vk:create-instance (make-instance 'vk:instance-create-info
                                                     :next (make-instance 'vk:debug-utils-messenger-create-info-ext
                                                                          :message-type '(:validation)
                                                                          :message-severity '(:info :warning :error)
                                                                          :pfn-user-callback (cffi:get-callback 'default-debug-utils-log-callback)
                                                                          :user-data (cffi:null-pointer))
                                                     :application-info (make-default-application-info app-name)
                                                     ;; we enable the validation layer to get validation messages
                                                     :enabled-layer-names (list *vk-validation-layer-name*)
                                                     :enabled-extension-names (list vk:+ext-debug-utils-extension-name+)))))
    (vk:destroy-instance instance)))
