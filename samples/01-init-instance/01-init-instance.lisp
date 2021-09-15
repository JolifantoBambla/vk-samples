;;;; 01-init-instance.lisp

(in-package #:vk-samples/01-init-instance)

(defun 01-init-instance (&optional (app-name "01-init-instance"))
  (let* ((application-info (vk:make-application-info
                            :application-name app-name
                            :application-version 1
                            :engine-name "vk"
                            :engine-version 1
                            :api-version *api-version*))
         (create-info (vk:make-instance-create-info
                       :application-info application-info))
         ;; create the instance
         (instance (vk:create-instance create-info)))
    ;;destroy the instance again
    (vk:destroy-instance instance)))
