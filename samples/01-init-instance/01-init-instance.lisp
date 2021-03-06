;;;; 01-init-instance.lisp

(in-package #:vk-samples/samples)

(defparameter *api-version* (vk:pack-version-number 1 2 153))

(defun 01-init-instance (&optional (app-name "01-init-instance") (engine-name "vk"))
  (let* ((application-info (make-instance 'vk:application-info
                                          :application-name app-name
                                          :application-version 1
                                          :engine-name engine-name
                                          :engine-version 1
                                          :api-version *api-version*))
         (create-info (make-instance 'vk:instance-create-info
                                     :application-info application-info))
         ;; create the instance
         (instance (vk:create-instance create-info)))
    ;;destroy the instance again
    (vk:destroy-instance instance)))
