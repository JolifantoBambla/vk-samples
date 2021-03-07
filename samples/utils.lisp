;;;; utils.lisp

(in-package #:vk-samples/samples)

(defparameter *api-version* (vk:make-api-version 1 2 153))

(defun create-instance (&optional (app-name "sample-app") (engine-name "vk"))
  (vk:create-instance (make-instance 'vk:instance-create-info
                                     :application-info (make-instance 'vk:application-info
                                                                      :application-name app-name
                                                                      :application-version 1
                                                                      :engine-name engine-name
                                                                      :engine-version 1
                                                                      :api-version *api-version*))))
