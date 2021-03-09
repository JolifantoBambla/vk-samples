;;;; utils.lisp

(in-package #:vk-samples/utils)

(defparameter *api-version* (vk:make-api-version 1 2 153))

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
