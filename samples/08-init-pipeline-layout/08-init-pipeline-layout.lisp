;;;; 08-init-pipeline-layout.lisp

(in-package #:vk-samples/08-init-pipeline-layout)

(defun 07-init-uniform-buffer (&optional (app-name "07-init-uniform-buffer"))
  ;; with-instance-and-device is located in utils - check 03-init-device to see how to create and destroy a device
  (with-instance-and-device (instance device physical-device :app-name app-name :window-extensions nil)
    (let* ((descriptor-set-layout-binding (make-instance 'vk:descriptor-set-layout-binding
                                                         :binding 0
                                                         :descriptor-type :uniform-buffer
                                                         :descriptor-count 1 ;; todo: bug in VK - descriptor-count can only be set from IMMUTABLE-SAMPLERS when DESCRIPTOR-TYPE is :SAMPLER or :COMBINED-IMAGE-SAMPLER, otherwise it needs to be set explicitly!
                                                         :stage-flags :vertex))
           (descriptor-set-layout-create-info (make-instance 'vk:descriptor-set-layout-create-info
                                                             :bindings (list descriptor-set-layout-binding)))
           (descriptor-set-layout (vk:create-descriptor-set-layout device descriptor-set-layout-create-info)))
      (unwind-protect
           (let* ((pipeline-layout-create-info (make-instance 'vk:pipeline-layout-create-info
                                                              :set-layouts (list descriptor-set-layout)))
                  (pipeline-layout (vk:create-pipeline-layout device pipeline-layout-create-info)))
             (vk:destroy-pipeline-layout device pipeline-layout))
        (vk:destroy-descriptor-set-layout device descriptor-set-layout)))))
