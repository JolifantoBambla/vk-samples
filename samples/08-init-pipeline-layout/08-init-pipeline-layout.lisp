;;;; 08-init-pipeline-layout.lisp

(in-package #:vk-samples/08-init-pipeline-layout)

(defun 08-init-pipeline-layout (&optional (app-name "08-init-pipeline-layout"))
  ;; with-instance-and-device is located in utils - check 03-init-device to see how to create and destroy a device
  (with-instance-and-device (instance device physical-device :app-name app-name :window-extensions nil)
    ;; A pipeline layout consists of multiple descriptor set layouts which in turn contain information about what
    ;; data types the pipelines built from the pipeline layout will require. (E.g. uniform buffers, samplers, etc.)
    (let* ((descriptor-set-layout-binding (vk:make-descriptor-set-layout-binding
                                           :binding 0
                                           :descriptor-type :uniform-buffer
                                           :descriptor-count 1
                                           :stage-flags :vertex))
           (descriptor-set-layout-create-info (vk:make-descriptor-set-layout-create-info
                                               :bindings (list descriptor-set-layout-binding)))
           (descriptor-set-layout (vk:create-descriptor-set-layout device descriptor-set-layout-create-info)))
      ;; with our descriptor set layout we can now create a pipeline layout
      (unwind-protect
           (let* ((pipeline-layout-create-info (vk:make-pipeline-layout-create-info
                                                :set-layouts (list descriptor-set-layout)))
                  (pipeline-layout (vk:create-pipeline-layout device pipeline-layout-create-info)))
             ;; as always we have to clean up after ourselves
             (vk:destroy-pipeline-layout device pipeline-layout))
        (vk:destroy-descriptor-set-layout device descriptor-set-layout)))))
