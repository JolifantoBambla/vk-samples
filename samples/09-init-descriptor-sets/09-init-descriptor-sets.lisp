;;;; 09-init-descriptor-sets.lisp

(in-package #:vk-samples/09-init-descriptor-sets)

(defun 09-init-descriptor-sets (&optional (app-name "09-init-descriptor-sets"))
  ;; with-instance-and-device is located in utils - check 03-init-device to see how to create and destroy a device
  (with-instance-and-device (instance device physical-device :app-name app-name :window-extensions nil)
    (multiple-value-bind (mvpc size-of-mvpc) (make-mvpc)
      (with-uniform-buffer (uniform-buffer
                            memory
                            memory-requirements
                            device
                            physical-device
                            size-of-mvpc
                            :float
                            :initial-contents mvpc)
        (with-simple-descriptor-set-layout (descriptor-set-layout device)
          (let* ((pool-size (make-instance 'vk:descriptor-pool-size
                                           :type :uniform-buffer
                                           :descriptor-count 1))
                 (descriptor-pool-create-info (make-instance 'vk:descriptor-pool-create-info
                                                             :flags :free-descriptor-set
                                                             :max-sets 1
                                                             :pool-sizes (list pool-size)))
                 (descriptor-pool (vk:create-descriptor-pool device
                                                             descriptor-pool-create-info)))
            (unwind-protect
                 (let* ((descriptor-set-allocate-info (make-instance 'vk:descriptor-set-allocate-info
                                                                     :descriptor-pool descriptor-pool
                                                                     :set-layouts (list descriptor-set-layout)))
                        (descriptor-set (first (vk:allocate-descriptor-sets device
                                                                            descriptor-set-allocate-info))))
                   (unwind-protect
                        (let* ((descriptor-buffer-info (make-instance 'vk:descriptor-buffer-info
                                                                      :buffer uniform-buffer
                                                                      :offset 0
                                                                      :range size-of-mvpc))
                               (write-descriptor-set (make-instance 'vk:write-descriptor-set
                                                                    :dst-set descriptor-set
                                                                    :dst-binding 0
                                                                    :dst-array-element 0
                                                                    :descriptor-type :uniform-buffer
                                                                    :buffer-info (list descriptor-buffer-info))))
                          (vk:update-descriptor-sets device (list write-descriptor-set) nil))
                     (vk:free-descriptor-sets device descriptor-pool (list descriptor-set))))
              (vk:destroy-descriptor-pool device descriptor-pool))))))))
