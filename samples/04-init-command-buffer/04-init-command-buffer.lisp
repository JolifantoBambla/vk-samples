;;;; 04-init-command-buffer

(in-package #:vk-samples/04-init-command-buffer)

(defun 04-init-command-buffer (&optional (app-name "04-init-command-buffer"))
  ;; with-debug-instance-and-device is located in utils - check 03-init-device to see how to create and destroy a device
  (with-debug-instance-and-device (instance device physical-device :app-name app-name :log-levels (:info :warning :error))
    (let* ((create-info (make-instance 'vk:command-pool-create-info
                                       ;; the device has created a queue from the same queue family (check out 03-init-device) 
                                       :queue-family-index (find-graphics-queue-family-index physical-device)))
           (command-pool (vk:create-command-pool device create-info)))
      (unwind-protect
           (let* ((allocate-info (make-instance 'vk:command-buffer-allocate-info
                                                :command-pool command-pool
                                                :level :primary
                                                :command-buffer-count 1))
                  ;; allocate the command buffer
                  (command-buffer (first (vk:allocate-command-buffers device allocate-info))))
             ;; free the command buffer - this is optional, since destroying the command pool will free it implicitly
             (vk:free-command-buffers device command-pool (list command-buffer)))
        ;; destroying the command pool implicitly frees all command buffers allocated from the pool
        (vk:destroy-command-pool device command-pool)))))
