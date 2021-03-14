;;;; 03-init-device.lisp

(in-package #:vk-samples/03-init-device)

(defun 03-init-device (&optional (app-name "03-init-device"))
  ;; with-instance is located in utils - check 01-init-instance to see how to create and destroy an instance
  (with-instance (instance :app-name app-name :window-extensions nil :log-levels nil)
    ;; get a physical device - we just take the first one here, but you might want to choose one based on the features it supports or whatever
    (let* ((physical-device (first (vk:enumerate-physical-devices instance)))
           ;; query the queue family properties of the chosen physical device
           (queue-family-properties (vk:get-physical-device-queue-family-properties physical-device))
           ;; find a queue family that supports graphics
           (graphics-queue-family-index (position-if (lambda (q)
                                                       (member :graphics (vk:queue-flags q)))
                                                     queue-family-properties))
           ;; setup the device create info
           (queue-priority 0.0)
           (device-queue-create-info (make-instance 'vk:device-queue-create-info
                                                    :queue-family-index graphics-queue-family-index
                                                    :queue-priorities (list queue-priority)))
           (device-create-info (make-instance 'vk:device-create-info
                                              :queue-create-infos (list device-queue-create-info)))
           ;; create the device
           (device (vk:create-device physical-device device-create-info)))
      ;; destroy the device
      (vk:destroy-device device))))
