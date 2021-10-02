;;;; ray-tracing

(in-package #:vk-samples/ray-tracing)

(defun supports-extensions-p (physical-device extensions)
  (let ((extension-names (map 'list
                              #'vk:extension-name
                              (vk:enumerate-device-extension-properties physical-device))))
    (= (length extensions)
       (length (intersection extensions extension-names :test #'string=)))))

(defun ray-tracing (&optional (app-name "ray-tracing")
                              (window-width 1280)
                              (window-height 720))
  (glfw:with-init-window (:title app-name
                          :width window-width
                          :height window-height
                          :client-api :no-api)
    (with-instance (instance
                    :app-name app-name
                    :window-extensions t)
      (with-surface (surface
                     instance)
        (let* ((device-extensions (list vk:+khr-swapchain-extension-name+
                                        vk:+khr-ray-tracing-pipeline-extension-name+
                                        vk:+khr-acceleration-structure-extension-name+
                                        vk:+khr-deferred-host-operations-extension-name+
                                        vk:+khr-get-memory-requirements-2-extension-name+))
               (physical-device (or (find-if (lambda (physical-device)
                                               (supports-extensions-p physical-device
                                                                      device-extensions))
                                             (vk:enumerate-physical-devices instance))
                                    (error "Could not find a device supporting all required extensions.")))
               ;; todo: [ERROR] (VALIDATION): Validation Error: [ VUID-VkPhysicalDeviceFeatures2-sType-sType ] Object 0: VK_NULL_HANDLE, type = VK_OBJECT_TYPE_DEVICE; | MessageID = 0xea690c8d | vkGetPhysicalDeviceFeatures2: parameter pFeatures->sType must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2. The Vulkan spec states: sType must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 (https://vulkan.lunarg.com/doc/view/1.2.189.0/linux/1.2-extensions/vkspec.html#VUID-VkPhysicalDeviceFeatures2-sType-sType)
               ;; I think the problem is that a C struct is created, but not from a CL class, so the stype isn't set
               ;; another problem with this is that additional features can't be queried using the VkPhysicalDeviceFeatures2 instance's next slot -> needs a new implementation!
               (supported-features (vk:get-physical-device-features-2 physical-device)))
          (vk-utils:with-device (device
                                 physical-device
                                 (vk:make-device-create-info
                                  ;; add :next stuff
                                  :queue-create-infos (make-default-queue-create-infos physical-device surface)
                                  :enabled-extension-names device-extensions
                                  ;; enable features
                                  ))
            (let ((vk:*default-extension-loader* (vk:make-extension-loader :instance instance
                                                                           :device device)))
              supported-features)))))))
