;;;; package.lisp

(defpackage #:vk-samples/utils
  (:use #:cl)
  (:export
   #:*api-version*
   #:*vk-validation-layer-name*
   #:glfw-get-required-instance-extensions
   #:glfw-create-window-surface
   #:make-default-application-info
   #:make-default-debug-utils-messenger-create-info
   #:with-instance
   #:with-device
   #:with-instance-and-device
   #:find-graphics-queue-family-index
   #:define-debug-utils-messenger-callback
   #:default-debug-utils-log-callback))

(defpackage #:vk-samples/01-init-instance
  (:documentation "Shows how to create and destroy a Vulkan instance.")
  (:use #:cl
        #:vk-samples/utils)
  (:export
   #:01-init-instance))

(defpackage #:vk-samples/02-enumerate-devices
  (:documentation "Shows how to enumerate physical devices.")
  (:use #:cl
        #:vk-samples/utils)
  (:export
   #:02-enumerate-devices))

(defpackage #:vk-samples/03-init-device
  (:documentation "Shows how to create and destroy a Vulkan device.")
  (:use #:cl
        #:vk-samples/utils)
  (:export
   #:03-init-device))

(defpackage #:vk-samples/04-init-command-buffer
  (:documentation "Shows how to create a Vulkan command buffer.")
   (:use #:cl
         #:vk-samples/utils)
  (:export
   #:04-init-command-buffer))

(defpackage #:vk-samples/05-init-swapchain
  (:documentation "Shows how to initialize Vulkan swapchain.")
   (:use #:cl
         #:vk-samples/utils)
  (:export
   #:05-init-swapchain))

(defpackage #:vk-samples/07-init-uniform-buffer
  (:documentation "Shows how to create a uniform buffer.")
   (:use #:cl
         #:vk-samples/utils)
  (:export
   #:07-init-uniform-buffer))

(defpackage #:vk-samples/create-debug-utils-messenger
  (:documentation "Shows how to create and destroy a debug callback.")
   (:use #:cl
         #:vk-samples/utils)
  (:export
   #:create-debug-utils-messenger))

(defpackage #:vk-samples/create-debug-utils-messenger-next
  (:documentation "Shows how to create and destroy a debug callback via the VkInstanceCreateInfo's pNext member.")
   (:use #:cl
         #:vk-samples/utils)
  (:export
   #:create-debug-utils-messenger-next))

(defpackage #:vk-samples
  (:documentation "Usage samples for the Vulkan bindings provided by VK.")
  (:use #:cl)
  (:import-from #:vk-samples/01-init-instance
                #:01-init-instance)
  (:import-from #:vk-samples/02-enumerate-devices
                #:02-enumerate-devices)
  (:import-from #:vk-samples/03-init-device
                #:03-init-device)
  (:import-from #:vk-samples/04-init-command-buffer
                #:04-init-command-buffer)
  (:import-from #:vk-samples/05-init-swapchain
                #:05-init-swapchain)
  (:import-from #:vk-samples/07-init-uniform-buffer
                #:07-init-uniform-buffer)
  (:import-from #:vk-samples/create-debug-utils-messenger
                #:create-debug-utils-messenger)
  (:import-from #:vk-samples/create-debug-utils-messenger-next
                #:create-debug-utils-messenger-next)
  (:export
   #:01-init-instance
   #:02-enumerate-devices
   #:03-init-device
   #:04-init-command-buffer
   #:05-init-swapchain
   #:07-init-uniform-buffer
   #:create-debug-utils-messenger
   #:create-debug-utils-messenger-next
   #:run-all-samples))
