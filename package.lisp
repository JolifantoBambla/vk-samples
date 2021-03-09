;;;; package.lisp

(defpackage #:vk-samples/utils
  (:use #:cl)
  (:export
   #:*api-version*
   #:with-instance
   #:with-device
   #:with-instance-and-device))

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

(defpackage #:vk-samples/create-debug-utils-messenger
  (:documentation "Shows how to create and destroy a debug callback.")
   (:use #:cl
        #:vk-samples/utils)
  (:export
   #:create-debug-utils-messenger))

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
  (:import-from #:vk-samples/create-debug-utils-messenger
                #:create-debug-utils-messenger)
  (:export
   #:01-init-instance
   #:02-enumerate-devices
   #:03-init-device
   #:04-init-command-buffer
   #:create-debug-utils-messenger))
