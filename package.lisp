;;;; package.lisp

(defpackage #:vk-samples/utils
  (:use #:cl)
  (:export
   #:*api-version*
   #:*vk-validation-layer-name*
   #:*fence-timeout*
   #:with-allocated-memory
   #:with-mapped-memory
   #:copy-to-device
   #:make-default-application-info
   #:make-default-debug-utils-messenger-create-info
   #:make-default-render-pass-begin-info
   #:with-instance
   #:with-device
   #:with-instance-and-device
   #:with-surface
   #:with-swapchain
   #:with-depth-buffer
   #:with-render-pass
   #:with-shader-module
   #:with-compiled-shader-module
   #:with-frame-buffers
   #:with-command-pool
   #:with-command-buffer
   #:record-command-buffer
   #:with-semaphore
   #:with-fence
   #:with-gfx-base
   #:with-gfx
   #:with-uniform-buffer
   #:with-vertex-buffer
   #:with-simple-descriptor-set-layout
   #:with-simple-descriptor-pool
   #:with-simple-descriptor-set
   #:with-simple-pipeline-layout
   #:with-simple-graphics-pipeline
   #:find-type-index
   #:find-graphics-queue-family-index
   #:find-graphics-and-present-queue-family-indices
   #:determine-swapchain-extent
   #:pick-color-format
   #:define-debug-utils-messenger-callback
   #:default-debug-utils-log-callback))

(defpackage #:vk-samples/data
  (:use #:cl)
  (:export
   #:make-mvpc
   #:make-colored-cube-data
   #:make-simple-vertex-shader
   #:make-simple-fragment-shader))

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

(defpackage #:vk-samples/06-init-depth-buffer
  (:documentation "Shows how to create a depth buffer.")
   (:use #:cl
         #:vk-samples/utils)
  (:export
   #:06-init-depth-buffer))

(defpackage #:vk-samples/07-init-uniform-buffer
  (:documentation "Shows how to create a uniform buffer and how to write and read from it.")
   (:use #:cl
         #:vk-samples/utils)
  (:export
   #:07-init-uniform-buffer))

(defpackage #:vk-samples/08-init-pipeline-layout
  (:documentation "Shows how to create and destroy a Vulkan pipeline layout.")
   (:use #:cl
         #:vk-samples/utils)
  (:export
   #:08-init-pipeline-layout))

(defpackage #:vk-samples/09-init-descriptor-sets
  (:documentation "Shows how to allocate descriptor sets and how to write to them.")
   (:use #:cl
         #:vk-samples/utils
         #:vk-samples/data)
  (:export
   #:09-init-descriptor-sets))

(defpackage #:vk-samples/10-init-render-pass
  (:documentation "Shows how to create and destroy a Vulkan render pass.")
   (:use #:cl
         #:vk-samples/utils)
  (:export
   #:10-init-render-pass))

(defpackage #:vk-samples/11-init-shaders
  (:documentation "Shows how to create and destroy a Vulkan shader modules from SPIR-V binaries.")
   (:use #:cl
         #:vk-samples/utils)
  (:export
   #:11-init-shaders))

(defpackage #:vk-samples/12-init-frame-buffers
  (:documentation "Shows how to create and destroy Vulkan frame buffers.")
   (:use #:cl
         #:vk-samples/utils)
  (:export
   #:12-init-frame-buffers))

(defpackage #:vk-samples/13-init-vertex-buffer
  (:documentation "Shows how to create and destroy a vertex buffer.")
   (:use #:cl
         #:vk-samples/utils
         #:vk-samples/data)
  (:export
   #:13-init-vertex-buffer))

(defpackage #:vk-samples/14-init-pipeline
  (:documentation "Shows how to create and destroy a graphics pipeline.")
   (:use #:cl
         #:vk-samples/utils
         #:vk-samples/data)
  (:export
   #:14-init-pipeline))

(defpackage #:vk-samples/15-draw-cube
  (:documentation "Shows how to draw a coloured cube using the Vulkan API.")
   (:use #:cl
         #:vk-samples/utils
         #:vk-samples/data)
  (:export
   #:15-draw-cube))

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

(defpackage #:vk-samples/compile-shaders-from-repl
  (:documentation "The same as 15-draw-cube, but the shaders are compiled from the REPL via the SHADERC package.")
   (:use #:cl
         #:vk-samples/utils
         #:vk-samples/data)
  (:export
   #:compile-shaders-from-repl))

(defpackage #:vk-samples/use-vari-shaders
  (:documentation "The same as compile-shaders-from-repl, but the shaders are written in Vari and transpiled to GLSL by Varjo.")
   (:use #:cl
         #:vk-samples/utils
         #:vk-samples/data)
  (:export
   #:use-vari-shaders))

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
  (:import-from #:vk-samples/06-init-depth-buffer
                #:06-init-depth-buffer)
  (:import-from #:vk-samples/07-init-uniform-buffer
                #:07-init-uniform-buffer)
  (:import-from #:vk-samples/08-init-pipeline-layout
                #:08-init-pipeline-layout)
  (:import-from #:vk-samples/09-init-descriptor-sets
                #:09-init-descriptor-sets)
  (:import-from #:vk-samples/10-init-render-pass
                #:10-init-render-pass)
  (:import-from #:vk-samples/11-init-shaders
                #:11-init-shaders)
  (:import-from #:vk-samples/12-init-frame-buffers
                #:12-init-frame-buffers)
  (:import-from #:vk-samples/13-init-vertex-buffer
                #:13-init-vertex-buffer)
  (:import-from #:vk-samples/14-init-pipeline
                #:14-init-pipeline)
  (:import-from #:vk-samples/15-draw-cube
                #:15-draw-cube)
  (:import-from #:vk-samples/create-debug-utils-messenger
                #:create-debug-utils-messenger)
  (:import-from #:vk-samples/create-debug-utils-messenger-next
                #:create-debug-utils-messenger-next)
  (:import-from #:vk-samples/compile-shaders-from-repl
                #:compile-shaders-from-repl)
  (:import-from #:vk-samples/use-vari-shaders
                #:use-vari-shaders)
  (:export
   #:01-init-instance
   #:02-enumerate-devices
   #:03-init-device
   #:04-init-command-buffer
   #:05-init-swapchain
   #:06-init-depth-buffer
   #:07-init-uniform-buffer
   #:08-init-pipeline-layout
   #:09-init-descriptor-sets
   #:10-init-render-pass
   #:11-init-shaders
   #:12-init-frame-buffers
   #:13-init-vertex-buffer
   #:14-init-pipeline
   #:15-draw-cube
   #:create-debug-utils-messenger
   #:create-debug-utils-messenger-next
   #:compile-shaders-from-repl
   #:use-vari-shaders
   #:run-all-samples))
