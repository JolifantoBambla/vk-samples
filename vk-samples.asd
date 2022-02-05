;;;; vk-samples.asd

(asdf:defsystem #:vk-samples
  :description "Describe vk-samples here"
  :author "Lukas Herzberger <herzberger.lukas at gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:vk
               #:alexandria
               #:cffi
               #:cl-glfw3
               #:rtg-math
               #:trivial-main-thread
               #:shaderc
               #:varjo)
  :components ((:file "package")
               (:module "samples"
                :components ((:file "utils")
                             (:file "data")
                             (:module "01-init-instance"
                              :components ((:file "01-init-instance")))
                             (:module "02-enumerate-devices"
                              :components ((:file "02-enumerate-devices")))
                             (:module "03-init-device"
                              :components ((:file "03-init-device")))
                             (:module "04-init-command-buffer"
                              :components ((:file "04-init-command-buffer")))
                             (:module "05-init-swapchain"
                              :components ((:file "05-init-swapchain")))
                             (:module "06-init-depth-buffer"
                              :components ((:file "06-init-depth-buffer")))
                             (:module "07-init-uniform-buffer"
                              :components ((:file "07-init-uniform-buffer")))
                             (:module "08-init-pipeline-layout"
                              :components ((:file "08-init-pipeline-layout")))
                             (:module "09-init-descriptor-sets"
                              :components ((:file "09-init-descriptor-sets")))
                             (:module "10-init-render-pass"
                              :components ((:file "10-init-render-pass")))
                             (:module "11-init-shaders"
                              :components ((:file "11-init-shaders")))
                             (:module "12-init-frame-buffers"
                              :components ((:file "12-init-frame-buffers")))
                             (:module "13-init-vertex-buffer"
                              :components ((:file "13-init-vertex-buffer")))
                             (:module "14-init-pipeline"
                              :components ((:file "14-init-pipeline")))
                             (:module "15-draw-cube"
                              :components ((:file "15-draw-cube")))
                             (:module "create-debug-utils-messenger"
                              :components ((:file "create-debug-utils-messenger")))
                             (:module "create-debug-utils-messenger-next"
                              :components ((:file "create-debug-utils-messenger-next")))
                             (:module "compile-shaders-from-repl"
                              :components ((:file "compile-shaders-from-repl")))
                             (:module "use-vari-shaders"
                              :components ((:file "use-vari-shaders")))
                             (:module "ray-tracing"
                              :components ((:file "ray-tracing")))))
               (:file "vk-samples")))
