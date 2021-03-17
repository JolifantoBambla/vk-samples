;;;; vk-samples.asd

(asdf:defsystem #:vk-samples
  :description "Describe vk-samples here"
  :author "Lukas Herzberger <herzberger.lukas at gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:vk #:cffi #:cl-glfw3 #:rtg-math #:trivial-main-thread)
  :components ((:file "package")
               (:module "samples"
                :components ((:file "utils")
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
                             (:module "07-init-uniform-buffer"
                              :components ((:file "07-init-uniform-buffer")))
                             (:module "08-init-pipeline-layout"
                              :components ((:file "08-init-pipeline-layout")))
                             (:module "create-debug-utils-messenger"
                              :components ((:file "create-debug-utils-messenger")))
                             (:module "create-debug-utils-messenger-next"
                              :components ((:file "create-debug-utils-messenger-next")))))
               (:file "vk-samples")))
