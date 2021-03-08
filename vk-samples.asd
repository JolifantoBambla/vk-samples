;;;; vk-samples.asd

(asdf:defsystem #:vk-samples
  :description "Describe vk-samples here"
  :author "Lukas Herzberger <herzberger.lukas at gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:vk)
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
                              :components ((:file "04-init-command-buffer")))))
               (:file "vk-samples")))
