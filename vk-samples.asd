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
                :components ((:module "01-init-instance"
                              :components ((:file "01-init-instance")))))
               (:file "vk-samples")))
