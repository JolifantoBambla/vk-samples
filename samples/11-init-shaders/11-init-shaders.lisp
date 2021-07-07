;;;; 11-init-shaders.lisp

(in-package #:vk-samples/11-init-shaders)

(defun 11-init-shaders (&optional (app-name "11-init-shaders"))
  ;; with-instance-and-device is located in utils - check 03-init-device to see how to create and destroy a device
  (with-instance-and-device (instance device physical-device :app-name app-name :window-extensions nil)
    (flet ((read-shader-file (file-name)
             (let ((file-path (merge-pathnames
                               file-name
                               (asdf:system-relative-pathname
                                'vk-samples
                                (make-pathname :directory '(:relative "samples" "11-init-shaders"))))))
               (vk-utils:read-shader-source file-path))))
      (let* ((vertex-shader-module-create-info
               (make-instance 'vk:shader-module-create-info
                              :code (read-shader-file "vertex-shader.spv")))
             (fragment-shader-module-create-info
               (make-instance 'vk:shader-module-create-info
                              :code (read-shader-file "fragment-shader.spv")))
             (vertex-shader-module (vk:create-shader-module device
                                                            vertex-shader-module-create-info)))
        (unwind-protect
             (let ((fragment-shader-module (vk:create-shader-module device
                                                                    fragment-shader-module-create-info)))
               (vk:destroy-shader-module device fragment-shader-module))
          (vk:destroy-shader-module device vertex-shader-module))))))
