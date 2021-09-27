;;;; ray-tracing

(in-package #:vk-samples/ray-tracing)

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
        (format t "found ~a devices which support ray tracing"
                (length
                 (loop for device in (vk:enumerate-physical-devices instance)
                       if (some (lambda (ext)
                                  (search "ray_tracing" (vk:extension-name ext)))
                                (vk:enumerate-device-extension-properties device))
                       collect device)))))))
