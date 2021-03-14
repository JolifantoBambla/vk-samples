;;;; 05-init-swapchain.lisp

(in-package #:vk-samples/05-init-swapchain)

(defun 05-init-swapchain (&optional (app-name "05-init-swapchain"))
  ;; graphics calls on OS X must occur in the main thread
  ;; so this should be wrapped within this: (trivial-main-thread:with-body-in-main-thread ()
  ;; but I'm not sure if I'm using it correctly since it only returns scheduled tasks and never(?) runs them
  ;; how do I get logs from the main thread?
  
  ;; to create a swapchain we first need a window. we'll use glfw3 for the window creation
  (glfw:with-init-window (:title app-name :width 64 :height 64 :client-api :no-api)
    ;;(format t "required extensions:~{~%  ~a~}~%" (glfw-get-required-instance-extensions))
    ;; Vulkan is a platform agnostic API, which means that it can not interface directly with the window system on its own.
    ;; We need to enable the WSI (Window System Integration) extensions to establish a connection between our instance and
    ;; the window system.
    ;; The required WSI extensions are different for each platform. Luckily glfw has us covered and we can query the names
    ;; of all required extensions.
    (format t "required extensions:~{~%  ~a~}~%" (glfw:get-required-instance-extensions))
    (let* ((required-extensions (glfw:get-required-instance-extensions))
           ;; we also enable the debug utils extension to get debug output - this is completely optional
           ;; check out the samples create-debug-utils-messenger(-next) to see how this works
           (extension-names (push vk:+ext-debug-utils-extension-name+ required-extensions))
           (instance (vk:create-instance (make-instance 'vk:instance-create-info
                                                       ;; we create a debug-utils-messenger together with the instance (optional)
                                                       :next (make-default-debug-utils-messenger-create-info)
                                                       :application-info (make-default-application-info app-name)
                                                       ;; we enable the validation layer to get validation messages (optional)
                                                       :enabled-layer-names (list *vk-validation-layer-name*)
                                                       :enabled-extension-names extension-names))))
      (unwind-protect
           ;; first we'll create a surface - the Vulkan API does not provide a function for this though
           ;; so we have to create this via glfw
           (let ((surface (glfw:create-window-surface instance glfw:*window* vk:*default-allocator*)))
             (unwind-protect
                  (progn
                    (format t "some instance proc address: ~a~%" (glfw:get-instance-proc-address instance "vkDestroySurfaceKHR"))
                    (loop for p in (vk:enumerate-physical-devices instance)
                          do (loop for q in (vk:get-physical-device-queue-family-properties p)
                                   for i from 0
                                   do (format t "physical device presentation support: ~a (queue family: ~a)~%"
                                              (glfw:physical-device-presentation-support-p instance p i) i))))
               ;; TODO: document that vk*KHR stuff is loaded by default, so we don't need to initialize an extension loader
               ;; even though we didn't create the surface using the Vulkan API, we must destroy it via VK:DESTROY-SURFACE-KHR
               (vk:destroy-surface-khr instance surface)))
        ;; finally destroy the instance
        (vk:destroy-instance instance)))))
