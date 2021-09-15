;;;; 06-init-depth-buffer.lisp

(in-package #:vk-samples/06-init-depth-buffer)

(defun 06-init-depth-buffer (&optional (app-name "05-init-swapchain") (window-width 500) (window-height 500))
  ;; WITH-GFX-BASE in located in utils - check out the previous examples to see how to create an instance, a device and a surface
  (with-gfx-base (instance device physical-device surface :app-name app-name :window-width window-width :window-height window-height)
    (let* ((depth-format :d16-unorm)
           (format-properties (vk:get-physical-device-format-properties physical-device depth-format))
           (tiling (cond
                     ((member :depth-stencil-attachment (vk:linear-tiling-features format-properties))
                      :linear)
                     ((member :depth-stencil-attachment (vk:optimal-tiling-features format-properties))
                      :optimal)
                     (t (error "The physical device does not support DEPTH-STENCIL-ATTACHMENT for D16-UNORM"))))
           (image-create-info (vk:make-image-create-info
                               :image-type :2d
                               :format depth-format
                               :extent (vk:make-extent-3d
                                        :width window-width
                                        :height window-height
                                        :depth 1)
                               :mip-levels 1
                               :array-layers 1
                               :samples :1
                               :tiling tiling
                               :sharing-mode :exclusive
                               :usage :depth-stencil-attachment
                               :initial-layout :undefined))
           (depth-image (vk:create-image device image-create-info)))
      (unwind-protect
           (let* ((memory-properties (vk:get-physical-device-memory-properties physical-device))
                  (memory-requirements (vk:get-image-memory-requirements device depth-image))
                  (type-index (loop with type-bits = (vk:memory-type-bits memory-requirements)
                                    for i from 0 below (vk:memory-type-count memory-properties)
                                    if (and (logand type-bits 1)
                                            (member :device-local
                                                    (vk:property-flags (nth i (vk:memory-types memory-properties)))))
                                    return i
                                    else
                                    do (setf type-bits (ash type-bits -1))))
                  (depth-memory (vk:allocate-memory device
                                                    (vk:make-memory-allocate-info
                                                     :allocation-size (vk:size memory-requirements)
                                                     :memory-type-index type-index))))
             (unwind-protect
                  (progn
                    (vk:bind-image-memory device
                                          depth-image
                                          depth-memory
                                          0) ;; offset
                    (let* ((component-mapping (vk:make-component-mapping
                                               :r :r
                                               :g :g
                                               :b :b
                                               :a :a))
                           (subresource-range (vk:make-image-subresource-range
                                               :aspect-mask :depth
                                               :base-mip-level 0
                                               :level-count 1
                                               :base-array-layer 0
                                               :layer-count 1))
                           (image-view-create-info (vk:make-image-view-create-info
                                                    :image depth-image
                                                    :view-type :2d
                                                    :format depth-format
                                                    :components component-mapping
                                                    :subresource-range subresource-range))
                           (depth-view (vk:create-image-view device image-view-create-info)))
                      (vk:destroy-image-view device depth-view)))
               (vk:free-memory device depth-memory)))
        (vk:destroy-image device depth-image)))))
