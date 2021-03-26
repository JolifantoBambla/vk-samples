;;;; 07-init-uniform-buffer.lisp

(in-package #:vk-samples/07-init-uniform-buffer)

(defun 07-init-uniform-buffer (&optional (app-name "07-init-uniform-buffer"))
  ;; with-instance-and-device is located in utils - check 03-init-device to see how to create and destroy a device
  (with-instance-and-device (instance device physical-device :app-name app-name :window-extensions nil)
    (let* ((model (m4:identity))
           (view (m4:look-at (v3:make  0.0 -1.0   0.0) ;; up
                             (v3:make -5.0  3.0 -10.0) ;; from / eye / camera
                             (v3:make  0.0  0.0   0.0))) ;; to / center of projection
           (projection (rtg-math.projection:perspective-radian-fov
                        64.0   ;; width (usually the window width)
                        64.0   ;; height (usually the window height)
                        0.1    ;; near
                        100.0  ;; far
                        (rtg-math:radians 45.0))) ;; field of view
           ;; if you're coming from OpenGL this might be new for you: Vulkan's clip space has an inverted y axis and
           ;; the z range is [0,1] instead of [-1,1]
           (clip (m4:make 1.0  0.0 0.0 0.0
                          0.0 -1.0 0.0 0.0
                          0.0  0.0 0.5 0.0
                          0.0  0.0 0.5 1.0))
           ;; that's the data we actually want to write to the buffer
           (mvpc (m4:* clip projection view model))
           (size-of-mvpc (* (cffi:foreign-type-size :float) (length mvpc)))
           (uniform-data-buffer (vk:create-buffer device
                                                  (make-instance 'vk:buffer-create-info
                                                                 :usage :uniform-buffer
                                                                 :sharing-mode :exclusive
                                                                 :size size-of-mvpc))))
      (unwind-protect
           ;; First we need to find out how we need to allocated the memory on our device
           ;; You might want to read up on all that's going on here, I'll just scratch the surface:
           ;; Each device offers one or more heaps and different types of memory that exist within
           ;; these heaps. Each memory type has different properties defining what you can actually
           ;; do with that memory. E.g. if a memory type has the :HOST-VISIBLE property, we can write
           ;; to that memory from the CPU.
           (let* ((memory-requirements (vk:get-buffer-memory-requirements device uniform-data-buffer))
                  (memory-properties (vk:get-physical-device-memory-properties physical-device))
                  (type-index (loop with type-bits = (vk:memory-type-bits memory-requirements)
                                    with requirements-mask = (cffi:foreign-bitfield-value '%vk:memory-property-flags
                                                                                          '(:host-visible :host-coherent))
                                    for i from 0 below (vk:memory-type-count memory-properties)
                                    for property-flags = (cffi:foreign-bitfield-value '%vk:memory-property-flags
                                                                                      (vk:property-flags (nth i (vk:memory-types memory-properties))))
                                    if (and (logand type-bits 1)
                                            (= (logand property-flags requirements-mask)
                                               requirements-mask))
                                      return i
                                    else
                                    do (setf type-bits (ash type-bits -1))))
                  ;; with all the information we gathered about the device memory, we can now allocate the memory where
                  ;; we can write our data to
                  (uniform-data-memory (vk:allocate-memory device
                                                           (make-instance 'vk:memory-allocate-info
                                                                          :allocation-size (vk:size memory-requirements)
                                                                          :memory-type-index type-index))))
             (unwind-protect
                  (progn
                    (format t "This is the data that will be written to the device:~%~a~%"
                            (m:to-string mvpc))
                    ;; first we write the data to the buffer
                    (cffi:with-foreign-object (p-data :pointer)
                      ;; VK:MAP-MEMORY is a bit special in that it needs a pointer (p-data) that the driver uses as a output
                      ;; parameter. Usually VK tries to avoid output parameters, but in this case it wouldn't be possible to
                      ;; to do this without handing out a pointer that has to be explicitly freed by the caller.
                      (vk:map-memory device
                                     uniform-data-memory
                                     0 ;; offset
                                     (vk:size memory-requirements)
                                     p-data)
                      (unwind-protect
                           (cffi:with-foreign-object (mvpc-data :float (array-total-size mvpc))
                             (dotimes (i (array-total-size mvpc))
                               (setf (cffi:mem-aref mvpc-data :float i) (aref mvpc i)))
                             ;; NOTE: MEMCPY is not a function defined by VK. In the future it will be included in the
                             ;; VK-UTILS package which will be a part of the VK-system.
                             ;; Until that's the case, we'll use the function from VK-SAMPLES/UTILS.
                             (memcpy (cffi:mem-aref p-data :pointer) mvpc-data size-of-mvpc)
                             (vk:bind-buffer-memory device
                                                    uniform-data-buffer
                                                    uniform-data-memory
                                                    0)) ;; offset
                        (vk:unmap-memory device uniform-data-memory)))
                    ;; then we read it back, just to show that we can not only write but also read data from a buffer
                    (cffi:with-foreign-object (p-data :pointer)
                      (vk:map-memory device
                                     uniform-data-memory
                                     0 ;; offset
                                     (vk:size memory-requirements)
                                     p-data)
                      (unwind-protect
                           (cffi:with-foreign-object (mvpc-data :float  (array-total-size mvpc))
                             (memcpy mvpc-data (cffi:mem-aref p-data :pointer) size-of-mvpc)
                             (format t "This is the data that has been read from the device:~%~a~%"
                                     (m:to-string
                                      (rtg-math:m! (loop for i from 0 below (array-total-size mvpc)
                                                         collect (cffi:mem-aref mvpc-data :float i))))))
                        (vk:unmap-memory device uniform-data-memory))))
               (vk:free-memory device uniform-data-memory)))
        (vk:destroy-buffer device uniform-data-buffer)))))
