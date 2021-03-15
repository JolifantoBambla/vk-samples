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
           (let* ((memory-requirements (vk:get-buffer-memory-requirements device uniform-data-buffer))
                  (memory-properties (vk:get-physical-device-memory-properties physical-device))
                  (type-index (loop with type-bits = (vk:memory-type-bits memory-requirements)
                                    with requirements-mask = (cffi:foreign-bitfield-value '%vk:memory-property-flags
                                                                                          '(:host-visible :host-coherent))
                                    for i from 0 below (vk:memory-type-count memory-properties)
                                    ;; todo (BUG): memory-type-counts is not a list -> the problem here is that this member has no "len" in the spec
                                    ;; it has only a max length [VK_MAX_MEMORY_TYPES]
                                    ;; I think it should be fine to just translate VK_MAX_MEMORY_TYPES of them, even if memory-type-count is much lower
                                    ;; from a memory point of view at least. obviously this is not ideal, so maybe there should be hard coded exceptions?
                                    ;; or: if a member is an array (it has a bit-count? - i think it's a bitcount) and it's not a string
                                    ;; then search for a member with a similar name (-s + Count, e.g.: memoryTypeCount & memoryTypes) and specify that as
                                    ;; if there is no such member then just translate the maximum amount (most of the time it's something like that: VK_UUID_SIZE)
                                    for property-flags = (cffi:foreign-bitfield-value '%vk:memory-property-flags
                                                                                      (vk:property-flags (nth i (vk:memory-types memory-properties))))
                                    if (and (logand type-bits 1)
                                            (= (logand property-flags requirements-mask)
                                               requirements-mask))
                                      return i
                                    else
                                      do (setf type-bits (ash type-bits -1))))))
        (vk:destroy-buffer device uniform-data-buffer)))))
