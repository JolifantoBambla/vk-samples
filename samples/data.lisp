;;;; data.lisp

(in-package #:vk-samples/data)

(defun make-mvpc (&key
                    (model (m4:identity))
                    (cam-pos (rtg-math:v! -5.0 3.0 -10.0))
                    (center-of-projection (rtg-math:v! 0.0 0.0 0.0))
                    (up (rtg-math:v! 0.0 -1.0 0.0))
                    (width 500.0)
                    (height 500.0)
                    (near 0.1)
                    (far 100.0)
                    (fov-deg 45.0))
  (let* ((view (m4:look-at up
                           cam-pos
                           center-of-projection))
         (projection (rtg-math.projection:perspective-radian-fov width
                                                                 height
                                                                 near
                                                                 far
                                                                 (rtg-math:radians fov-deg)))
         (clip (rtg-math:m! 1.0  0.0 0.0 0.0
                            0.0 -1.0 0.0 0.0
                            0.0  0.0 0.5 0.0
                            0.0  0.0 0.5 1.0))
         (mvpc (m4:* clip projection view model))
         (size-of-mvpc (* (cffi:foreign-type-size :float) (length mvpc))))
    (values mvpc size-of-mvpc)))
