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

(defun make-colored-cube-data ()
  (let ((cube-data
          #(;; red face
      -1.0 -1.0  1.0 1.0    1.0 0.0 0.0 1.0
      -1.0  1.0  1.0 1.0    1.0 0.0 0.0 1.0
      1.0 -1.0  1.0 1.0    1.0 0.0 0.0 1.0
      1.0 -1.0  1.0 1.0    1.0 0.0 0.0 1.0
      -1.0  1.0  1.0 1.0    1.0 0.0 0.0 1.0
      1.0  1.0  1.0 1.0    1.0 0.0 0.0 1.0
      ;; green face
      -1.0 -1.0 -1.0 1.0    0.0 1.0 0.0 1.0
      1.0 -1.0 -1.0 1.0    0.0 1.0 0.0 1.0
      -1.0  1.0 -1.0 1.0    0.0 1.0 0.0 1.0
      -1.0  1.0 -1.0 1.0    0.0 1.0 0.0 1.0
      1.0 -1.0 -1.0 1.0    0.0 1.0 0.0 1.0
      1.0  1.0 -1.0 1.0    0.0 1.0 0.0 1.0
      ;; blue face
      -1.0  1.0  1.0 1.0    0.0 0.0 1.0 1.0
      -1.0 -1.0  1.0 1.0    0.0 0.0 1.0 1.0
      -1.0  1.0 -1.0 1.0    0.0 0.0 1.0 1.0
      -1.0  1.0 -1.0 1.0    0.0 0.0 1.0 1.0
      -1.0 -1.0  1.0 1.0    0.0 0.0 1.0 1.0
      -1.0 -1.0 -1.0 1.0    0.0 0.0 1.0 1.0
      ;; yellow face
      1.0  1.0  1.0 1.0    1.0 1.0 0.0 1.0
      1.0  1.0 -1.0 1.0    1.0 1.0 0.0 1.0
      1.0 -1.0  1.0 1.0    1.0 1.0 0.0 1.0
      1.0 -1.0  1.0 1.0    1.0 1.0 0.0 1.0
      1.0  1.0 -1.0 1.0    1.0 1.0 0.0 1.0
      1.0 -1.0 -1.0 1.0    1.0 1.0 0.0 1.0
      ;; magenta face
      1.0  1.0  1.0 1.0    1.0 0.0 1.0 1.0
      -1.0  1.0  1.0 1.0    1.0 0.0 1.0 1.0
      1.0  1.0 -1.0 1.0    1.0 0.0 1.0 1.0
      1.0  1.0 -1.0 1.0    1.0 0.0 1.0 1.0
      -1.0  1.0  1.0 1.0    1.0 0.0 1.0 1.0
      -1.0  1.0 -1.0 1.0    1.0 0.0 1.0 1.0
      ;; cyan face
      1.0 -1.0  1.0 1.0    0.0 1.0 1.0 1.0
      1.0 -1.0 -1.0 1.0    0.0 1.0 1.0 1.0
      -1.0 -1.0  1.0 1.0    0.0 1.0 1.0 1.0
      -1.0 -1.0  1.0 1.0    0.0 1.0 1.0 1.0
      1.0 -1.0 -1.0 1.0    0.0 1.0 1.0 1.0
            -1.0 -1.0 -1.0 1.0    0.0 1.0 1.0 1.0)))
    (values
     cube-data
     (* (cffi:foreign-type-size :float)
        (length cube-data)))))

(defun make-simple-vertex-shader ()
  "
#version 400

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout (std140, binding = 0) uniform buffer {
       mat4 mvpc;
} uniformBuffer;

layout (location = 0) in vec4 pos;
layout (location = 1) in vec4 inColor;

layout (location = 0) out vec4 outColor;

void main() {
   outColor = inColor;
   gl_Position = uniformBuffer.mvpc * pos;
}
")

(defun make-simple-fragment-shader ()
  "
#version 400

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout (location = 0) in vec4 color;

layout (location = 0) out vec4 outColor;

void main() {
  outColor = color;
}
")
