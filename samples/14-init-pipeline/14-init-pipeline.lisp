;;;; 14-init-pipeline.lisp

(in-package #:vk-samples/14-init-pipeline)

(defun 14-init-pipeline (&optional (app-name "14-init-pipeline") (window-width 500) (window-height 500))
  ;; WITH-GFX is a bit convoluted, but it contains everything from the previous samples
  (with-gfx (instance
             device
             physical-device
             surface
             swapchain
             swapchain-extent
             swapchain-images
             swapchain-image-views
             swapchain-image-format
             depth-image
             depth-image-view
             render-pass
             framebuffers
             command-pool
             graphics-queue
             graphics-index
             present-index
             :app-name app-name
             :window-width window-width
             :window-height window-height
             :log-levels (:info :warning :error))
    (with-simple-descriptor-set-layout (descriptor-set-layout
                                        device)
      (with-simple-pipeline-layout (pipeline-layout
                                    device
                                    descriptor-set-layout)
        (with-shader-module (vertex-shader-module
                             device
                             "vertex-shader.spv")
          (with-shader-module (fragment-shader-module
                               device
                               "fragment-shader.spv")
            (let* ((vertex-shader-stage-create-info (make-instance 'vk:pipeline-shader-stage-create-info
                                                                   :stage :vertex
                                                                   :module vertex-shader-module
                                                                   :name "main"))
                   (fragment-shader-stage-create-info (make-instance 'vk:pipeline-shader-stage-create-info
                                                                     :stage :fragment
                                                                     :module fragment-shader-module
                                                                     :name "main"))
                   (pipeline-shader-stage-create-infos (list
                                                        vertex-shader-stage-create-info
                                                        fragment-shader-stage-create-info))
                   (vertex-input-binding-description (make-instance 'vk:vertex-input-binding-description
                                                                    :binding 0
                                                                    :stride (* (cffi:foreign-type-size :float) 8)
                                                                    :input-rate :vertex))
                   (vertex-input-attribute-descriptions (list
                                                         (make-instance 'vk:vertex-input-attribute-description
                                                                        :location 0
                                                                        :binding 0
                                                                        :format :r32g32b32a32-sfloat
                                                                        :offset 0)
                                                         (make-instance 'vk:vertex-input-attribute-description
                                                                        :location 1
                                                                        :binding 0
                                                                        :format :r32g32b32a32-sfloat
                                                                        :offset 16)))
                   (pipeline-vertex-input-state-create-info (make-instance 'vk:pipeline-vertex-input-state-create-info
                                                                           :vertex-binding-descriptions (list vertex-input-binding-description)
                                                                           :vertex-attribute-descriptions vertex-input-attribute-descriptions))
                   (pipeline-input-assembly-state-create-info (make-instance 'vk:pipeline-input-assembly-state-create-info
                                                                             :topology :triangle-list
                                                                             :primitive-restart-enable nil))
                   (pipeline-viewport-state-create-info (make-instance 'vk:pipeline-viewport-state-create-info
                                                                       :viewports (list (make-instance 'vk:viewport
                                                                                                       :x 0.0
                                                                                                       :y 0.0
                                                                                                       :width (float (vk:width swapchain-extent))
                                                                                                       :height (float (vk:height swapchain-extent))
                                                                                                       :min-depth 0.0
                                                                                                       :max-depth 1.0))
                                                                       :scissors (list (make-instance 'vk:rect-2d
                                                                                                      :offset (make-instance 'vk:offset-2d
                                                                                                                             :x 0
                                                                                                                             :y 0)
                                                                                                      :extent swapchain-extent))))
                   (pipeline-rasterization-state-create-info (make-instance 'vk:pipeline-rasterization-state-create-info
                                                                            :depth-clamp-enable nil
                                                                            :rasterizer-discard-enable nil
                                                                            :polygon-mode :fill
                                                                            :cull-mode :back
                                                                            :front-face :clockwise
                                                                            :depth-bias-enable nil
                                                                            :depth-bias-constant-factor 0.0
                                                                            :depth-bias-clamp 0.0
                                                                            :depth-bias-slope-factor 0.0
                                                                            :line-width 1.0))
                   (pipeline-multisample-state-create-info (make-instance 'vk:pipeline-multisample-state-create-info
                                                                          :rasterization-samples :1
                                                                          :min-sample-shading 0.0
                                                                          :sample-mask nil
                                                                          :sample-shading-enable nil
                                                                          :alpha-to-coverage-enable nil
                                                                          :alpha-to-one-enable nil))
                   (stencil-op-state (make-instance 'vk:stencil-op-state
                                                    :fail-op :keep
                                                    :pass-op :keep
                                                    :depth-fail-op :keep
                                                    :compare-op :always
                                                    :compare-mask 0
                                                    :write-mask 0
                                                    :reference 0))
                   (pipeline-depth-stencil-state-create-info (make-instance 'vk:pipeline-depth-stencil-state-create-info
                                                                            :depth-test-enable t
                                                                            :depth-write-enable t
                                                                            :depth-compare-op :less-or-equal
                                                                            :depth-bounds-test-enable nil
                                                                            :stencil-test-enable nil
                                                                            :front stencil-op-state
                                                                            :back stencil-op-state
                                                                            :min-depth-bounds 0.0
                                                                            :max-depth-bounds 0.0))
                   (color-component-flags '(:r :g :b :a))
                   (pipeline-color-blend-attachment-state (make-instance 'vk:pipeline-color-blend-attachment-state
                                                                         :blend-enable nil
                                                                         :src-color-blend-factor :zero
                                                                         :dst-color-blend-factor :zero
                                                                         :color-blend-op :add
                                                                         :src-alpha-blend-factor :zero
                                                                         :dst-alpha-blend-factor :zero
                                                                         :alpha-blend-op :add
                                                                         :color-write-mask color-component-flags))
                   (pipeline-color-blend-state-create-info (make-instance 'vk:pipeline-color-blend-state-create-info
                                                                          :logic-op-enable nil
                                                                          :logic-op :no-op
                                                                          :attachments (list pipeline-color-blend-attachment-state)
                                                                          :blend-constants (make-array 4
                                                                                                       :initial-contents '(1.0 1.0 1.0 1.0))))
                   (dynamic-states '(:viewport :scissor))
                   (pipeline-dynamic-state-create-info (make-instance 'vk:pipeline-dynamic-state-create-info
                                                                      :dynamic-states dynamic-states))
                   (graphics-pipeline-create-info (make-instance 'vk:graphics-pipeline-create-info
                                                                 :stages pipeline-shader-stage-create-infos
                                                                 :vertex-input-state pipeline-vertex-input-state-create-info
                                                                 :input-assembly-state pipeline-input-assembly-state-create-info
                                                                 :tessellation-state nil
                                                                 :viewport-state pipeline-viewport-state-create-info
                                                                 :rasterization-state pipeline-rasterization-state-create-info
                                                                 :multisample-state pipeline-multisample-state-create-info
                                                                 :depth-stencil-state pipeline-depth-stencil-state-create-info
                                                                 :color-blend-state pipeline-color-blend-state-create-info
                                                                 :dynamic-state pipeline-dynamic-state-create-info
                                                                 :layout pipeline-layout
                                                                 :render-pass render-pass
                                                                 :subpass 0
                                                                 :base-pipeline-handle nil
                                                                 :base-pipeline-index -1)))
              (multiple-value-bind (pipelines result)
                  (vk:create-graphics-pipelines device
                                                (list graphics-pipeline-create-info))
                (unwind-protect
                     (cond
                       ((eq result :success)
                        (format t "Successfully created ~a graphics pipeline(s)!~%"
                                (length pipelines)))
                       ((eq result :pipeline-compile-required-ext)
                        (format t "VK:CREATE-GRAPHICS-PIPELINES returned: ~a~%"
                                result))
                       (t
                        (error "VK:CREATE-GRAPHICS-PIPELINES failed with: ~a~%"
                               result)))
                  (loop for pipeline in pipelines
                        do (vk:destroy-pipeline device pipeline)))))))))))
