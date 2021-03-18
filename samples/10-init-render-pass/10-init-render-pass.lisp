;;;; 10-init-render-pass

(in-package #:vk-samples/10-init-render-pass)

(defun 10-init-render-pass (&optional (app-name "10-init-render-pass") (window-width 64) (window-height 64))
  ;; WITH-GFX in located in utils - check out the previous examples to see how to create an instance, a device and a surface
  (with-gfx (instance device physical-device surface :app-name app-name :window-width window-width :window-height window-height)
    (let* ((color-format (vk:format (pick-color-format physical-device surface)))
           (depth-format :d16-unorm)
           (color-attachment-description (make-instance 'vk:attachment-description
                                                        :format color-format
                                                        :samples :1
                                                        :load-op :clear
                                                        :store-op :store
                                                        :stencil-load-op :dont-care
                                                        :stencil-store-op :dont-care
                                                        :initial-layout :undefined
                                                        :final-layout :present-src-khr))
           (depth-attachment-description (make-instance 'vk:attachment-description
                                                        :format depth-format
                                                        :samples :1
                                                        :load-op :clear
                                                        :store-op :dont-care
                                                        :stencil-load-op :dont-care
                                                        :stencil-store-op :dont-care
                                                        :initial-layout :undefined
                                                        :final-layout :depth-stencil-attachment-optimal))
           (attachment-descriptions (list color-attachment-description
                                          depth-attachment-description))
           (color-reference (make-instance 'vk:attachment-reference
                                           :attachment 0 ;; the index of the color attachment
                                           :layout :color-attachment-optimal))
           (depth-reference (make-instance 'vk:attachment-reference
                                           :attachment 1 ;; the index of the depth attachment
                                           :layout :depth-stencil-attachment-optimal))
           (subpass-description (make-instance 'vk:subpass-description
                                               :pipeline-bind-point :graphics
                                               :color-attachments (list color-reference)
                                               :depth-stencil-attachment (list depth-reference)))
           (render-pass-create-info (make-instance 'vk:render-pass-create-info
                                                   :attachments attachment-descriptions
                                                   :subpasses (list subpass-description)))
           (render-pass (vk:create-render-pass device render-pass-create-info)))
      (vk:destroy-render-pass device render-pass))))
