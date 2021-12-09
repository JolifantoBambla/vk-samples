;;;; ray-tracing

(in-package #:vk-samples/ray-tracing)

(defparameter *queued-frames* 2)

(defparameter *vertex-shader*
  "#version 450
#extension GL_ARB_separate_shader_objects : enable
layout(binding = 0) uniform UniformBufferObject
{
  mat4 model;
  mat4 view;
  mat4 proj;
  mat4 modelIT;
} ubo;
layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec2 inTexCoord;
layout(location = 3) in int  inMatID;
layout(location = 0) flat out int  outMatID;
layout(location = 1)      out vec2 outTexCoord;
layout(location = 2)      out vec3 outNormal;
out gl_PerVertex
{
  vec4 gl_Position;
};
void main()
{
  gl_Position = ubo.proj * ubo.view * ubo.model * vec4(inPosition, 1.0);
  outMatID    = inMatID;
  outTexCoord = inTexCoord;
  outNormal   = vec3(ubo.modelIT * vec4(inNormal, 0.0));
}
")

(defparameter *fragment-shader*
  "#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_EXT_nonuniform_qualifier : enable
layout(location = 0) flat in int  matIndex;
layout(location = 1)      in vec2 texCoord;
layout(location = 2)      in vec3 normal;
struct Material
{
  vec3 diffuse;
  int  textureID;
};
const int sizeofMat = 1;
layout(binding = 1) buffer MaterialBufferObject { vec4[] m; } materials;
layout(binding = 2) uniform sampler2D[] textureSamplers;
Material unpackMaterial()
{
  Material m;
  vec4 d0 = materials.m[sizeofMat * matIndex + 0];
  m.diffuse = d0.xyz;
  m.textureID = floatBitsToInt(d0.w);
  return m;
}
layout(location = 0) out vec4 outColor;
void main()
{
  vec3 lightVector = normalize(vec3(5, 4, 3));
  float dot_product = max(dot(lightVector, normalize(normal)), 0.2);
  Material m = unpackMaterial();
  vec3 c = m.diffuse;
  if (0 <= m.textureID)
  {
    c *= texture(textureSamplers[m.textureID], texCoord).xyz;
  }
  c *= dot_product;
  outColor = vec4(c, 1);
}
")

(defparameter *raygen-shader*
  "#version 460
#extension GL_NV_ray_tracing : require
layout(binding = 0, set = 0) uniform accelerationStructureNV topLevelAS;
layout(binding = 1, set = 0, rgba8) uniform image2D image;
layout(binding=2, set = 0) uniform UniformBufferObject
{
  mat4 model;
  mat4 view;
  mat4 proj;
  mat4 modelIT;
  mat4 viewInverse;
  mat4 projInverse;
} cam;
layout(location = 0) rayPayloadNV vec3 hitValue;
void main() 
{
  const vec2 pixelCenter = vec2(gl_LaunchIDNV.xy) + vec2(0.5);
  const vec2 inUV = pixelCenter/vec2(gl_LaunchSizeNV.xy);
  vec2 d = inUV * 2.0 - 1.0;
  vec4 origin = cam.viewInverse*vec4(0,0,0,1);
  vec4 target = cam.projInverse * vec4(d.x, d.y, 1, 1) ;
  vec4 direction = cam.viewInverse*vec4(normalize(target.xyz), 0) ;
  uint rayFlags = gl_RayFlagsOpaqueNV;
  uint cullMask = 0xff;
  float tmin = 0.001;
  float tmax = 10000.0;
  traceNV(topLevelAS, rayFlags, cullMask, 0 /*sbtRecordOffset*/, 0 /*sbtRecordStride*/, 0 /*missIndex*/, origin.xyz, tmin, direction.xyz, tmax, 0 /*payload*/);
  imageStore(image, ivec2(gl_LaunchIDNV.xy), vec4(hitValue, 0.0));
}")

(defparameter *miss-shader*
  "#version 460
#extension GL_NV_ray_tracing : require
layout(location = 0) rayPayloadInNV vec3 hitValue;
void main()
{
  hitValue = vec3(0.0, 0.1, 0.3);
}")

(defparameter *shadow-miss-shader*
  "#version 460
#extension GL_NV_ray_tracing : require
layout(location = 2) rayPayloadInNV bool isShadowed;
void main()
{
  isShadowed = false;
}")

(defparameter *closest-hit-shader*
  "#version 460
#extension GL_NV_ray_tracing : require
#extension GL_EXT_nonuniform_qualifier : enable
layout(location = 0) rayPayloadInNV vec3 hitValue;
layout(location = 2) rayPayloadNV bool isShadowed;
hitAttributeNV vec3 attribs;
layout(binding = 0, set = 0) uniform accelerationStructureNV topLevelAS;
layout(binding = 3, set = 0) buffer Vertices { vec4 v[]; } vertices;
layout(binding = 4, set = 0) buffer Indices { uint i[]; } indices;
layout(binding = 5, set = 0) buffer MatColorBufferObject { vec4[] m; } materials;
layout(binding = 6, set = 0) uniform sampler2D[] textureSamplers;
struct Vertex
{
  vec3 pos;
  vec3 nrm;
  vec2 texCoord;
  int matIndex;
};
// Number of vec4 values used to represent a vertex
uint vertexSize = 3;
Vertex unpackVertex(uint index)
{
  Vertex v;
  vec4 d0 = vertices.v[vertexSize * index + 0];
  vec4 d1 = vertices.v[vertexSize * index + 1];
  vec4 d2 = vertices.v[vertexSize * index + 2];
  v.pos = d0.xyz;
  v.nrm = vec3(d0.w, d1.xy);
  v.texCoord = d1.zw;
  v.matIndex = floatBitsToInt(d2.x);
  return v;
}
struct Material
{
  vec3 diffuse;
  int textureID;
};
// Number of vec4 values used to represent a material
const int sizeofMat = 1;
Material unpackMaterial(int matIndex)
{
  Material m;
  vec4 d0 = materials.m[sizeofMat * matIndex + 0];
  m.diffuse = d0.xyz;
  m.textureID = floatBitsToInt(d0.w);
  return m;
}
void main()
{
  ivec3 ind = ivec3(indices.i[3 * gl_PrimitiveID], indices.i[3 * gl_PrimitiveID + 1], indices.i[3 * gl_PrimitiveID + 2]);
  Vertex v0 = unpackVertex(ind.x);
  Vertex v1 = unpackVertex(ind.y);
  Vertex v2 = unpackVertex(ind.z);
  const vec3 barycentrics = vec3(1.0 - attribs.x - attribs.y, attribs.x, attribs.y);
  vec3 normal = normalize(v0.nrm * barycentrics.x + v1.nrm * barycentrics.y + v2.nrm * barycentrics.z);
  vec3 lightVector = normalize(vec3(5, 4, 3));
  float dot_product = max(dot(lightVector, normal), 0.2);
  Material mat = unpackMaterial(v1.matIndex);
  vec3 c = dot_product * mat.diffuse; 
  if (0 <= mat.textureID)
  {
    vec2 texCoord = v0.texCoord * barycentrics.x + v1.texCoord * barycentrics.y + v2.texCoord * barycentrics.z;
    c *= texture(textureSamplers[mat.textureID], texCoord).xyz;
  }
  float tmin = 0.001;
  float tmax = 100.0;
  vec3 origin = gl_WorldRayOriginNV + gl_WorldRayDirectionNV * gl_HitTNV;
  isShadowed = true;
  traceNV(topLevelAS, gl_RayFlagsTerminateOnFirstHitNV|gl_RayFlagsOpaqueNV|gl_RayFlagsSkipClosestHitShaderNV,  0xFF, 1 /* sbtRecordOffset */, 0 /* sbtRecordStride */, 1 /* missIndex */, origin,
          tmin, lightVector, tmax, 2 /*payload location*/);
  hitValue = c;
  if (isShadowed)
  {
    hitValue *= 0.3f;
  }
}")

(defclass per-frame-data ()
  ((command-pool
    :initarg :command-pool
    :reader command-pool)
   (command-buffer
    :initarg :command-buffer
    :reader command-buffer)
   (fence
    :initarg :fence
    :reader fence)
   (present-complete-semaphore
    :initarg :present-complete-semaphore
    :reader present-complete-semaphore)
   (render-complete-semaphore
    :initarg :render-complete-semaphore
    :reader render-complete-semaphore)))

(defmethod clear-handle-data (device (handle-data per-frame-data))
  (vk:free-command-buffers device (command-pool handle-data) (list (command-buffer handle-data)))
  (vk:destroy-command-pool device (command-pool handle-data))
  (vk:destroy-fence device (fence handle-data))
  (vk:destroy-semaphore device (present-complete-semaphore handle-data))
  (vk:destroy-semaphore device (render-complete-semaphore handle-data)))

(defun prepare-per-frame-data (device graphics-and-present-queue-indices)
  (loop for i from 0 below *queued-frames*
        collect (let ((command-pool (vk:create-command-pool device
                                                            (vk:make-command-pool-create-info
                                                             :flags '(:reset-command-buffer)
                                                             :queue-family-index (first graphics-and-present-queue-indices)))))
                  (make-instance 'per-frame-data
                                 :command-pool command-pool
                                 :command-buffer (first (vk:allocate-command-buffers device
                                                                                     (vk:make-command-buffer-allocate-info
                                                                                      :command-pool command-pool
                                                                                      :level :primary
                                                                                      :command-buffer-count 1)))
                                 :fence (vk:create-fence device
                                                         (vk:make-fence-create-info
                                                          :flags '(:signaled)))
                                 :present-complete-semaphore (vk:create-semaphore device
                                                                                  (vk:make-semaphore-create-info))
                                 :render-complete-semaphore (vk:create-semaphore device
                                                                                 (vk:make-semaphore-create-info))))))

(defun make-textures (physical-device device enable-sampler-anisotropy-p command-pool graphics-queue)
  (let ((textures (loop for i from 0 below 10
                        collect (make-texture-data physical-device
                                                   device
                                                   (vk:make-extent-2d
                                                    :width (* 16 (+ 2 (random 6)))
                                                    :height (* 16 (+ 2 (random 6))))
                                                   '(:transfer-dst :sampled)
                                                   nil
                                                   enable-sampler-anisotropy-p
                                                   t))))
    (one-time-submit device
                     command-pool
                     graphics-queue
                     (lambda (command-buffer)
                       (loop for texture in textures
                             do (set-texture-image texture
                                                   device
                                                   command-buffer
                                                   (checkerboard-image-generator
                                                    (vector (random 255)
                                                            (random 255)
                                                            (random 255))
                                                    (vector (random 255)
                                                            (random 255)
                                                            (random 255)))))))
    textures))

(defun make-descriptor-pool (device pool-sizes)
  (let ((max-sets (reduce #'+ (map 'list #'vk:descriptor-count pool-sizes))))
    (vk:create-descriptor-pool device
                               (vk:make-descriptor-pool-create-info
                                :flags '(:free-descriptor-set)
                                :max-sets max-sets
                                :pool-sizes pool-sizes))))

(defun supports-extensions-p (physical-device extensions)
  (let ((extension-names (map 'list
                              #'vk:extension-name
                              (vk:enumerate-device-extension-properties physical-device))))
    (= (length extensions)
       (length (intersection extensions extension-names :test #'string=)))))

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
        (let* ((device-extensions (list vk:+khr-swapchain-extension-name+
                                        vk:+khr-ray-tracing-pipeline-extension-name+
                                        vk:+khr-acceleration-structure-extension-name+ ;; this is not enabled in the 
                                        vk:+khr-deferred-host-operations-extension-name+ ;; this is not enabled 
                                        vk:+khr-get-memory-requirements-2-extension-name+))
               (physical-device (or (find-if (lambda (physical-device)
                                               (supports-extensions-p physical-device
                                                                      device-extensions))
                                             (vk:enumerate-physical-devices instance))
                                    (error "Could not find a device supporting all required extensions.")))
               (supported-features (vk:get-physical-device-features-2 physical-device
                                                                      (vk:make-physical-device-features-2
                                                                       :next (vk:make-physical-device-descriptor-indexing-features))))
               (enable-sampler-anisotropy-p (vk:sampler-anisotropy (vk:features supported-features)))
               (queue-create-infos (make-default-queue-create-infos physical-device surface))
               (graphics-and-present-queue-indices (map 'list #'vk:queue-family-index queue-create-infos))
               (graphics-queue-index (first graphics-and-present-queue-indices))
               (present-queue-index (or (second graphics-and-present-queue-indices)
                                        (first graphics-and-present-queue-indices))))
          (vk-utils:with-device (device
                                 physical-device
                                 (vk:make-device-create-info
                                  :next (vk:next supported-features)
                                  :queue-create-infos queue-create-infos
                                  :enabled-extension-names device-extensions
                                  :enabled-features (vk:features supported-features)))
            (let ((vk:*default-extension-loader* (vk:make-extension-loader :instance instance
                                                                           :device device)))
              (let* ((window-extent (vk:make-extent-2d
                                     :width window-width
                                     :height window-height))
                     (per-frame-data (prepare-per-frame-data device graphics-and-present-queue-indices))
                     (graphics-queue (vk:get-device-queue device graphics-queue-index 0))
                     (present-queue (vk:get-device-queue device present-queue-index 0))
                     (descriptor-pool (make-descriptor-pool device
                                                            (list (vk:make-descriptor-pool-size
                                                                   :type :combined-image-sampler
                                                                   :descriptor-count 1000)
                                                                  (vk:make-descriptor-pool-size
                                                                   :type :uniform-buffer
                                                                   :descriptor-count 1000)
                                                                  (vk:make-descriptor-pool-size
                                                                   :type :storage-buffer
                                                                   :descriptor-count 1000))))
                     (swapchain-data (make-swapchain-data physical-device
                                                          device
                                                          surface
                                                          window-extent
                                                          '(:color-attachment :storage)
                                                          nil
                                                          graphics-queue-index
                                                          present-queue-index))
                     (color-format (color-format swapchain-data))
                     (depth-format (pick-depth-format physical-device))
                     (render-pass (create-render-pass device
                                                      color-format
                                                      depth-format))
                     (depth-buffer-data (make-depth-buffer-data physical-device
                                                                device
                                                                depth-format
                                                                window-extent))
                     (textures (make-textures physical-device
                                              device
                                              enable-sampler-anisotropy-p
                                              (command-pool (first per-frame-data))
                                              graphics-queue)))
                (unwind-protect
                     (with-framebuffers (framebuffers
                                         device
                                         render-pass
                                         (image-views swapchain-data)
                                         (image-view depth-buffer-data)
                                         window-extent)
                       )
                  (loop for texture in textures
                        do (clear-handle-data device texture))
                  (clear-handle-data device depth-buffer-data)
                  (vk:destroy-render-pass device render-pass)
                  (clear-handle-data device swapchain-data)
                  (vk:destroy-descriptor-pool device descriptor-pool)
                  (loop for frame-data in per-frame-data
                        do (clear-handle-data device frame-data)))))))))))
