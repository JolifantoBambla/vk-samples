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

(defclass uniform-buffer-object ()
  ((model
    :initarg :model
    :reader model)
   (view
    :initarg :view
    :reader view)
   (proj
    :initarg :proj
    :reader proj)
   (model-i-t
    :initarg :model-i-t
    :reader model-i-t)
   (view-inverse
    :initarg :view-inverse
    :reader view-inverse)
   (proj-inverse
    :initarg :proj-inverse
    :reader proj-inverse)))

(defclass material ()
  ((diffuse
    :initarg :diffuse
    :initform (rtg-math:v! 0.7 0.7 0.7)
    :reader diffuse)
   (texture-id
    :initarg :texture-id
    :initform -1
    :reader texture-id)))

(defclass vertex ()
  ((pos
    :initarg :pos
    :reader pos)
   (nrm
    :initarg :nrm
    :reader nrm)
   (tex-coord
    :initarg :tex-coord
    :reader tex-coord)
   (mat-id
    :initarg :mat-id
    :initform 0
    :reader mat-id)))

(defun v! (pos nrm tex-coord mat-id)
  (make-instance 'vertex
                 :pos pos
                 :nrm nrm
                 :tex-coord tex-coord
                 :mat-id mat-id))

(defmacro stride (num-floats num-ints)
  (* 16
     (floor (/ (+ (* (cffi:foreign-type-size :float)
                     num-floats)
                  (* (cffi:foreign-type-size :int)
                     num-ints)
                  15)
               16))))

(defconstant +material-stride+
  (stride 3 1))

(defconstant +vertex-stride+
  (stride 8 1))

(cffi:defcstruct (uniform-buffer-object :class c-uniform-buffer-object)
  (model :float :count 16)
  (view :float :count 16)
  (proj :float :count 16)
  (model-i-t :float :count 16)
  (view-inverse :float :count 16)
  (proj-inverse :float :count 16))

(cffi:defcstruct (material :size #.+material-stride+ :class c-material)
  (diffuse :float :count 3)
  (texture-id :int))

(cffi:defcstruct (vertex :size #.+vertex-stride+ :class c-vertex)
  (pos :float :count 3)
  (nrm :float :count 3)
  (tex-coord :float :count 2)
  (mat-id :int))

(defmethod cffi:translate-into-foreign-memory (value (type c-uniform-buffer-object) ptr)
  (cffi:with-foreign-slots ((model view proj model-i-t view-inverse proj-inverse)
                            ptr
                            (:struct uniform-buffer-object))
    (cffi:lisp-array-to-foreign (model value) model '(:array :float 16))
    (cffi:lisp-array-to-foreign (view value) view '(:array :float 16))
    (cffi:lisp-array-to-foreign (proj value) proj '(:array :float 16))
    (cffi:lisp-array-to-foreign (model-i-t value) model-i-t '(:array :float 16))
    (cffi:lisp-array-to-foreign (view-inverse value) view-inverse '(:array :float 16))
    (cffi:lisp-array-to-foreign (proj-inverse value) proj-inverse '(:array :float 16))))

(defmethod cffi:expand-into-foreign-memory (value (type c-uniform-buffer-object) ptr)
  `(cffi:with-foreign-slots ((model view proj model-i-t view-inverse proj-inverse)
                             ,ptr
                             (:struct uniform-buffer-object))
     (cffi:lisp-array-to-foreign (model ,value) model '(:array :float 16))
     (cffi:lisp-array-to-foreign (view ,value) view '(:array :float 16))
     (cffi:lisp-array-to-foreign (proj ,value) proj '(:array :float 16))
     (cffi:lisp-array-to-foreign (model-i-t ,value) model-i-t '(:array :float 16))
     (cffi:lisp-array-to-foreign (view-inverse ,value) view-inverse '(:array :float 16))
     (cffi:lisp-array-to-foreign (proj-inverse ,value) proj-inverse '(:array :float 16))))

(defmethod cffi:translate-into-foreign-memory (value (type c-material) ptr)
  (cffi:with-foreign-slots ((diffuse texture-id)
                            ptr
                            (:struct material))
    (cffi:lisp-array-to-foreign (diffuse value) diffuse '(:array :float 3))
    (setf texture-id (texture-id value))))

(defmethod cffi:expand-into-foreign-memory (value (type c-material) ptr)
  `(cffi:with-foreign-slots ((diffuse texture-id)
                             ,ptr
                             (:struct material))
     (cffi:lisp-array-to-foreign (diffuse ,value) diffuse '(:array :float 3))
     (setf texture-id (texture-id ,value))))

(defmethod cffi:translate-into-foreign-memory (value (type c-vertex) ptr)
  (cffi:with-foreign-slots ((pos nrm tex-coord mat-id)
                            ptr
                            (:struct vertex))
    (cffi:lisp-array-to-foreign (pos value) pos '(:array :float 3))
    (cffi:lisp-array-to-foreign (nrm value) nrm '(:array :float 3))
    (cffi:lisp-array-to-foreign (tex-coord value) tex-coord '(:array :float 2))
    (setf mat-id (mat-id value))))

(defmethod cffi:expand-into-foreign-memory (value (type c-vertex) ptr)
  `(cffi:with-foreign-slots ((pos nrm tex-coord mat-id)
                             ,ptr
                             (:struct vertex))
     (cffi:lisp-array-to-foreign (pos ,value) pos '(:array :float 3))
     (cffi:lisp-array-to-foreign (nrm ,value) nrm '(:array :float 3))
     (cffi:lisp-array-to-foreign (tex-coord ,value) tex-coord '(:array :float 2))
     (setf mat-id (mat-id ,value))))

(defparameter +cube-data+
  (coerce
   (list
    ;;  pos                          nrm                          tex-coord             mat-id
    ;; front face
    (v! (rtg-math:v! -1.0 -1.0  1.0) (rtg-math:v!  0.0  0.0  1.0) (rtg-math:v! 0.0 0.0) 0)
    (v! (rtg-math:v!  1.0 -1.0  1.0) (rtg-math:v!  0.0  0.0  1.0) (rtg-math:v! 1.0 0.0) 0)
    (v! (rtg-math:v!  1.0  1.0  1.0) (rtg-math:v!  0.0  0.0  1.0) (rtg-math:v! 1.0 1.0) 0)
    (v! (rtg-math:v!  1.0  1.0  1.0) (rtg-math:v!  0.0  0.0  1.0) (rtg-math:v! 1.0 1.0) 0)
    (v! (rtg-math:v! -1.0  1.0  1.0) (rtg-math:v!  0.0  0.0  1.0) (rtg-math:v! 0.0 1.0) 0)
    (v! (rtg-math:v! -1.0 -1.0  1.0) (rtg-math:v!  0.0  0.0  1.0) (rtg-math:v! 0.0 0.0) 0)
    ;; back face
    (v! (rtg-math:v!  1.0 -1.0 -1.0) (rtg-math:v!  0.0  0.0 -1.0) (rtg-math:v! 0.0 0.0) 0)
    (v! (rtg-math:v! -1.0 -1.0 -1.0) (rtg-math:v!  0.0  0.0 -1.0) (rtg-math:v! 1.0 0.0) 0)
    (v! (rtg-math:v! -1.0  1.0 -1.0) (rtg-math:v!  0.0  0.0 -1.0) (rtg-math:v! 1.0 1.0) 0)
    (v! (rtg-math:v! -1.0  1.0 -1.0) (rtg-math:v!  0.0  0.0 -1.0) (rtg-math:v! 1.0 1.0) 0)
    (v! (rtg-math:v!  1.0  1.0 -1.0) (rtg-math:v!  0.0  0.0 -1.0) (rtg-math:v! 0.0 1.0) 0)
    (v! (rtg-math:v!  1.0 -1.0 -1.0) (rtg-math:v!  0.0  0.0 -1.0) (rtg-math:v! 0.0 0.0) 0)
    ;; left face
    (v! (rtg-math:v! -1.0 -1.0 -1.0) (rtg-math:v! -1.0  0.0  0.0) (rtg-math:v! 0.0 0.0) 0)
    (v! (rtg-math:v! -1.0 -1.0  1.0) (rtg-math:v! -1.0  0.0  0.0) (rtg-math:v! 1.0 0.0) 0)
    (v! (rtg-math:v! -1.0  1.0  1.0) (rtg-math:v! -1.0  0.0  0.0) (rtg-math:v! 1.0 1.0) 0)
    (v! (rtg-math:v! -1.0  1.0  1.0) (rtg-math:v! -1.0  0.0  0.0) (rtg-math:v! 1.0 1.0) 0)
    (v! (rtg-math:v! -1.0  1.0 -1.0) (rtg-math:v! -1.0  0.0  0.0) (rtg-math:v! 0.0 1.0) 0)
    (v! (rtg-math:v! -1.0 -1.0 -1.0) (rtg-math:v! -1.0  0.0  0.0) (rtg-math:v! 0.0 0.0) 0)
    ;; right face
    (v! (rtg-math:v!  1.0 -1.0  1.0) (rtg-math:v!  1.0  0.0  0.0) (rtg-math:v! 0.0 0.0) 0)
    (v! (rtg-math:v!  1.0 -1.0 -1.0) (rtg-math:v!  1.0  0.0  0.0) (rtg-math:v! 1.0 0.0) 0)
    (v! (rtg-math:v!  1.0  1.0 -1.0) (rtg-math:v!  1.0  0.0  0.0) (rtg-math:v! 1.0 1.0) 0)
    (v! (rtg-math:v!  1.0  1.0 -1.0) (rtg-math:v!  1.0  0.0  0.0) (rtg-math:v! 1.0 1.0) 0)
    (v! (rtg-math:v!  1.0  1.0  1.0) (rtg-math:v!  1.0  0.0  0.0) (rtg-math:v! 0.0 1.0) 0)
    (v! (rtg-math:v!  1.0 -1.0  1.0) (rtg-math:v!  1.0  0.0  0.0) (rtg-math:v! 0.0 0.0) 0)
    ;; top face
    (v! (rtg-math:v! -1.0  1.0  1.0) (rtg-math:v!  0.0  1.0  0.0) (rtg-math:v! 0.0 0.0) 0)
    (v! (rtg-math:v!  1.0  1.0  1.0) (rtg-math:v!  0.0  1.0  0.0) (rtg-math:v! 1.0 0.0) 0)
    (v! (rtg-math:v!  1.0  1.0 -1.0) (rtg-math:v!  0.0  1.0  0.0) (rtg-math:v! 1.0 1.0) 0)
    (v! (rtg-math:v!  1.0  1.0 -1.0) (rtg-math:v!  0.0  1.0  0.0) (rtg-math:v! 1.0 1.0) 0)
    (v! (rtg-math:v! -1.0  1.0 -1.0) (rtg-math:v!  0.0  1.0  0.0) (rtg-math:v! 0.0 1.0) 0)
    (v! (rtg-math:v! -1.0  1.0  1.0) (rtg-math:v!  0.0  1.0  0.0) (rtg-math:v! 0.0 0.0) 0)
    ;; bottom face
    (v! (rtg-math:v! -1.0 -1.0 -1.0) (rtg-math:v!  0.0 -1.0  0.0) (rtg-math:v! 0.0 0.0) 0)
    (v! (rtg-math:v!  1.0 -1.0 -1.0) (rtg-math:v!  0.0 -1.0  0.0) (rtg-math:v! 1.0 0.0) 0)
    (v! (rtg-math:v!  1.0 -1.0  1.0) (rtg-math:v!  0.0 -1.0  0.0) (rtg-math:v! 1.0 1.0) 0)
    (v! (rtg-math:v!  1.0 -1.0  1.0) (rtg-math:v!  0.0 -1.0  0.0) (rtg-math:v! 1.0 1.0) 0)
    (v! (rtg-math:v! -1.0 -1.0  1.0) (rtg-math:v!  0.0 -1.0  0.0) (rtg-math:v! 0.0 1.0) 0)
    (v! (rtg-math:v! -1.0 -1.0 -1.0) (rtg-math:v!  0.0 -1.0  0.0) (rtg-math:v! 0.0 0.0) 0))
   'vector))

(defun random-in-range (min-value max-value)
  (+ (random (- max-value min-value))
     min-value))

(defun random-vec3 (min-value max-value)
  (rtg-math:v! (random-in-range min-value max-value)
               (random-in-range min-value max-value)
               (random-in-range min-value max-value)))

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

(defun make-textures (physical-device device enable-sampler-anisotropy-p command-pool graphics-queue &optional (num-textures 10))
  (let ((textures (loop for i from 0 below num-textures
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

(defun make-materials-buffer (physical-device device num-textures)
  (let ((materials (loop for i from 0 below num-textures
                         collect (make-instance 'material
                                                :diffuse (random-vec3 0.0 1.0)
                                                :texture-id i)))
        (buffer (make-buffer-data physical-device
                                  device
                                  (* num-textures
                                     +material-stride+)
                                  '(:storage-buffer))))
    (copy-to-device device
                    (device-memory buffer)
                    materials
                    '(:struct material))
    buffer))

(defun make-vertex-buffer (physical-device device command-pool queue num-materials &optional (x-max 10) (y-max 10) (z-max 10))
  (let* ((vertices (loop for x from 0 below x-max
                         return (loop for y from 0 below y-max
                                      return (loop for z from 0 below z-max
                                                   for m = (random num-materials)
                                                   for jitter = (random-vec3 0.0 0.6)
                                                   return (loop for v across +cube-data+
                                                                collect (v! (rtg-math.vector3:*s
                                                                             (rtg-math.vector3:+ (pos v)
                                                                                                 jitter)
                                                                             3.0)
                                                                            (nrm v)
                                                                            (tex-coord v)
                                                                            m))))))
         (vertex-buffer (make-buffer-data physical-device
                                          device
                                          (* (length vertices)
                                             +vertex-stride+)
                                          '(:transfer-dst
                                            :vertex-buffer
                                            :storage-buffer)
                                          :device-local)))
    (copy-to-buffer vertex-buffer
                    physical-device
                    device
                    command-pool
                    queue
                    vertices
                    '(:struct vertex))
    vertex-buffer))

(defun make-vertex-index-buffer (physical-device device command-pool queue num-vertices)
  (let* ((indices (loop for i from 0 below num-vertices
                        collect i))
         (vertex-index-buffer (make-buffer-data physical-device
                                                device
                                                (* (length indices)
                                                   (cffi:foreign-type-size :uint32))
                                                '(:transfer-dst
                                                  :index-buffer
                                                  :storage-buffer)
                                                :device-local)))
    (copy-to-buffer vertex-index-buffer
                    physical-device
                    device
                    command-pool
                    queue
                    indices
                    :uint32)
    vertex-index-buffer))

(defun make-descriptor-pool (device pool-sizes)
  (let ((max-sets (reduce #'+ (map 'list #'vk:descriptor-count pool-sizes))))
    (vk:create-descriptor-pool device
                               (vk:make-descriptor-pool-create-info
                                :flags '(:free-descriptor-set)
                                :max-sets max-sets
                                :pool-sizes pool-sizes))))

(defun make-descriptor-set-layout (device binding-data &optional flags)
  (vk:create-descriptor-set-layout
   device
   (vk:make-descriptor-set-layout-create-info
    :flags flags
    :bindings (loop for (descriptor-type descriptor-count stage-flags) in binding-data
                    for i from 0
                    collect (vk:make-descriptor-set-layout-binding
                             :binding i
                             :descriptor-type descriptor-type
                             :descriptor-count descriptor-count
                             :stage-flags stage-flags)))))

(defun supports-extensions-p (physical-device extensions)
  (let ((extension-names (map 'list
                              #'vk:extension-name
                              (vk:enumerate-device-extension-properties physical-device))))
    (= (length extensions)
       (length (intersection extensions extension-names :test #'string=)))))

(defun ray-tracing (&optional (app-name "ray-tracing")
                              (window-width 1280)
                              (window-height 720)
                              (x-max 10)
                              (y-max 10)
                              (z-max 10))
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
                                              graphics-queue))
                     (material-buffer-data (make-materials-buffer physical-device device (length textures)))
                     (num-vertices (* x-max y-max z-max (length +cube-data+)))
                     (vertex-buffer-data (make-vertex-buffer physical-device
                                                             device
                                                             (command-pool (first per-frame-data))
                                                             graphics-queue
                                                             (length textures)
                                                             x-max
                                                             y-max
                                                             z-max))
                     (vertex-index-buffer-data (make-vertex-index-buffer physical-device
                                                                         device
                                                                         (command-pool (first per-frame-data))
                                                                         graphics-queue
                                                                         num-vertices))
                     (uniform-buffer-data (make-buffer-data physical-device
                                                            device
                                                            (cffi:foreign-type-size '(:struct uniform-buffer-object))
                                                            '(:uniform-buffer)))
                     (descriptor-set-layout (make-descriptor-set-layout device
                                                                        (list '(:uniform-buffer 1 (:vertex))
                                                                              '(:storage-buffer 1 (:vertex :fragment))
                                                                              (list :combined-image-sampler
                                                                                    (length textures)
                                                                                    '(:fragment)))))
                     (pipeline-layout (vk:create-pipeline-layout device
                                                                 (vk:make-pipeline-layout-create-info
                                                                  :set-layouts (list descriptor-set-layout))))
                     (vertex-shader-module (vk:create-shader-module device
                                                                    (vk:make-shader-module-create-info
                                                                     :code (shaderc:compile-to-spv *vertex-shader*
                                                                                                   :vertex-shader))))
                     (fragment-shader-module (vk:create-shader-module device
                                                                      (vk:make-shader-module-create-info
                                                                       :code (shaderc:compile-to-spv *fragment-shader*
                                                                                                     :fragment-shader))))
                     (graphics-pipeline (create-graphics-pipeline device
                                                                  nil
                                                                  (list vertex-shader-module nil)
                                                                  (list fragment-shader-module nil)
                                                                  +vertex-stride+
                                                                  (list
                                                                   (:r32g32b32-sfloat (cffi:foreign-slot-offset '(:struct vertex) pos))
                                                                   (:r32g32b32-sfloat (cffi:foreign-slot-offset '(:struct vertex) nrm))
                                                                   (:r32g32b32-sfloat (cffi:foreign-slot-offset '(:struct vertex) tex-coord))
                                                                   (:r32g32b32-sfloat (cffi:foreign-slot-offset '(:struct vertex) mat-id)))
                                                                  :counter-clockwise
                                                                  t
                                                                  pipeline-layout
                                                                  render-pass))
                     (transform (rtg-math.matrix4:identity)))
                (unwind-protect
                     (with-framebuffers (framebuffers
                                         device
                                         render-pass
                                         (image-views swapchain-data)
                                         (image-view depth-buffer-data)
                                         window-extent)
                       )
                  (vk:destroy-shader-module device fragment-shader-module)
                  (vk:destroy-shader-module device vertex-shader-module)
                  (vk:destroy-pipeline-layout device pipeline-layout)
                  (vk:destroy-descriptor-set-layout device descriptor-set-layout)
                  (clear-handle-data device uniform-buffer-data)
                  (clear-handle-data device vertex-index-buffer-data)
                  (clear-handle-data device vertex-buffer-data)
                  (clear-handle-data device material-buffer-data)
                  (loop for texture in textures
                        do (clear-handle-data device texture))
                  (clear-handle-data device depth-buffer-data)
                  (vk:destroy-render-pass device render-pass)
                  (clear-handle-data device swapchain-data)
                  (vk:destroy-descriptor-pool device descriptor-pool)
                  (loop for frame-data in per-frame-data
                        do (clear-handle-data device frame-data)))))))))))
