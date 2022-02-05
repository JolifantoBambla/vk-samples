# vk-samples
Samples for [vk](https://github.com/JolifantoBambla/vk) (Common Lisp/CFFI bindings for the Vulkan API).

## Requirements
### Supported implementations & Operating systems
See [vk](https://github.com/JolifantoBambla/vk) for supported CL implementations.

`vk-samples` has currently only been tested on Linux, but Windows should work as well.
Some of the samples might also work on MacOS, but currently it is not guaranteed that samples using a window are run on the main thread (which is required for graphics applications on MacOS), so you might need to wrap them in a `(trivial-main-thread:with-body-in-main-thread)`.

### CL dependencies
#### Quicklisp
* `alexandria`
* `cffi`
* `rtg-math`
* `trivial-main-thread` (TODO: actually use this - [MacOS support](https://github.com/JolifantoBambla/vk-samples/issues/27))
* `cl-glfw`

#### Not on Quicklisp (yet)
All of the following dependencies can be loaded via Quicklisp when they are cloned to one of your `ql:*local-project-directories*`.

* [vk](https://github.com/JolifantoBambla/vk): The current version of `vk-samples` uses features that will be on Quicklisp in the next update (version `3.0.0-v1.2.198`).
* [shaderc](https://github.com/JolifantoBambla/shadercl)
* [varjo](https://github.com/JolifantoBambla/varjo): this is my fork of Varjo, which adds Vulkan support and is not merged into the original repo (yet?).

### External dependencies
* [Vulkan SDK](https://vulkan.lunarg.com/sdk/home)
* [GLFW](https://www.glfw.org)
* [libffi](http://sourceware.org/libffi)

## Samples
The following table shows all currently available samples.
You can either run them individually (e.g. `(vk-samples:01-init-instance)`) or run all of them except for `ray-tracing` using `(vk-samples:run-all-samples)`.
As with the original [VulkanSamples by LunarG](https://github.com/LunarG/VulkanSamples) all samples prefixed with a number represent the progression from initializing a Vulkan instance to drawing a cube on screen.
For the most part the things shown in the samples have also been refactored into the package `vk-samples/utils`, so later
sample don't repeat too much from the earlier samples.

| Name | Description |
| -----| ----------- |
| 01-init-instance | Shows how to create and destroy a Vulkan instance. |
| 02-enumerate-devices | Shows how to enumerate physical devices. |
| 03-init-device | Shows how to create and destroy a Vulkan device. |
| 04-init-command-buffer | Shows how to create and destroy a command pool and allocate and free command buffers. |
| 05-init-swapchain | Shows how to initialize a Vulkan swapchain. |
| 06-init-depth-buffer | Shows how to create and destroy a depth buffer. |
| 07-init-uniform-buffer | Shows how to create and destroy a uniform buffer and how to write and read memory from it. |
| 08-init-pipeline-layout | Shows how to create and destroy a Vulkan pipeline layout. |
| 09-init-descriptor-sets | Shows how to allocate descriptor sets and how to write to them. |
| 10-init-render-pass | Shows how to create and destroy a Vulkan render pass with a color and a depth attachment. |
| 11-init-shaders | Shows how to create and destroy Vulkan shader modules from SPIR-V binaries. |
| 12-init-frame-buffers | Shows how to create and destroy Vulkan framebuffers. |
| 13-init-vertex-buffer | Shows how to create, fill and destroy a vertex buffer and how to record commands and submit them to a queue. |
| 14-init-pipeline | Shows how to create and destroy a Vulkan graphics pipeline. |
| 15-draw-cube | This puts everything from the previous samples together and shows how to finally draw a cube onto a window. |
| create-debug-utils-messenger | Shows how to create and destroy a debug callback. |
| create-debug-utils-messenger-next | Shows how to use the `NEXT` slot of a `VK:INSTANCE-CREATE-INFO` to create and destroy a debug callback alongside a Vulkan instance. |
| compile-shaders-from-repl | This is the same as 15-draw-cube, but it compiles the shaders at runtime using `shaderc`. |
| compile-shaders-from-repl | This is the same as compile-shaders-from-repl, but the shaders are written in [Vari](https://github.com/cbaggers/varjo). The shaders are slightly different, because the earliest GLSL version Varjo can produce shaders targeted at Vulkan (i.e. SPIR-V compilation) for. |
| ray-tracing | Shows how to use the `Vk_KHR_ray_tracing` extension. This sample is not run by `run-all-samples`. |

### Ray Tracing Sample
Run `(vk-samples:ray-tracing)` to start the sample. Note that `VK_KHR_ray_tracing` needs to be supported on the device.
In contrast to all other samples, this sample starts an infinite loop that has to be terminated by the user by closing the window, or by pressing **Q** or **ESCAPE**.
The sample renderes a bunch of textured cubes using ray tracing or standard rasterization.
Pressing **R** switches the rendering pipeline used.
When using the ray tracing pipeline, shadows are computed using secondary rays.
The camera can be moved by pressing the left or right mouse button while moving the mouse or by using the scroll wheel.
Use **M** to cycle through camera modes and **CTRL**, **SHIFT**, and **ALT** while moving to change the movement type. 

## Acknowledgements
The samples in this repository closely follows the [VulkanSamples by LunarG](https://github.com/LunarG/VulkanSamples) as well as the samples for [Vulkan-Hpp](https://github.com/KhronosGroup/Vulkan-Hpp).
