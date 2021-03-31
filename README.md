# vk-samples
Samples for [vk](https://github.com/JolifantoBambla/vk) (Common Lisp/CFFI bindings for the Vulkan API).

## Requirements
### Supported implementations & Operating systems
`vk` has currently only been tested on `SBCL 2.0.0`, but other implementations might work as well.

`vk` has currently only been tested on Linux, but Windows should work as well.
Some of the samples might also work on MacOS, but currently it is not guaranteed that samples using a window are run on the main thread (which is required for graphics applications on MacOS), so you might need to wrap them in a `(trivial-main-thread:with-body-in-main-thread)`.

### CL dependencies
#### Quicklisp
* `alexandria`
* `cffi`
* `rtg-math`
* `trivial-main-thread` (TODO: actually use this - [MacOS support](https://github.com/JolifantoBambla/vk-samples/issues/27))

#### Not on Quicklisp (yet)
* `vk`: Get it [here](https://github.com/JolifantoBambla/vk).
* `cl-glfw`: The `cl-glfw` on Quicklisp does not (yet?) support Vulkan. There is an open pull request though, so this might change. In the meantime you can use [my fork](https://github.com/JolifantoBambla/cl-glfw3).

### External dependencies
* [Vulkan SDK](https://vulkan.lunarg.com/sdk/home)
* [GLFW](https://www.glfw.org)
* [libffi](http://sourceware.org/libffi)

## Samples
The following table shows all currently available samples.
You can either run them individually (e.g. `(vk-samples:01-init-instance)`) or run all of them using `(vk-samples:run-all-samples)`.
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

## Acknowledgements
The samples in this repository closely follows the [VulkanSamples by LunarG](https://github.com/LunarG/VulkanSamples) as well as the samples for [Vulkan-Hpp](https://github.com/KhronosGroup/Vulkan-Hpp).
