# vk-samples
Samples for [vk](https://github.com/JolifantoBambla/vk) (Common Lisp/CFFI bindings for the Vulkan API).

## Requirements
*TODO*

* `glfw3` for `cl-glfw`
* `libffi` for `cl-glfw`

## Samples
| Name | Description |
| -----| ----------- |
| 01-init-instance | Shows how to create and destroy a Vulkan instance. |
| 02-enumerate-devices | Shows how to enumerate physical devices. |
| 03-init-device | Shows how to create and destroy a Vulkan device. |
| 04-init-command-buffer | Shows how to create and destroy a command pool and allocate and free command buffers. |
| 05-init-swapchain | Shows how to initialize a Vulkan swapchain. |
| 07-init-uniform-buffer | Shows how to create and destroy a uniform buffer and how to write and read memory from it. |
| 08-init-pipeline-layout | Shows how to create and destroy a Vulkan pipeline layout. |
| create-debug-utils-messenger | Shows how to create and destroy a debug callback. |
| create-debug-utils-messenger-next | Shows how to use the `NEXT` slot of a `VK:INSTANCE-CREATE-INFO` to create and destroy a debug callback alongside a Vulkan instance. |

You can also run `(vk-samples:run-all-samples)` to test all the samples above.

## Acknowledgements
The samples in this repository closely follows the [VulkanSamples by LunarG](https://github.com/LunarG/VulkanSamples) as well as the samples for [Vulkan-Hpp](https://github.com/KhronosGroup/Vulkan-Hpp).
