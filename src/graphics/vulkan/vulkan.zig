const std = @import("std");
const builtin = @import("builtin");
const core = @import("../../core/core.zig");

pub const c = @cImport({
    @cDefine("VK_NO_PROTOTYPES", "true");
    @cDefine("VK_EXT_debug_utils", "true");
    if (core.info.using_backend == .win32) {
        @cDefine("VK_USE_PLATFORM_WIN32_KHR", "true");
    }
    @cInclude("vulkan/vulkan.h");
    @cInclude("volk.h");
});

const Context = struct {
    instance: c.VkInstance,
    debug_messenger: c.VkDebugUtilsMessengerEXT,
};
var ctx: Context = undefined;
var existing_contexts: u32 = 0;

const api_version = c.VK_MAKE_VERSION(1, 3, 0);
/// Subsequent calls to `init` will return the same context.
/// If the context is already initialized, the reference count is incremented.
/// The context is only deinitialized when the reference count reaches 0.
/// This does not allocate any long-lived memory, feel free to use a stack allocator.
pub fn init(allocator: std.mem.Allocator) !*const Context {
    try checkVk(c.volkInitialize());

    if (existing_contexts > 0) {
        existing_contexts += 1;
        return &ctx;
    }

    // check for 1.3 support
    {
        var temp_api_version = api_version;
        try checkVk(c.vkEnumerateInstanceVersion.?(@ptrCast(&temp_api_version)));
    }

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var temp = std.heap.stackFallback(1024, arena.allocator());
    const temp_allocator = temp.get();

    const instance = try createInstance(temp_allocator);
    c.volkLoadInstance(instance);
    const debug_messenger = try createDebugCallback(instance);

    ctx = .{
        .instance = instance,
        .debug_messenger = debug_messenger,
    };
    existing_contexts += 1;

    return &ctx;
}
pub fn deinit() void {
    if (existing_contexts > 1) {
        existing_contexts -= 1;
        return;
    }
    if (ctx.debug_messenger) |debug_messenger| {
        c.vkDestroyDebugUtilsMessengerEXT.?(ctx.instance, debug_messenger, null);
    }
    c.vkDestroyInstance.?(ctx.instance, null);
    c.volkFinalize();
}

fn createInstance(allocator: std.mem.Allocator) !c.VkInstance {
    // Get supported layers and extensions
    var layer_count: u32 = 0;
    try checkVk(c.vkEnumerateInstanceLayerProperties.?(&layer_count, null));
    const layer_props = try allocator.alloc(c.VkLayerProperties, layer_count);
    defer allocator.free(layer_props);
    try checkVk(c.vkEnumerateInstanceLayerProperties.?(&layer_count, layer_props.ptr));

    var extension_count: u32 = 0;
    try checkVk(c.vkEnumerateInstanceExtensionProperties.?(null, &extension_count, null));
    const extension_props = try allocator.alloc(c.VkExtensionProperties, extension_count);
    defer allocator.free(extension_props);
    try checkVk(c.vkEnumerateInstanceExtensionProperties.?(null, &extension_count, extension_props.ptr));

    var enable_validation = builtin.mode == .Debug;

    // Check if the validation layer is supported
    var layers = std.ArrayListUnmanaged([*c]const u8){};
    defer layers.deinit(allocator);
    if (enable_validation) {
        enable_validation = blk: for (layer_props) |layer_prop| {
            const layer_name: [*c]const u8 = @ptrCast(layer_prop.layerName[0..]);
            const validation_layer_name: [*c]const u8 = "VK_LAYER_KHRONOS_validation";
            if (std.mem.eql(u8, std.mem.span(validation_layer_name), std.mem.span(layer_name))) {
                try layers.append(allocator, validation_layer_name);
                break :blk true;
            }
        } else false;
    }

    // Check if the required extensions are supported
    var extensions = std.ArrayListUnmanaged([*c]const u8){};
    defer extensions.deinit(allocator);

    const ExtensionFinder = struct {
        fn find(name: [*c]const u8, props: []c.VkExtensionProperties) bool {
            for (props) |prop| {
                const prop_name: [*c]const u8 = @ptrCast(prop.extensionName[0..]);
                if (std.mem.eql(u8, std.mem.span(name), std.mem.span(prop_name))) {
                    return true;
                }
            }
            return false;
        }
    };

    for (getRequiredExtensions()) |required_ext| {
        if (ExtensionFinder.find(required_ext, extension_props)) {
            try extensions.append(allocator, required_ext);
        } else {
            core.err("Required vulkan extension not supported: {s}", .{required_ext});
            return error.vulkan_extension_not_supported;
        }
    }

    // If we need validation, also add the debug utils extension
    if (enable_validation) {
        try extensions.append(allocator, "VK_EXT_debug_utils");
    } else {
        enable_validation = false;
    }

    const app_info: c.VkApplicationInfo = .{
        .sType = c.VK_STRUCTURE_TYPE_APPLICATION_INFO,
        .apiVersion = api_version,
        .pApplicationName = core.info.app_name.ptr,
        .pEngineName = "sock",
    };

    const instance_info: c.VkInstanceCreateInfo = .{
        .sType = c.VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
        .pApplicationInfo = &app_info,
        .enabledLayerCount = @as(u32, @intCast(layers.items.len)),
        .ppEnabledLayerNames = layers.items.ptr,
        .enabledExtensionCount = @as(u32, @intCast(extensions.items.len)),
        .ppEnabledExtensionNames = extensions.items.ptr,
    };

    var instance: c.VkInstance = null;
    try checkVk(c.vkCreateInstance.?(&instance_info, null, &instance));
    return instance;
}

fn getRequiredExtensions() []const [*c]const u8 {
    const common: []const [*c]const u8 = &.{
        "VK_KHR_surface",
    };
    switch (core.info.using_backend) {
        .win32 => {
            return common ++ .{
                "VK_KHR_win32_surface",
            };
        },
    }
}

fn createDebugCallback(instance: c.VkInstance) !c.VkDebugUtilsMessengerEXT {
    if (c.vkCreateDebugUtilsMessengerEXT) |create_fn| {
        const create_info = c.VkDebugUtilsMessengerCreateInfoEXT{
            .sType = c.VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,
            .messageSeverity = c.VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT |
                c.VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT |
                c.VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
            .messageType = c.VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT |
                c.VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT |
                c.VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
            .pfnUserCallback = defaultDebugCallback,
            .pUserData = null,
        };
        var debug_messenger: c.VkDebugUtilsMessengerEXT = null;
        try checkVk(create_fn(instance, &create_info, null, &debug_messenger));
        core.log("Created vulkan debug messenger.", .{});
        return debug_messenger;
    }
    return null;
}

fn defaultDebugCallback(
    severity: c.VkDebugUtilsMessageSeverityFlagBitsEXT,
    msg_type: c.VkDebugUtilsMessageTypeFlagsEXT,
    callback_data: ?*const c.VkDebugUtilsMessengerCallbackDataEXT,
    user_data: ?*anyopaque,
) callconv(.C) c.VkBool32 {
    _ = user_data;
    const severity_str = switch (severity) {
        c.VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT => "verbose",
        c.VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT => "info",
        c.VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT => "warning",
        c.VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT => "error",
        else => "unknown",
    };

    const type_str = switch (msg_type) {
        c.VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT => "general",
        c.VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT => "validation",
        c.VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT => "performance",
        c.VK_DEBUG_UTILS_MESSAGE_TYPE_DEVICE_ADDRESS_BINDING_BIT_EXT => "device address",
        else => "unknown",
    };

    const message: [*c]const u8 = if (callback_data) |cb_data| cb_data.pMessage else "NO MESSAGE!";
    core.err("[{s}][{s}]. Message:\n  {s}", .{ severity_str, type_str, message });

    if (severity >= c.VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT) {
        @panic("Unrecoverable vulkan error.");
    }

    return c.VK_FALSE;
}

pub const Surface = struct {
    handle: c.VkSurfaceKHR,
    // capabilities: c.VkSurfaceCapabilitiesKHR,
    // formats: []c.VkSurfaceFormatKHR,
    // present_modes: []c.VkPresentModeKHR,
};

pub fn createSurface(context: *const Context, window: *core.Window) !Surface {
    switch (core.info.using_backend) {
        .win32 => {
            const VkWin32SurfaceCreateInfoKHR = extern struct {
                sType: c.VkStructureType = @import("std").mem.zeroes(c.VkStructureType),
                pNext: ?*const anyopaque = @import("std").mem.zeroes(?*const anyopaque),
                flags: c.VkWin32SurfaceCreateFlagsKHR = @import("std").mem.zeroes(c.VkWin32SurfaceCreateFlagsKHR),
                hinstance: std.os.windows.HINSTANCE,
                hwnd: std.os.windows.HWND,
            };

            const create_info = VkWin32SurfaceCreateInfoKHR{
                .sType = c.VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR,
                .hinstance = @ptrCast(core.getInstance()),
                .hwnd = @alignCast(@ptrCast(window.getNativeWindow())),
            };

            var surface: c.VkSurfaceKHR = null;
            try checkVk(c.vkCreateWin32SurfaceKHR.?(context.instance, @ptrCast(&create_info), null, &surface));
            return .{
                .handle = surface,
                // .capabilities = getSurfaceCapabilities(context, surface),
                // .formats = getSurfaceFormats(context, surface),
                // .present_modes = getPresentModes(context, surface),
            };
        },
    }
}

pub fn destroySurface(context: *const Context, surface: *const Surface) void {
    c.vkDestroySurfaceKHR.?(context.instance, surface.handle, null);
}

pub const PhysicalDevice = struct {
    handle: c.VkPhysicalDevice,
    /// The selected physical device properties.
    properties: c.VkPhysicalDeviceProperties = .{},
    /// Queue family indices.
    graphics_queue_family: u32 = 0,
    present_queue_family: u32 = 0,
    compute_queue_family: u32 = 0,
    transfer_queue_family: u32 = 0,

    const invalid_queue = std.math.maxInt(u32);
};

pub const PhysicalDeviceSelectOpts = struct {
    /// Presentation surface.
    surface: *const Surface,
    /// Selection criteria.
    criteria: enum { first, discrete } = .first,
};

pub fn selectPhysicalDevice(context: *const Context, allocator: std.mem.Allocator, opts: PhysicalDeviceSelectOpts) !PhysicalDevice {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var temp = std.heap.stackFallback(1024, arena.allocator());
    const temp_allocator = temp.get();

    var physical_device_count: u32 = 0;
    try checkVk(c.vkEnumeratePhysicalDevices.?(context.instance, &physical_device_count, null));

    const physical_devices = try temp_allocator.alloc(c.VkPhysicalDevice, physical_device_count);
    try checkVk(c.vkEnumeratePhysicalDevices.?(context.instance, &physical_device_count, physical_devices.ptr));

    var suitable_pd: ?PhysicalDevice = null;

    for (physical_devices) |device| {
        const pd = makePhysicalDevice(allocator, device, opts.surface.handle) catch continue;
        _ = isPhysicalDeviceSuitable(allocator, pd, opts) catch continue;

        if (opts.criteria == .first) {
            suitable_pd = pd;
            break;
        }

        if (pd.properties.deviceType == c.VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU) {
            suitable_pd = pd;
            break;
        } else if (suitable_pd == null) {
            suitable_pd = pd;
        }
    }

    if (suitable_pd == null) {
        core.err("No suitable physical device found.", .{});
        return error.vulkan_no_suitable_physical_device;
    }
    const res = suitable_pd.?;

    const device_name = @as([*:0]const u8, @ptrCast(@alignCast(res.properties.deviceName[0..])));
    core.log("Selected physical device: {s}", .{device_name});

    return res;
}

pub fn destroyPhysicalDevice(allocator: std.mem.Allocator, device: *PhysicalDevice) void {
    _ = allocator;
    _ = device;
}

fn makePhysicalDevice(allocator: std.mem.Allocator, physical_device: c.VkPhysicalDevice, surface: c.VkSurfaceKHR) !PhysicalDevice {
    var props = c.VkPhysicalDeviceProperties{};
    c.vkGetPhysicalDeviceProperties.?(physical_device, &props);

    var graphics_queue_family: u32 = PhysicalDevice.invalid_queue;
    var present_queue_family: u32 = PhysicalDevice.invalid_queue;
    var compute_queue_family: u32 = PhysicalDevice.invalid_queue;
    var transfer_queue_family: u32 = PhysicalDevice.invalid_queue;

    var queue_family_count: u32 = 0;
    c.vkGetPhysicalDeviceQueueFamilyProperties.?(physical_device, &queue_family_count, null);
    const queue_families = try allocator.alloc(c.VkQueueFamilyProperties, queue_family_count);
    defer allocator.free(queue_families);
    c.vkGetPhysicalDeviceQueueFamilyProperties.?(physical_device, &queue_family_count, queue_families.ptr);

    for (queue_families, 0..) |queue_family, i| {
        const index: u32 = @intCast(i);

        if (graphics_queue_family == PhysicalDevice.invalid_queue and
            queue_family.queueFlags & c.VK_QUEUE_GRAPHICS_BIT != 0)
        {
            graphics_queue_family = index;
        }

        if (present_queue_family == PhysicalDevice.invalid_queue) {
            var present_support: c.VkBool32 = c.VK_FALSE;
            try checkVk(c.vkGetPhysicalDeviceSurfaceSupportKHR.?(physical_device, index, surface, &present_support));
            if (present_support == c.VK_TRUE) {
                present_queue_family = index;
            }
        }

        if (compute_queue_family == PhysicalDevice.invalid_queue and
            queue_family.queueFlags & c.VK_QUEUE_COMPUTE_BIT != 0)
        {
            compute_queue_family = index;
        }

        if (transfer_queue_family == PhysicalDevice.invalid_queue and
            queue_family.queueFlags & c.VK_QUEUE_TRANSFER_BIT != 0)
        {
            transfer_queue_family = index;
        }

        if (graphics_queue_family != PhysicalDevice.invalid_queue and
            present_queue_family != PhysicalDevice.invalid_queue and
            compute_queue_family != PhysicalDevice.invalid_queue and
            transfer_queue_family != PhysicalDevice.invalid_queue)
        {
            break;
        }
    }

    return .{
        .handle = physical_device,
        .properties = props,
        .graphics_queue_family = graphics_queue_family,
        .present_queue_family = present_queue_family,
        .compute_queue_family = compute_queue_family,
        .transfer_queue_family = transfer_queue_family,
    };
}

fn isPhysicalDeviceSuitable(allocator: std.mem.Allocator, device: PhysicalDevice, opts: PhysicalDeviceSelectOpts) !bool {
    if (device.properties.apiVersion < api_version) {
        return false;
    }

    if (device.graphics_queue_family == PhysicalDevice.invalid_queue or
        device.present_queue_family == PhysicalDevice.invalid_queue or
        device.compute_queue_family == PhysicalDevice.invalid_queue or
        device.transfer_queue_family == PhysicalDevice.invalid_queue)
    {
        return false;
    }

    const swapchain_support = try SwapchainSupportInfo.init(allocator, device.handle, opts.surface.handle);
    defer swapchain_support.deinit(allocator);
    if (swapchain_support.formats.len == 0 or swapchain_support.present_modes.len == 0) {
        return false;
    }

    const required_extensions = getRequiredDeviceExtensions();
    if (required_extensions.len > 0) {
        var device_extension_count: u32 = 0;
        try checkVk(c.vkEnumerateDeviceExtensionProperties.?(device.handle, null, &device_extension_count, null));
        const device_extensions = try allocator.alloc(c.VkExtensionProperties, device_extension_count);
        defer allocator.free(device_extensions);
        try checkVk(c.vkEnumerateDeviceExtensionProperties.?(device.handle, null, &device_extension_count, device_extensions.ptr));

        _ = blk: for (required_extensions) |req_ext| {
            for (device_extensions) |device_ext| {
                const device_ext_name: [*c]const u8 = @ptrCast(device_ext.extensionName[0..]);
                if (std.mem.eql(u8, std.mem.span(req_ext), std.mem.span(device_ext_name))) {
                    break :blk true;
                }
            }
        } else return false;
    }

    var vk13_features = c.VkPhysicalDeviceVulkan13Features{
        .sType = c.VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES,
    };
    var vk12_features = c.VkPhysicalDeviceVulkan12Features{
        .sType = c.VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES,
    };
    var physical_features2 = c.VkPhysicalDeviceFeatures2{
        .sType = c.VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2,
    };
    physical_features2.pNext = &vk13_features;
    vk13_features.pNext = &vk12_features;
    c.vkGetPhysicalDeviceFeatures2.?(device.handle, &physical_features2);

    // we need dynamicRendering, synchronization2, descriptorIndexing, and bufferDeviceAddress
    if (vk12_features.bufferDeviceAddress == c.VK_FALSE) {
        return false;
    }
    if (vk12_features.descriptorIndexing == c.VK_FALSE) {
        return false;
    }
    if (vk13_features.synchronization2 == c.VK_FALSE) {
        return false;
    }
    if (vk13_features.dynamicRendering == c.VK_FALSE) {
        return false;
    }

    return true;
}

fn getRequiredDeviceExtensions() []const [*c]const u8 {
    return &.{
        "VK_KHR_swapchain",
    };
}

const SwapchainSupportInfo = struct {
    capabilities: c.VkSurfaceCapabilitiesKHR = .{},
    formats: []c.VkSurfaceFormatKHR = &.{},
    present_modes: []c.VkPresentModeKHR = &.{},

    fn init(allocator: std.mem.Allocator, device: c.VkPhysicalDevice, surface: c.VkSurfaceKHR) !SwapchainSupportInfo {
        var capabilities: c.VkSurfaceCapabilitiesKHR = .{};
        try checkVk(c.vkGetPhysicalDeviceSurfaceCapabilitiesKHR.?(device, surface, &capabilities));

        var format_count: u32 = 0;
        try checkVk(c.vkGetPhysicalDeviceSurfaceFormatsKHR.?(device, surface, &format_count, null));
        const formats = try allocator.alloc(c.VkSurfaceFormatKHR, format_count);
        try checkVk(c.vkGetPhysicalDeviceSurfaceFormatsKHR.?(device, surface, &format_count, formats.ptr));

        var present_mode_count: u32 = 0;
        try checkVk(c.vkGetPhysicalDeviceSurfacePresentModesKHR.?(device, surface, &present_mode_count, null));
        const present_modes = try allocator.alloc(c.VkPresentModeKHR, present_mode_count);
        try checkVk(c.vkGetPhysicalDeviceSurfacePresentModesKHR.?(device, surface, &present_mode_count, present_modes.ptr));

        return .{
            .capabilities = capabilities,
            .formats = formats,
            .present_modes = present_modes,
        };
    }

    fn deinit(self: *const SwapchainSupportInfo, a: std.mem.Allocator) void {
        a.free(self.formats);
        a.free(self.present_modes);
    }
};

pub const Swapchain = struct {
    const FrameData = struct {
        image: Image,
        view: ImageView,

        render_fence: c.VkFence = null,
        present_semaphore: c.VkSemaphore = null,
        render_semaphore: c.VkSemaphore = null,

        pub fn deinit(self: *FrameData, logical_device: *const LogicalDevice) void {
            // image is owned by the swapchain, so we don't need to free it.
            // self.image.deinit();
            self.view.deinit(logical_device);

            logical_device.dispatch.vkDestroyFence.?(logical_device.handle, self.render_fence, null);
            logical_device.dispatch.vkDestroySemaphore.?(logical_device.handle, self.present_semaphore, null);
            logical_device.dispatch.vkDestroySemaphore.?(logical_device.handle, self.render_semaphore, null);
        }
    };
    pub const max_frames_in_flight = 3;

    handle: c.VkSwapchainKHR = null,
    format: Image.Format = .undefined,
    extent: c.VkExtent2D = undefined,

    current_frame: u32 = 0,

    frames: std.BoundedArray(FrameData, max_frames_in_flight),

    pub fn deinit(self: *Swapchain, logical_device: *const LogicalDevice) void {
        self.destroyResources(logical_device);
        logical_device.dispatch.vkDestroySwapchainKHR.?(logical_device.handle, self.handle, null);
    }

    fn destroyResources(self: *Swapchain, logical_device: *const LogicalDevice) void {
        for (self.frames.slice()) |*frame_data| {
            frame_data.deinit(logical_device);
        }
    }

    pub fn acquireNextImage(self: *Swapchain, logical_device: *const LogicalDevice) !u32 {
        var image_index: u32 = 0;
        try checkVk(logical_device.dispatch.vkAcquireNextImageKHR.?(
            logical_device.handle,
            self.handle,
            std.time.ns_per_s,
            self.frames.buffer[self.current_frame].present_semaphore,
            null,
            &image_index,
        ));
        return image_index;
    }

    pub fn getImage(self: *Swapchain, index: usize) !*Image {
        return &self.frames.buffer[index].image;
    }

    pub fn getImageView(self: *Swapchain, index: usize) !*ImageView {
        return &self.frames.buffer[index].image_view;
    }

    pub fn wait(self: *Swapchain, logical_device: *const LogicalDevice, frame_index: u32) !void {
        const frame_data = &self.frames.buffer[frame_index];
        try checkVk(logical_device.dispatch.vkWaitForFences.?(
            logical_device.handle,
            1,
            &frame_data.render_fence,
            c.VK_TRUE,
            std.time.ns_per_s,
        ));
        try checkVk(logical_device.dispatch.vkResetFences.?(
            logical_device.handle,
            1,
            &frame_data.render_fence,
        ));
    }

    pub fn submit(
        self: *Swapchain,
        logical_device: *const LogicalDevice,
        buffers: []const CommandBuffer,
        swapchain_image_index: u32,
    ) !void {
        const frame_data = &self.frames.buffer[self.current_frame];

        const wait_stage: u32 = c.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
        const submit_info = c.VkSubmitInfo{
            .sType = c.VK_STRUCTURE_TYPE_SUBMIT_INFO,
            .waitSemaphoreCount = 1,
            .pWaitSemaphores = &frame_data.present_semaphore,
            .pWaitDstStageMask = &wait_stage,
            .commandBufferCount = @intCast(buffers.len),
            .pCommandBuffers = @ptrCast(buffers.ptr),
            .signalSemaphoreCount = 1,
            .pSignalSemaphores = &frame_data.render_semaphore,
        };

        try checkVk(logical_device.dispatch.vkQueueSubmit.?(
            logical_device.graphics_queue,
            1,
            &submit_info,
            frame_data.render_fence,
        ));

        const present_info = c.VkPresentInfoKHR{
            .sType = c.VK_STRUCTURE_TYPE_PRESENT_INFO_KHR,
            .waitSemaphoreCount = 1,
            .pWaitSemaphores = &frame_data.render_semaphore,
            .swapchainCount = 1,
            .pSwapchains = &self.handle,
            .pImageIndices = &swapchain_image_index,
        };

        try checkVk(
            logical_device.dispatch.vkQueuePresentKHR.?(logical_device.present_queue, &present_info),
        );

        self.current_frame = (self.current_frame + 1) % Swapchain.max_frames_in_flight;
    }

    pub fn configure(self: *Swapchain, logical_device: *LogicalDevice, opts: SwapchainCreateOpts) !void {
        try logical_device.waitIdle();
        self.deinit(logical_device);
        self.* = try createSwapchain(opts);
    }
};

pub const SwapchainCreateOpts = struct {
    physical_device: PhysicalDevice,
    graphics_queue_family: u32,
    present_queue_family: u32,
    logical_device: *const LogicalDevice,
    surface: Surface,
    old_swapchain: c.VkSwapchainKHR = null,
    vsync: bool = false,
    window_width: u32 = 0,
    window_height: u32 = 0,
};

pub fn createSwapchain(opts: SwapchainCreateOpts) !Swapchain {
    var temp = std.heap.stackFallback(2048, core.FailingAllocator.allocator());
    const temp_allocator = temp.get();

    const support_info = try SwapchainSupportInfo.init(temp_allocator, opts.physical_device.handle, opts.surface.handle);

    const format = pickSwapchainFormat(support_info.formats, opts);
    const present_mode = pickSwapchainPresentMode(support_info.present_modes, opts);
    const extent = makeSwapchainExtent(support_info.capabilities, opts);

    const image_count = blk: {
        const desired_count = support_info.capabilities.minImageCount + 1;
        if (support_info.capabilities.maxImageCount > 0) {
            break :blk @min(desired_count, support_info.capabilities.maxImageCount);
        }
        break :blk desired_count;
    };

    var swapchain_info = c.VkSwapchainCreateInfoKHR{
        .sType = c.VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR,
        .surface = opts.surface.handle,
        .minImageCount = image_count,
        .imageFormat = format.toVk(),
        .imageColorSpace = c.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR,
        .imageExtent = extent,
        .imageArrayLayers = 1,
        .imageUsage = c.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | c.VK_IMAGE_USAGE_TRANSFER_DST_BIT,
        .preTransform = support_info.capabilities.currentTransform,
        .compositeAlpha = c.VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
        .presentMode = present_mode,
        .clipped = c.VK_TRUE,
        .oldSwapchain = opts.old_swapchain,
    };

    if (opts.graphics_queue_family != opts.present_queue_family) {
        const queue_family_indices: []const u32 = &.{
            opts.graphics_queue_family,
            opts.present_queue_family,
        };
        swapchain_info.imageSharingMode = c.VK_SHARING_MODE_CONCURRENT;
        swapchain_info.queueFamilyIndexCount = 2;
        swapchain_info.pQueueFamilyIndices = queue_family_indices.ptr;
    } else {
        swapchain_info.imageSharingMode = c.VK_SHARING_MODE_EXCLUSIVE;
    }

    var swapchain: c.VkSwapchainKHR = undefined;
    try checkVk(opts.logical_device.dispatch.vkCreateSwapchainKHR.?(opts.logical_device.handle, &swapchain_info, null, &swapchain));
    errdefer opts.logical_device.dispatch.vkDestroySwapchainKHR.?(opts.logical_device.handle, swapchain, null);
    core.log("Created vulkan swapchain.", .{});

    // Try and fetch the images from the swpachain.
    var swapchain_image_count: u32 = 0;
    try checkVk(opts.logical_device.dispatch.vkGetSwapchainImagesKHR.?(opts.logical_device.handle, swapchain, &swapchain_image_count, null));

    var frames = std.BoundedArray(Swapchain.FrameData, Swapchain.max_frames_in_flight).init(swapchain_image_count) catch return error.oom;
    var swapchain_images = std.BoundedArray(c.VkImage, Swapchain.max_frames_in_flight).init(frames.len) catch return error.oom;
    try checkVk(opts.logical_device.dispatch.vkGetSwapchainImagesKHR.?(opts.logical_device.handle, swapchain, &swapchain_image_count, &swapchain_images.buffer));

    for (&swapchain_images.buffer, 0..) |vk_image, index| {
        frames.buffer[index].image = Image.init(vk_image, format, .{});

        frames.buffer[index].view = try createImageView(
            frames.buffer[index].image,
            opts.logical_device,
            format,
            c.VK_IMAGE_ASPECT_COLOR_BIT,
        );
    }

    const fence_info = c.VkFenceCreateInfo{
        .sType = c.VK_STRUCTURE_TYPE_FENCE_CREATE_INFO,
        .flags = c.VK_FENCE_CREATE_SIGNALED_BIT,
    };

    const semaphore_info = c.VkSemaphoreCreateInfo{
        .sType = c.VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO,
    };

    for (0..Swapchain.max_frames_in_flight) |index| {
        const frame = &frames.buffer[index];
        try checkVk(opts.logical_device.dispatch.vkCreateFence.?(
            opts.logical_device.handle,
            &fence_info,
            null,
            &frame.render_fence,
        ));
        try checkVk(opts.logical_device.dispatch.vkCreateSemaphore.?(
            opts.logical_device.handle,
            &semaphore_info,
            null,
            &frame.present_semaphore,
        ));
        try checkVk(opts.logical_device.dispatch.vkCreateSemaphore.?(
            opts.logical_device.handle,
            &semaphore_info,
            null,
            &frame.render_semaphore,
        ));
    }

    return .{
        .handle = swapchain,
        .frames = frames,
        .format = format,
        .extent = extent,
    };
}

fn createImageView(
    image: Image,
    logical_device: *const LogicalDevice,
    format: Image.Format,
    aspect_flags: c.VkImageAspectFlags,
) !ImageView {
    const view_info = c.VkImageViewCreateInfo{
        .sType = c.VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
        .image = image.handle,
        .viewType = c.VK_IMAGE_VIEW_TYPE_2D,
        .format = format.toVk(),
        .components = .{
            .r = c.VK_COMPONENT_SWIZZLE_IDENTITY,
            .g = c.VK_COMPONENT_SWIZZLE_IDENTITY,
            .b = c.VK_COMPONENT_SWIZZLE_IDENTITY,
            .a = c.VK_COMPONENT_SWIZZLE_IDENTITY,
        },
        .subresourceRange = .{
            .aspectMask = aspect_flags,
            .baseMipLevel = 0,
            .levelCount = 1,
            .baseArrayLayer = 0,
            .layerCount = 1,
        },
    };

    var image_view: c.VkImageView = null;
    try checkVk(c.vkCreateImageView.?(logical_device.handle, &view_info, null, &image_view));
    return .{
        .handle = image_view,
        .image = image,
    };
}

fn pickSwapchainFormat(formats: []const c.VkSurfaceFormatKHR, opts: SwapchainCreateOpts) Image.Format {
    // TODO: Add support for specifying desired format.
    _ = opts;
    for (formats) |format| {
        if (format.format == c.VK_FORMAT_B8G8R8A8_SRGB and
            format.colorSpace == c.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR)
        {
            return @enumFromInt(format.format);
        }
    }

    return @enumFromInt(formats[0].format);
}

fn pickSwapchainPresentMode(modes: []const c.VkPresentModeKHR, opts: SwapchainCreateOpts) c.VkPresentModeKHR {
    if (opts.vsync == false) {
        // Prefer immediate mode if present.
        for (modes) |mode| {
            if (mode == c.VK_PRESENT_MODE_IMMEDIATE_KHR) {
                return mode;
            }
        }
        core.log("Immediate present mode is not possible. Falling back to vsync", .{});
    }

    // Prefer triple buffering if possible.
    // for (modes) |mode| {
    //     if (mode == c.VK_PRESENT_MODE_MAILBOX_KHR and opts.triple_buffer) {
    //         return mode;
    //     }
    // }

    // If nothing else is present, FIFO is guaranteed to be available by the specs.
    return c.VK_PRESENT_MODE_FIFO_KHR;
}

fn makeSwapchainExtent(capabilities: c.VkSurfaceCapabilitiesKHR, opts: SwapchainCreateOpts) c.VkExtent2D {
    if (capabilities.currentExtent.width != std.math.maxInt(u32)) {
        return capabilities.currentExtent;
    }

    var extent = c.VkExtent2D{
        .width = opts.window_width,
        .height = opts.window_height,
    };

    extent.width = @max(capabilities.minImageExtent.width, @min(capabilities.maxImageExtent.width, extent.width));
    extent.height = @max(capabilities.minImageExtent.height, @min(capabilities.maxImageExtent.height, extent.height));

    return extent;
}

pub const LogicalDevice = struct {
    handle: c.VkDevice = null,
    dispatch: c.VolkDeviceTable = undefined,
    graphics_queue: c.VkQueue = null,
    present_queue: c.VkQueue = null,
    compute_queue: c.VkQueue = null,
    transfer_queue: c.VkQueue = null,

    pub fn waitIdle(self: *LogicalDevice) !void {
        try checkVk(self.dispatch.vkDeviceWaitIdle.?(self.handle));
    }
};

pub const LogicalDeviceCreateOpts = struct {
    /// The physical device.
    physical_device: *const PhysicalDevice,
    /// The logical device features.
    features: c.VkPhysicalDeviceFeatures = .{},
    /// Optional pnext chain for VkDeviceCreateInfo.
    pnext: ?*anyopaque = null,
};

pub fn createLogicalDevice(allocator: std.mem.Allocator, opts: LogicalDeviceCreateOpts) !LogicalDevice {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var temp = std.heap.stackFallback(1024, arena.allocator());
    const temp_allocator = temp.get();

    var queue_create_infos = std.ArrayListUnmanaged(c.VkDeviceQueueCreateInfo){};
    const queue_priorities: f32 = 1.0;

    var queue_family_set = std.AutoArrayHashMapUnmanaged(u32, void){};
    try queue_family_set.put(temp_allocator, opts.physical_device.graphics_queue_family, {});
    try queue_family_set.put(temp_allocator, opts.physical_device.present_queue_family, {});
    try queue_family_set.put(temp_allocator, opts.physical_device.compute_queue_family, {});
    try queue_family_set.put(temp_allocator, opts.physical_device.transfer_queue_family, {});

    var qfi_iter = queue_family_set.iterator();
    try queue_create_infos.ensureTotalCapacity(temp_allocator, queue_family_set.count());
    while (qfi_iter.next()) |qfi| {
        try queue_create_infos.append(temp_allocator, c.VkDeviceQueueCreateInfo{
            .sType = c.VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
            .queueFamilyIndex = qfi.key_ptr.*,
            .queueCount = 1,
            .pQueuePriorities = &queue_priorities,
        });
    }

    const device_extensions: []const [*c]const u8 = &.{
        "VK_KHR_swapchain",
    };

    var features13 = c.VkPhysicalDeviceVulkan13Features{
        .sType = c.VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES,
        .synchronization2 = c.VK_TRUE,
        .dynamicRendering = c.VK_TRUE,
    };

    var features12 = c.VkPhysicalDeviceVulkan12Features{
        .sType = c.VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES,
        .bufferDeviceAddress = c.VK_TRUE,
        .descriptorIndexing = c.VK_TRUE,
        .pNext = @ptrCast(&features13),
    };

    var features2 = c.VkPhysicalDeviceFeatures2{
        .sType = c.VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2,
        .pNext = @ptrCast(&features12),
    };

    const device_info = c.VkDeviceCreateInfo{
        .sType = c.VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
        .pNext = &features2,
        .queueCreateInfoCount = @as(u32, @intCast(queue_create_infos.items.len)),
        .pQueueCreateInfos = queue_create_infos.items.ptr,
        .enabledLayerCount = 0,
        .ppEnabledLayerNames = null,
        .enabledExtensionCount = @as(u32, @intCast(device_extensions.len)),
        .ppEnabledExtensionNames = device_extensions.ptr,
        .pEnabledFeatures = null,
    };

    var device: c.VkDevice = null;
    try checkVk(c.vkCreateDevice.?(opts.physical_device.handle, &device_info, null, &device));

    var table: c.VolkDeviceTable = .{};
    c.volkLoadDeviceTable(&table, device);

    var graphics_queue: c.VkQueue = null;
    table.vkGetDeviceQueue.?(device, opts.physical_device.graphics_queue_family, 0, &graphics_queue);
    var present_queue: c.VkQueue = null;
    table.vkGetDeviceQueue.?(device, opts.physical_device.present_queue_family, 0, &present_queue);
    var compute_queue: c.VkQueue = null;
    table.vkGetDeviceQueue.?(device, opts.physical_device.compute_queue_family, 0, &compute_queue);
    var transfer_queue: c.VkQueue = null;
    table.vkGetDeviceQueue.?(device, opts.physical_device.transfer_queue_family, 0, &transfer_queue);

    return .{
        .handle = device,
        .dispatch = table,
        .graphics_queue = graphics_queue,
        .present_queue = present_queue,
        .compute_queue = compute_queue,
        .transfer_queue = transfer_queue,
    };
}

pub fn destroyLogicalDevice(device: *LogicalDevice) void {
    c.vkDestroyDevice.?(device.handle, null);
}

pub const CommandPool = struct {
    handle: c.VkCommandPool = null,
    queue_family: u32 = PhysicalDevice.invalid_queue,
};

pub const CommandPoolCreateOpts = struct {
    logical_device: *const LogicalDevice,
    queue_family: u32,
    flags: c.VkCommandPoolCreateFlags = c.VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
};

pub fn createCommandPool(opts: CommandPoolCreateOpts) !CommandPool {
    const pool_info = c.VkCommandPoolCreateInfo{
        .sType = c.VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        .queueFamilyIndex = opts.queue_family,
        .flags = opts.flags,
    };

    var command_pool: c.VkCommandPool = null;
    try checkVk(opts.logical_device.dispatch.vkCreateCommandPool.?(opts.logical_device.handle, &pool_info, null, &command_pool));
    return .{
        .handle = command_pool,
        .queue_family = opts.queue_family,
    };
}

pub fn destroyCommandPool(device: *LogicalDevice, pool: *CommandPool) void {
    device.dispatch.vkDestroyCommandPool.?(device.handle, pool.handle, null);
}

pub const CommandBuffer = struct {
    handle: c.VkCommandBuffer = null,
    command_pool: *const CommandPool,

    pub fn reset(self: *CommandBuffer, logical_device: *const LogicalDevice) !void {
        try checkVk(logical_device.dispatch.vkResetCommandBuffer.?(self.handle, 0));
    }

    pub fn begin(self: *CommandBuffer, logical_device: *const LogicalDevice, opts: CommandBufferBeginOpts) !void {
        const begin_info = c.VkCommandBufferBeginInfo{
            .sType = c.VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
            .flags = @bitCast(opts.flags),
            .pInheritanceInfo = null,
        };
        try checkVk(logical_device.dispatch.vkBeginCommandBuffer.?(self.handle, &begin_info));
    }

    pub fn end(self: *CommandBuffer, logical_device: *const LogicalDevice) !void {
        try checkVk(logical_device.dispatch.vkEndCommandBuffer.?(self.handle));
    }
};

pub const CommandBufferCreateOpts = struct {
    logical_device: *const LogicalDevice,
    command_pool: *const CommandPool,
    level: enum { primary, secondary } = .primary,
};

pub const CommandBufferBeginOpts = struct {
    flags: packed struct(u32) {
        one_time: bool = true,
        render_pass: bool = false,
        simultaneous_use: bool = false,
        _: u29 = 0,
    } = .{},
};

pub fn allocateCommandBuffer(opts: CommandBufferCreateOpts) !CommandBuffer {
    const alloc_info = c.VkCommandBufferAllocateInfo{
        .sType = c.VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        .commandPool = opts.command_pool.handle,
        .level = switch (opts.level) {
            .primary => c.VK_COMMAND_BUFFER_LEVEL_PRIMARY,
            .secondary => c.VK_COMMAND_BUFFER_LEVEL_SECONDARY,
        },
        .commandBufferCount = 1,
    };

    var command_buffer: c.VkCommandBuffer = null;
    try checkVk(opts.logical_device.dispatch.vkAllocateCommandBuffers.?(opts.logical_device.handle, &alloc_info, &command_buffer));
    return .{
        .handle = command_buffer,
        .command_pool = opts.command_pool,
    };
}

pub fn freeCommandBuffer(device: *LogicalDevice, buffer: *CommandBuffer) void {
    device.dispatch.vkFreeCommandBuffers.?(device.handle, buffer.command_pool.handle, 1, &buffer.handle);
}

pub const Image = struct {
    handle: c.VkImage = null,
    memory: c.VkDeviceMemory = null,
    format: Format = .undefined,
    extent: c.VkExtent3D = undefined,

    pub fn init(handle: c.VkImage, format: Format, extent: c.VkExtent3D) Image {
        return .{
            .handle = handle,
            .format = format,
            .extent = extent,
        };
    }

    pub fn deinit(self: *Image, logical_device: *const LogicalDevice) void {
        logical_device.dispatch.vkDestroyImage.?(logical_device.handle, self.handle, null);
        logical_device.dispatch.vkFreeMemory.?(logical_device.handle, self.memory, null);
    }

    pub const Format = enum(u32) {
        undefined = 0,
        r4g4_unorm_pack8,
        r4g4b4a4_unorm_pack16,
        b4g4r4a4_unorm_pack16,
        r5g6b5_unorm_pack16,
        b5g6r5_unorm_pack16,
        r5g5b5a1_unorm_pack16,
        b5g5r5a1_unorm_pack16,
        a1r5g5b5_unorm_pack16,
        r8_unorm,
        r8_snorm,
        r8_uscaled,
        r8_sscaled,
        r8_uint,
        r8_sint,
        r8_srgb,
        r8g8_unorm,
        r8g8_snorm,
        r8g8_uscaled,
        r8g8_sscaled,
        r8g8_uint,
        r8g8_sint,
        r8g8_srgb,
        r8g8b8_unorm,
        r8g8b8_snorm,
        r8g8b8_uscaled,
        r8g8b8_sscaled,
        r8g8b8_uint,
        r8g8b8_sint,
        r8g8b8_srgb,
        b8g8r8_unorm,
        b8g8r8_snorm,
        b8g8r8_uscaled,
        b8g8r8_sscaled,
        b8g8r8_uint,
        b8g8r8_sint,
        b8g8r8_srgb,
        r8g8b8a8_unorm,
        r8g8b8a8_snorm,
        r8g8b8a8_uscaled,
        r8g8b8a8_sscaled,
        r8g8b8a8_uint,
        r8g8b8a8_sint,
        r8g8b8a8_srgb,
        b8g8r8a8_unorm,
        b8g8r8a8_snorm,
        b8g8r8a8_uscaled,
        b8g8r8a8_sscaled,
        b8g8r8a8_uint,
        b8g8r8a8_sint,
        b8g8r8a8_srgb,
        a8b8g8r8_unorm_pack32,
        a8b8g8r8_snorm_pack32,
        a8b8g8r8_uscaled_pack32,
        a8b8g8r8_sscaled_pack32,
        a8b8g8r8_uint_pack32,
        a8b8g8r8_sint_pack32,
        a8b8g8r8_srgb_pack32,
        a2r10g10b10_unorm_pack32,
        a2r10g10b10_snorm_pack32,
        a2r10g10b10_uscaled_pack32,
        a2r10g10b10_sscaled_pack32,
        a2r10g10b10_uint_pack32,
        a2r10g10b10_sint_pack32,
        a2b10g10r10_unorm_pack32,
        a2b10g10r10_snorm_pack32,
        a2b10g10r10_uscaled_pack32,
        a2b10g10r10_sscaled_pack32,
        a2b10g10r10_uint_pack32,
        a2b10g10r10_sint_pack32,
        r16_unorm,
        r16_snorm,
        r16_uscaled,
        r16_sscaled,
        r16_uint,
        r16_sint,
        r16_sfloat,
        r16g16_unorm,
        r16g16_snorm,
        r16g16_uscaled,
        r16g16_sscaled,
        r16g16_uint,
        r16g16_sint,
        r16g16_sfloat,
        r16g16b16_unorm,
        r16g16b16_snorm,
        r16g16b16_uscaled,
        r16g16b16_sscaled,
        r16g16b16_uint,
        r16g16b16_sint,
        r16g16b16_sfloat,
        r16g16b16a16_unorm,
        r16g16b16a16_snorm,
        r16g16b16a16_uscaled,
        r16g16b16a16_sscaled,
        r16g16b16a16_uint,
        r16g16b16a16_sint,
        r16g16b16a16_sfloat,
        r32_uint,
        r32_sint,
        r32_sfloat,
        r32g32_uint,
        r32g32_sint,
        r32g32_sfloat,
        r32g32b32_uint,
        r32g32b32_sint,
        r32g32b32_sfloat,
        r32g32b32a32_uint,
        r32g32b32a32_sint,
        r32g32b32a32_sfloat,
        r64_uint,
        r64_sint,
        r64_sfloat,
        r64g64_uint,
        r64g64_sint,
        r64g64_sfloat,
        r64g64b64_uint,
        r64g64b64_sint,
        r64g64b64_sfloat,
        r64g64b64a64_uint,
        r64g64b64a64_sint,
        r64g64b64a64_sfloat,
        b10g11r11_ufloat_pack32,
        e5b9g9r9_ufloat_pack32,
        d16_unorm,
        x8_d24_unorm_pack32,
        d32_sfloat,
        s8_uint,
        d16_unorm_s8_uint,
        d24_unorm_s8_uint,
        d32_sfloat_s8_uint,
        bc1_rgb_unorm_block,
        bc1_rgb_srgb_block,
        bc1_rgba_unorm_block,
        bc1_rgba_srgb_block,
        bc2_unorm_block,
        bc2_srgb_block,
        bc3_unorm_block,
        bc3_srgb_block,
        bc4_unorm_block,
        bc4_snorm_block,
        bc5_unorm_block,
        bc5_snorm_block,
        bc6h_ufloat_block,
        bc6h_sfloat_block,
        bc7_unorm_block,
        bc7_srgb_block,
        etc2_r8g8b8_unorm_block,
        etc2_r8g8b8_srgb_block,
        etc2_r8g8b8a1_unorm_block,
        etc2_r8g8b8a1_srgb_block,
        etc2_r8g8b8a8_unorm_block,
        etc2_r8g8b8a8_srgb_block,
        eac_r11_unorm_block,
        eac_r11_snorm_block,
        eac_r11g11_unorm_block,
        eac_r11g11_snorm_block,
        astc_4x4_unorm_block,
        astc_4x4_srgb_block,
        astc_5x4_unorm_block,
        astc_5x4_srgb_block,
        astc_5x5_unorm_block,
        astc_5x5_srgb_block,
        astc_6x5_unorm_block,
        astc_6x5_srgb_block,
        astc_6x6_unorm_block,
        astc_6x6_srgb_block,
        astc_8x5_unorm_block,
        astc_8x5_srgb_block,
        astc_8x6_unorm_block,
        astc_8x6_srgb_block,
        astc_8x8_unorm_block,
        astc_8x8_srgb_block,
        astc_10x5_unorm_block,
        astc_10x5_srgb_block,
        astc_10x6_unorm_block,
        astc_10x6_srgb_block,
        astc_10x8_unorm_block,
        astc_10x8_srgb_block,
        astc_10x10_unorm_block,
        astc_10x10_srgb_block,
        astc_12x10_unorm_block,
        astc_12x10_srgb_block,
        astc_12x12_unorm_block,
        astc_12x12_srgb_block,
        _,

        pub fn toVk(self: Format) c.VkFormat {
            return @intFromEnum(self);
        }

        pub fn fromVk(format: c.VkFormat) Format {
            return @enumFromInt(format);
        }
    };

    pub const Layout = enum(u32) {
        undefined = 0,
        general,
        color_attachment_optimal,
        depth_stencil_attachment_optimal,
        depth_stencil_read_only_optimal,
        shader_read_only_optimal,
        transfer_src_optimal,
        transfer_dst_optimal,
        preinitialised,
        present = c.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
        _,

        pub fn toVk(self: Layout) c.VkImageLayout {
            return switch (self) {
                .undefined => c.VK_IMAGE_LAYOUT_UNDEFINED,
                .general => c.VK_IMAGE_LAYOUT_GENERAL,
                .color_attachment_optimal => c.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                .depth_stencil_attachment_optimal => c.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                .depth_stencil_read_only_optimal => c.VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,
                .shader_read_only_optimal => c.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                .transfer_src_optimal => c.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                .transfer_dst_optimal => c.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                .preinitialised => c.VK_IMAGE_LAYOUT_PREINITIALIZED,
                else => |layout| @intFromEnum(layout),
            };
        }

        pub fn fromVk(layout: c.VkImageLayout) Layout {
            return switch (layout) {
                c.VK_IMAGE_LAYOUT_UNDEFINED => .undefined,
                c.VK_IMAGE_LAYOUT_GENERAL => .general,
                c.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL => .color_attachment_optimal,
                c.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL => .depth_stencil_attachment_optimal,
                c.VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL => .depth_stencil_read_only_optimal,
                c.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL => .shader_read_only_optimal,
                c.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL => .transfer_src_optimal,
                c.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL => .transfer_dst_optimal,
                c.VK_IMAGE_LAYOUT_PREINITIALIZED => .preinitialised,
                else => @enumFromInt(layout),
            };
        }
    };
};

pub const ImageView = struct {
    handle: c.VkImageView = null,
    image: Image,

    pub fn deinit(self: *ImageView, logical_device: *const LogicalDevice) void {
        logical_device.dispatch.vkDestroyImageView.?(logical_device.handle, self.handle, null);
    }
};

pub fn transitionImage(command_buffer: *CommandBuffer, logical_device: *const LogicalDevice, image: *Image, old_layout: Image.Layout, new_layout: Image.Layout) !void {
    if (old_layout == new_layout) {
        return;
    }

    const image_aspect_mask: u32 = switch (new_layout) {
        .depth_stencil_attachment_optimal => c.VK_IMAGE_ASPECT_DEPTH_BIT,
        else => c.VK_IMAGE_ASPECT_COLOR_BIT,
    };

    const image_barrier = c.VkImageMemoryBarrier2{
        .sType = c.VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2,
        .srcStageMask = c.VK_PIPELINE_STAGE_ALL_COMMANDS_BIT,
        .srcAccessMask = c.VK_ACCESS_2_MEMORY_WRITE_BIT,
        .dstStageMask = c.VK_PIPELINE_STAGE_ALL_COMMANDS_BIT,
        .dstAccessMask = c.VK_ACCESS_2_MEMORY_WRITE_BIT | c.VK_ACCESS_2_MEMORY_READ_BIT,

        .oldLayout = old_layout.toVk(),
        .newLayout = new_layout.toVk(),

        .subresourceRange = imageSubresourceRange(image_aspect_mask),
        .image = image.handle,
    };

    const dependency_info = c.VkDependencyInfo{
        .sType = c.VK_STRUCTURE_TYPE_DEPENDENCY_INFO,
        .imageMemoryBarrierCount = 1,
        .pImageMemoryBarriers = &image_barrier,
    };

    logical_device.dispatch.vkCmdPipelineBarrier2.?(command_buffer.handle, &dependency_info);
}

pub fn imageSubresourceRange(aspect_mask: u32) c.VkImageSubresourceRange {
    return .{
        .aspectMask = aspect_mask,
        .baseMipLevel = 0,
        .levelCount = c.VK_REMAINING_MIP_LEVELS,
        .baseArrayLayer = 0,
        .layerCount = c.VK_REMAINING_ARRAY_LAYERS,
    };
}

pub fn checkVk(result: c.VkResult) !void {
    return switch (result) {
        c.VK_SUCCESS => {},
        c.VK_NOT_READY => error.vk_not_ready,
        c.VK_TIMEOUT => error.vk_timeout,
        c.VK_EVENT_SET => error.vk_event_set,
        c.VK_EVENT_RESET => error.vk_event_reset,
        c.VK_INCOMPLETE => error.vk_incomplete,
        c.VK_ERROR_OUT_OF_HOST_MEMORY => error.vk_error_out_of_host_memory,
        c.VK_ERROR_OUT_OF_DEVICE_MEMORY => error.vk_error_out_of_device_memory,
        c.VK_ERROR_INITIALIZATION_FAILED => error.vk_error_initialization_failed,
        c.VK_ERROR_DEVICE_LOST => error.vk_error_device_lost,
        c.VK_ERROR_MEMORY_MAP_FAILED => error.vk_error_memory_map_failed,
        c.VK_ERROR_LAYER_NOT_PRESENT => error.vk_error_layer_not_present,
        c.VK_ERROR_EXTENSION_NOT_PRESENT => error.vk_error_extension_not_present,
        c.VK_ERROR_FEATURE_NOT_PRESENT => error.vk_error_feature_not_present,
        c.VK_ERROR_INCOMPATIBLE_DRIVER => error.vk_error_incompatible_driver,
        c.VK_ERROR_TOO_MANY_OBJECTS => error.vk_error_too_many_objects,
        c.VK_ERROR_FORMAT_NOT_SUPPORTED => error.vk_error_format_not_supported,
        c.VK_ERROR_FRAGMENTED_POOL => error.vk_error_fragmented_pool,
        c.VK_ERROR_UNKNOWN => error.vk_error_unknown,
        c.VK_ERROR_OUT_OF_POOL_MEMORY => error.vk_error_out_of_pool_memory,
        c.VK_ERROR_INVALID_EXTERNAL_HANDLE => error.vk_error_invalid_external_handle,
        c.VK_ERROR_FRAGMENTATION => error.vk_error_fragmentation,
        c.VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS => error.vk_error_invalid_opaque_capture_address,
        c.VK_PIPELINE_COMPILE_REQUIRED => error.vk_pipeline_compile_required,
        c.VK_ERROR_SURFACE_LOST_KHR => error.vk_error_surface_lost_khr,
        c.VK_ERROR_NATIVE_WINDOW_IN_USE_KHR => error.vk_error_native_window_in_use_khr,
        c.VK_SUBOPTIMAL_KHR => error.vk_suboptimal_khr,
        c.VK_ERROR_OUT_OF_DATE_KHR => error.vk_error_out_of_date_khr,
        c.VK_ERROR_INCOMPATIBLE_DISPLAY_KHR => error.vk_error_incompatible_display_khr,
        c.VK_ERROR_VALIDATION_FAILED_EXT => error.vk_error_validation_failed_ext,
        c.VK_ERROR_INVALID_SHADER_NV => error.vk_error_invalid_shader_nv,
        c.VK_ERROR_IMAGE_USAGE_NOT_SUPPORTED_KHR => error.vk_error_image_usage_not_supported_khr,
        c.VK_ERROR_VIDEO_PICTURE_LAYOUT_NOT_SUPPORTED_KHR => error.vk_error_video_picture_layout_not_supported_khr,
        c.VK_ERROR_VIDEO_PROFILE_OPERATION_NOT_SUPPORTED_KHR => error.vk_error_video_profile_operation_not_supported_khr,
        c.VK_ERROR_VIDEO_PROFILE_FORMAT_NOT_SUPPORTED_KHR => error.vk_error_video_profile_format_not_supported_khr,
        c.VK_ERROR_VIDEO_PROFILE_CODEC_NOT_SUPPORTED_KHR => error.vk_error_video_profile_codec_not_supported_khr,
        c.VK_ERROR_VIDEO_STD_VERSION_NOT_SUPPORTED_KHR => error.vk_error_video_std_version_not_supported_khr,
        c.VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT => error.vk_error_invalid_drm_format_modifier_plane_layout_ext,
        c.VK_ERROR_NOT_PERMITTED_KHR => error.vk_error_not_permitted_khr,
        c.VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT => error.vk_error_full_screen_exclusive_mode_lost_ext,
        c.VK_THREAD_IDLE_KHR => error.vk_thread_idle_khr,
        c.VK_THREAD_DONE_KHR => error.vk_thread_done_khr,
        c.VK_OPERATION_DEFERRED_KHR => error.vk_operation_deferred_khr,
        c.VK_OPERATION_NOT_DEFERRED_KHR => error.vk_operation_not_deferred_khr,
        c.VK_ERROR_COMPRESSION_EXHAUSTED_EXT => error.vk_error_compression_exhausted_ext,
        c.VK_ERROR_INCOMPATIBLE_SHADER_BINARY_EXT => error.vk_error_incompatible_shader_binary_ext,
        else => error.vk_errror_unknown,
    };
}
