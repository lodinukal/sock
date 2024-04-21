const c = @cImport({
    @cDefine("VK_NO_PROTOTYPES", "true");
    @cDefine("VK_EXT_debug_utils", "true");
    if (core.using_backend == .win32) {
        @cDefine("VK_USE_PLATFORM_WIN32_KHR", "true");
    }
    @cInclude("vulkan/vulkan.h");
    @cInclude("volk.h");
});
const std = @import("std");
const builtin = @import("builtin");
const core = @import("../../core/core.zig");

const ApiVersion = packed struct(u32) {
    variant: u7,
    major: u6,
    minor: u10,
    patch: u9,
};

pub fn vkCheck(result: c.VkResult, comptime msg: []const u8, extra: anytype) !void {
    if (result != c.VK_SUCCESS) {
        core.err("vulkan: {}\n" ++ msg, .{result} ++ extra);
    }
}

fn getRequiredExtensions(allocator: std.mem.Allocator, validation: bool) !std.ArrayListUnmanaged([*c]const u8) {
    var extensions = std.ArrayListUnmanaged([*c]const u8){};
    try extensions.appendSlice(allocator, switch (core.using_backend) {
        .win32 => &.{ "VK_KHR_surface", "VK_KHR_win32_surface" },
    });
    if (validation) {
        try extensions.appendSlice(allocator, &.{c.VK_EXT_DEBUG_UTILS_EXTENSION_NAME});
    }
    return extensions;
}

fn getValidationLayers() []const [*c]const u8 {
    return comptime switch (builtin.mode) {
        .Debug => &.{"VK_LAYER_KHRONOS_validation"},
        else => *.{},
    };
}

fn validationLayersSupported(required_layers: []const [*c]const u8) !bool {
    var layer_count: u32 = 0;
    try vkCheck(
        c.vkEnumerateInstanceLayerProperties.?(&layer_count, null),
        "failed to enumerate instance layers (get layer count)",
        .{},
    );

    var temp = std.heap.stackFallback(@sizeOf(c.VkLayerProperties) * 100, std.heap.c_allocator);
    const allocator = temp.get();

    var layers = std.ArrayListUnmanaged(c.VkLayerProperties).initCapacity(
        allocator,
        layer_count,
    ) catch return error.OutOfMemory;
    defer layers.deinit(allocator);
    layers.resize(allocator, layer_count) catch return false;

    try vkCheck(
        c.vkEnumerateInstanceLayerProperties.?(&layer_count, layers.items.ptr),
        "failed to enumerate instance layers (get layers buffer)",
        .{},
    );

    for (required_layers) |required_layer| {
        const required_layer_name = std.mem.span(required_layer);
        var found = false;
        for (layers.items) |layer| {
            const name = std.mem.span(@as([*:0]const u8, @ptrCast(&layer.layerName)));
            if (std.mem.eql(u8, name, required_layer_name)) {
                found = true;
                break;
            }
        }
        if (!found) {
            return false;
        }
    }

    return true;
}

fn debugCallback(
    messageSeverity: c.VkDebugUtilsMessageSeverityFlagBitsEXT,
    messageType: c.VkDebugUtilsMessageTypeFlagsEXT,
    pCallbackData: [*c]const c.VkDebugUtilsMessengerCallbackDataEXT,
    pUserData: ?*anyopaque,
) callconv(.C) c.VkBool32 {
    _ = pUserData;
    const message = std.mem.span(@as([*:0]const u8, pCallbackData.*.pMessage));
    const severity = switch (messageSeverity) {
        c.VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT => "VERBOSE",
        c.VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT => "INFO",
        c.VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT => "WARNING",
        c.VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT => "ERROR",
        else => "UNKNOWN",
    };
    const msg_type = switch (messageType) {
        c.VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT => "GENERAL",
        c.VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT => "VALIDATION",
        c.VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT => "PERFORMANCE",
        else => "UNKNOWN",
    };
    if (messageSeverity == c.VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT)
        core.warn(
            "validation layer: {s} ({s}): {s}\n",
            .{ severity, msg_type, message },
        )
    else
        core.err(
            "validation layer: {s} ({s}): {s}\n",
            .{ severity, msg_type, message },
        );

    return c.VK_FALSE;
}

const QueueType = enum {
    graphics,
    transfer,
    compute,

    pub fn toCFlag(self: QueueType) c.VkQueueFlagBits {
        return switch (self) {
            .graphics => c.VK_QUEUE_GRAPHICS_BIT,
            .transfer => c.VK_QUEUE_TRANSFER_BIT,
            .compute => c.VK_QUEUE_COMPUTE_BIT,
        };
    }

    pub fn fromCFlag(flag: c.VkQueueFlagBits) ?QueueType {
        return switch (flag) {
            c.VK_QUEUE_GRAPHICS_BIT => .graphics,
            c.VK_QUEUE_TRANSFER_BIT => .transfer,
            c.VK_QUEUE_COMPUTE_BIT => .compute,
            else => null,
        };
    }
};

var context: struct {
    instance: c.VkInstance = null,
    messenger: c.VkDebugUtilsMessengerEXT = null,
    physical_device: c.VkPhysicalDevice = null,
    logical_device: c.VkDevice = null,

    queues: std.EnumArray(QueueType, c.VkQueue) =
        std.EnumArray(QueueType, c.VkQueue).initFill(null),
} = .{};

fn findQueueFamilies(device: c.VkPhysicalDevice, allocator: std.mem.Allocator) !std.ArrayListUnmanaged(c.VkQueueFamilyProperties) {
    var queue_family_count: u32 = 0;
    c.vkGetPhysicalDeviceQueueFamilyProperties.?(device, &queue_family_count, null);

    var queue_families = std.ArrayListUnmanaged(c.VkQueueFamilyProperties).initCapacity(
        allocator,
        queue_family_count,
    ) catch return error.OutOfMemory;
    queue_families.resize(allocator, queue_family_count) catch unreachable;

    c.vkGetPhysicalDeviceQueueFamilyProperties.?(device, &queue_family_count, queue_families.items.ptr);
    return queue_families;
}

/// 0 is unsupported, anything else is supported
fn deviceRating(device: c.VkPhysicalDevice) !u32 {
    var properties = c.VkPhysicalDeviceProperties{};
    c.vkGetPhysicalDeviceProperties.?(device, &properties);

    // check
    var got_13 = c.VkPhysicalDeviceVulkan13Features{
        .sType = c.VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES,
    };
    var got_12 = c.VkPhysicalDeviceVulkan12Features{
        .sType = c.VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES,
    };
    var physical_features2 = c.VkPhysicalDeviceFeatures2{
        .sType = c.VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2,
    };
    physical_features2.pNext = &got_13;
    got_13.pNext = &got_12;
    c.vkGetPhysicalDeviceFeatures2.?(device, &physical_features2);

    const supported = (got_13.dynamicRendering == c.VK_TRUE and got_13.synchronization2 == c.VK_TRUE and
        got_12.bufferDeviceAddress == c.VK_TRUE and got_12.descriptorIndexing == c.VK_TRUE);

    if (!supported) {
        return 0;
    }

    var temp_queue_families = std.heap.stackFallback(512, std.heap.c_allocator);
    const allocator = temp_queue_families.get();
    var queues = try findQueueFamilies(device, allocator);
    defer queues.deinit(allocator);

    // needs a graphics and a present queue
    var graphics: u32 = 0;
    var transfer: u32 = 0;
    var compute: u32 = 0;
    for (queues.items) |queue| {
        if (queue.queueFlags & c.VK_QUEUE_GRAPHICS_BIT != 0) {
            graphics += 1;
        }
        if (queue.queueFlags & c.VK_QUEUE_TRANSFER_BIT != 0) {
            transfer += 1;
        }
        if (queue.queueFlags & c.VK_QUEUE_COMPUTE_BIT != 0) {
            compute += 1;
        }
    }

    if (graphics == 0 or transfer == 0 or compute == 0) return 0;

    return 1;
}

fn pickPhysicalDevice() !c.VkPhysicalDevice {
    var device_count: u32 = 0;
    try vkCheck(
        c.vkEnumeratePhysicalDevices.?(context.instance, &device_count, null),
        "failed to enumerate physical devices (get device count)",
        .{},
    );
    if (device_count == 0) {
        core.err("failed to find GPUs with Vulkan support", .{});
    }

    var temp = std.heap.stackFallback(@sizeOf(c.VkPhysicalDevice) * 5, std.heap.c_allocator);

    var devices = std.ArrayList(c.VkPhysicalDevice).initCapacity(
        temp.get(),
        device_count,
    ) catch return error.OutOfMemory;
    defer devices.deinit();
    devices.resize(device_count) catch unreachable;

    try vkCheck(
        c.vkEnumeratePhysicalDevices.?(context.instance, &device_count, devices.items.ptr),
        "failed to enumerate physical devices (get devices buffer)",
        .{},
    );

    var max_rating_index: ?usize = null;
    var max_rating: u32 = 0;
    for (devices.items, 0..) |device, i| {
        const rating = try deviceRating(device);
        if (rating > max_rating and rating != 0) {
            max_rating = rating;
            max_rating_index = i;
        }
    }
    core.assert(max_rating_index != null, "failed to find a suitable GPU", .{});
    const best_device: c.VkPhysicalDevice = devices.items[max_rating_index.?];
    return best_device;
}

pub fn init(app_name: []const u8, validation: bool) !void {
    try vkCheck(c.volkInitialize(), "failed to initialise volk", .{});
    const app_info = c.VkApplicationInfo{
        .sType = c.VK_STRUCTURE_TYPE_APPLICATION_INFO,
        .pApplicationName = app_name.ptr,
        .applicationVersion = c.VK_MAKE_VERSION(0, 0, 1),
        .pEngineName = "No Engine",
        .engineVersion = c.VK_MAKE_VERSION(0, 0, 1),
        .apiVersion = @bitCast(ApiVersion{
            .variant = 0,
            .major = 1,
            .minor = 3,
            .patch = 0,
        }),
    };

    var temp_required_extensions = std.heap.stackFallback(512, std.heap.c_allocator);
    const allocator = temp_required_extensions.get();

    var required_extensions = getRequiredExtensions(allocator, validation) catch unreachable;
    defer required_extensions.deinit(allocator);

    const validation_layers = getValidationLayers();
    if (validation and !(try validationLayersSupported(validation_layers))) {
        core.err("validation layers requested, but not available", .{});
        return error.ValidationLayersUnavailable;
    }

    var debug_create_info = c.VkDebugUtilsMessengerCreateInfoEXT{
        .sType = c.VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,
        .messageSeverity = c.VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT |
            c.VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
        .messageType = c.VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT |
            c.VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT |
            c.VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
        .pfnUserCallback = &debugCallback,
    };

    const instance_create_info = c.VkInstanceCreateInfo{
        .sType = c.VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
        .pApplicationInfo = &app_info,
        .enabledExtensionCount = @intCast(required_extensions.items.len),
        .ppEnabledExtensionNames = required_extensions.items.ptr,
        .enabledLayerCount = if (validation) @intCast(validation_layers.len) else 0,
        .ppEnabledLayerNames = validation_layers.ptr,
        .pNext = if (validation) &debug_create_info else null,
    };

    try vkCheck(
        c.vkCreateInstance.?(&instance_create_info, null, &context.instance),
        "failed to create instance",
        .{},
    );

    c.volkLoadInstance(context.instance);

    // check extensions
    var extension_count: u32 = 0;
    try vkCheck(
        c.vkEnumerateInstanceExtensionProperties.?(null, &extension_count, null),
        "failed to enumerate instance extensions (get extension count)",
        .{},
    );

    var temp_extension_properties = std.heap.stackFallback(@sizeOf(c.VkExtensionProperties) * 100, std.heap.c_allocator);
    const allocator_extension_properties = temp_extension_properties.get();

    var extensions = std.ArrayListUnmanaged(c.VkExtensionProperties).initCapacity(
        allocator_extension_properties,
        extension_count,
    ) catch return error.OutOfMemory;
    defer extensions.deinit(allocator_extension_properties);
    extensions.resize(allocator_extension_properties, extension_count) catch unreachable;

    try vkCheck(
        c.vkEnumerateInstanceExtensionProperties.?(null, &extension_count, extensions.items.ptr),
        "failed to enumerate instance extensions (get extensions buffer)",
        .{},
    );

    std.debug.print("available extensions ({}):\n", .{extension_count});
    for (extensions.items) |ext| {
        const name = std.mem.span(@as([*:0]const u8, @ptrCast(&ext.extensionName)));
        std.debug.print("\t{s}\n", .{name});
    }

    context.physical_device = try pickPhysicalDevice();

    // load the queues
    var temp_queue_families = std.heap.stackFallback(512, std.heap.c_allocator);
    const allocator_queue_families = temp_queue_families.get();
    var queue_families = try findQueueFamilies(context.physical_device, allocator_queue_families);
    defer queue_families.deinit(allocator_queue_families);

    // create a logical device
    var queue_create_infos = std.BoundedArray(c.VkDeviceQueueCreateInfo, 3).init(0) catch unreachable;
    var queue_type_to_info_index = std.EnumMap(QueueType, usize).initFull(0);

    const default_queue_priority: [:0]const f32 = &.{1.0};
    for (std.enums.values(QueueType)) |queue_type| {
        // check currently emplaced create infos to find a valid queue
        var found = false;
        const queue_bit = queue_type.toCFlag();
        for (queue_create_infos.slice(), 0..) |*queue_family, index| {
            const original_queue_family = &queue_families.items[queue_family.queueFamilyIndex];
            if (original_queue_family.queueFlags & queue_bit != 0 and original_queue_family.queueCount > 0) {
                queue_family.queueCount += 1;
                original_queue_family.queueCount -= 1;
                found = true;
                queue_type_to_info_index.put(queue_type, index);
                break;
            }
        }
        if (!found) {
            for (queue_families.items, 0..) |*queue_family, family_index| {
                if (queue_family.queueFlags & queue_bit != 0 and queue_family.queueCount > 0) {
                    queue_create_infos.appendAssumeCapacity(c.VkDeviceQueueCreateInfo{
                        .sType = c.VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
                        .queueFamilyIndex = @intCast(family_index),
                        .queueCount = 1,
                        .pQueuePriorities = default_queue_priority.ptr,
                    });
                    queue_family.queueCount -= 1;
                    found = true;
                    queue_type_to_info_index.put(queue_type, queue_create_infos.len - 1);
                    break;
                }
            }
        }
        core.assert(found, "failed to find a suitable queue family for {s}", .{@tagName(queue_type)});
    }

    std.debug.print("queue families:\n", .{});
    for (queue_create_infos.constSlice()) |queue_family| {
        std.debug.print(
            "\tqueue family: {d}, queue count: {d}, priority: {d}\n",
            .{ queue_family.queueFamilyIndex, queue_family.queueCount, queue_family.pQueuePriorities[0] },
        );
    }

    // creating the logical device
    const device_create_info = c.VkDeviceCreateInfo{
        .sType = c.VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
        .queueCreateInfoCount = @intCast(queue_create_infos.len),
        .pQueueCreateInfos = queue_create_infos.constSlice().ptr,
        .enabledExtensionCount = 0,
        .ppEnabledExtensionNames = null,
        .enabledLayerCount = 0,
        .ppEnabledLayerNames = null,
        .pEnabledFeatures = null,
    };

    try vkCheck(
        c.vkCreateDevice.?(context.physical_device, &device_create_info, null, &context.logical_device),
        "failed to create logical device",
        .{},
    );

    // get queues
    for (std.enums.values(QueueType)) |queue_type| {
        var queue: c.VkQueue = null;
        const info_index = queue_type_to_info_index.get(queue_type) orelse {
            core.err("failed to find queue family for {s}", .{@tagName(queue_type)});
            unreachable;
        };
        const info: *c.VkDeviceQueueCreateInfo = &queue_create_infos.buffer[info_index];
        if (info.queueCount > 0) {
            c.vkGetDeviceQueue.?(context.logical_device, info.queueFamilyIndex, info.queueCount - 1, &queue);
            info.queueCount -= 1;
        }
        context.queues.set(queue_type, queue);
    }

    std.debug.print("queues:\n", .{});
    for (std.enums.values(QueueType)) |queue_type| {
        const queue = context.queues.get(queue_type);
        std.debug.print("\t{s}: {?}\n", .{ @tagName(queue_type), queue });
    }

    std.debug.print("vulkan: initialised\n", .{});
}

pub fn deinit() void {
    if (context.logical_device) |logical_device|
        c.vkDestroyDevice.?(logical_device, null);

    if (context.messenger) |messenger|
        c.vkDestroyDebugUtilsMessengerEXT.?(context.instance, messenger, null);
    c.vkDestroyInstance.?(context.instance, null);
    // though this is not strictly necessary, it's good practice to clean up
    c.volkFinalize();
}

pub fn createSurface(window: *core.Window) !c.VkSurfaceKHR {
    switch (core.using_backend) {
        .win32 => {
            const h_wnd = window.getNativeWindow();

            const VkWin32SurfaceCreateInfoKHR = extern struct {
                sType: c.VkStructureType = @import("std").mem.zeroes(c.VkStructureType),
                pNext: ?*const anyopaque = @import("std").mem.zeroes(?*const anyopaque),
                flags: c.VkWin32SurfaceCreateFlagsKHR = @import("std").mem.zeroes(c.VkWin32SurfaceCreateFlagsKHR),
                hinstance: std.os.windows.HINSTANCE,
                hwnd: std.os.windows.HWND,
            };

            const surface_create_info = VkWin32SurfaceCreateInfoKHR{
                .sType = c.VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR,
                .hinstance = @alignCast(@ptrCast(core.getInstance().?)),
                .hwnd = @ptrCast(h_wnd),
            };

            var surface: c.VkSurfaceKHR = null;
            try vkCheck(
                c.vkCreateWin32SurfaceKHR.?(context.instance, @ptrCast(&surface_create_info), null, &surface),
                "failed to create win32 surface",
                .{},
            );

            return surface;
        },
    }
}

pub fn destroySurface(surface: c.VkSurfaceKHR) void {
    c.vkDestroySurfaceKHR.?(context.instance, surface, null);
}
