const c = @cImport({
    @cDefine("VK_NO_PROTOTYPES", "true");
    @cDefine("VK_EXT_debug_utils", "true");
    @cInclude("vulkan/vulkan.h");
    @cInclude("volk.h");
});
const std = @import("std");
const builtin = @import("builtin");
const core = @import("../../core/core.zig");

pub const Context = struct {
    instance: c.VkInstance,
};

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

fn validationLayersSupported(required_layers: []const [*c]const u8) bool {
    var layer_count: u32 = 0;
    try vkCheck(
        c.vkEnumerateInstanceLayerProperties.?(&layer_count, null),
        "failed to enumerate instance layers (get layer count)",
        .{},
    );

    var layers = std.ArrayList(c.VkLayerProperties).initCapacity(
        std.heap.c_allocator,
        layer_count,
    ) catch return false;
    defer layers.deinit();
    layers.resize(layer_count) catch return false;

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

var context: struct {
    instance: c.VkInstance = null,
    messenger: c.VkDebugUtilsMessengerEXT = null,
    physical_device: c.VkPhysicalDevice = null,
} = .{};

fn suitableDevice(device: c.VkPhysicalDevice) bool {
    var properties = c.VkPhysicalDeviceProperties{};
    c.vkGetPhysicalDeviceProperties.?(device, &properties);

    // check
    var got_13 = c.VkPhysicalDeviceVulkan13Features{
        .sType = c.VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES,
    };
    c.vkGetPhysicalDeviceFeatures.?(device, @ptrCast(&got_13));
    var got_12 = c.VkPhysicalDeviceVulkan12Features{
        .sType = c.VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES,
    };
    c.vkGetPhysicalDeviceFeatures.?(device, @ptrCast(&got_12));

    const supported = (got_13.dynamicRendering == c.VK_TRUE and got_13.synchronization2 == c.VK_TRUE and
        got_12.bufferDeviceAddress == c.VK_TRUE and got_12.descriptorIndexing == c.VK_TRUE);
    return properties.deviceType == c.VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU and supported;
}

fn pickPhysicalDevice() void {
    var device_count: u32 = 0;
    try vkCheck(
        c.vkEnumeratePhysicalDevices.?(context.instance, &device_count, null),
        "failed to enumerate physical devices (get device count)",
        .{},
    );
    if (device_count == 0) {
        core.err("failed to find GPUs with Vulkan support", .{});
    }

    var devices = std.ArrayList(c.VkPhysicalDevice).initCapacity(
        std.heap.c_allocator,
        device_count,
    ) catch return;
    defer devices.deinit();
    devices.resize(device_count) catch unreachable;

    try vkCheck(
        c.vkEnumeratePhysicalDevices.?(context.instance, &device_count, devices.items.ptr),
        "failed to enumerate physical devices (get devices buffer)",
        .{},
    );

    for (devices.items) |device| {
        const suitable = suitableDevice(device);
        if (suitable) {
            context.physical_device = device;
            break;
        }
    }

    core.assert(context.physical_device != null, "failed to find a suitable GPU", .{});
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

    var temp_required_extensions = core.Scratch(512){};
    temp_required_extensions.init();

    const required_extensions = getRequiredExtensions(temp_required_extensions.allocator(), validation) catch unreachable;

    const validation_layers = getValidationLayers();
    if (validation and !validationLayersSupported(validation_layers)) {
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

    var extensions = std.ArrayList(c.VkExtensionProperties).initCapacity(
        std.heap.c_allocator,
        extension_count,
    ) catch return error.OutOfMemory;
    defer extensions.deinit();
    extensions.resize(extension_count) catch unreachable;

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

    pickPhysicalDevice();
}

pub fn deinit() void {
    if (context.messenger) |messenger|
        c.vkDestroyDebugUtilsMessengerEXT.?(context.instance, messenger, null);
    c.vkDestroyInstance.?(context.instance, null);
    // though this is not strictly necessary, it's good practice to clean up
    c.volkFinalize();
}
