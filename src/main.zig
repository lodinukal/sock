const std = @import("std");
const sock = @import("sock.zig");

const vulkan = @import("graphics/vulkan/vulkan.zig");

pub const app_name = "Graphics";

fn rendering(allocator: std.mem.Allocator, window: *sock.core.Window) !void {
    const ctx = try vulkan.init(allocator);
    defer vulkan.deinit();

    const surface = try vulkan.createSurface(ctx, window);
    defer vulkan.destroySurface(ctx, &surface);

    var pd = try vulkan.selectPhysicalDevice(ctx, allocator, .{
        .surface = &surface,
    });
    defer vulkan.destroyPhysicalDevice(allocator, &pd);

    var ld = try vulkan.createLogicalDevice(allocator, .{
        .physical_device = &pd,
        .features = .{},
    });
    defer vulkan.destroyLogicalDevice(&ld);

    const window_size = window.getDimensions();
    var swap = try vulkan.createSwapchain(allocator, .{
        .physical_device = pd,
        .logical_device = &ld,
        .surface = surface,
        .window_width = window_size.@"0",
        .window_height = window_size.@"1",
        .graphics_queue_family = pd.graphics_queue_family,
        .present_queue_family = pd.present_queue_family,
    });
    defer vulkan.destroySwapchain(allocator, &swap, &ld);

    const FrameData = struct {
        command_pool: vulkan.CommandPool,
        command_buffer: vulkan.CommandBuffer,
    };
    var frame_data = std.BoundedArray(FrameData, 2).init(2) catch unreachable;
    inline for (0..2) |i| {
        const command_pool = try vulkan.createCommandPool(.{
            .logical_device = &ld,
            .queue_family = pd.graphics_queue_family,
        });
        frame_data.buffer[i].command_pool = command_pool;
        const command_buffer = try vulkan.allocateCommandBuffer(.{
            .logical_device = &ld,
            .command_pool = &frame_data.buffer[i].command_pool,
        });
        frame_data.buffer[i].command_buffer = command_buffer;
    }
    defer for (&frame_data.buffer) |*fd| {
        vulkan.freeCommandBuffer(&ld, &fd.command_buffer);
        vulkan.destroyCommandPool(&ld, &fd.command_pool);
    };

    var frame_index: usize = 0;
    while (sock.core.Window.pumpMessages() and !window.shouldClose()) {
        std.time.sleep(std.time.ns_per_ms * 16);
        frame_index = (frame_index + 1) % 2;
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer if (gpa.deinit() == .leak) {
        @import("std").debug.print("Memory leak detected\n", .{});
    };

    const window = try sock.core.Window.create(allocator, .{ .title = "Graphics" }, .{});
    defer window.destroy(allocator);

    try rendering(allocator, window);
}
