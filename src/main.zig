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

    var window_size = window.getDimensions();
    var swap_opts: vulkan.SwapchainCreateOpts = .{
        .physical_device = pd,
        .logical_device = &ld,
        .surface = surface,
        .window_width = window_size.@"0",
        .window_height = window_size.@"1",
        .graphics_queue_family = pd.graphics_queue_family,
        .present_queue_family = pd.present_queue_family,
    };
    var swap = try vulkan.createSwapchain(swap_opts);
    defer swap.deinit(&ld);

    var command_pool = try vulkan.createCommandPool(.{
        .logical_device = &ld,
        .queue_family = pd.graphics_queue_family,
    });
    defer vulkan.destroyCommandPool(&ld, &command_pool);
    var command_buffers = std.BoundedArray(vulkan.CommandBuffer, vulkan.Swapchain.max_frames_in_flight){};
    for (0..command_buffers.buffer.len) |_| {
        command_buffers.appendAssumeCapacity(try vulkan.allocateCommandBuffer(.{
            .logical_device = &ld,
            .command_pool = &command_pool,
        }));
    }
    defer for (&command_buffers.buffer) |*cb| {
        vulkan.freeCommandBuffer(&ld, cb);
    };

    // wait for everything to idle before cleanups
    defer ld.waitIdle() catch {};

    const red_clear_value = vulkan.c.VkClearColorValue{ .float32 = .{ 1.0, 0.0, 0.0, 1.0 } };
    const green_clear_value = vulkan.c.VkClearColorValue{ .float32 = .{ 0.0, 1.0, 0.0, 1.0 } };
    const blue_clear_value = vulkan.c.VkClearColorValue{ .float32 = .{ 0.0, 0.0, 1.0, 1.0 } };

    var frame_index: u32 = 0;
    while (sock.core.Window.pumpMessages() and !window.shouldClose()) {
        const start_time = std.time.nanoTimestamp();

        const new_window_size = window.getDimensions();
        if (new_window_size.@"0" != window_size.@"0" or new_window_size.@"1" != window_size.@"1" and
            new_window_size.@"0" != 0 and new_window_size.@"1" != 0)
        {
            window_size = new_window_size;
            swap_opts.window_width = window_size.@"0";
            swap_opts.window_height = window_size.@"1";
            try swap.configure(&ld, swap_opts);
            continue;
        }
        std.time.sleep(std.time.ns_per_us * 300);

        try swap.wait(&ld, swap.current_frame);
        const index = try swap.acquireNextImage(&ld);
        const image = try swap.getImage(index);

        const command_buffer = &command_buffers.buffer[frame_index];

        try command_buffer.reset(&ld);

        try command_buffer.begin(&ld, .{});

        try vulkan.transitionImage(command_buffer, &ld, image, .undefined, .general);

        ld.dispatch.vkCmdClearColorImage.?(
            command_buffer.handle,
            image.handle,
            vulkan.Image.Layout.general.toVk(),
            switch (index % 3) {
                0 => &red_clear_value,
                1 => &green_clear_value,
                2 => &blue_clear_value,
                else => unreachable,
            },
            1,
            &vulkan.imageSubresourceRange(vulkan.c.VK_IMAGE_ASPECT_COLOR_BIT),
        );

        try vulkan.transitionImage(command_buffer, &ld, image, .general, .present);

        try command_buffer.end(&ld);
        try swap.submit(&ld, &.{command_buffer.*}, index);

        frame_index = (frame_index + 1) % swap.frames.len;

        const end_time = std.time.nanoTimestamp();
        const elapsed_time = end_time - start_time;
        const in_seconds = @divTrunc(elapsed_time, std.time.ns_per_s);
        _ = in_seconds;
        // std.debug.print("FPS: {}\n", .{1.0 / @as(f64, @floatFromInt(in_seconds))});
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
