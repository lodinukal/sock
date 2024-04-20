const std = @import("std");
const sock = @import("sock.zig");

const vulkan = @import("graphics/vulkan/vulkan.zig");

fn rendering() !void {
    try vulkan.init("Graphics", true);
    defer vulkan.deinit();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer if (gpa.deinit() == .leak) {
        @import("std").debug.print("Memory leak detected\n", .{});
    };

    const window = try sock.core.Window.create(allocator, .{ .title = "Graphics" }, .{});
    defer window.destroy(allocator);

    try rendering();

    while (sock.core.Window.pumpMessages() and !window.shouldClose()) {
        std.time.sleep(std.time.ns_per_ms * 16);
    }
}
