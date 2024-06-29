const root = @import("root");
const std = @import("std");

pub const info = @import("info.zig");

pub fn getInstance() ?*anyopaque {
    return switch (info.using_backend) {
        .win32 => @ptrCast(@import("win32.zig").getInstance()),
    };
}

pub const Window = @import("Window.zig");
pub const FailingAllocator = @import("FailingAllocator.zig");

threadlocal var temp_scratch = std.heap.stackFallback(4096, std.testing.failing_allocator);

pub fn tallocator() std.mem.Allocator {
    // already thread safe
    return temp_scratch.get();
}

pub fn treset() void {
    temp_scratch.fixed_buffer_allocator.reset();
}

pub fn err(comptime fmt: []const u8, args: anytype) void {
    std.debug.panic("Error: " ++ fmt ++ "\n", args);
}

pub fn warn(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("Warning: " ++ fmt ++ "\n", args);
}

pub fn log(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("Info: " ++ fmt ++ "\n", args);
}

pub fn assert(check: bool, comptime fmt: []const u8, args: anytype) void {
    if (!check) {
        err(fmt, args);
    }
}
