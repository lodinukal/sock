const root = @import("root");
const std = @import("std");

pub const Backend = enum {
    win32,
};

pub const using_backend: Backend = if (@hasDecl(root, "backend")) root.backend else Backend.win32;
fn inferBackend() Backend {
    const builtin = @import("builtin");
    switch (builtin.target.os.tag) {
        .windows => Backend.win32,
        else => @compileError("Unsupported OS; add a backend to the root file."),
    }
}

pub const Window = @import("Window.zig");

pub fn Scratch(comptime size: u32) type {
    return struct {
        const Self = @This();
        initialised: bool = false,
        buf: [size]u8 = std.mem.zeroes([size]u8),
        fba: std.heap.FixedBufferAllocator = undefined,

        pub fn init(self: *Self) void {
            if (!self.initialised) {
                self.fba = std.heap.FixedBufferAllocator.init(&self.buf);
                self.initialised = true;
            }
        }

        pub fn allocator(self: *Self) std.mem.Allocator {
            self.init();
            return self.fba.allocator();
        }

        pub fn reset(self: *Self) void {
            if (!self.initialised) return;
            self.fba.reset();
        }
    };
}

threadlocal var temp_scratch = Scratch(4096){};

pub fn tallocator() std.mem.Allocator {
    // already thread safe
    return temp_scratch.allocator();
}

pub fn treset() void {
    temp_scratch.reset();
}

pub fn err(comptime fmt: []const u8, args: anytype) void {
    std.debug.panic("Error: " ++ fmt, args);
}

pub fn warn(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("Warning: " ++ fmt, args);
}

pub fn assert(check: bool, comptime fmt: []const u8, args: anytype) void {
    if (!check) {
        err(fmt, args);
    }
}
