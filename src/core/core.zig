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

threadlocal var temp_memory = std.mem.zeroes([4096]u8);
threadlocal var temp_allocator: ?std.heap.FixedBufferAllocator = null;

pub fn tallocator() std.mem.Allocator {
    if (temp_allocator == null) {
        temp_allocator = std.heap.FixedBufferAllocator.init(&temp_memory);
    }
    // already thread safe
    return temp_allocator.?.allocator();
}

pub fn treset() void {
    temp_allocator.?.reset();
}
