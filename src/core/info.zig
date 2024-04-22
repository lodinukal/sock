const root = @import("root");
const std = @import("std");

pub const Backend = enum {
    win32,
};

pub const using_backend: Backend = if (@hasDecl(root, "backend")) root.backend else inferBackend();
fn inferBackend() Backend {
    const builtin = @import("builtin");
    return switch (builtin.target.os.tag) {
        .windows => Backend.win32,
        else => @compileError("Unsupported OS; add a backend to the root file."),
    };
}

pub const app_name: []const u8 = if (@hasDecl(root, "app_name")) root.app_name else "sock_app";
pub const app_version: std.SemanticVersion = if (@hasDecl(root, "app_version")) root.app_version else .{ .major = 0, .minor = 1, .patch = 0 };
