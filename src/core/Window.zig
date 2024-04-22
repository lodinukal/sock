const std = @import("std");

internal_window: InternalWindow,
should_close: bool = false,

pub const Properties = struct {
    title: []const u8,
    width: u32 = 800,
    height: u32 = 600,
};

pub const Style = struct {
    resizable: bool = true,
    visible: bool = true,
    caption: bool = true,
    system_menu: bool = true,
};

pub const WindowError = std.mem.Allocator.Error || error{};

const InternalWindow = switch (@import("core.zig").info.using_backend) {
    .win32 => @import("win32.zig").Window,
};

const Self = @This();

pub fn create(allocator: std.mem.Allocator, properties: Properties, style: Style) WindowError!*Self {
    const owned_window = try allocator.create(Self);
    try owned_window.init(properties, style);
    return owned_window;
}

pub fn init(window: *Self, properties: Properties, style: Style) WindowError!void {
    window.internal_window.init(properties, style);
}

pub fn destroy(window: *Self, allocator: std.mem.Allocator) void {
    window.deinit();
    allocator.destroy(window);
}

pub fn deinit(window: *Self) void {
    window.internal_window.deinit();
}

/// returns false if the application should close
pub fn pumpMessages() bool {
    return InternalWindow.pumpMessages();
}

/// returns true if the window should close
pub fn shouldClose(window: *Self) bool {
    return window.should_close;
}

/// gets the native window
pub fn getNativeWindow(window: *Self) *anyopaque {
    return window.internal_window.getNativeWindow();
}

/// gets the dimensions of the window
pub fn getDimensions(window: *Self) struct { u32, u32 } {
    return window.internal_window.getDimensions();
}
