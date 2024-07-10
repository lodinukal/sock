const std = @import("std");
const ast = @import("ast.zig");

pub const Error = struct {
    location: ?ast.Location,
    message: []const u8,
    kind: ReportKind,

    pub fn format(self: Error, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("[{}] {}: {s}", .{ self.kind, self.location, self.message });
    }
};

const Reporter = @This();

allocator: std.mem.Allocator,
source: []const u8 = &.{},
reports: std.ArrayListUnmanaged(Error) = .{},
extra_strings: std.ArrayListUnmanaged([]const u8) = .{},
warning_count: u32 = 0,
error_count: u32 = 0,
info_count: u32 = 0,

pub const ReportKind = enum {
    err,
    warning,
    info,

    pub inline fn getAsciiColourStart(self: ReportKind) []const u8 {
        switch (self) {
            .err => return "\x1b[31m",
            .warning => return "\x1b[33m",
            .info => return "\x1b[34m",
        }
    }
};

pub fn deinit(self: *Reporter) void {
    for (self.reports.items) |got| {
        self.allocator.free(got.message);
    }
    self.reports.deinit(self.allocator);
    for (self.extra_strings.items) |got| {
        self.allocator.free(got);
    }
    self.extra_strings.deinit(self.allocator);
}

pub fn push(
    self: *Reporter,
    kind: ReportKind,
    location: ?ast.Location,
    comptime fmt: []const u8,
    args: anytype,
) !void {
    try self.reports.append(self.allocator, .{
        .location = location,
        .message = try std.fmt.allocPrint(self.allocator, fmt, args),
        .kind = kind,
    });
    switch (kind) {
        .err => self.error_count += 1,
        .warning => self.warning_count += 1,
        .info => self.info_count += 1,
    }
}

pub fn format(self: Reporter, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    const ASCII_END = "\x1b[0m";
    for (self.reports.items) |got| {
        var temp = std.mem.zeroes([1000]u8);
        var fba = std.heap.FixedBufferAllocator.init(&temp);
        const temp_allocator = fba.allocator();

        try writer.print("{s}{s}" ++ ASCII_END ++ "\n", .{ got.kind.getAsciiColourStart(), got.message });
        if (got.location == null) {
            continue;
        }
        const location = got.location.?;
        try writer.print("--> {}\n", .{location});

        const line_as_string = std.fmt.allocPrint(temp_allocator, "{d}", .{location.begin.line + 1}) catch unreachable;
        const pad_amount = line_as_string.len + 2;

        const whole_line = extractTokenLine(self.source, location.begin.line) orelse continue;
        for (0..pad_amount) |_| {
            try writer.print(" ", .{});
        }
        try writer.print("|\n", .{});
        try writer.print(" {} | {s}\n", .{ location.begin.line + 1, whole_line });

        for (0..pad_amount) |_| {
            try writer.print(" ", .{});
        }
        try writer.print("| ", .{});
        for (0..location.begin.column) |_| {
            try writer.print(" ", .{});
        }
        // ascii green
        const ASCII_GREEN = "\x1b[32m";
        try writer.writeAll(ASCII_GREEN);
        for (location.begin.column..location.end.column) |_| {
            try writer.print("^", .{});
        }
        try writer.writeAll(ASCII_END);
        try writer.print("\n", .{});
        for (0..pad_amount) |_| {
            try writer.print(" ", .{});
        }
        try writer.print("|\n", .{});
    }
}

pub fn extractTokenLine(source: []const u8, line_target: u32) ?[]const u8 {
    var line_it = std.mem.splitScalar(u8, source, '\n');
    var index: u32 = 0;
    while (line_it.next()) |line| {
        if (index == line_target) {
            return line;
        }
        index += 1;
    }
    return null;
}

pub fn allocPrint(self: *Reporter, comptime fmt: []const u8, args: anytype) ![]const u8 {
    const string = try std.fmt.allocPrint(self.allocator, fmt, args);
    try self.extra_strings.append(self.allocator, string);
    return string;
}
