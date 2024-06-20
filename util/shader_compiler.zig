const std = @import("std");
const dxc = @import("mach-dxcompiler");

fn showHelp() void {
    std.debug.print(
        \\shader_compiler [options] <input file>
        \\Options:
        \\  -help: Show this help message
        \\  -o <output file>: Output file, defaults to stdout
        \\  -entry <entry point>: Entry point
        \\  -target <target profile>: Target profile, pixel, compute, fragment
        \\  -spirv: Output SPIR-V
    , .{});
}

pub const Profile = enum {
    pixel,
    compute,
    fragment,

    pub fn toArg(self: Profile) [:0]const u8 {
        return switch (self) {
            .pixel => "ps_6_0",
            .compute => "cs_6_0",
            .fragment => "ps_6_0",
        };
    }
};

pub const Flag = enum {
    help,
    output,
    entry,
    target,
    spirv,
};

pub fn main() !void {
    var input_args = try std.process.argsWithAllocator(std.heap.c_allocator);
    defer input_args.deinit();

    _ = input_args.next(); // Skip the program name

    var input_file: ?std.fs.File = null;
    var output_file: std.fs.File = std.io.getStdOut();
    var entry_point: ?[]const u8 = null;
    var spirv: bool = false;
    var target: ?Profile = null;

    while (input_args.next()) |arg| {
        if (arg.len > 2 and arg[0] == '-' and arg[1] == '-') {
            const flag = std.meta.stringToEnum(Flag, arg[2..]) orelse {
                std.debug.print("Unknown flag: {s}\n", .{arg});
                showHelp();
                return;
            };
            switch (flag) {
                .help => {
                    showHelp();
                    return;
                },
                .output => {
                    const next_arg = input_args.next() orelse {
                        std.debug.print("Expected output file\n", .{});
                        showHelp();
                        return;
                    };
                    output_file = std.fs.cwd().openFile(next_arg, .{ .mode = .read_write }) catch |err| switch (err) {
                        error.FileNotFound => try std.fs.cwd().createFile(next_arg, .{}),
                        else => return err,
                    };
                },
                .entry => {
                    const next_arg = input_args.next() orelse {
                        std.debug.print("Expected entry point\n", .{});
                        showHelp();
                        return;
                    };
                    if (entry_point != null) {
                        std.debug.print("Entry point already set\n", .{});
                        showHelp();
                        return;
                    }
                    entry_point = next_arg;
                },
                .target => {
                    const next_arg = input_args.next() orelse {
                        std.debug.print("Expected target profile\n", .{});
                        showHelp();
                        return;
                    };
                    if (target != null) {
                        std.debug.print("Target profile already set\n", .{});
                        showHelp();
                        return;
                    }
                    target = std.meta.stringToEnum(Profile, next_arg);
                    if (target == null) {
                        std.debug.print("Unknown target profile: {s}\n", .{next_arg});
                        showHelp();
                        return;
                    }
                },
                .spirv => {
                    spirv = true;
                },
            }
        } else {
            if (input_file != null) {
                std.debug.print("Input file already set\n", .{});
                showHelp();
                return;
            }
            input_file = try std.fs.cwd().openFile(arg, .{});
        }
    }

    if (input_file == null) {
        std.debug.print("No input file\n", .{});
        showHelp();
        return;
    }

    if (entry_point == null) {
        std.debug.print("No entry point\n", .{});
        showHelp();
        return;
    }

    if (target == null) {
        std.debug.print("No target profile\n", .{});
        showHelp();
        return;
    }

    const use_input_file = input_file orelse unreachable;
    defer use_input_file.close();

    const file_stat = try use_input_file.stat();
    const contents = try use_input_file.readToEndAlloc(std.heap.c_allocator, file_stat.size);
    defer std.heap.c_allocator.free(contents);

    var function_table = try dxc.FunctionTable.load("machdxcompilerlink");
    defer function_table.unload();

    const compiler = dxc.DynamicCompiler.initDynamic(&function_table);
    defer compiler.deinit();

    var args = std.ArrayList([*:0]const u8).init(std.heap.c_allocator);
    defer args.deinit();

    const use_target = target orelse unreachable;
    try args.appendSlice(&.{ "-T", @ptrCast(use_target.toArg()) });
    try args.appendSlice(&.{ "-E", @ptrCast(entry_point orelse "") });
    if (spirv) {
        try args.appendSlice(&.{"-spirv"});
    }

    const result = compiler.compile(contents, args.items);
    defer result.deinit();

    if (result.getError()) |err| {
        err.deinit();
        std.debug.print("Compilation failed\n{s}\n", .{err.getString()});
        return error.CompilationFailed;
    }

    const object = result.getObject();
    defer object.deinit();

    const bytes = object.getBytes();
    try output_file.writeAll(bytes);
}
