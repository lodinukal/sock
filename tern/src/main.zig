const std = @import("std");
const ast = @import("ast.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

pub fn main() !void {}

const test_source = @embedFile("test.tn");

pub fn extractTokenLine(source: []const u8, line_target: u32) ?[]const u8 {
    var line_it = std.mem.tokenizeScalar(u8, source, '\n');
    var index: u32 = 0;
    while (line_it.next()) |line| {
        if (index == line_target) {
            return line;
        }
        index += 1;
    }
    return null;
}

test {
    var container = ast.Container.init(std.testing.allocator, std.testing.allocator);
    defer container.deinit();
    var parser = Parser{
        .allocator = std.testing.allocator,
        .file_path = "tern/src/test.tn",
        .buffer = test_source,
        .container = &container,
    };
    try parser.init();
    defer parser.deinit();

    std.debug.print("Tokens:\n", .{});
    while (true) {
        const token = parser.parseTopLevel() catch |err| {
            if (err == error.FinishedParsing) {
                break;
            }
            const ASCII_START_RED = "\x1b[31m";
            const ASCII_END = "\x1b[0m";
            for (parser.errors.items) |got| {
                var temp = std.mem.zeroes([1000]u8);
                var tall = std.heap.FixedBufferAllocator.init(&temp);
                const temp_allocator = tall.allocator();

                std.debug.print(ASCII_START_RED ++ "error" ++ ASCII_END ++ ": {s}\n", .{got.message});
                std.debug.print("--> {}\n", .{got.location});

                const line_as_string = std.fmt.allocPrint(temp_allocator, "{d}", .{got.location.begin.line}) catch unreachable;
                const pad_amount = line_as_string.len + 2;

                const whole_line = extractTokenLine(test_source, got.location.begin.line) orelse continue;
                for (0..pad_amount) |_| {
                    std.debug.print(" ", .{});
                }
                std.debug.print("|\n", .{});
                std.debug.print(" {} | {s}\n", .{ got.location.begin.line, whole_line });

                for (0..pad_amount) |_| {
                    std.debug.print(" ", .{});
                }
                std.debug.print("| ", .{});
                for (0..got.location.begin.column) |_| {
                    std.debug.print(" ", .{});
                }
                for (got.location.begin.column..got.location.end.column) |_| {
                    std.debug.print("^", .{});
                }
                std.debug.print("\n", .{});
                for (0..pad_amount) |_| {
                    std.debug.print(" ", .{});
                }
                std.debug.print("|\n", .{});
            }
            return err;
        };
        std.debug.print("{}\n", .{token.variant});
        // const token = (parser.next() catch {
        //     for (parser.errors.items) |err| {
        //         std.debug.print("{}\n", .{err});
        //     }
        //     break;
        // }) orelse return;
        // const fields = token.variant.statement.variant.type_declaration.type.@"struct".fields;
        // for (fields) |field| {
        //     std.debug.print("  {}\n", .{field.type.?.type.primitive});
        // }
    }
}
