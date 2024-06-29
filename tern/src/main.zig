const std = @import("std");
const ast = @import("ast.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

pub fn main() !void {}

const test_source = @embedFile("shader.tn");

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

fn recurseExpressionTree(exp: *ast.Expression, depth: usize) void {
    for (0..depth) |_| {
        std.debug.print(" ", .{});
    }

    // switch
    switch (exp.variant) {
        .binary => {
            std.debug.print("binary: {}\n", .{exp.variant.binary.op});
            recurseExpressionTree(exp.variant.binary.left, depth + 1);
            recurseExpressionTree(exp.variant.binary.right, depth + 1);
        },
        .unary => {
            std.debug.print("unary: {}\n", .{exp.variant.unary.op});
            recurseExpressionTree(exp.variant.unary.operand, depth + 1);
        },
        .identifier => {
            std.debug.print("identifier: {s}\n", .{exp.variant.identifier});
        },
        .call => {
            std.debug.print("call:\n", .{});
            for (exp.variant.call.arguments) |arg| {
                recurseExpressionTree(arg, depth + 1);
            }
            for (0..depth) |_| {
                std.debug.print(" ", .{});
            }
            std.debug.print("+callee:\n", .{});
            recurseExpressionTree(exp.variant.call.callee, depth + 1);
        },
        .subscript => {
            std.debug.print("subscript\n", .{});
            recurseExpressionTree(exp.variant.subscript.array, depth + 1);
            recurseExpressionTree(exp.variant.subscript.index, depth + 1);
        },
        .integer_literal => {
            std.debug.print("integer: {}\n", .{exp.variant.integer_literal});
        },
        .float_literal => {
            std.debug.print("float: {}\n", .{exp.variant.float_literal});
        },
        .string_literal => {
            std.debug.print("string: {s}\n", .{exp.variant.string_literal});
        },
        .enum_literal => {
            std.debug.print("enum: {s}\n", .{exp.variant.enum_literal});
        },
        .boolean_literal => {
            std.debug.print("boolean: {}\n", .{exp.variant.boolean_literal});
        },
        .field => {
            std.debug.print("field: {s}\n", .{exp.variant.field.field});
            recurseExpressionTree(exp.variant.field.record, depth + 1);
        },
        else => {
            std.debug.print("other\n", .{});
        },
    }
}

test {
    var count = @import("CountingAllocator.zig").init(std.testing.allocator);
    const allocator = count.allocator();

    var container: ast.Container = undefined;
    container.init(allocator, allocator);
    try container.preheat(5000);
    defer container.deinit();
    var parser = Parser{
        .allocator = allocator,
        .file_path = "tern/src/shader.tn",
        .buffer = test_source,
        .container = &container,
    };
    try parser.init();
    defer parser.deinit();

    const start = std.time.nanoTimestamp();
    while (true) {
        const stmt: *ast.Statement = parser.parseTopLevel() catch |err| {
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

                const line_as_string = std.fmt.allocPrint(temp_allocator, "{d}", .{got.location.begin.line + 1}) catch unreachable;
                const pad_amount = line_as_string.len + 2;

                const whole_line = extractTokenLine(test_source, got.location.begin.line) orelse continue;
                for (0..pad_amount) |_| {
                    std.debug.print(" ", .{});
                }
                std.debug.print("|\n", .{});
                std.debug.print(" {} | {s}\n", .{ got.location.begin.line + 1, whole_line });

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
        if (stmt.variant == .@"if") {
            recurseExpressionTree(stmt.variant.@"if".condition, 0);
        }
    }
    const end = std.time.nanoTimestamp();
    _ = start;
    _ = end;
}
