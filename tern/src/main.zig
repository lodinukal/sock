const std = @import("std");
const ast = @import("ast.zig");
const Parser = @import("Parser.zig");

pub fn main() !void {}

const test_source = @embedFile("test.tn");

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
            for (parser.errors.items) |got| {
                std.debug.print("{}\n", .{got});
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
