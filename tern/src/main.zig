const std = @import("std");
const ast = @import("ast.zig");
const Reporter = @import("Reporter.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
// const Check = @import("Check.zig");
const Sosh = @import("Sosh.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var reporter = Reporter{
        .allocator = allocator,
        .source = test_source,
    };
    defer reporter.deinit();

    var container: ast.Container = undefined;
    container.init(allocator, allocator);
    defer container.deinit();
    var parser = Parser{
        .allocator = allocator,
        .file_path = "tern/src/shader.tn",
        .buffer = test_source,
        .container = &container,
        .reporter = &reporter,
    };
    try parser.init();
    defer parser.deinit();

    const start_parse = std.time.nanoTimestamp();
    while (true) {
        const stmt: *ast.Statement = parser.parseTopLevel() catch |err| {
            if (err == error.FinishedParsing) {
                break;
            }
            std.debug.print("{}\n", .{reporter});
            return err;
        };
        if (stmt.variant == .@"if") {
            recurseExpressionTree(stmt.variant.@"if".condition, 0);
        }
    }
    const end_parse = std.time.nanoTimestamp();

    std.debug.print("Parsed successfully\n", .{});

    // var symbols = try Check.SymbolTable.init(allocator);
    // defer symbols.deinit(allocator);

    // var check = Check{
    //     .allocator = allocator,
    //     .reporter = &reporter,
    //     .symbols = &symbols,
    // };
    // defer {
    //     std.debug.print("{}", .{reporter});
    //     check.deinit();
    // }

    // const start_check = std.time.nanoTimestamp();
    // try check.checkContainer(&container);
    // const end_check = std.time.nanoTimestamp();

    // var gen = IrGen{};
    // try gen.init(allocator);
    // defer gen.deinit();

    // const start_gen = std.time.nanoTimestamp();
    // for (0..8) |i| {
    //     std.mem.doNotOptimizeAway(try gen.analyseStatement(container.root_stmts.items[i]));
    // }
    // const end_gen = std.time.nanoTimestamp();

    // std.debug.print("{}\n", .{gen.module});

    var sosh = Sosh.Context{
        .allocator = allocator,
        .reporter = &reporter,
    };
    sosh.init();
    defer sosh.deinit();

    const start_check = std.time.nanoTimestamp();
    try sosh.resolveContainer(&container);
    const end_check = std.time.nanoTimestamp();

    var it = sosh.global_environment.values.iterator();
    while (it.next()) |got| {
        const got_item = sosh.get(got.value_ptr.*);
        std.debug.print("{s}: {}\n", .{ got.key_ptr.*, got_item });
    }

    std.debug.print("Parse time: {d}ns\n", .{end_parse - start_parse});
    std.debug.print("Check time: {d}ns\n", .{end_check - start_check});
}

const test_source = @embedFile("test.tn");

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
