const std = @import("std");
const ast = @import("ast.zig");
const Reporter = @import("Reporter.zig");

pub const ErrorSet = error{
    OutOfMemory,
    OutOfRange,
    Invalid,
};

const Self = @This();
allocator: std.mem.Allocator,
symbols: *SymbolTable,
reporter: ?*Reporter = null,

pub fn deinit(self: *Self) void {
    _ = self;
}

pub fn pushErr(self: *Self, location: ?ast.Location, comptime fmt: []const u8, args: anytype) ErrorSet!void {
    if (self.reporter) |reporter| {
        try reporter.push(.err, location, fmt, args);
    }
}

pub fn pushInfo(self: *Self, location: ?ast.Location, comptime fmt: []const u8, args: anytype) ErrorSet!void {
    if (self.reporter) |reporter| {
        try reporter.push(.info, location, fmt, args);
    }
}

pub fn checkStatement(self: *Self, stmt: *ast.Statement) ErrorSet!void {
    switch (stmt.variant) {
        .expression => |expr| try self.checkExpression(expr),
        .declaration => |*decl| try self.checkDeclaration(decl),
        .@"if" => |*if_stmt| try self.checkIfStatement(if_stmt),
        .@"while" => |while_stmt| try self.checkWhileStatement(while_stmt),
        .@"for" => |for_stmt| try self.checkForStatement(for_stmt),
        .@"return" => |return_expr| if (return_expr) |expr| try self.checkExpression(expr),
        // .match => |match_stmt| try self.checkMatchStatement(match_stmt),
        // .assignment => |assign| try self.checkAssignment(assign),
        else => {
            try self.pushErr(stmt.location, "Unhandled statement type in type checker {s}", .{@tagName(stmt.variant)});
            return error.Invalid;
        },
    }
}

pub fn checkExpression(self: *Self, expr: *ast.Expression) ErrorSet!void {
    switch (expr.variant) {
        .integer_literal => |int_lit| {
            expr.typ = try inferIntegerLiteralType(int_lit);
        },
        .float_literal => expr.typ = &Primitives.f64_type,
        .boolean_literal => expr.typ = &Primitives.bool_type,
        .string_literal => expr.typ = &Primitives.string_type,
        .identifier => |ident| try self.checkIdentifier(expr, ident),
        .binary => |bin| try self.checkBinaryExpression(expr, bin),
        .unary => |un| try self.checkUnaryExpression(expr, un),
        .call => |call| try self.checkCallExpression(expr, call),
        // .structure_literal => |struct_lit| try self.checkStructureLiteral(expr, struct_lit),
        .function => |func| try self.checkFunction(expr, func),
        .block => |block| {
            try self.symbols.enterScope(self.allocator);
            defer self.symbols.exitScope();

            for (block.statements) |stmt| {
                try self.checkStatement(stmt);
            }

            if (block.expression) |expr| {
                try self.checkExpression(expr);
                expr.typ = block.typ;
            }
        },
        // Add other expression types as needed
        else => {
            try self.pushErr(expr.location, "Unhandled expression type in type checker: {s}", .{@tagName(expr.variant)});
            return error.Invalid;
        },
    }
}

fn inferIntegerLiteralType(int_lit: ast.IntegerLiteral) ErrorSet!*const ast.Type {
    if (int_lit.negative) {
        return if (int_lit.value <= std.math.maxInt(i32)) &Primitives.i32_type else &Primitives.i64_type;
    } else {
        return if (int_lit.value <= std.math.maxInt(u32)) &Primitives.u32_type else &Primitives.u64_type;
    }
}

fn isNumericType(typ: *const ast.Type) bool {
    return typ.* == .primitive and switch (typ.primitive) {
        .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .f32, .f64 => true,
        else => false,
    };
}

fn promoteNumericTypes(left: *const ast.Type, right: *const ast.Type) ErrorSet!*const ast.Type {
    if (left == right) {
        return left;
    }

    inline for (.{
        &Primitives.f64_type,
        &Primitives.f32_type,
        &Primitives.i64_type,
        &Primitives.i32_type,
        &Primitives.i16_type,
        &Primitives.i8_type,
        &Primitives.u64_type,
        &Primitives.u32_type,
        &Primitives.u16_type,
        &Primitives.u8_type,
    }) |typ| {
        if (left == typ or right == typ) {
            return typ;
        }
    }

    return error.Invalid;
}

fn typesEqual(self: *Self, left: *const ast.Type, right: *const ast.Type) bool {
    self.checkExpression(left.expression) catch return false;
    self.checkExpression(right.expression) catch return false;

    if (left == right) {
        return true;
    }

    const left_tag: ast.TypeKind = std.meta.activeTag(left.*);
    const right_tag: ast.TypeKind = std.meta.activeTag(right.*);

    if (left_tag != right_tag) {
        return false;
    }

    switch (left.*) {
        .primitive => |left_primitive| {
            if (left_primitive != right.primitive) {
                return false;
            }
        },
        .pointer => |left_pointer| {
            if (!self.typesEqual(left_pointer.element, right.pointer.element)) {
                return false;
            }
        },
        .slice => |left_slice| {
            if (left_slice.mutable != right.slice.mutable) {
                return false;
            }
            if (!self.typesEqual(left_slice.element, right.slice.element)) {
                return false;
            }
        },
        .array => |left_array| {
            if (left_array.length != right.array.length) {
                return false;
            }
            if (!self.typesEqual(left_array.element, right.array.element)) {
                return false;
            }
        },
        .optional => |left_optional| {
            if (!self.typesEqual(left_optional, right.optional)) {
                return false;
            }
        },
        .function => |left_function| {
            if (left_function.parameters.len != right.function.parameters.len) {
                return false;
            }
            for (left_function.parameters, right.function.parameters) |left_param, right_param| {
                if (!self.typesEqual(left_param.typ.?, right_param.typ.?)) {
                    return false;
                }
            }
            if (left_function.return_type == null and right.function.return_type == null) {
                return true;
            }
            if (left_function.return_type != right.function.return_type) {
                return false;
            }
        },
        .expression => {
            unreachable;
        },
        else => {},
    }

    return true;
}

fn unifyTypes(self: *Self, left: *const ast.Type, right: *const ast.Type) ErrorSet!*const ast.Type {
    if (left == right) {
        return left;
    }

    const left_tag: ast.TypeKind = std.meta.activeTag(left.*);
    const right_tag: ast.TypeKind = std.meta.activeTag(right.*);

    if (left_tag != right_tag) {
        return error.Invalid;
    }

    switch (left.*) {
        .primitive => |left_primitive| {
            if (left_primitive != right.primitive) {
                return error.Invalid;
            }
        },
        .pointer => |left_pointer| {
            if (!self.typesEqual(left_pointer.element, right.pointer.element)) {
                return error.Invalid;
            }
        },
        .slice => |left_slice| {
            if (left_slice.mutable != right.slice.mutable) {
                return error.Invalid;
            }
            if (!self.typesEqual(left_slice.element, right.slice.element)) {
                return error.Invalid;
            }
        },
        .array => |left_array| {
            if (left_array.length != right.array.length) {
                return error.Invalid;
            }
            if (!self.typesEqual(left_array.element, right.array.element)) {
                return error.Invalid;
            }
        },
        .optional => |left_optional| {
            if (!self.typesEqual(left_optional, right.optional)) {
                return error.Invalid;
            }
        },
        .function => |left_function| {
            if (left_function.parameters.len != right.function.parameters.len) {
                return error.Invalid;
            }
            for (left_function.parameters, right.function.parameters) |left_param, right_param| {
                if (!self.typesEqual(left_param.typ.?, right_param.typ.?)) {
                    return error.Invalid;
                }
            }
            if (left_function.return_type == null and right.function.return_type == null) {
                return left;
            }
            if (left_function.return_type != right.function.return_type) {
                return error.Invalid;
            }
        },
        .expression => {
            unreachable;
        },
        else => {},
    }

    return left;
}

fn checkIdentifier(self: *Self, expr: *ast.Expression, ident: []const u8) ErrorSet!void {
    while (expr.variant == .identifier) {
        if (Primitives.lookup(ident)) |got| {
            expr.typ = &Primitives.type_type;
            expr.variant = .{ .typ = got };
            return;
        }
        if (self.symbols.lookup(ident)) |got| {
            expr.typ = got.typ;
            expr.variant = if (got.expression) |inner_expr| inner_expr.variant else .undefined;
            continue;
        }
        try self.pushErr(expr.location, "Unknown identifier '{s}'", .{ident});
    }
}

fn canConstantFoldBinary(bin: ast.BinaryOp) bool {
    const left_tag: ast.ExpressionKind = std.meta.activeTag(bin.left.variant);
    const right_tag: ast.ExpressionKind = std.meta.activeTag(bin.right.variant);
    if (left_tag != right_tag) {
        return false;
    }

    switch (left_tag) {
        .integer_literal => {
            switch (bin.op) {
                .plus, .minus, .star, .slash => return true,
                // .slash_slash => {
                //     if (bin.right.variant.integer_literal.value == 0 or bin.left.variant.integer_literal.negative) {
                //         return false;
                //     }
                //     return true;
                // },
                else => return false,
            }
        },
        .float_literal => {
            switch (bin.op) {
                .plus, .minus, .star, .slash => return true,
                else => return false,
            }
        },
        .boolean_literal => {
            switch (bin.op) {
                .equal, .bang_equal, .@"and", .@"or" => return true,
                else => return false,
            }
        },
        else => return false,
    }
}

fn constantFoldBinary(expr: *ast.Expression, bin: ast.BinaryOp) void {
    switch (bin.left.variant) {
        .integer_literal => {
            switch (bin.op) {
                .plus, .minus => {
                    const invert_rhs_sign = bin.op == .minus;

                    const lhs_value = bin.left.variant.integer_literal.value;
                    const rhs_value = bin.right.variant.integer_literal.value;

                    const lhs_negative = bin.left.variant.integer_literal.negative;
                    const rhs_negative = if (invert_rhs_sign)
                        !bin.right.variant.integer_literal.negative
                    else
                        bin.right.variant.integer_literal.negative;

                    // situation 1, lhs > 0, rhs > 0
                    // situation 2, lhs < 0, rhs < 0
                    // situation 3, lhs > 0, rhs < 0
                    // situation 4, lhs < 0, rhs > 0
                    // situation 5, lhs == 0, rhs == 0

                    const result_value: u64, const result_negative: bool = result: {
                        if (lhs_value == 0) break :result .{ lhs_value, lhs_negative };
                        if (rhs_value == 0) break :result .{ rhs_value, rhs_negative };

                        if (!lhs_negative and !rhs_negative) {
                            break :result .{ lhs_value +| rhs_value, false };
                        }
                        if (lhs_negative and rhs_negative) {
                            break :result .{ lhs_value +| rhs_value, true };
                        }
                        if (!lhs_negative and rhs_negative) {
                            if (lhs_value > rhs_value) {
                                break :result .{ lhs_value -| rhs_value, false };
                            } else {
                                break :result .{ rhs_value -| lhs_value, true };
                            }
                        }
                        if (lhs_negative and !rhs_negative) {
                            if (lhs_value > rhs_value) {
                                break :result .{ lhs_value -| rhs_value, true };
                            } else {
                                break :result .{ rhs_value -| lhs_value, false };
                            }
                        }
                        unreachable;
                    };

                    expr.variant = .{
                        .integer_literal = .{
                            .value = result_value,
                            .negative = result_negative,
                        },
                    };
                },
                .star => {
                    const lhs_value = bin.left.variant.integer_literal.value;
                    const rhs_value = bin.right.variant.integer_literal.value;

                    const lhs_negative = bin.left.variant.integer_literal.negative;
                    const rhs_negative = bin.right.variant.integer_literal.negative;

                    const result_value: u64, const result_negative: bool = result: {
                        if (lhs_value == 0 or rhs_value == 0) break :result .{ 0, false };

                        if (!lhs_negative and !rhs_negative) {
                            break :result .{ lhs_value * rhs_value, false };
                        }
                        if (lhs_negative and rhs_negative) {
                            break :result .{ lhs_value * rhs_value, false };
                        }
                        if (!lhs_negative and rhs_negative) {
                            break :result .{ lhs_value * rhs_value, true };
                        }
                        if (lhs_negative and !rhs_negative) {
                            break :result .{ lhs_value * rhs_value, true };
                        }
                        unreachable;
                    };

                    expr.variant = .{
                        .integer_literal = .{
                            .value = result_value,
                            .negative = result_negative,
                        },
                    };
                },
                // .slash_slash => {
                //     const lhs_value = bin.left.variant.integer_literal.value;
                //     const rhs_value = bin.right.variant.integer_literal.value;

                //     const result_value = lhs_value / rhs_value;

                //     expr.variant = .{
                //         .integer_literal = .{
                //             .value = result_value,
                //             .negative = false,
                //         },
                //     };
                // },
                .slash => {
                    // convert to floats
                    const lhs_value: f64 = @floatFromInt(bin.left.variant.integer_literal.value);
                    const rhs_value: f64 = @floatFromInt(bin.right.variant.integer_literal.value);

                    const result_value = lhs_value / rhs_value;

                    expr.variant = .{
                        .float_literal = result_value,
                    };
                },
                else => unreachable,
            }
        },
        .float_literal => {
            const lhs_value = bin.left.variant.float_literal;
            const rhs_value = bin.right.variant.float_literal;

            const result_value: f64 = result: {
                switch (bin.op) {
                    .plus => break :result lhs_value + rhs_value,
                    .minus => break :result lhs_value - rhs_value,
                    .star => break :result lhs_value * rhs_value,
                    .slash => break :result lhs_value / rhs_value,
                    else => unreachable,
                }
            };

            expr.variant = .{
                .float_literal = result_value,
            };
        },
        .boolean_literal => {
            const lhs_value = bin.left.variant.boolean_literal;
            const rhs_value = bin.right.variant.boolean_literal;

            const result_value: bool = result: {
                switch (bin.op) {
                    .equal => break :result lhs_value == rhs_value,
                    .bang_equal => break :result lhs_value != rhs_value,
                    .@"and" => break :result lhs_value and rhs_value,
                    .@"or" => break :result lhs_value or rhs_value,
                    else => unreachable,
                }
            };

            expr.variant = .{
                .boolean_literal = result_value,
            };
        },
        else => unreachable,
    }
}

fn checkBinaryExpression(self: *Self, expr: *ast.Expression, bin: ast.BinaryOp) ErrorSet!void {
    try self.checkExpression(bin.left);
    try self.checkExpression(bin.right);

    if (bin.left.typ == null or bin.right.typ == null) {
        try self.pushErr(expr.location, "Unable to infer types for binary expression", .{});
        return;
    }

    switch (bin.op) {
        .plus, .minus, .star, .slash => {
            if (isNumericType(bin.left.typ.?) and isNumericType(bin.right.typ.?)) {
                expr.typ = try promoteNumericTypes(bin.left.typ.?, bin.right.typ.?);
            } else {
                try self.pushErr(expr.location, "Invalid operands for arithmetic operator", .{});
            }
        },
        .equal, .bang_equal => {
            if (self.typesEqual(bin.left.typ.?, bin.right.typ.?)) {
                expr.typ = &Primitives.bool_type;
            } else {
                try self.pushErr(expr.location, "Incompatible types for equality comparison", .{});
            }
        },
        .less, .less_equal, .greater, .greater_equal => {
            if (isNumericType(bin.left.typ.?) and isNumericType(bin.right.typ.?)) {
                expr.typ = &Primitives.bool_type;
            } else {
                try self.pushErr(expr.location, "Invalid operands for comparison operator", .{});
            }
        },
        .@"and", .@"or" => {
            if (self.typesEqual(bin.left.typ.?, &Primitives.bool_type) and self.typesEqual(bin.right.typ.?, &Primitives.bool_type)) {
                expr.typ = &Primitives.bool_type;
            } else {
                try self.pushErr(expr.location, "Invalid operands for logical operator", .{});
            }
        },
        else => try self.pushErr(expr.location, "Unhandled binary operator", .{}),
    }

    // Perform Value folding
    if (canConstantFoldBinary(bin)) {
        constantFoldBinary(expr, bin);
    }
}

fn canConstantFoldUnary(un: ast.UnaryOp) bool {
    switch (un.op) {
        .minus => {
            switch (un.operand.variant) {
                .integer_literal => return true,
                .float_literal => return true,
                else => return false,
            }
        },
        .bang => {
            switch (un.operand.variant) {
                .boolean_literal => return true,
                else => return false,
            }
        },
        else => return false,
    }
}

fn constantFoldUnary(expr: *ast.Expression, un: ast.UnaryOp) void {
    switch (un.op) {
        .minus => {
            switch (un.operand.variant) {
                .integer_literal => {
                    const operand = un.operand.variant.integer_literal;
                    expr.variant = .{
                        .integer_literal = .{
                            .value = operand.value,
                            .negative = !operand.negative,
                        },
                    };
                },
                .float_literal => {
                    const operand = un.operand.variant.float_literal;
                    expr.variant = .{
                        .float_literal = -operand,
                    };
                },
                else => unreachable,
            }
        },
        .bang => {
            switch (un.operand.variant) {
                .boolean_literal => {
                    const operand = un.operand.variant.boolean_literal;
                    expr.variant = .{
                        .boolean_literal = !operand,
                    };
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

fn checkUnaryExpression(self: *Self, expr: *ast.Expression, un: ast.UnaryOp) ErrorSet!void {
    try self.checkExpression(un.operand);

    if (un.operand.typ == null) {
        try self.pushErr(expr.location, "Unable to infer type for unary expression", .{});
        return;
    }

    switch (un.op) {
        .minus => {
            if (isNumericType(un.operand.typ.?)) {
                expr.typ = un.operand.typ;
            } else {
                try self.pushErr(expr.location, "Invalid operand for unary minus", .{});
            }
        },
        .bang => {
            if (self.typesEqual(un.operand.typ.?, &Primitives.bool_type)) {
                expr.typ = &Primitives.bool_type;
            } else {
                try self.pushErr(expr.location, "Invalid operand for logical not", .{});
            }
        },
        else => try self.pushErr(expr.location, "Unhandled unary operator", .{}),
    }

    // Perform Value folding
    if (canConstantFoldUnary(un)) {
        constantFoldUnary(expr, un);
    }
}

fn checkCallExpression(self: *Self, expr: *ast.Expression, call: ast.Call) ErrorSet!void {
    try self.checkExpression(call.callee);

    if (call.callee.typ == null) {
        try self.pushErr(expr.location, "Unable to infer type for call expression", .{});
        return;
    }

    if (call.callee.typ.?.* != .function) {
        try self.pushErr(expr.location, "Attempted to call non-function type", .{});
        return;
    }

    const func_type = call.callee.typ.?.function;

    if (call.arguments.len != func_type.parameters.len) {
        try self.pushErr(expr.location, "Incorrect number of arguments for function call", .{});
        return;
    }

    for (call.arguments, func_type.parameters) |arg, parameter| {
        try self.checkExpression(arg);

        if (arg.typ == null) {
            try self.pushErr(expr.location, "Unable to infer type for argument", .{});
            return;
        }

        if (!self.typesEqual(arg.typ.?, parameter.typ.?)) {
            try self.pushErr(arg.location, "Argument type mismatch", .{});
            return;
        }
    }

    expr.typ = func_type.return_type;
}

fn checkFunction(self: *Self, expr: *ast.Expression, func: ast.Function) ErrorSet!void {
    try self.symbols.enterScope(self.allocator);
    defer self.symbols.exitScope();

    for (func.typ.function.parameters) |param| {
        try self.symbols.add(self.allocator, param.key.variant.identifier, .{
            .mutable = false,
            .typ = param.typ.?,
            .location = param.location,
        });
    }

    if (func.body) |body|
        try self.checkExpression(body);
    expr.typ = func.typ;
}

fn checkDeclaration(self: *Self, decl: *ast.DeclarationStatement) ErrorSet!void {
    const init = decl.initialiser;
    try self.checkExpression(init);
    if (decl.typ) |typ| {
        if (!self.typesEqual(typ, init.typ.?)) {
            try self.pushErr(init.location, "Initialiser type does not match declared type", .{});
        }
    } else {
        decl.typ = init.typ;
    }

    try self.symbols.add(self.allocator, decl.identifier, .{
        .mutable = decl.mutable,
        .expression = init,
        .location = init.location,
        .typ = decl.typ.?,
    });
}

fn checkIfStatement(self: *Self, if_stmt: *ast.IfStatement) ErrorSet!void {
    try self.checkExpression(if_stmt.condition);
    const condition_typ = if_stmt.condition.typ.?;
    if (!self.typesEqual(condition_typ, &Primitives.bool_type) and !(condition_typ.* == .optional)) {
        try self.pushErr(if_stmt.condition.location, "If condition must be a boolean or optional expression", .{});
    }
    {
        try self.symbols.enterScope(self.allocator);
        defer self.symbols.exitScope();

        if (if_stmt.capture) |capture| {
            if (condition_typ.* == .primitive) {
                try self.pushErr(if_stmt.condition.location, "If condition can only capture optionals", .{});
                return error.Invalid;
            }
            // assert only one capture
            if (capture.len != 1) {
                try self.pushErr(if_stmt.condition.location, "If statement can only capture one variable", .{});
                return error.Invalid;
            }
            const field = capture[0];
            try self.symbols.add(self.allocator, field.key.variant.identifier, .{
                .mutable = false, // Assuming captured variables are immutable
                .location = field.key.location,
                .typ = condition_typ,
            });
        }

        try self.checkExpression(if_stmt.then_branch);
    }
    {
        try self.symbols.enterScope(self.allocator);
        defer self.symbols.exitScope();
        if (if_stmt.else_branch) |else_branch| {
            try self.checkExpression(else_branch);
        }
    }

    // Infer result type if it's an expression-if
    if (if_stmt.result == null) {
        if (if_stmt.else_branch) |else_branch| {
            if_stmt.result = try self.unifyTypes(if_stmt.then_branch.typ.?, else_branch.typ.?);
        } else {
            if_stmt.result = &Primitives.unit_type;
        }
    }
}

fn checkWhileStatement(self: *Self, while_stmt: ast.WhileStatement) ErrorSet!void {
    try self.checkExpression(while_stmt.condition);
    if (!self.typesEqual(while_stmt.condition.typ.?, &Primitives.bool_type)) {
        try self.pushErr(while_stmt.condition.location, "While condition must be a boolean expression", .{});
    }

    try self.symbols.enterScope(self.allocator);
    defer self.symbols.exitScope();

    try self.checkExpression(while_stmt.body);
}

fn checkForStatement(self: *Self, for_stmt: ast.ForStatement) ErrorSet!void {
    try self.symbols.enterScope(self.allocator);
    defer self.symbols.exitScope();

    if (for_stmt.condition) |condition| {
        try self.checkExpression(condition);
        // Check if condition is iterable
        // This depends on how you've defined iterable types in your language
    }

    if (for_stmt.capture) |capture| {
        // assert only one capture
        if (capture.len != 1) {
            try self.pushErr(for_stmt.condition.?.location, "For loop can only capture one variable", .{});
            return error.Invalid;
        }
        const field = capture[0];
        try self.symbols.add(self.allocator, field.key.variant.identifier, .{
            .mutable = false, // Assuming captured variables in for loops are immutable
            .typ = for_stmt.condition.?.typ.?,
            .location = field.key.location,
        });
    }

    try self.checkExpression(for_stmt.body);
}

pub const Value = union(Kind) {
    integer: ast.IntegerLiteral,
    float: f64,
    boolean: bool,
    string: []const u8,
    enum_literal: EnumLiteral,
    structure_literal: StructureLiteral,
    function: Function,
    typ: *const Type,
    nil,
    undefined,
    external: External,

    pub const External = struct {
        value: *const ast.Expression,
        typ: ?*const Type = null,
    };

    pub const EnumLiteral = struct {
        explicit_type: ?*const Type = null,
        member: []const u8,
    };

    pub const StructureLiteral = struct {
        explicit_type: ?*const Type = null,
        fields: std.StringHashMapUnmanaged(Value),
    };

    pub const Function = struct {
        typ: *const Type,
        block: *const ast.Block,
    };

    pub const Type = union(TypeKind) {
        structure: Structure,
        enumeration: Enumeration,
        primitive: ast.PrimitiveType,
        optional: *const Type,
        pointer: ReferenceType,
        array: ArrayType,
        slice: ReferenceType,
        function: FunctionType,
    };

    pub const ReferenceType = struct {
        mutable: bool,
        element: *const Type,
    };

    pub const ArrayType = struct {
        element: *const Type,
        length: ?Value = null,
    };

    pub const FunctionType = struct {
        generics: []const FunctionParam,
        parameters: []const FunctionParam,
        return_type: ?*const Type = null,
    };

    pub const FunctionParam = struct {
        identifier: []const u8,
        typ: *const Type,
    };

    pub const TypeKind = enum {
        structure,
        enumeration,
        primitive,
        optional,
        pointer,
        array,
        slice,
        function,
    };

    pub const Structure = struct {
        fields: std.StringArrayHashMapUnmanaged(StructureField),
    };

    pub const StructureField = struct {
        typ: *const Type,
        default_value: ?Value = null,
    };

    pub const Enumeration = struct {
        members: std.StringArrayHashMapUnmanaged(EnumerationMember),
    };

    pub const EnumerationMember = struct {
        value: Value,
    };

    pub const Kind = enum {
        integer,
        float,
        boolean,
        string,
        enum_literal,
        structure_literal,
        function,
        typ,
        nil,
        undefined,
        external,
    };

    pub fn fromExpression(allocator: std.mem.Allocator, check: *Self, expr: *const ast.Expression, symbols: *const SymbolTable) ErrorSet!Value {
        switch (expr.variant) {
            .boolean_literal => |boolean| {
                return .{
                    .boolean = boolean,
                };
            },
            .integer_literal => |int_literal| {
                return .{
                    .integer = int_literal,
                };
            },
            .float_literal => |float_literal| {
                return .{
                    .float = float_literal,
                };
            },
            .string_literal => |string_literal| {
                return .{
                    .string = string_literal,
                };
            },
            .enum_literal => |enum_literal| {
                return .{
                    .enum_literal = .{
                        .explicit_type = null,
                        .member = enum_literal,
                    },
                };
            },
            .char_literal => |char_literal| {
                return .{
                    .integer = ast.IntegerLiteral{
                        .value = char_literal,
                        .negative = false,
                    },
                };
            },
            .structure_literal => |structure_literal| {
                var fields = std.StringHashMapUnmanaged(Value){};
                errdefer fields.deinit(allocator);

                for (structure_literal.fields) |field| {
                    try fields.put(
                        allocator,
                        field.key.variant.identifier,
                        try fromExpression(allocator, check, field.initialiser.?, symbols),
                    );
                }

                return .{ .structure_literal = .{
                    .explicit_type = null,
                    .fields = fields,
                } };
            },
            .typ => |typ| {
                return .{ .typ = try fromType(allocator, check, typ, symbols) };
            },
            .identifier => |identifier| {
                if (Primitives.lookup(identifier)) |primitive| {
                    return .{
                        .typ = fromPrimitiveType(primitive.primitive),
                    };
                }
                if (symbols.lookup(identifier)) |symbol| {
                    return symbol.value;
                }
                return error.Invalid;
            },
            .binary => |binary| {
                const lhs = try fromExpression(allocator, check, binary.left, symbols);
                const rhs = try fromExpression(allocator, check, binary.right, symbols);

                switch (binary.op) {
                    // .slash_slash,
                    // .percent,
                    // .ampersand,
                    // .pipe,
                    // .caret,
                    // .less,
                    // .greater,
                    // .equal_equal,
                    // .bang_equal,
                    // .less_equal,
                    // .greater_equal,
                    // .less_less,
                    // .greater_greater,
                    // .@"and",
                    // .@"or",
                    // .@"orelse",
                    .plus, .minus => {
                        const invert_rhs_sign = binary.op == .minus;
                        switch (lhs) {
                            .integer => {
                                const lhs_value = lhs.integer.value;
                                const rhs_value = rhs.integer.value;

                                const lhs_negative = lhs.integer.negative;
                                const rhs_negative = if (invert_rhs_sign) !rhs.integer.negative else rhs.integer.negative;

                                // situation 1, lhs > 0, rhs > 0
                                // situation 2, lhs < 0, rhs < 0
                                // situation 3, lhs > 0, rhs < 0
                                // situation 4, lhs < 0, rhs > 0
                                // situation 5, lhs == 0, rhs == 0

                                const result_value: u64, const result_negative: bool = result: {
                                    if (lhs_value == 0) break :result .{ lhs_value, lhs_negative };
                                    if (rhs_value == 0) break :result .{ rhs_value, rhs_negative };

                                    if (!lhs_negative and !rhs_negative) {
                                        break :result .{ lhs_value +| rhs_value, false };
                                    }
                                    if (lhs_negative and rhs_negative) {
                                        break :result .{ lhs_value +| rhs_value, true };
                                    }
                                    if (!lhs_negative and rhs_negative) {
                                        if (lhs_value > rhs_value) {
                                            break :result .{ lhs_value -| rhs_value, false };
                                        } else {
                                            break :result .{ rhs_value -| lhs_value, true };
                                        }
                                    }
                                    if (lhs_negative and !rhs_negative) {
                                        if (lhs_value > rhs_value) {
                                            break :result .{ lhs_value -| rhs_value, true };
                                        } else {
                                            break :result .{ rhs_value -| lhs_value, false };
                                        }
                                    }
                                    unreachable;
                                };

                                return .{
                                    .integer = ast.IntegerLiteral{
                                        .value = result_value,
                                        .negative = result_negative,
                                    },
                                };
                            },
                            .float => {
                                return .{
                                    .float = lhs.float + if (invert_rhs_sign) -rhs.float else rhs.float,
                                };
                            },
                            else => {
                                try check.pushErr(binary.left.location.merge(binary.right.location), "Invalid operands for arithmetic +", .{});
                                try check.pushInfo(binary.left.location, "Left operand type: {s}", .{@tagName(lhs)});
                                try check.pushInfo(binary.right.location, "Right operand type: {s}", .{@tagName(rhs)});
                                return error.Invalid;
                            },
                        }
                    },
                    .star, .slash => {
                        const one_over = binary.op == .slash;

                        switch (lhs) {
                            .integer => {
                                const lhs_value = lhs.integer.value;
                                const rhs_value = rhs.integer.value;

                                const lhs_negative = lhs.integer.negative;
                                const rhs_negative = rhs.integer.negative;

                                const result_value: u64, const result_negative: bool = result: {
                                    if (lhs_value == 0 or rhs_value == 0) break :result .{ 0, false };

                                    if (!lhs_negative and !rhs_negative) {
                                        break :result .{ lhs_value * rhs_value, false };
                                    }
                                    if (lhs_negative and rhs_negative) {
                                        break :result .{ lhs_value * rhs_value, false };
                                    }
                                    if (!lhs_negative and rhs_negative) {
                                        break :result .{ lhs_value * rhs_value, true };
                                    }
                                    if (lhs_negative and !rhs_negative) {
                                        break :result .{ lhs_value * rhs_value, true };
                                    }
                                    unreachable;
                                };

                                return .{
                                    .integer = ast.IntegerLiteral{
                                        .value = result_value,
                                        .negative = result_negative,
                                    },
                                };
                            },
                            .float => {
                                return .{
                                    .float = lhs.float * if (one_over) 1.0 / rhs.float else rhs.float,
                                };
                            },
                            else => {
                                try check.pushErr(binary.left.location.merge(binary.right.location), "Invalid operands for arithmetic *", .{});
                                try check.pushInfo(binary.left.location, "Left operand type: {s}", .{@tagName(lhs)});
                                try check.pushInfo(binary.right.location, "Right operand type: {s}", .{@tagName(rhs)});
                                return error.Invalid;
                            },
                        }
                    },
                    else => {
                        return .{ .external = .{
                            .value = expr,
                            .typ = if (expr.typ) |typ| try fromType(allocator, check, typ, symbols) else null,
                        } };
                    },
                }
                return .{
                    .typ = try fromType(allocator, check, binary.typ, symbols),
                };
            },
            .function => {
                return .{
                    .typ = try fromType(allocator, check, expr.typ, symbols),
                };
            },
            else => {
                try check.pushErr(expr.location, "Unhandled expression type in type checker: {s}", .{@tagName(expr.variant)});
                return error.Invalid;
            },
            // unary: UnaryOp,
            // call: Call,
            // subscript: Subscript,
            // deref: Deref,
            // field: FieldAccess,
            // function: Function,
            // lambda: Lambda,
            // block: Block,
            // pipeline: Pipeline,
            // undefined: void,
            // nil: void,
        }
        return error.Invalid;
    }

    pub fn fromType(allocator: std.mem.Allocator, check: *Self, typ: *const ast.Type, symbols: *const SymbolTable) ErrorSet!*const Type {
        switch (typ.*) {
            .expression => |expression| {
                return (try fromExpression(allocator, check, expression, symbols)).typ;
            },
            .structure => |structure| {
                var fields = std.StringArrayHashMapUnmanaged(StructureField){};
                errdefer fields.deinit(allocator);

                for (structure.fields) |field| {
                    try fields.put(allocator, field.key.variant.identifier, .{
                        .typ = try fromType(allocator, check, field.typ.?, symbols),
                    });
                }

                return try allocType(allocator, .{
                    .structure = .{
                        .fields = fields,
                    },
                });
            },
            .enumeration => |enumeration| {
                var members = std.StringArrayHashMapUnmanaged(EnumerationMember){};
                errdefer members.deinit(allocator);

                for (enumeration.fields) |member| {
                    try members.put(allocator, member.key.variant.identifier, .{
                        .value = try fromExpression(allocator, check, member.initialiser.?, symbols),
                    });
                }

                return try allocType(allocator, .{
                    .enumeration = .{
                        .members = members,
                    },
                });
            },
            .primitive => |primitive| {
                return fromPrimitiveType(primitive);
            },
            .optional => |optional| {
                return try allocType(allocator, .{
                    .optional = try fromType(allocator, check, optional, symbols),
                });
            },
            .pointer => |pointer| {
                return try allocType(allocator, .{
                    .pointer = .{
                        .mutable = pointer.mutable,
                        .element = try fromType(allocator, check, pointer.element, symbols),
                    },
                });
            },
            .slice => |slice| {
                return try allocType(allocator, .{
                    .slice = .{
                        .mutable = slice.mutable,
                        .element = try fromType(allocator, check, slice.element, symbols),
                    },
                });
            },
            .array => |array| {
                return try allocType(allocator, .{
                    .array = .{
                        .element = try fromType(allocator, check, array.element, symbols),
                        .length = if (array.length) |length| try fromExpression(allocator, check, length, symbols) else null,
                    },
                });
            },
            .function => |function| {
                var generics = std.ArrayListUnmanaged(FunctionParam){};
                errdefer generics.deinit(allocator);

                for (function.generics) |generic| {
                    try generics.append(allocator, .{
                        .identifier = generic.key.variant.identifier,
                        .typ = try fromType(allocator, check, generic.typ.?, symbols),
                    });
                }

                var parameters = std.ArrayListUnmanaged(FunctionParam){};
                errdefer parameters.deinit(allocator);

                for (function.parameters) |parameter| {
                    try parameters.append(allocator, .{
                        .identifier = parameter.key.variant.identifier,
                        .typ = try fromType(allocator, check, parameter.typ.?, symbols),
                    });
                }

                return try allocType(allocator, .{
                    .function = .{
                        .generics = generics.items,
                        .parameters = parameters.items,
                        .return_type = if (function.return_type) |return_type| try fromType(allocator, check, return_type, symbols) else null,
                    },
                });
            },
        }
    }

    fn allocType(allocator: std.mem.Allocator, typ: Type) !*const Type {
        const created = try allocator.create(Type);
        created.* = typ;
        return created;
    }

    pub fn integer(self: ast.IntegerLiteral) Value {
        return .{
            .integer = self,
        };
    }

    pub const i8_type = &Type{
        .primitive = .i8,
    };
    pub const i16_type = &Type{
        .primitive = .i16,
    };
    pub const i32_type = &Type{
        .primitive = .i32,
    };
    pub const i64_type = &Type{
        .primitive = .i64,
    };
    pub const u8_type = &Type{
        .primitive = .u8,
    };
    pub const u16_type = &Type{
        .primitive = .u16,
    };
    pub const u32_type = &Type{
        .primitive = .u32,
    };
    pub const u64_type = &Type{
        .primitive = .u64,
    };
    pub const f32_type = &Type{
        .primitive = .f32,
    };
    pub const f64_type = &Type{
        .primitive = .f64,
    };
    pub const bool_type = &Type{
        .primitive = .bool,
    };
    pub const unit_type = &Type{
        .primitive = .unit,
    };
    pub const type_type = &Type{
        .primitive = .typ,
    };
    pub const string_type = &Type{ .slice = .{
        .mutable = false,
        .element = &u8_type,
    } };

    pub fn fromPrimitiveType(typ: ast.PrimitiveType) *const Type {
        switch (typ) {
            .i8 => return i8_type,
            .i16 => return i16_type,
            .i32 => return i32_type,
            .i64 => return i64_type,
            .u8 => return u8_type,
            .u16 => return u16_type,
            .u32 => return u32_type,
            .u64 => return u64_type,
            .f32 => return f32_type,
            .f64 => return f64_type,
            .bool => return bool_type,
            .unit => return unit_type,
            .typ => return type_type,
        }
    }
};

pub const SymbolTable = struct {
    scopes: std.ArrayListUnmanaged(*Scope) = .{},
    current_scope: ?*Scope = null,

    pub fn init(allocator: std.mem.Allocator) ErrorSet!SymbolTable {
        var self: SymbolTable = .{};
        try self.enterScope(allocator);
        return self;
    }

    pub fn deinit(self: *SymbolTable, allocator: std.mem.Allocator) void {
        for (self.scopes.items) |scope| {
            scope.deinit(allocator);
            allocator.destroy(scope);
        }
        self.scopes.deinit(allocator);
    }

    pub fn enterScope(self: *SymbolTable, allocator: std.mem.Allocator) ErrorSet!void {
        const scope = try allocator.create(Scope);
        try self.scopes.append(allocator, scope);
        scope.* = .{
            .parent = self.current_scope,
        };
        if (self.current_scope) |this_scope| {
            scope.next = this_scope.child;
            this_scope.child = scope;
        }
        self.current_scope = scope;
    }

    pub fn exitScope(self: *SymbolTable) void {
        if (self.current_scope) |scope| {
            self.current_scope = scope.parent;
        }
    }

    pub const AddInfo = struct {
        mutable: bool,
        expression: ?*ast.Expression = null,
        location: ast.Location,
        typ: *const ast.Type,
    };
    pub fn add(self: *SymbolTable, allocator: std.mem.Allocator, name: []const u8, info: AddInfo) ErrorSet!void {
        const scope = self.current_scope orelse return error.Invalid;
        try scope.put(allocator, name, .{
            .name = name,
            .mutable = info.mutable,
            .expression = info.expression,
            .location = info.location,
            .typ = info.typ,
        });
    }

    pub fn lookup(self: *const SymbolTable, name: []const u8) ?Symbol {
        var current_scope = self.current_scope;
        while (current_scope) |scope| {
            if (scope.lookup(name)) |symbol| {
                return symbol;
            }
            current_scope = scope.parent;
        }
        return null;
    }
};

const Primitives = struct {
    var i8_type = ast.Type{
        .primitive = .i8,
    };
    var i16_type = ast.Type{
        .primitive = .i16,
    };
    var i32_type = ast.Type{
        .primitive = .i32,
    };
    var i64_type = ast.Type{
        .primitive = .i64,
    };
    var u8_type = ast.Type{
        .primitive = .u8,
    };
    var u16_type = ast.Type{
        .primitive = .u16,
    };
    var u32_type = ast.Type{
        .primitive = .u32,
    };
    var u64_type = ast.Type{
        .primitive = .u64,
    };
    var f32_type = ast.Type{
        .primitive = .f32,
    };
    var f64_type = ast.Type{
        .primitive = .f64,
    };
    var bool_type = ast.Type{
        .primitive = .bool,
    };
    var unit_type = ast.Type{
        .primitive = .unit,
    };
    var type_type = ast.Type{
        .primitive = .typ,
    };
    var string_type = ast.Type{ .slice = .{
        .mutable = false,
        .element = &u8_type,
    } };

    pub fn lookup(name: []const u8) ?*const ast.Type {
        inline for (.{
            .{ "i8", &i8_type },
            .{ "i16", &i16_type },
            .{ "i32", &i32_type },
            .{ "i64", &i64_type },
            .{ "u8", &u8_type },
            .{ "u16", &u16_type },
            .{ "u32", &u32_type },
            .{ "u64", &u64_type },
            .{ "f32", &f32_type },
            .{ "f64", &f64_type },
            .{ "bool", &bool_type },
            .{ "unit", &unit_type },
            .{ "type", &type_type },
            .{ "string", &string_type },
        }) |set| {
            if (std.mem.eql(u8, name, set.@"0")) {
                return set.@"1";
            }
        }
        return null;
    }
};

const Scope = struct {
    symbols: std.StringHashMapUnmanaged(Symbol) = undefined,
    typ: ?ast.Type = null,

    parent: ?*Scope = null,
    child: ?*Scope = null,
    next: ?*Scope = null,

    pub fn deinit(self: *Scope, allocator: std.mem.Allocator) void {
        self.symbols.deinit(allocator);
    }

    pub fn lookup(self: *Scope, name: []const u8) ?Symbol {
        return self.symbols.get(name);
    }

    pub fn put(self: *Scope, allocator: std.mem.Allocator, name: []const u8, symbol: Symbol) ErrorSet!void {
        try self.symbols.put(allocator, name, symbol);
    }
};

pub const Symbol = struct {
    name: []const u8,
    mutable: bool,
    location: ast.Location,
    typ: *const ast.Type,
    expression: ?*ast.Expression = null,
};
