const std = @import("std");
const ast = @import("ast.zig");
const Reporter = @import("Reporter.zig");

pub const ErrorSet = error{
    OutOfMemory,
    OutOfRange,
    Invalid,
    NoEntryPoint,
    NotFound,
};

const Self = @This();
allocator: std.mem.Allocator,
symbols: *SymbolTable,
reporter: ?*Reporter = null,

current_function: ?*ast.Expression = null,
block_stack: std.ArrayListUnmanaged(*ast.Expression) = .{},
current_pipeline: ?*ast.Expression = null,

declarations: std.ArrayListUnmanaged(*ast.Declaration) = .{},
unresolved_statements: std.ArrayListUnmanaged(*ast.Statement) = .{},
top_level: bool = false,

is_mutable_access: bool = false,
is_deref: bool = false,
is_taking_mutable_address: bool = false,
end_current_block: bool = false,
current_block_never: bool = false,

implicit_passby_first_arg: ?*ast.Expression = null,

current_initialiser_expression: ?*ast.Expression = null,
current_function_name: ?[]const u8 = null,

pub fn deinit(self: *Self) void {
    self.block_stack.deinit(self.allocator);
    for (self.declarations.items) |declaration| {
        self.allocator.destroy(declaration);
    }
    self.declarations.deinit(self.allocator);
    self.unresolved_statements.deinit(self.allocator);
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

pub fn checkEntryPoint(self: *Self, identifier: []const u8) ErrorSet!void {
    if (self.symbols.lookup(identifier)) |symbol| {
        const variant: *ast.Statement.Variant = @fieldParentPtr("declaration", symbol);
        const statement: *ast.Statement = @fieldParentPtr("variant", variant);
        try self.checkStatement(statement);
    } else {
        return error.NoEntryPoint;
    }
}

pub fn loadContainer(self: *Self, container: *ast.Container) ErrorSet!void {
    self.top_level = true;
    defer self.top_level = false;
    try self.unresolved_statements.appendSlice(self.allocator, container.root_stmts.items);
    while (self.unresolved_statements.items.len > 0) {
        const statement = self.unresolved_statements.pop();
        try self.checkStatement(statement);
    }
}

pub fn checkExpression(self: *Self, expression: *ast.Expression) ErrorSet!void {
    self.implicit_passby_first_arg = null;
    const is_deref = self.is_deref;
    self.is_deref = false;
    const is_taking_mutable_address = self.is_taking_mutable_address;
    self.is_taking_mutable_address = false;
    switch (expression.variant) {
        .binary => |*binary| {
            try self.checkExpression(binary.left);
            try self.checkExpression(binary.right);

            const left_type = try self.getTypeOfExpression(binary.left);
            const right_type = try self.getTypeOfExpression(binary.right);

            switch (binary.op) {
                .plus, .minus, .star, .slash, .percent => {
                    if (left_type.* != .primitive or right_type.* != .primitive) {
                        try self.pushErr(expression.location, "binary operator '{}' requires operands of type 'primitive'", .{binary.op});
                        try self.pushInfo(binary.left.location, "left operand is of type '{s}'", .{try prettyPrintType(left_type, self.reporter.?)});
                        try self.pushInfo(binary.right.location, "right operand is of type '{s}'", .{try prettyPrintType(right_type, self.reporter.?)});
                        return error.Invalid;
                    }
                    if (left_type.primitive != right_type.primitive) {
                        try self.pushErr(expression.location, "binary operator '{}' requires operands of the same type", .{binary.op});
                        try self.pushInfo(binary.left.location, "left operand is of type '{s}'", .{try prettyPrintType(left_type, self.reporter.?)});
                        try self.pushInfo(binary.right.location, "right operand is of type '{s}'", .{try prettyPrintType(right_type, self.reporter.?)});
                        return error.Invalid;
                    }
                    binary.result_type = left_type;
                    expression.typ = binary.result_type;
                },
                .equal_equal, .bang_equal, .less, .less_equal, .greater, .greater_equal => {
                    if (left_type.* != .primitive or right_type.* != .primitive) {
                        try self.pushErr(expression.location, "binary operator '{}' requires operands of type 'primitive'", .{binary.op});
                        try self.pushInfo(binary.left.location, "left operand is of type '{s}'", .{try prettyPrintType(left_type, self.reporter.?)});
                        try self.pushInfo(binary.right.location, "right operand is of type '{s}'", .{try prettyPrintType(right_type, self.reporter.?)});
                        return error.Invalid;
                    }
                    binary.result_type = ast.Primitives.bool_type;
                    expression.typ = binary.result_type;
                    expression.location = binary.left.location.merge(binary.right.location);
                },
                .slash_slash => {
                    // rhs needs to be an unsigned integer
                    if (right_type.* != .primitive or !right_type.primitive.isInteger() or right_type.primitive.isSigned()) {
                        try self.pushErr(expression.location, "binary operator '{}' requires right operand which is signed", .{binary.op});
                        try self.pushInfo(binary.right.location, "right operand is of type '{s}'", .{try prettyPrintType(right_type, self.reporter.?)});
                        return error.Invalid;
                    }
                    binary.result_type = right_type;
                    expression.typ = binary.result_type;
                    expression.location = binary.left.location.merge(binary.right.location);
                },
                .ampersand, .pipe, .caret, .less_less, .greater_greater => {
                    // lhs needs to be an unsigned integer
                    if (left_type.* != .primitive or !left_type.primitive.isInteger() or left_type.primitive.isSigned()) {
                        try self.pushErr(expression.location, "binary operator '{}' requires left operand which is signed", .{binary.op});
                        try self.pushInfo(binary.left.location, "left operand is of type '{s}'", .{try prettyPrintType(left_type, self.reporter.?)});
                        return error.Invalid;
                    }
                    // rhs needs to be an unsigned integer
                    if (right_type.* != .primitive or !right_type.primitive.isInteger() or right_type.primitive.isSigned()) {
                        try self.pushErr(expression.location, "binary operator '{}' requires right operand which is signed", .{binary.op});
                        try self.pushInfo(binary.right.location, "right operand is of type '{s}'", .{try prettyPrintType(right_type, self.reporter.?)});
                        return error.Invalid;
                    }
                    binary.result_type = left_type;
                    expression.typ = binary.result_type;
                    expression.location = binary.left.location.merge(binary.right.location);
                },
                .@"and", .@"or" => {
                    if (left_type.* != .primitive or !left_type.primitive.isBool()) {
                        try self.pushErr(expression.location, "binary operator '{}' requires left operand which is a boolean", .{binary.op});
                        try self.pushInfo(binary.left.location, "left operand is of type '{s}'", .{try prettyPrintType(left_type, self.reporter.?)});
                        return error.Invalid;
                    }
                    if (right_type.* != .primitive or !right_type.primitive.isBool()) {
                        try self.pushErr(expression.location, "binary operator '{}' requires right operand which is a boolean", .{binary.op});
                        try self.pushInfo(binary.right.location, "right operand is of type '{s}'", .{try prettyPrintType(right_type, self.reporter.?)});
                        return error.Invalid;
                    }
                    binary.result_type = ast.Primitives.bool_type;
                    expression.typ = binary.result_type;
                    expression.location = binary.left.location.merge(binary.right.location);
                },
                .@"orelse" => {
                    // rhs is a block
                    if (left_type.* != .optional) {
                        try self.pushErr(binary.left.location, "left operand of 'orelse' must be an optional", .{});
                        return error.Invalid;
                    }
                    const inner_left_type = left_type.optional;

                    try self.coerceTo(binary.right, inner_left_type);
                    binary.result_type = inner_left_type;
                    expression.typ = binary.result_type;
                },
                else => unreachable,
            }
        },
        .unary => |*unary| {
            try self.checkExpression(unary.operand);
            switch (unary.op) {
                .minus => {
                    const operand_type = try self.getTypeOfExpression(unary.operand);
                    if (operand_type.* != .primitive and operand_type.primitive.isBool()) {
                        try self.pushErr(expression.location, "unary operator '{}' requires operand with a number type", .{unary.op});
                        try self.pushInfo(unary.operand.location, "operand is of type '{s}'", .{try prettyPrintType(operand_type, self.reporter.?)});
                        return error.Invalid;
                    }
                    unary.result_type = operand_type;
                    expression.typ = unary.result_type;
                },
                .bang => {
                    const operand_type = try self.getTypeOfExpression(unary.operand);
                    if (operand_type.* != .primitive and !operand_type.primitive.isBool()) {
                        try self.pushErr(expression.location, "unary operator '{}' requires operand with a boolean type", .{unary.op});
                        try self.pushInfo(unary.operand.location, "operand is of type '{s}'", .{try prettyPrintType(operand_type, self.reporter.?)});
                        return error.Invalid;
                    }
                    unary.result_type = ast.Primitives.bool_type;
                    expression.typ = unary.result_type;
                },
                .tilde => {
                    const operand_type = try self.getTypeOfExpression(unary.operand);
                    if (operand_type.* != .primitive and !operand_type.primitive.isInteger()) {
                        try self.pushErr(expression.location, "unary operator '{}' requires operand with an integer type", .{unary.op});
                        try self.pushInfo(unary.operand.location, "operand is of type '{s}'", .{try prettyPrintType(operand_type, self.reporter.?)});
                        return error.Invalid;
                    }
                    unary.result_type = operand_type;
                    expression.typ = unary.result_type;
                },
                else => unreachable,
            }
        },
        .block => |*block| {
            try self.block_stack.append(self.allocator, expression);
            defer _ = self.block_stack.pop();

            const old_end_block = self.end_current_block;
            self.end_current_block = false;
            defer self.end_current_block = old_end_block;

            const old_never = self.current_block_never;
            self.current_block_never = false;
            defer self.current_block_never = old_never;

            for (block.statements) |statement| {
                if (self.end_current_block) {
                    try self.pushErr(statement.location, "unreachable statement", .{});
                    return error.Invalid;
                }
                try self.checkStatement(statement);
            }

            if (self.current_block_never) {
                expression.typ = ast.Primitives.never;
            } else if (block.result_type) |result_type| {
                expression.typ = result_type;
            } else {
                expression.typ = ast.Primitives.unit_type;
            }
        },
        .typ => |typ| {
            try self.checkType(typ);
            expression.typ = ast.Primitives.type_type;
        },
        .identifier => |identifier| {
            if (std.mem.eql(u8, identifier, "_")) {
                if (self.current_pipeline) |pipeline| {
                    expression.typ = pipeline.variant.pipeline.result_type;
                    return;
                } else {
                    try self.pushErr(expression.location, "use of '_' outside of pipeline", .{});
                    return error.Invalid;
                }
            }
            if (ast.Primitives.lookup(identifier)) |typ| {
                expression.typ = ast.Primitives.type_type;
                expression.variant = .{
                    .typ = typ,
                };
                return;
            }
            if (self.symbols.lookup(identifier)) |symbol| {
                try self.ensureResolveDeclaration(symbol);
                // pointers can be accessed in a mutable context even if they are immutable
                const mutable_pointer = if (symbol.typ.?.* == .pointer) symbol.typ.?.pointer.mutable else false;
                if (is_deref) {
                    if (!mutable_pointer) {
                        try self.pushErr(expression.location, "dereference of immutable pointer in assignment context", .{});
                        return error.Invalid;
                    }
                } else if (!symbol.mutable and self.is_mutable_access) {
                    try self.pushErr(expression.location, "use of immutable symbol in assignment context", .{});
                    return error.Invalid;
                } else if (!symbol.mutable and is_taking_mutable_address) {
                    try self.pushErr(expression.location, "taking address of immutable symbol", .{});
                    return error.Invalid;
                }
                expression.typ = symbol.typ;
                return;
            }
            try self.pushErr(expression.location, "undeclared identifier '{s}'", .{identifier});
        },
        .match => |*match| {
            try self.checkExpression(match.expression);
            const expression_type = try self.getTypeOfExpression(match.expression);
            const resolved_expression_type = try self.resolveTypeFromAliasing(expression_type);

            // only switch on enums and booleans for now
            const got_as_enum: ?*const ast.Type = if (resolved_expression_type.* == .enumeration)
                resolved_expression_type
            else
                null;
            const is_boolean = resolved_expression_type.* == .primitive and resolved_expression_type.primitive == .bool;

            if (got_as_enum == null and is_boolean == false) {
                try self.pushErr(match.expression.location, "match statement on non-enumeration or boolean type", .{});
                return error.Invalid;
            }

            var resulting_type: ?*const ast.Type = null;
            var else_location: ?ast.Location = null;

            var is_exhaustive = false;

            var got_true = false;
            var got_false = false;

            var used_field_names = std.StringHashMapUnmanaged(void){};
            defer used_field_names.deinit(self.allocator);

            for (match.cases) |case| {
                if (case.pattern) |pattern| {
                    try self.checkExpression(pattern);
                    if (got_as_enum) |got_type| {
                        try self.coerceTo(pattern, got_type);
                        const field_name = try self.resolveEnumerationMember(pattern);
                        if (used_field_names.get(field_name)) |_| {
                            try self.pushErr(pattern.location, "multiple cases for field '{s}'", .{field_name});
                            return error.Invalid;
                        }
                        try used_field_names.put(self.allocator, field_name, {});

                        const exhausted = used_field_names.count() == got_type.enumeration.fields.len;
                        if (!is_exhaustive) {
                            is_exhaustive = exhausted;
                        }
                    } else if (is_boolean) {
                        try self.coerceTo(pattern, ast.Primitives.bool_type);
                        if (try self.resolveBooleanLiteral(pattern)) {
                            if (got_true) {
                                try self.pushErr(pattern.location, "multiple true cases", .{});
                                try self.pushInfo(pattern.location, "previous true case here", .{});
                                return error.Invalid;
                            }
                            got_true = true;
                        } else {
                            if (got_false) {
                                try self.pushErr(pattern.location, "multiple false cases", .{});
                                try self.pushInfo(pattern.location, "previous false case here", .{});
                                return error.Invalid;
                            }
                            got_false = true;
                        }
                        const exhausted = got_true and got_false;
                        if (!is_exhaustive) {
                            is_exhaustive = exhausted;
                        }
                    }
                } else {
                    if (else_location) |location| {
                        try self.pushErr(case.location, "multiple else cases", .{});
                        try self.pushInfo(location, "previous else case here", .{});
                        return error.Invalid;
                    }
                    else_location = case.location;
                }

                const old_scope = try self.symbols.enterScope(self.allocator, case.body, null);
                defer self.symbols.restoreScope(old_scope);

                try self.checkExpression(case.body);
                if (case.body.typ) |typ| {
                    if (resulting_type == null or resulting_type.?.isNever()) {
                        resulting_type = typ;
                    } else {
                        try self.coerceTo(case.body, resulting_type.?);
                    }
                } else if (resulting_type != null) {
                    try self.pushErr(case.body.location, "match statement case type mismatch", .{});
                    try self.pushInfo(case.body.location, "expected '{s}'", .{try prettyPrintType(resulting_type.?, self.reporter.?)});
                    try self.pushInfo(case.body.location, "got nothing", .{});
                    return error.Invalid;
                }
            }

            if (!is_exhaustive) {
                if (else_location == null) {
                    try self.pushErr(match.expression.location, "non-exhaustive match statement", .{});
                    if (got_as_enum) |got_type| {
                        for (got_type.enumeration.fields) |field| {
                            if (used_field_names.get(field.key.variant.identifier) == null) {
                                try self.pushInfo(null, "missing case for field '{s}'", .{field.key.variant.identifier});
                            }
                        }
                    } else if (is_boolean) {
                        if (!got_true) {
                            try self.pushInfo(null, "missing case for true", .{});
                        }
                        if (!got_false) {
                            try self.pushInfo(null, "missing case for false", .{});
                        }
                    }
                    return error.Invalid;
                }
            } else {
                if (else_location) |_| {
                    try self.pushErr(else_location, "exhaustive match statement with else case", .{});
                    return error.Invalid;
                }
            }
            match.result_type = resulting_type orelse ast.Primitives.unit_type;
            expression.typ = match.result_type.?;
        },
        .function => |function| {
            try self.checkType(function.typ);
            const old_scope = try self.symbols.enterScope(self.allocator, expression, self.symbols.current_namespace);
            self.current_function_name = null;
            defer self.symbols.restoreScope(old_scope);

            const old_function = self.current_function;
            self.current_function = expression;
            defer {
                self.current_function = old_function;
            }

            for (function.typ.function.parameters) |*field| {
                try self.checkFunctionParameterDeclaration(field);
            }

            if (function.body) |body| {
                try self.checkExpression(body);
                // TODO: check return type is the same as function body return type
            }
            expression.typ = function.typ;
        },
        .float_literal => {
            expression.typ = ast.Primitives.f32_type;
        },
        .integer_literal => |literal| {
            expression.typ = ast.Primitives.fromPrimitive(literal.specifier);
        },
        .boolean_literal => {
            expression.typ = ast.Primitives.bool_type;
        },
        .string_literal => {
            expression.typ = ast.Primitives.string_type;
        },
        .structure_literal => |literal| {
            if (literal.explicit_type) |explicit| {
                try self.resolveExpression(explicit, false);
                if (explicit.variant != .typ) {
                    try self.pushErr(expression.location, "structure literal explicit type must be a type", .{});
                    return error.Invalid;
                }
                const resolved = try self.resolveTypeFromAliasing(explicit.variant.typ);
                try self.checkStructureLiteral(expression, resolved);
            } else {
                expression.typ = ast.Primitives.structure_literal;
            }
        },
        .enum_literal => {
            expression.typ = ast.Primitives.enum_literal;
        },
        .pipeline => |*pipeline| {
            // for now let's just use the initial
            const old_pipeline = self.current_pipeline;
            self.current_pipeline = expression;
            defer {
                self.current_pipeline = old_pipeline;
            }

            try self.checkExpression(pipeline.stages[0]);
            pipeline.result_type = pipeline.stages[0].typ;
            for (pipeline.stages[1..]) |stage| {
                try self.checkExpression(stage);
                pipeline.result_type = stage.typ;
            }

            expression.typ = pipeline.result_type;
        },
        .field => |*field_access| {
            try self.checkExpression(field_access.record);
            const record_type = try self.resolveTypeFromAliasing(try self.getTypeOfExpression(field_access.record));
            if (record_type.* == .structure) {
                for (record_type.structure.fields) |field| {
                    if (std.mem.eql(u8, field.key.variant.identifier, field_access.field)) {
                        field_access.result_type = field.typ.?;
                        expression.location = field_access.record.location.merge(field_access.end_location);
                        expression.typ = field_access.result_type.?;
                        return;
                    }
                }

                for (record_type.structure.decls) |*decl| {
                    if (std.mem.eql(u8, decl.identifier, field_access.field)) {
                        try self.checkDeclaration(expression.location, decl);

                        field_access.result_type = decl.typ.?;
                        expression.location = field_access.record.location.merge(field_access.end_location);
                        expression.typ = field_access.result_type.?;
                        self.implicit_passby_first_arg = field_access.record;
                        return;
                    }
                }

                try self.pushErr(expression.location, "field '{s}' not found in structure", .{field_access.field});
                try self.pushInfo(record_type.structure.location, "declared here", .{});
                return error.Invalid;
            } else if (record_type.* == .enumeration) {
                for (record_type.enumeration.decls) |*decl| {
                    if (std.mem.eql(u8, decl.identifier, field_access.field)) {
                        try self.checkDeclaration(expression.location, decl);
                        field_access.result_type = record_type;
                        expression.location = field_access.record.location.merge(field_access.end_location);
                        expression.typ = decl.typ.?;
                        self.implicit_passby_first_arg = field_access.record;
                        return;
                    }
                }
                try self.pushErr(expression.location, "field '{s}' not found in enumeration", .{field_access.field});
            } else if (record_type.* == .primitive and record_type.primitive == .typ) {
                try self.resolveExpression(field_access.record, false);
                if (field_access.record.variant == .typ) {
                    switch (field_access.record.variant.typ.*) {
                        inline .enumeration, .structure => |x| {
                            for (x.decls) |*decl| {
                                try self.checkDeclaration(.{}, decl);
                                if (std.mem.eql(u8, decl.identifier, field_access.field)) {
                                    field_access.result_type = decl.typ.?;
                                    expression.location = field_access.record.location.merge(field_access.end_location);
                                    expression.typ = field_access.result_type.?;
                                    return;
                                }
                            }

                            if (field_access.record.variant.typ.* == .enumeration) {
                                for (field_access.record.variant.typ.enumeration.fields) |field| {
                                    if (std.mem.eql(u8, field.key.variant.identifier, field_access.field)) {
                                        field_access.result_type = field_access.record.variant.typ;
                                        expression.location = field_access.record.location.merge(field_access.end_location);
                                        expression.typ = field_access.result_type.?;
                                        return;
                                    }
                                }
                            }

                            try self.pushErr(expression.location, "field/decl '{s}' not found in structure/enumeration", .{field_access.field});
                            return error.Invalid;
                        },
                        inline else => unreachable,
                    }
                } else if (field_access.record.variant == .field) {
                    std.debug.print("field access on field {}\n", .{field_access.record.variant.field});
                    unreachable;
                }
            } else if (record_type.* == .slice) {
                // ONLY ACCESS .len AND .ptr
                if (std.mem.eql(u8, field_access.field, "len")) {
                    field_access.result_type = ast.Primitives.u64_type;
                    expression.typ = field_access.result_type;
                    return;
                } else if (std.mem.eql(u8, field_access.field, "ptr")) {
                    field_access.result_type = record_type.slice.element;
                    expression.typ = field_access.result_type;
                    return;
                } else {
                    try self.pushErr(expression.location, "field access on slice type must be 'len' or 'ptr'", .{});
                    try self.pushInfo(field_access.record.location, "got '{s}'", .{field_access.field});
                    return error.Invalid;
                }
            } else if (record_type.* == .array) {
                // ONLY ACCESS .len AND .ptr
                if (std.mem.eql(u8, field_access.field, "len")) {
                    field_access.result_type = ast.Primitives.u64_type;
                    expression.typ = field_access.result_type;
                    return;
                } else if (std.mem.eql(u8, field_access.field, "ptr")) {
                    field_access.result_type = record_type.array.element;
                    expression.typ = field_access.result_type;
                    return;
                } else {
                    try self.pushErr(expression.location, "field access on array type must be 'len' or 'ptr'", .{});
                    try self.pushInfo(field_access.record.location, "got '{s}'", .{field_access.field});
                    return error.Invalid;
                }
            }
            try self.pushErr(expression.location, "field access on non-structure/enumeration type", .{});
            try self.pushInfo(field_access.record.location, "expected structure/enumeration type, got {s}", .{try prettyPrintType(record_type, self.reporter.?)});
            return error.Invalid;
        },
        .call => |*call| {
            try self.checkExpression(call.callee);
            const passing_first = self.implicit_passby_first_arg;
            self.implicit_passby_first_arg = null;
            const callee_type = try self.resolveTypeFromAliasing(try self.getTypeOfExpression(call.callee));
            if (callee_type.* == .function) {
                const length = if (passing_first != null) call.arguments.len + 1 else call.arguments.len;
                if (callee_type.function.parameters.len != length) {
                    try self.pushErr(expression.location, "function call argument count mismatch", .{});
                    try self.pushInfo(null, "passing with an implicit self", .{});
                    try self.pushInfo(call.callee.location, "expected '{}'", .{callee_type.function.parameters.len});
                    try self.pushInfo(expression.location, "got '{}'", .{length});
                    return error.Invalid;
                }
                if (passing_first) |first| {
                    try self.coerceTo(first, callee_type.function.parameters[0].typ.?);
                }
                for (call.arguments, 0..) |argument, index| {
                    try self.checkExpression(argument);
                    const check_against_index = if (passing_first != null) index + 1 else index;
                    try self.coerceTo(argument, callee_type.function.parameters[check_against_index].typ.?);
                }
                call.result_type = callee_type.function.return_type orelse ast.Primitives.unit_type;
                const end_location = if (call.arguments.len > 0) call.arguments[call.arguments.len - 1].location else call.callee.location;
                expression.location = call.callee.location.merge(end_location);
                expression.typ = call.result_type.?;
            } else {
                try self.pushErr(expression.location, "call on non-function type {}", .{callee_type});
                return error.Invalid;
            }
        },
        .subscript => |*subscript| {
            try self.checkExpression(subscript.array);
            try self.checkExpression(subscript.index);

            const array_type = try self.resolveTypeFromAliasing(try self.getTypeOfExpression(subscript.array));
            switch (array_type.*) {
                .slice, .array => {
                    const index_intermediate_type = try self.getTypeOfExpression(subscript.index);
                    const index_type = try self.resolveTypeFromAliasing(index_intermediate_type);
                    if (index_type.* != .primitive or !index_type.primitive.isInteger() or index_type.primitive.isSigned()) {
                        try self.pushErr(subscript.index.location, "array index must be an unsigned integer", .{});
                        try self.pushInfo(subscript.index.location, "got '{s}'", .{index_type.primitive.name()});
                        return error.Invalid;
                    }

                    if (subscript.index.variant == .integer_literal and array_type.* == .array) {
                        const index = try self.resolveUnsignedIntegerLiteral(subscript.index);
                        const length = try self.resolveUnsignedIntegerLiteral(array_type.array.length.?);
                        if (index >= length) {
                            try self.pushErr(subscript.index.location, "array index out of bounds", .{});
                            try self.pushInfo(null, "expected index < {}", .{length});
                            return error.Invalid;
                        }
                    }
                    expression.location = subscript.array.location.merge(subscript.end_location);
                    switch (array_type.*) {
                        inline .array, .slice => |x| {
                            subscript.result_type = x.element;
                            expression.typ = x.element;
                        },
                        else => unreachable,
                    }
                },
                else => {
                    try self.pushErr(expression.location, "subscript on non-array type", .{});
                    try self.pushInfo(subscript.array.location, "expected array type, got '{}'", .{array_type});
                    return error.Invalid;
                },
            }
        },
        .ref => |*ref| {
            try self.checkExpression(ref.operand);
            const type_from_expression = try self.getTypeOfExpression(ref.operand);
            const ref_type = try self.resolveTypeFromAliasing(type_from_expression);

            self.is_taking_mutable_address = ref.mutable;
            try self.checkExpression(ref.operand);

            ref.result_type.pointer.element = ref_type;
            expression.typ = ref.result_type;
        },
        .deref => |*deref| {
            self.is_deref = true;
            try self.checkExpression(deref.operand);
            const type_from_expression = try self.getTypeOfExpression(deref.operand);
            const deref_type = try self.resolveTypeFromAliasing(type_from_expression);
            if (deref_type.* == .pointer) {
                deref.result_type = deref_type.pointer.element;
                expression.typ = deref_type.pointer.element;
            } else {
                try self.pushErr(expression.location, "deref on non-pointer type", .{});
                try self.pushInfo(deref.operand.location, "expected pointer type, got '{}'", .{deref_type});
                return error.Invalid;
            }
        },
        .expressive_statement => |*expressive_statement| {
            try self.checkStatement(expressive_statement.statement);
            expression.typ = switch (expressive_statement.statement.variant) {
                inline .@"if" => |x| blk: {
                    break :blk x.result_type;
                },
                else => ast.Primitives.never,
            };
        },
        .nil => {
            expression.typ = ast.Primitives.nil_type;
        },
        .undefined => {
            expression.typ = ast.Primitives.never;
        },
        else => {
            try self.pushInfo(expression.location, "unimplemented expression type '{s}'", .{@tagName(expression.variant)});
        },
    }
}

pub fn checkStructureLiteral(self: *Self, expression: *ast.Expression, backing: *const ast.Type) ErrorSet!void {
    const literal = expression.variant.structure_literal;
    try self.checkType(backing);
    expression.typ = backing;

    switch (backing.*) {
        .array => {
            const field_length = try self.resolveUnsignedIntegerLiteral(backing.array.length orelse {
                return error.Invalid;
            });

            const literal_length = literal.fields.len;

            if (field_length != literal_length) {
                const location = literal.fields[0].key.location.merge(literal.fields[literal.fields.len - 1].key.location);
                try self.pushErr(expression.location, "array literal length mismatch", .{});
                try self.pushInfo(backing.array.length.?.location, "expected {}", .{field_length});
                try self.pushInfo(location, "got {}", .{literal_length});
                return error.Invalid;
            }

            for (literal.fields) |field| {
                try self.checkExpression(field.key);

                // ensure same type
                self.coerceTo(field.key, backing.array.element) catch {
                    try self.pushErr(field.location, "element not coercible", .{});
                    return error.Invalid;
                };
            }
        },
        .structure => {
            var field_map = std.StringHashMap(ast.Field).init(self.allocator);
            defer field_map.deinit();

            for (backing.structure.fields) |field| {
                try field_map.put(field.key.variant.identifier, field);
            }

            for (literal.fields) |field| {
                const field_name = field.key.variant.identifier;
                if (field_map.get(field_name)) |to_field| {
                    try self.coerceTo(field.initialiser orelse return error.Invalid, to_field.typ orelse
                        return error.Invalid);
                } else {
                    try self.pushErr(expression.location, "field '{s}' not found in structure", .{field_name});
                    return error.Invalid;
                }

                _ = field_map.remove(field_name);
            }

            var it = field_map.iterator();
            while (it.next()) |entry| {
                const field = entry.value_ptr.*;
                const field_name = field.key.variant.identifier;
                if (field.initialiser == null) {
                    try self.pushErr(expression.location, "field '{s}' not initialised", .{field_name});
                    return error.Invalid;
                }
            }

            for (literal.fields) |field| {
                try self.checkExpression(field.initialiser orelse return error.Invalid);
            }

            expression.typ = backing;
        },
        .slice => {
            try self.pushErr(expression.location, "slice literals must be allocated", .{});
            return error.Invalid;
        },
        else => {
            try self.pushErr(expression.location, "invalid type for structure literal, got {}", .{backing});
            return error.Invalid;
        },
    }
}

pub fn checkEnumLiteral(self: *Self, expression: *ast.Expression, backing: *const ast.Type) ErrorSet!void {
    const literal = expression.variant.enum_literal;
    try self.checkType(backing);
    expression.typ = backing;

    switch (backing.*) {
        .enumeration => |enumeration| {
            for (enumeration.fields) |field| {
                if (std.mem.eql(u8, literal, field.key.variant.identifier)) {
                    expression.typ = backing;
                    return;
                }
            }
            try self.pushErr(expression.location, "enum literal '{s}' not found in enumeration", .{literal});
            return error.Invalid;
        },
        else => {
            try self.pushErr(expression.location, "invalid type for enum literal, got {}", .{backing});
            return error.Invalid;
        },
    }
}

/// replaces the variant if it can
pub fn resolveExpression(self: *Self, expression: *ast.Expression, silent: bool) ErrorSet!void {
    switch (expression.variant) {
        .identifier => |identifier| {
            if (ast.Primitives.lookup(identifier)) |primitive| {
                expression.typ = ast.Primitives.type_type;
                expression.variant = .{
                    .typ = primitive,
                };
            } else if (self.symbols.lookup(identifier)) |symbol| {
                try self.ensureResolveDeclaration(symbol);
                expression.typ = symbol.typ;
                expression.variant = (symbol.initialiser orelse {
                    try self.pushErr(expression.location, "resolution of runtime value '{s}'", .{identifier});
                    return error.Invalid;
                }).variant;
            } else if (silent) {
                return error.NotFound;
            } else {
                try self.pushErr(expression.location, "undeclared identifier '{s}'", .{identifier});
                return error.Invalid;
            }
        },
        .field => |*field_access| {
            try self.resolveExpression(field_access.record, silent);
            switch (field_access.record.variant) {
                .typ => |typ| {
                    switch (typ.*) {
                        inline .enumeration, .structure => |x| {
                            for (x.decls) |*decl| {
                                if (std.mem.eql(u8, decl.identifier, field_access.field)) {
                                    if (decl.initialiser == null) {
                                        if (!silent)
                                            try self.pushErr(expression.location, "field '{s}' not initialised", .{field_access.field});
                                        return error.Invalid;
                                    }
                                    try self.checkDeclaration(decl.initialiser.?.location, decl);
                                    field_access.result_type = decl.typ.?;
                                    expression.typ = field_access.result_type.?;
                                    expression.variant = decl.initialiser.?.variant;
                                    return;
                                }
                            }

                            if (typ.* == .enumeration) {
                                for (typ.enumeration.fields) |field| {
                                    if (std.mem.eql(u8, field.key.variant.identifier, field_access.field)) {
                                        field_access.result_type = typ;
                                        expression.typ = field_access.result_type.?;
                                        expression.variant = .{
                                            .enum_literal = field.key.variant.identifier,
                                        };
                                        return;
                                    }
                                }
                            }

                            if (!silent)
                                try self.pushErr(expression.location, "field/decl '{s}' not found in structure/enumeration", .{field_access.field});
                            return error.Invalid;
                        },
                        else => {
                            if (!silent) {
                                try self.pushErr(expression.location, "field access on non-structure/enumeration type", .{});
                                try self.pushInfo(field_access.record.location, "expected structure/enumeration type, got {s}", .{
                                    try prettyPrintType(typ, self.reporter.?),
                                });
                            }
                            return error.Invalid;
                        },
                    }
                },
                else => {
                    if (!silent) {
                        try self.pushErr(expression.location, "field access on non-structure/enumeration type", .{});
                        try self.pushInfo(field_access.record.location, "expected structure/enumeration type, got {s}", .{
                            try prettyPrintType(try self.getTypeOfExpression(field_access.record), self.reporter.?),
                        });
                    }
                    return error.Invalid;
                },
            }
        },
        else => {
            try self.checkExpression(expression);
        },
    }
}

pub fn resolveUnsignedIntegerLiteral(_: Self, expression: *ast.Expression) ErrorSet!u64 {
    if (expression.variant == .integer_literal) {
        return expression.variant.integer_literal.value;
    }
    return error.Invalid;
}

pub fn resolveBooleanLiteral(_: Self, expression: *ast.Expression) ErrorSet!bool {
    if (expression.variant == .boolean_literal) {
        return expression.variant.boolean_literal;
    }
    return error.Invalid;
}

pub fn resolveEnumerationMember(_: Self, expression: *ast.Expression) ErrorSet![]const u8 {
    if (expression.variant == .enum_literal) {
        return expression.variant.enum_literal;
    } else {
        return expression.variant.field.field;
    }
    return error.Invalid;
}

pub fn getTypeOfExpression(_: Self, expression: *ast.Expression) ErrorSet!*const ast.Type {
    if (expression.typ) |typ| {
        return typ;
    }
    return error.Invalid;
}

pub fn prettyPrintType(typ: *const ast.Type, reporter: *Reporter) ![]const u8 {
    const is_string = typ == ast.Primitives.string_type;
    if (is_string) {
        return "string";
    }
    switch (typ.*) {
        .primitive => return typ.primitive.name(),
        .pointer => return try reporter.allocPrint("*{s}{s}", .{
            if (typ.pointer.mutable) "mut " else "",
            try prettyPrintType(typ.pointer.element, reporter),
        }),
        .array => return try reporter.allocPrint("[{d}]{s}", .{
            typ.array.length.?.variant.integer_literal.value,
            try prettyPrintType(typ.array.element, reporter),
        }),
        .function => return "function",
        .structure => return "structure",
        .enumeration => return "enumeration",
        .optional => return try reporter.allocPrint("?{s}", .{try prettyPrintType(typ.optional, reporter)}),
        .slice => return try reporter.allocPrint("[]{s}", .{try prettyPrintType(typ.slice.element, reporter)}),
        .expression => return "expression",
        .structure_literal => return "structure literal",
        .enum_literal => return "enum literal",
    }
}

pub fn checkStatement(self: *Self, statement: *ast.Statement) ErrorSet!void {
    switch (statement.variant) {
        .expression => |expression| {
            try self.checkExpression(expression);
        },
        .declaration => |*declaration| {
            if (self.top_level) {
                try self.preCheckDeclaration(statement.location, declaration);
                return;
            }
            try self.checkDeclaration(statement.location, declaration);
        },
        .@"return" => |return_stmt| {
            if (return_stmt) |got| {
                try self.checkExpression(got);
            }

            if (self.current_function) |function| {
                const return_type = function.variant.function.typ.function.return_type orelse ast.Primitives.unit_type;
                const resolved_return_type = try self.resolveTypeFromAliasing(return_type);
                if (return_stmt) |got| {
                    try self.coerceTo(got, resolved_return_type);
                    self.end_current_block = true;
                    self.current_block_never = true;
                } else if (resolved_return_type.* != .primitive or resolved_return_type.primitive != .unit) {
                    try self.pushErr(statement.location, "expected return value", .{});
                    return error.Invalid;
                }
            } else {
                try self.pushErr(statement.location, "return statement outside of function", .{});
                return error.Invalid;
            }
        },
        .assignment => |assignment| {
            // resolve the lhs
            {
                const old_assignment = self.is_mutable_access;
                self.is_mutable_access = true;
                defer self.is_mutable_access = old_assignment;
                try self.checkExpression(assignment.target);
            }
            try self.checkExpression(assignment.value);

            switch (assignment.kind) {
                .equal => {
                    try self.coerceTo(assignment.value, try self.getTypeOfExpression(assignment.target));
                },
                else => {
                    try self.pushErr(statement.location, "unimplemented assignment kind '{s}'", .{@tagName(assignment.kind)});
                    return;
                },
            }
        },
        .@"if" => |*if_statement| {
            // condition: *Expression,
            // capture: ?[]Field = null,
            // then_branch: *Expression,
            // else_branch: ?*Expression = null,

            try self.checkExpression(if_statement.condition);

            var resulting_type: ?*const ast.Type = null;
            {
                const old_scope = try self.symbols.enterScope(self.allocator, if_statement.then_branch, null);
                defer self.symbols.restoreScope(old_scope);

                if (if_statement.capture) |capture| {
                    // only one capture
                    if (capture.len != 1) {
                        try self.pushErr(statement.location, "if statement capture must have exactly one element", .{});
                        return error.Invalid;
                    }
                    try self.checkFunctionParameterDeclaration(&capture[0]);
                }

                try self.checkExpression(if_statement.then_branch);
                if (if_statement.then_branch.typ) |typ| {
                    resulting_type = typ;
                }
            }

            if (if_statement.else_branch) |else_branch| {
                const old_scope = try self.symbols.enterScope(self.allocator, else_branch, null);
                defer self.symbols.restoreScope(old_scope);
                try self.checkExpression(else_branch);
                if (resulting_type) |result| {
                    self.coerceTo(else_branch, result) catch {
                        try self.pushErr(else_branch.location, "if statement else branch type mismatch", .{});
                        return error.Invalid;
                    };
                } else {
                    // type is null, but this this might break into something
                    if (else_branch.typ) |_| {
                        try self.pushErr(else_branch.location, "if statement else branch type mismatch", .{});
                        return error.Invalid;
                    }
                }
            }

            if_statement.result_type = resulting_type;
        },
        .@"while" => |while_statement| {
            try self.checkExpression(while_statement.condition);

            {
                const old_scope = try self.symbols.enterScope(self.allocator, while_statement.body, null);
                defer self.symbols.restoreScope(old_scope);

                try self.checkExpression(while_statement.body);
            }
        },
        .@"for" => |for_statement| {
            // condition: ?*Expression = null,
            // capture: ?[]Field = null,
            // body: *Expression,
            try self.checkExpression(for_statement.condition);

            {
                const old_scope = try self.symbols.enterScope(self.allocator, for_statement.body, null);
                defer self.symbols.restoreScope(old_scope);

                if (for_statement.capture) |capture| {
                    for (capture) |*field| {
                        try self.checkFunctionParameterDeclaration(field);
                    }
                }

                try self.checkExpression(for_statement.body);
            }
        },
        .@"continue" => |*continue_statement| {
            if (self.block_stack.items.len == 0) {
                try self.pushErr(statement.location, "continue statement outside of block", .{});
                return error.Invalid;
            }

            // check the block stack
            // if this thing has a label attached then look for a corresponding block,
            // if it doesn't break at the top block
            if (continue_statement.label) |search_label| {
                for (self.block_stack.items) |block| {
                    if (block.variant.block.label) |got_label| {
                        if (std.mem.eql(u8, search_label, got_label)) {
                            continue_statement.resolved_block = block;
                        }
                    }
                }
            } else {
                continue_statement.resolved_block = self.block_stack.getLastOrNull();
            }
            self.end_current_block = true;
        },
        .@"break" => |*break_statement| {
            if (self.block_stack.items.len == 0) {
                try self.pushErr(statement.location, "break statement outside of block", .{});
                return error.Invalid;
            }

            // check the block stack
            // if this thing has a label attached then look for a corresponding block,
            // if it doesn't break at the top block
            const resolved_block = resolved: {
                if (break_statement.label) |search_label| {
                    for (self.block_stack.items) |block| {
                        if (block.variant.block.label) |got_label| {
                            if (std.mem.eql(u8, search_label, got_label)) {
                                break :resolved block;
                            }
                        }
                    }
                    try self.pushErr(statement.location, "label '{s}' not found", .{search_label});
                    return error.Invalid;
                } else {
                    break :resolved self.block_stack.getLast();
                }
            };

            if (resolved_block.variant.block.result_type) |block_type| {
                if (block_type.isUnit() and break_statement.expression == null) {
                    return;
                }
                try self.checkExpression(break_statement.expression.?);
                try self.coerceTo(break_statement.expression.?, block_type);
                self.end_current_block = true;
            } else {
                resolved_block.variant.block.result_type = if (break_statement.expression) |expression| result: {
                    try self.checkExpression(expression);
                    break :result try self.getTypeOfExpression(expression);
                } else ast.Primitives.unit_type;
                self.end_current_block = true;
            }
        },
        // inline else => {
        //     try self.pushInfo(statement.location, "unimplemented statement type '{s}'", .{@tagName(statement.variant)});
        // },
    }
}

pub fn checkFunctionParameterDeclaration(self: *Self, field: *ast.Field) ErrorSet!void {
    const field_name = field.key.variant.identifier;
    const is_discard = std.mem.eql(u8, field_name, "_");
    if (is_discard) {
        return;
    }

    if (ast.Primitives.lookup(field_name)) |_| {
        try self.pushErr(field.location, "redeclaration of primitive '{s}'", .{field_name});
        return error.Invalid;
    }

    if (self.symbols.lookup(field_name)) |symbol| {
        try self.pushErr(field.location, "redeclaration of symbol '{s}'", .{field_name});
        if (symbol.initialiser) |initialiser|
            try self.pushInfo(initialiser.location, "previous declaration here", .{});
        try self.pushInfo(field.location, "redeclaration here", .{});
        return error.Invalid;
    }
    try self.checkType(field.typ orelse return error.Invalid);
    const declaration = try self.allocator.create(ast.Declaration);
    declaration.* = .{
        .location = field.location,
        .identifier = field_name,
        .typ = field.typ,
        .mutable = false,
    };
    try self.declarations.append(self.allocator, declaration);
    try self.symbols.add(self.allocator, declaration);
}

pub fn preCheckDeclaration(
    self: *Self,
    location: ast.Location,
    declaration: *ast.Declaration,
) ErrorSet!void {
    if (ast.Primitives.lookup(declaration.identifier)) |_| {
        try self.pushErr(location, "redeclaration of primitive '{s}'", .{declaration.identifier});
        return;
    }

    if (self.symbols.lookup(declaration.identifier)) |symbol| {
        try self.pushErr(location, "redeclaration of symbol '{s}'", .{declaration.identifier});
        if (symbol.initialiser) |initialiser|
            try self.pushInfo(initialiser.location, "previous declaration here", .{});
        return error.Invalid;
    }

    if (declaration.is_type and declaration.mutable) {
        try self.pushErr(location, "type cannot be mutable", .{});
        return error.Invalid;
    }

    const is_function = if (declaration.initialiser) |initialiser| initialiser.variant == .function else false;
    if (is_function) {
        declaration.typ = declaration.initialiser.?.variant.function.typ;
        try self.symbols.add(self.allocator, declaration);
    }

    // deduce from initialiser
    if (declaration.typ == null and declaration.initialiser != null) {
        declaration.typ = declaration.initialiser.?.typ;
    }

    if (!is_function) {
        try self.symbols.add(self.allocator, declaration);
    }
}

pub fn ensureResolveDeclaration(self: *Self, declaration: *ast.Declaration) ErrorSet!void {
    const initialiser = declaration.initialiser orelse return;
    const location = initialiser.location;
    if (declaration.resolution == .none) {
        declaration.resolution = .resolving;
        self.current_initialiser_expression = initialiser;
        try self.checkExpression(initialiser);
        // just in case
        self.current_initialiser_expression = null;
        declaration.resolution = .resolved;
    }

    if (declaration.typ) |typ| {
        if (declaration.resolution == .resolved) {
            try self.checkType(typ);
            const coerecion_type = try self.resolveTypeFromAliasing(typ);
            try self.coerceTo(initialiser, coerecion_type);
        }
    }

    // deduce from initialiser
    if (declaration.typ == null) {
        declaration.typ = initialiser.typ;
    }

    if (declaration.typ == null or declaration.typ.?.isNever()) {
        try self.pushErr(location, "cannot deduce type", .{});
        return error.Invalid;
    }
}

pub fn checkDeclaration(self: *Self, location: ast.Location, declaration: *ast.Declaration) ErrorSet!void {
    if (ast.Primitives.lookup(declaration.identifier)) |_| {
        try self.pushErr(location, "redeclaration of primitive '{s}'", .{declaration.identifier});
        return error.Invalid;
    }

    if (self.symbols.lookup(declaration.identifier)) |symbol| {
        if (symbol.resolution == .resolving) {
            try self.pushErr(location, "redeclaration of symbol '{s}'", .{declaration.identifier});
            if (symbol.initialiser) |initialiser|
                try self.pushInfo(initialiser.location, "previous declaration here", .{});
            return error.Invalid;
        }
    }

    if (declaration.is_type and declaration.mutable) {
        try self.pushErr(location, "type cannot be mutable", .{});
        return error.Invalid;
    }

    const initialiser = declaration.initialiser;
    const is_function = if (initialiser) |got_initialiser| got_initialiser.variant == .function else false;
    if (is_function) {
        declaration.typ = initialiser.?.variant.function.typ;
        try self.symbols.add(self.allocator, declaration);
    }

    try self.ensureResolveDeclaration(declaration);

    if (!is_function) {
        try self.symbols.add(self.allocator, declaration);
    }
}

pub fn checkType(self: *Self, typ: *const ast.Type) ErrorSet!void {
    switch (typ.*) {
        .primitive => {},
        .pointer => |inner| {
            try self.checkType(inner.element);
        },
        .array => |array| {
            if (array.length) |length|
                try self.resolveExpression(length, false);
            try self.checkType(array.element);
        },
        .function => |function| {
            for (function.parameters) |parameter| {
                try self.checkType(parameter.typ orelse return error.Invalid);
            }
            if (function.return_type) |return_type|
                try self.checkType(return_type);
        },
        .structure => |structure| {
            for (structure.fields) |field| {
                try self.checkType(field.typ orelse return error.Invalid);
            }

            if (self.current_initialiser_expression) |type_expr| {
                self.current_initialiser_expression = null;
                const old_scope = try self.symbols.enterScope(self.allocator, type_expr, self.symbols.current_namespace);
                defer self.symbols.restoreScope(old_scope);
                const old_namespace = self.symbols.current_namespace;
                self.symbols.current_namespace = self.symbols.current_scope;
                defer self.symbols.current_namespace = old_namespace;

                for (structure.decls) |*decl| {
                    try self.preCheckDeclaration(decl.location, decl);
                }
            }
        },
        .enumeration => |enumeration| {
            for (enumeration.fields) |variant| {
                if (variant.initialiser) |value| {
                    try self.checkExpression(value);
                }
            }

            if (self.current_initialiser_expression) |type_expr| {
                self.current_initialiser_expression = null;
                const old_scope = try self.symbols.enterScope(self.allocator, type_expr, self.symbols.current_namespace);
                defer self.symbols.restoreScope(old_scope);
                const old_namespace = self.symbols.current_namespace;
                self.symbols.current_namespace = self.symbols.current_scope;
                defer self.symbols.current_namespace = old_namespace;

                for (enumeration.decls) |*decl| {
                    try self.preCheckDeclaration(decl.location, decl);
                }
            }
        },
        .optional => |inner| {
            try self.checkType(inner);
        },
        .slice => |inner| {
            try self.checkType(inner.element);
        },
        .expression => |expression| {
            try self.checkExpression(expression);
        },
        .structure_literal, .enum_literal => {},
    }
}

pub fn resolveTypeFromAliasing(self: *Self, typ: *const ast.Type) ErrorSet!*const ast.Type {
    const old_mutable_access = self.is_mutable_access;
    self.is_mutable_access = false;
    defer self.is_mutable_access = old_mutable_access;

    var initial_typ = typ;
    while (initial_typ.* == .expression) {
        // search the scope
        try self.checkExpression(typ.expression);
        const expression_type = typ.expression.typ.?;
        if (expression_type.* == .primitive and expression_type.primitive == .typ) {
            switch (typ.expression.variant) {
                .identifier => {
                    if (ast.Primitives.lookup(typ.expression.variant.identifier)) |primitive| {
                        initial_typ = primitive;
                        break;
                    }
                    if (self.symbols.lookup(typ.expression.variant.identifier)) |symbol| {
                        initial_typ = symbol.initialiser.?.variant.typ;
                        break;
                    }
                    try self.pushErr(typ.expression.location, "undeclared identifier '{s}'", .{typ.expression.variant.identifier});
                    return error.Invalid;
                },
                .typ => |this_typ| {
                    initial_typ = this_typ;
                    break;
                },
                else => {
                    try self.pushErr(typ.expression.location, "unimplemented expression type '{s}'", .{@tagName(typ.expression.variant)});
                    return error.Invalid;
                },
            }
        }
    }
    return initial_typ;
}

pub fn coerceTypesTo(self: *Self, from: *const ast.Type, to: *const ast.Type, location: ast.Location) ErrorSet!void {
    if (self.areTypesSame(from, to)) {
        return;
    }

    if (from.isPrimitive(.never)) {
        return;
    }

    if (from.* == .function and to.* == .function) {
        if (self.areTypesSame(from, to)) {
            return;
        }
        try self.pushErr(location, "function type mismatch", .{});
        return error.Invalid;
    }

    if (from.* == .pointer and to.* == .pointer) {
        try self.checkType(from.pointer.element);
        try self.checkType(to.pointer.element);

        const from_pointer_element = try self.resolveTypeFromAliasing(from.pointer.element);
        const to_pointer_element = try self.resolveTypeFromAliasing(to.pointer.element);

        if (!self.areTypesSame(from_pointer_element, to_pointer_element)) {
            try self.pushErr(location, "pointer type mismatch", .{});
            try self.pushInfo(null, "expected {}", .{(to_pointer_element)});
            try self.pushInfo(null, "got {}", .{(from_pointer_element)});
            return error.Invalid;
        }
        if (!from.pointer.mutable and to.pointer.mutable) {
            try self.pushErr(location, "cannot coerce immutable pointer to mutable pointer", .{});
            return error.Invalid;
        }
        return;
    }

    if (from.* == .slice and to.* == .slice) {
        if (!self.areTypesSame(from.slice.element, to.slice.element)) {
            try self.pushErr(location, "slice type mismatch", .{});
            return error.Invalid;
        }
        if (!from.slice.mutable and to.slice.mutable) {
            try self.pushErr(location, "cannot coerce immutable slice to mutable slice", .{});
            return error.Invalid;
        }
        return;
    }

    if (from.* == .primitive and to.* == .primitive) {
        // non narrowing conversion
        if (from.primitive == to.primitive) {
            return;
        }
        if (from.primitive.isBool() and to.primitive.isBool()) {
            return;
        }
        const is_integer = from.primitive.isInteger() and to.primitive.isInteger();
        const is_float = from.primitive.isFloat() and to.primitive.isFloat();
        const same_sign = from.primitive.isSigned() == to.primitive.isSigned();
        if (same_sign and is_integer and from.primitive.getSize() <= to.primitive.getSize()) {
            return;
        }
        if (is_float and from.primitive.getSize() <= to.primitive.getSize()) {
            return;
        }
        try self.pushErr(location, "cannot coerce {s} to {s}", .{
            try prettyPrintType(from, self.reporter.?),
            try prettyPrintType(to, self.reporter.?),
        });
        return error.Invalid;
    }

    try self.pushErr(location, "cannot coerce {s} to {s}", .{
        try prettyPrintType(from, self.reporter.?),
        try prettyPrintType(to, self.reporter.?),
    });
    return error.Invalid;
}

pub fn coerceTo(self: *Self, from: *ast.Expression, _to: *const ast.Type) ErrorSet!void {
    const from_type: *const ast.Type = try self.resolveTypeFromAliasing(try self.getTypeOfExpression(from));
    const to_type = try self.resolveTypeFromAliasing(_to);

    if (from.variant == .structure_literal) {
        try self.checkStructureLiteral(from, to_type);
        return;
    }
    if (from.variant == .enum_literal) {
        try self.checkEnumLiteral(from, to_type);
        return;
    }
    if (from_type.* == .primitive and from_type.primitive == .nil) {
        if (to_type.* == .optional) {
            return;
        }
        try self.pushErr(from.location, "cannot coerce nil to {s}", .{try prettyPrintType(to_type, self.reporter.?)});
        return error.Invalid;
    }
    if (from_type.* == .array and to_type.* == .array) {
        const from_length = try self.resolveUnsignedIntegerLiteral(from_type.array.length orelse {
            return error.Invalid;
        });
        const to_length = try self.resolveUnsignedIntegerLiteral(to_type.array.length orelse {
            return error.Invalid;
        });
        if (from_length != to_length) {
            try self.pushErr(from.location, "array length mismatch", .{});
            try self.pushInfo(from_type.array.length.?.location, "expected {} got {}", .{ to_length, from_length });
            return error.Invalid;
        }
        if (!self.areTypesSame(from_type.array.element, to_type.array.element)) {
            try self.pushErr(from.location, "array element type mismatch", .{});
            return error.Invalid;
        }
        return;
    }

    try self.coerceTypesTo(from_type, to_type, from.location);
}

pub fn areTypesSame(self: *Self, a: *const ast.Type, b: *const ast.Type) bool {
    if (a == b) {
        return true;
    }
    if (a.* == .primitive and b.* == .primitive) {
        return a.primitive == b.primitive;
    }
    if (a.* == .pointer and b.* == .pointer) {
        return self.areTypesSame(a.pointer.element, b.pointer.element);
    }
    if (a.* == .array and b.* == .array) {
        return self.areTypesSame(a.array.element, b.array.element);
    }
    if (a.* == .function and b.* == .function) {
        if (a.function.parameters.len != b.function.parameters.len) {
            return false;
        }
        for (a.function.parameters, b.function.parameters) |a_param, b_param| {
            self.checkType(a_param.typ.?) catch return false;
            const resolved_a = self.resolveTypeFromAliasing(a_param.typ.?) catch return false;
            self.checkType(b_param.typ.?) catch return false;
            const resolved_b = self.resolveTypeFromAliasing(b_param.typ.?) catch return false;
            if (!self.areTypesSame(resolved_a, resolved_b)) {
                return false;
            }
        }
        const a_return = a.function.return_type orelse ast.Primitives.unit_type;
        const b_return = b.function.return_type orelse ast.Primitives.unit_type;
        self.checkType(a_return) catch return false;
        self.checkType(b_return) catch return false;

        return self.areTypesSame(
            self.resolveTypeFromAliasing(a_return) catch return false,
            self.resolveTypeFromAliasing(b_return) catch return false,
        );
    }
    if (a.* == .optional and b.* == .optional) {
        return self.areTypesSame(a.optional, b.optional);
    }
    if (a.* == .slice and b.* == .slice) {
        return self.areTypesSame(a.slice.element, b.slice.element);
    }
    return false;
}

pub const SymbolTable = struct {
    tagged_scopes: std.AutoArrayHashMapUnmanaged(?*const ast.Expression, *Scope) = .{},
    current_scope: ?*Scope = null,
    current_namespace: ?*Scope = null,
    root_scope: *Scope,

    anon_function_name_index: u64 = 0,

    pub fn init(allocator: std.mem.Allocator) ErrorSet!SymbolTable {
        var self: SymbolTable = .{
            .root_scope = undefined,
        };
        _ = try self.enterScope(allocator, null, null);
        self.root_scope = self.current_scope.?;
        self.current_namespace = self.root_scope;
        return self;
    }

    pub fn deinit(self: *SymbolTable, allocator: std.mem.Allocator) void {
        var it = self.tagged_scopes.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit(allocator);
            allocator.destroy(entry.value_ptr.*);
        }
        self.tagged_scopes.deinit(allocator);
    }

    /// returns the old scope
    pub fn enterScope(
        self: *SymbolTable,
        allocator: std.mem.Allocator,
        tag: ?*const ast.Expression,
        parent: ?*Scope,
    ) ErrorSet!?*Scope {
        const old_scope = self.current_scope;
        const scope = blk: {
            if (self.tagged_scopes.get(tag)) |got_scope| {
                break :blk got_scope;
            }
            const scope = try allocator.create(Scope);
            scope.* = .{
                .parent = parent orelse self.current_scope,
            };
            try self.tagged_scopes.putNoClobber(allocator, tag, scope);
            break :blk scope;
        };
        self.current_scope = scope;
        return old_scope;
    }

    pub fn restoreScope(self: *SymbolTable, scope: ?*Scope) void {
        self.current_scope = scope;
    }

    pub fn add(self: *SymbolTable, allocator: std.mem.Allocator, decl: Symbol) ErrorSet!void {
        const scope = self.current_scope orelse return error.Invalid;
        try scope.put(allocator, decl.identifier, decl);
        // std.debug.print("added symbol '{s}'\n", .{decl.identifier});
    }

    pub fn lookup(self: *SymbolTable, name: []const u8) ?Symbol {
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

const Scope = struct {
    symbols: std.StringHashMapUnmanaged(Symbol) = .{},
    parent: ?*Scope = null,

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

pub const Symbol = *ast.Declaration;
