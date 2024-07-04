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

current_function: ?*ast.Expression = null,
current_structure: ?*ast.Expression = null,
current_enumeration: ?*ast.Expression = null,
current_block: ?*ast.Expression = null,
current_pipeline: ?*ast.Expression = null,

pub fn pushErr(self: *Self, location: ast.Location, comptime fmt: []const u8, args: anytype) ErrorSet!void {
    if (self.reporter) |reporter| {
        try reporter.push(.err, location, fmt, args);
    }
}

pub fn pushInfo(self: *Self, location: ast.Location, comptime fmt: []const u8, args: anytype) ErrorSet!void {
    if (self.reporter) |reporter| {
        try reporter.push(.info, location, fmt, args);
    }
}

pub fn checkExpression(self: *Self, expression: *ast.Expression) ErrorSet!void {
    switch (expression.variant) {
        .binary => |*binary| {
            try self.checkExpression(binary.left);
            try self.checkExpression(binary.right);

            // TODO: replace with sound logic

            const left_type = try self.getTypeFromExpression(binary.left);
            const right_type = try self.getTypeFromExpression(binary.right);

            switch (binary.op) {
                .plus, .minus, .star, .slash, .percent => {
                    if (left_type != .primitive or right_type != .primitive) {
                        try self.pushErr(expression.location, "binary operator '{}' requires operands of type 'primitive'", .{binary.op});
                        try self.pushInfo(binary.left.location, "left operand is of type '{}'", .{left_type});
                        try self.pushInfo(binary.right.location, "right operand is of type '{}'", .{right_type});
                        return error.Invalid;
                    }
                    if (left_type.primitive != right_type.primitive) {
                        try self.pushErr(expression.location, "binary operator '{}' requires operands of the same type", .{binary.op});
                        try self.pushInfo(binary.left.location, "left operand is of type '{}'", .{left_type});
                        try self.pushInfo(binary.right.location, "right operand is of type '{}'", .{right_type});
                        return error.Invalid;
                    }
                    binary.result_type = left_type;
                    expression.typ = binary.result_type;
                },
                else => {
                    try self.pushErr(expression.location, "unimplemented binary operator '{}'", .{binary.op});
                    return error.Invalid;
                },
            }

            // TODO: check types are compatible
        },
        .block => |block| {
            const old_block = self.current_block;

            self.current_block = expression;
            defer {
                self.current_block = old_block;
            }

            for (block.statements) |statement| {
                try self.checkStatement(statement);
            }

            // TODO: get all the exit points and check they all return the same type
        },
        .typ => |typ| {
            try self.checkType(typ);
            expression.typ = Primitives.type_type;
        },
        .identifier => |identifier| {
            if (std.mem.eql(u8, identifier, "_")) {
                if (self.current_pipeline) |pipeline| {
                    expression.typ = pipeline.variant.pipeline.result_type;
                    return;
                } else {
                    try self.pushErr(expression.location, "use of '_' outside of pipeline", .{});
                    return;
                }
            }
            if (Primitives.lookup(identifier)) |typ| {
                expression.typ = Primitives.type_type;
                expression.variant = .{
                    .typ = typ,
                };
                return;
            }
            if (self.symbols.lookup(identifier)) |symbol| {
                expression.typ = symbol.typ;
                return;
            }
            try self.pushErr(expression.location, "undeclared identifier '{s}'", .{identifier});
        },
        .function => |function| {
            try self.checkType(function.typ);
            try self.symbols.enterScope(self.allocator);
            defer self.symbols.exitScope();

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

            expression.typ = function.typ.*;
        },
        .float_literal => {
            expression.typ = Primitives.f32_type;
        },
        .integer_literal => {
            expression.typ = Primitives.i32_type;
        },
        .boolean_literal => {
            expression.typ = Primitives.bool_type;
        },
        .string_literal => {
            // expression.typ = Primitives
        },
        .structure_literal => {},
        .pipeline => |*pipeline| {
            // TODO: actually resolve all type passing through
            // for now let's just use the initial
            const old_pipeline = self.current_pipeline;
            self.current_pipeline = expression;
            defer {
                self.current_pipeline = old_pipeline;
            }

            pipeline.result_type = pipeline.stages[0].typ;
            for (pipeline.stages[1..]) |stage| {
                try self.checkExpression(stage);
                pipeline.result_type = stage.typ;
            }

            expression.typ = pipeline.result_type;
            std.debug.print("pipeline result type: {?}\n", .{expression.typ});
        },
        .field => |field_access| {
            try self.checkExpression(field_access.record);
            const record_type = try self.resolveTypeFromAliasing(try self.getTypeFromExpression(field_access.record));
            if (record_type == .structure) {
                for (record_type.structure.fields) |field| {
                    if (std.mem.eql(u8, field.key.variant.identifier, field_access.field)) {
                        expression.typ = field.typ.?.*;
                        return;
                    }
                }
                try self.pushErr(expression.location, "field '{s}' not found in structure", .{field_access.field});
                return error.Invalid;
            } else {
                try self.pushErr(expression.location, "field access on non-structure type", .{});
                try self.pushInfo(field_access.record.location, "expected structure type ({})", .{record_type});
                return error.Invalid;
            }
        },
        .call => |call| {
            try self.checkExpression(call.callee);
            const callee_type = try self.resolveTypeFromAliasing(try self.getTypeFromExpression(call.callee));
            if (callee_type == .function) {
                if (callee_type.function.parameters.len != call.arguments.len) {
                    try self.pushErr(expression.location, "function call argument count mismatch", .{});
                    try self.pushInfo(call.callee.location, "expected '{}'", .{callee_type.function.parameters.len});
                    try self.pushInfo(expression.location, "got '{}'", .{call.arguments.len});
                    return error.Invalid;
                }
                for (call.arguments) |argument| {
                    try self.checkExpression(argument);
                }
                expression.typ = (callee_type.function.return_type orelse &Primitives.unit_type).*;
            } else {
                try self.pushErr(expression.location, "call on non-function type", .{});
                return error.Invalid;
            }
        },
        else => {
            try self.pushInfo(expression.location, "unimplemented expression type '{s}'", .{@tagName(expression.variant)});
        },
    }
}

pub fn getTypeFromExpression(_: Self, expression: *ast.Expression) ErrorSet!ast.Type {
    if (expression.typ) |typ| {
        return typ;
    }
    return error.Invalid;
}

pub fn checkStatement(self: *Self, statement: *ast.Statement) ErrorSet!void {
    switch (statement.variant) {
        .expression => |expression| {
            try self.checkExpression(expression);
        },
        .declaration => |*declaration| {
            try self.checkDeclaration(statement.location, declaration);
        },
        .@"return" => |return_stmt| {
            if (return_stmt) |got| {
                try self.checkExpression(got);
            }

            if (self.current_function) |function| {
                const got_return_type: ?*const ast.Type = if (function.variant.function.typ.function.return_type) |typ| typ else null;
                const return_type = if (got_return_type) |typ| typ.* else Primitives.unit_type;
                const resolved_return_type = try self.resolveTypeFromAliasing(return_type);
                if (return_stmt) |got| {
                    self.coerceTo(got, resolved_return_type) catch |err| {
                        try self.pushErr(got.location, "return type mismatch", .{});
                        try self.pushInfo(function.location, "expected return type '{}'", .{resolved_return_type});
                        try self.pushInfo(got.location, "got '{}'", .{got});
                        return err;
                    };
                } else if (resolved_return_type != .primitive or resolved_return_type.primitive != .unit) {
                    try self.pushErr(statement.location, "expected return value", .{});
                    return error.Invalid;
                }
            } else {
                try self.pushErr(statement.location, "return statement outside of function", .{});
                return error.Invalid;
            }
        },
        inline else => {
            try self.pushInfo(statement.location, "unimplemented statement type '{s}'", .{@tagName(statement.variant)});
        },
    }
}

pub fn checkFunctionParameterDeclaration(self: *Self, field: *ast.Field) ErrorSet!void {
    const field_name = field.key.variant.identifier;
    if (Primitives.lookup(field_name)) |_| {
        try self.pushErr(field.location, "redeclaration of primitive '{s}'", .{field_name});
        return;
    }

    if (self.symbols.lookup(field_name)) |symbol| {
        try self.pushErr(field.location, "redeclaration of symbol '{s}'", .{field_name});
        try self.pushInfo(symbol.location, "previous declaration here", .{});
        return;
    }
    try self.checkType(field.typ orelse return error.Invalid);
    try self.symbols.add(self.allocator, field_name, .{
        .typ = field.typ.?.*,
        .mutable = false,
        .expression = field.key,
        .location = field.location,
    });
}

pub fn checkDeclaration(self: *Self, location: ast.Location, declaration: *ast.Declaration) ErrorSet!void {
    if (Primitives.lookup(declaration.identifier)) |_| {
        try self.pushErr(location, "redeclaration of primitive '{s}'", .{declaration.identifier});
        return;
    }

    if (self.symbols.lookup(declaration.identifier)) |symbol| {
        try self.pushErr(location, "redeclaration of symbol '{s}'", .{declaration.identifier});
        try self.pushInfo(symbol.location, "previous declaration here", .{});
        return;
    }
    try self.checkExpression(declaration.initialiser);
    if (declaration.typ) |typ|
        try self.checkType(typ);

    // deduce from initialiser
    if (declaration.typ == null) {
        declaration.typ = if (declaration.initialiser.typ) |typ| &typ else null;
    }

    if (declaration.typ == null) {
        try self.pushErr(location, "cannot deduce type", .{});
        return error.Invalid;
    }

    try self.symbols.add(self.allocator, declaration.identifier, .{
        .typ = (declaration.typ orelse &Primitives.i32_type).*,
        .mutable = declaration.mutable,
        .expression = declaration.initialiser,
        .location = location,
    });
}

pub fn checkType(self: *Self, typ: *const ast.Type) ErrorSet!void {
    switch (typ.*) {
        .primitive => {},
        .pointer => |inner| {
            try self.checkType(inner.element);
        },
        .array => |array| {
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
        },
        .enumeration => |_| {
            // for (enumeration.fields) |variant| {
            //     if (variant.key) |value| {
            //         try self.checkExpression(value);
            //     }
            // }
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
    }
}

pub fn resolveTypeFromAliasing(self: *Self, typ: ast.Type) ErrorSet!ast.Type {
    var initial_typ = typ;
    while (initial_typ == .expression) {
        // search the scope
        try self.checkExpression(typ.expression);
        const expression_type = typ.expression.typ.?;
        if (expression_type == .primitive and expression_type.primitive == .typ) {
            switch (typ.expression.variant) {
                .identifier => {
                    std.debug.print("identifier: {s}\n", .{typ.expression.variant.identifier});
                    if (Primitives.lookup(typ.expression.variant.identifier)) |primitive| {
                        initial_typ = primitive.*;
                    }
                    if (self.symbols.lookup(typ.expression.variant.identifier)) |symbol| {
                        initial_typ = symbol.expression.variant.typ.*;
                    }
                    try self.pushErr(typ.expression.location, "undeclared identifier '{s}'", .{typ.expression.variant.identifier});
                    return error.Invalid;
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

pub fn coerceTo(self: *Self, from: *ast.Expression, to_type: ast.Type) ErrorSet!void {
    const to = try self.resolveTypeFromAliasing(to_type);
    if (from.typ) |from_typ| {
        if (from_typ == .primitive and to == .primitive) {
            // non narrowing conversion
            if (from_typ.primitive == to.primitive) {
                return;
            }
            if (from_typ.primitive.isBool() and to.primitive.isBool()) {
                return;
            }
            const is_integer = from_typ.primitive.isInteger() and to.primitive.isInteger();
            const is_float = from_typ.primitive.isFloat() and to.primitive.isFloat();
            const same_sign = from_typ.primitive.isSigned() == to.primitive.isSigned();
            if (same_sign and is_integer and from_typ.primitive.getSize() <= to.primitive.getSize()) {
                return;
            }
            if (is_float and from_typ.primitive.getSize() <= to.primitive.getSize()) {
                return;
            }
            try self.pushErr(from.location, "cannot coerce '{}' to '{}'", .{ from_typ, to });
            return error.Invalid;
        }
    }
    if (from.variant == .structure_literal) {
        if (to == .structure) {
            // check all fields make a hash map
            var field_map = std.StringHashMap(ast.Field).init(self.allocator);
            defer field_map.deinit();

            for (to.structure.fields) |field| {
                try field_map.put(field.key.variant.identifier, field);
            }

            for (from.variant.structure_literal) |field| {
                const field_name = field.key.variant.identifier;
                if (field_map.get(field_name)) |to_field| {
                    self.coerceTo(field.initialiser orelse {
                        return error.Invalid;
                    }, (to_field.typ orelse return error.Invalid).*) catch {
                        try self.pushErr(from.location, "field '{s}' not coercible", .{field_name});
                    };
                } else {
                    try self.pushErr(from.location, "field '{s}' not found in structure", .{field_name});
                }

                _ = field_map.remove(field_name);
            }

            var it = field_map.iterator();
            while (it.next()) |entry| {
                const field = entry.value_ptr.*;
                const field_name = field.key.variant.identifier;
                if (field.initialiser == null) {
                    try self.pushErr(from.location, "field '{s}' not initialised", .{field_name});
                }
            }
        }
    }
}

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
        typ: ast.Type,
        mutable: bool,
        expression: *ast.Expression,
        location: ast.Location,
    };
    pub fn add(self: *SymbolTable, allocator: std.mem.Allocator, name: []const u8, info: AddInfo) ErrorSet!void {
        const scope = self.current_scope orelse return error.Invalid;
        try scope.put(allocator, name, .{
            .name = name,
            .typ = info.typ,
            .mutable = info.mutable,
            .expression = info.expression,
            .location = info.location,
        });
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
    typ: ast.Type,
    mutable: bool,
    location: ast.Location,
    expression: *ast.Expression,
};
