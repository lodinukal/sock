const std = @import("std");
const ast = @import("ast.zig");
const Lexer = @import("Lexer.zig");
const ir = @import("ir.zig");

const SymbolTable = @import("check.zig").SymbolTable;

const Self = @This();

pub const ErrorSet = error{
    UndefinedSymbol,
    DuplicateSymbol,
    InvalidType,
    OutOfRange,
    OutOfMemory,
    NotImplemented,
};

allocator: std.mem.Allocator = undefined,
node_area: std.heap.ArenaAllocator = undefined,
node_allocator: std.mem.Allocator = undefined,
container: ?*ast.Container = null,
symbols: SymbolTable = undefined,
module: ir.Module = undefined,

pub fn init(self: *Self, allocator: std.mem.Allocator) ErrorSet!void {
    self.* = .{
        .allocator = allocator,
        .node_area = std.heap.ArenaAllocator.init(allocator),
        .node_allocator = self.node_area.allocator(),
        .symbols = try SymbolTable.init(self.node_allocator),
        .module = ir.Module.init(self.node_allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.node_area.deinit();
}

pub fn analyseExpression(self: *Self, expression: *ast.Expression) ErrorSet!ir.InstructionIndex {
    switch (expression.variant) {
        .boolean_literal => |boolean| {
            return try self.module.addValue(.{
                .boolean = boolean,
            });
        },
        .integer_literal => |integer| {
            return if (integer.signed)
                try self.module.addValue(.{ .signed_integer = integer.value })
            else
                try self.module.addValue(.{ .unsigned_integer = integer.value });
        },
        .float_literal => |float| {
            return try self.module.addValue(.{
                .float = float,
            });
        },
        .string_literal => |string| {
            return try self.module.addValue(.{
                .string = try self.module.intern(string),
            });
        },
        .enum_literal => |enumeration| {
            return try self.module.addValue(.{
                .enum_literal = try self.module.intern(enumeration),
            });
        },
        .char_literal => |char| {
            return try self.module.addValue(.{
                .char = char,
            });
        },
        .structure_literal => |structure| {
            _ = structure;
            unreachable;
            // return try self.module.addInstruction(.{ .structure_literal = structure });
        },
        .typ => |typ| {
            return try self.analyseType(typ);
        },
        .identifier => |identifier| {
            if (self.symbols.lookup(identifier)) |symbol| {
                return symbol.instruction;
            }
            std.debug.print("undefined symbol: {s}\n", .{identifier});
            return error.UndefinedSymbol;
        },
        else => unreachable,
        // binary,
        // unary,
        // call,
        // subscript,
        // field,
        // function,
        // lambda,
        // block,
        // pipeline,
        // undefined,
    }
}

pub fn analyseStatement(self: *Self, statement: *ast.Statement) ErrorSet!ir.InstructionIndex {
    switch (statement.variant) {
        .declaration => |*declaration| {
            return try self.analyseDeclaration(declaration);
        },
        // .expression => |*expression| {
        //     try self.analyseExpression(container, expression);
        // },
        else => return error.NotImplemented,
    }
}

/// returns the instruction index of the lhs of the assignment
pub fn analyseDeclaration(self: *Self, declaration: *ast.Declaration) ErrorSet!ir.InstructionIndex {
    var typ: ir.InstructionIndex = if (declaration.typ) |typ| try self.analyseType(typ) else .none;
    const initialiser: ir.InstructionIndex = try self.analyseExpression(declaration.initialiser);

    if (self.symbols.lookup(declaration.identifier)) |_| {
        return error.DuplicateSymbol;
    }
    if (typ != .none) {
        const typ_x = self.module.getInstruction(typ);
        std.debug.print("RAHH: {}\n", .{typ_x});
    }
    const initialiser_instruction = self.module.getInstruction(initialiser);
    var was_inferred = false;
    if (typ == .none) {
        // determine based on initialiser
        if (initialiser_instruction == .value) {
            const inferred = try self.inferTypeFromValue(initialiser_instruction.value);
            if (inferred == .none) {
                return error.InvalidType;
            }
            typ = inferred;
        } else if (initialiser_instruction == .typ) {
            typ = .type_type;
        } else {
            return error.InvalidType;
        }
        was_inferred = true;
    }

    if (typ == .none) {
        return error.InvalidType;
    }

    if (initialiser == .none) {
        return error.InvalidType;
    }

    if (!was_inferred and !self.isValueAssignable(initialiser_instruction.value, typ)) {
        return error.InvalidType;
    }

    const index = try self.module.addInstruction(.{ .declaration = .{
        .identifier = try self.module.intern(declaration.identifier),
        .mutable = declaration.mutable,
        .typ = typ,
        .init = initialiser,
    } });

    try self.symbols.add(self.node_allocator, declaration.identifier, .{
        .typ = typ,
        .mutable = declaration.mutable,
        .instruction = index,
    });

    return index;
}

pub fn analyseType(self: *Self, typ: *ast.Type) ErrorSet!ir.InstructionIndex {
    switch (typ.*) {
        .unit => {
            return .type_unit;
        },
        .primitive => |primitive| {
            switch (primitive) {
                .u8,
                .u16,
                .u32,
                .u64,
                .i8,
                .i16,
                .i32,
                .i64,
                => {
                    return switch (primitive) {
                        .i8 => .type_i8,
                        .i16 => .type_i16,
                        .i32 => .type_i32,
                        .i64 => .type_i64,
                        .u8 => .type_u8,
                        .u16 => .type_u16,
                        .u32 => .type_u32,
                        .u64 => .type_u64,
                        else => unreachable,
                    };
                },
                .f32,
                .f64,
                => {
                    return switch (primitive) {
                        .f32 => .type_f32,
                        .f64 => .type_f64,
                        else => unreachable,
                    };
                },
                .bool => {
                    return .type_boolean;
                },
            }
        },
        .optional => |optional| {
            const inner = try self.analyseType(optional);
            return try self.module.addType(.{
                .optional = inner,
            });
        },
        .pointer => |pointer| {
            const element = try self.analyseType(pointer.element);
            return try self.module.addType(.{
                .pointer = .{
                    .mutable = pointer.mutable,
                    .typ = element,
                },
            });
        },
        .array => |array| {
            const element = try self.analyseType(array.element);
            const length = try self.analyseExpression(array.length orelse return error.InvalidType);

            const length_instruction = self.module.getInstruction(length);
            if (length_instruction != .value) {
                return error.InvalidType;
            }

            if (length_instruction.value != .unsigned_integer) {
                return error.InvalidType;
            }

            return try self.module.addType(.{
                .array = .{
                    .size = @intCast(length_instruction.value.unsigned_integer),
                    .typ = element,
                },
            });
        },
        .slice => |slice| {
            const element = try self.analyseType(slice.element);
            return try self.module.addType(.{
                .slice = .{
                    .mutable = slice.mutable,
                    .typ = element,
                },
            });
        },
        .function => |function| {
            const return_type: ir.InstructionIndex = if (function.return_type) |return_type| try self.analyseType(return_type) else .type_unit;
            var args = try std.ArrayListUnmanaged(ir.InstructionIndex).initCapacity(self.node_allocator, function.parameters.len);
            for (function.parameters) |parameter| {
                const param_typ = try self.analyseType(parameter.typ orelse return error.InvalidType);
                args.appendAssumeCapacity(param_typ);
            }
            return try self.module.addType(.{
                .function = .{
                    .args = try self.module.addListOwned(args.items),
                    .return_type = return_type,
                },
            });
        },
        .expression => |expression| {
            const got = self.unwrapValue(try self.analyseExpression(expression));
            const instruction = self.module.getInstruction(got);
            if (instruction != .typ) {
                return error.InvalidType;
            }
            return got;
        },
        .enumeration => |enumeration| {
            var fields = try std.ArrayListUnmanaged(ir.InstructionIndex).initCapacity(self.node_allocator, enumeration.fields.len);
            var current: usize = 0;
            for (enumeration.fields) |field| {
                const field_name = try self.module.intern(field.key.variant.identifier);
                const value = if (field.initialiser) |initialiser| blk: {
                    const field_value = try self.analyseExpression(initialiser);
                    const value_instruction = self.module.getInstruction(field_value);

                    if (value_instruction.value == .instruction) {
                        return error.InvalidType;
                    }

                    if (value_instruction != .value) {
                        return error.InvalidType;
                    }

                    if (value_instruction.value != .unsigned_integer) {
                        return error.InvalidType;
                    }

                    break :blk value_instruction.value.unsigned_integer;
                } else blk: {
                    break :blk current + 1;
                };

                const field_index = try self.module.addInstruction(.{ .enumeration_field = .{
                    .name = field_name,
                    .value = @intCast(value),
                } });
                fields.appendAssumeCapacity(field_index);

                current = value;
            }
            return try self.module.addType(.{
                .enumeration = .{
                    .fields = try self.module.addListOwned(fields.items),
                },
            });
        },
        .structure => |structure| {
            var fields = try std.ArrayListUnmanaged(ir.InstructionIndex).initCapacity(self.node_allocator, structure.fields.len);
            for (structure.fields) |field| {
                const field_name = try self.module.intern(field.key.variant.identifier);
                const field_type = try self.analyseType(field.typ orelse return error.InvalidType);
                const default_value: ir.InstructionIndex = if (field.initialiser) |default|
                    try self.analyseExpression(default)
                else
                    .none;
                if (default_value != .none) {
                    const default_value_instruction = self.module.getInstruction(default_value);
                    if (default_value_instruction != .value) {
                        return error.InvalidType;
                    }
                    if (!self.isValueAssignable(default_value_instruction.value, field_type)) {
                        return error.InvalidType;
                    }
                }
                const field_index = try self.module.addInstruction(.{ .structure_field = .{
                    .name = field_name,
                    .typ = field_type,
                    .default = default_value,
                } });
                fields.appendAssumeCapacity(field_index);
            }
            return try self.module.addType(.{
                .structure = .{
                    .fields = try self.module.addListOwned(fields.items),
                },
            });
        },
    }
}

pub fn unwrapValue(self: *Self, instruction: ir.InstructionIndex) ir.InstructionIndex {
    switch (self.module.getInstruction(instruction)) {
        .declaration => |inst| return inst.init,
        else => return instruction,
    }
    return .none;
}

pub fn inferTypeFromValue(self: *Self, from: ir.Instruction.Value) ErrorSet!ir.InstructionIndex {
    switch (from) {
        .boolean => return .type_boolean,
        .char => return .type_u32,
        .signed_integer => return .type_i64,
        .unsigned_integer => return .type_u64,
        .float => return .type_f32,
        .string => return .type_string,
        .array => |array| {
            const elements = self.module.getList(array.items);
            const element_index = elements.items[0];
            const element = self.module.getInstruction(element_index);
            if (element != .value) {
                return error.InvalidType;
            }
            const element_type = try self.inferTypeFromValue(element.value);

            const size = elements.items.len;
            return try self.module.addType(.{
                .array = .{
                    .size = @intCast(size),
                    .typ = element_type,
                },
            });
        },
        .enum_literal => {
            return .type_enum_literal;
        },
        .nil => return .nil,
        .undefined => return .none,
        .instruction => unreachable,
    }
}

pub fn isValueAssignable(self: Self, from: ir.Instruction.Value, to: ir.InstructionIndex) bool {
    const to_type = self.module.getInstruction(to).typ;
    switch (from) {
        .boolean => {
            return to_type == .boolean;
        },
        .char, .signed_integer => {
            return to_type == .integer and to_type.integer.signed();
        },
        .unsigned_integer => {
            return to_type == .integer and !to_type.integer.signed();
        },
        .float => {
            return to_type == .float;
        },
        .string => {
            return to_type == .string;
        },
        .nil => {
            return to_type == .optional;
        },
        .enum_literal => {
            return self.isEnumMember(to, self.module.getSlice(u8, from.enum_literal));
        },
        .array => |array| {
            if (to_type != .array) {
                return false;
            }
            const list = self.module.getList(array.items);
            if (to_type.array.size != list.items.len) {
                return false;
            }
            return self.areTypesSame(to_type.array.typ, array.typ);
        },
        .undefined => return true,
        .instruction => unreachable,
    }
}

pub fn isEnumMember(self: Self, enumeration: ir.TaggedInstructionIndex(ir.Instruction.Type), member: []const u8) bool {
    const enumeration_instruction = self.module.getInstruction(enumeration).typ;
    if (enumeration_instruction != .enumeration) {
        return false;
    }
    const list_index = enumeration_instruction.enumeration.fields;
    const list = self.module.getList(list_index);
    for (list.items) |field_index| {
        const field = self.module.getInstruction(field_index).enumeration_field;
        const field_name = self.module.getSlice(u8, field.name);
        if (std.mem.eql(u8, field_name, member)) {
            return true;
        }
    }
    return false;
}

pub fn areTypesSame(self: Self, a: ir.InstructionIndex, b: ir.InstructionIndex) bool {
    const a_instruction_index = (a);
    const b_instruction_index = (b);

    if (a_instruction_index == b_instruction_index) {
        return true;
    }

    const a_instruction = self.module.getInstruction(a_instruction_index).typ;
    const b_instruction = self.module.getInstruction(b_instruction_index).typ;

    switch (a_instruction) {
        .unit => return b_instruction == .unit,
        .boolean => return b_instruction == .boolean,
        .integer => |integer| {
            return b_instruction == .integer and integer == b_instruction.integer;
        },
        .float => |float| {
            return b_instruction == .float and float == b_instruction.float;
        },
        .string => return b_instruction == .string,
        .optional => |optional| {
            return b_instruction == .optional and
                self.areTypesSame(optional, b_instruction.optional);
        },
        .pointer => |pointer| {
            return b_instruction == .pointer and
                self.areTypesSame(pointer.typ, b_instruction.pointer.typ) and
                pointer.mutable == b_instruction.pointer.mutable;
        },
        .array => |array| {
            return b_instruction == .array and
                self.areTypesSame(array.typ, b_instruction.array.typ);
        },
        .slice => |slice| {
            return b_instruction == .slice and
                self.areTypesSame(slice.typ, b_instruction.slice.typ) and
                slice.mutable == b_instruction.slice.mutable;
        },
        .enumeration,
        .structure,
        => return false,
        else => return true,
    }
}

pub fn isTypeAssignableTo(self: Self, from: ir.InstructionIndex, to: ir.InstructionIndex) bool {
    if (from == to) {
        return true;
    }

    const from_instruction_index = (from);
    const to_instruction_index = (to);

    if (from_instruction_index == to_instruction_index) {
        return true;
    }

    const from_instruction = self.module.getInstruction(from_instruction_index).typ;
    const to_instruction = self.module.getInstruction(to_instruction_index).typ;

    const active_from = std.meta.activeTag(from_instruction);
    const active_to = std.meta.activeTag(to_instruction);

    if (active_from != active_to) {
        return false;
    }

    switch (from_instruction) {
        .float => |float_kind| {
            return float_kind == to_instruction.float;
        },
        .integer => |integer_kind| {
            return integer_kind == to_instruction.integer;
        },
        .slice => |slice| {
            if (!to_instruction.slice.mutable) {
                return false;
            }
            return self.isTypeAssignableTo(slice.typ, to_instruction.slice.typ);
        },
        .pointer => |pointer| {
            if (pointer.mutable != to_instruction.pointer.mutable) {
                return false;
            }
            return self.isTypeAssignableTo(pointer.typ, to_instruction.pointer.typ);
        },
        .optional => |optional| {
            return self.isTypeAssignableTo(optional, to_instruction.optional);
        },
        .array => |array| {
            if (array.size != to_instruction.array.size) {
                return false;
            }
            return self.isTypeAssignableTo(array.typ, to_instruction.array.typ);
        },
        // these types are distinct and so must be resolved with the same index
        .enumeration,
        .structure,
        => return false,
        else => return true,
    }
}
