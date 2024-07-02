const std = @import("std");
const ast = @import("ast.zig");
const Lexer = @import("Lexer.zig");

pub fn TaggedInstructionIndex(comptime T: type) type {
    _ = T;
    return InstructionIndex;
}
pub const InstructionIndex = enum(u32) {
    type_unit = 0,
    type_u8 = 1,
    type_u16 = 2,
    type_u32 = 3,
    type_u64 = 4,
    type_i8 = 5,
    type_i16 = 6,
    type_i32 = 7,
    type_i64 = 8,
    type_f32 = 9,
    type_f64 = 10,
    type_boolean = 11,
    type_string = 12,
    type_enum_literal = 13,
    type_type = 14,
    nil = 15,
    undefined = 16,

    none = std.math.maxInt(u32),
    _,

    pub const max: u32 = @intFromEnum(InstructionIndex.undefined);
};
pub const DataIndex = enum(u32) { none = std.math.maxInt(u32), _ };
pub const Instruction = union(enum) {
    typ: Type,
    value: Value,
    declaration: Declaration,
    assignment: Assignment,

    function: Function,
    block: Block,

    bin_op: BinaryOperation,
    unary_op: UnaryOperation,
    call: Call,
    field_access: FieldAccess,
    subscript: SubscriptAccess,

    if_statement: IfStatement,
    while_statement: WhileStatement,
    for_statement: ForStatement,
    match_statement: MatchStatement,
    return_statement: ReturnStatement,
    break_statement: BreakStatement,

    enumeration_field: EnumerationField,
    structure_field: StructureField,
    case: Case,

    pub fn formatExtra(self: Instruction, module: Module, writer: anytype) !void {
        switch (self) {
            .declaration => |*declaration| {
                try writer.print("[decl] {s}: {} (mutable: {}) = {}", .{
                    module.getSlice(u8, declaration.identifier),
                    declaration.typ,
                    declaration.mutable,
                    declaration.init,
                });
            },
            .typ => |*typ| {
                switch (typ.*) {
                    .unit => try writer.print("[type] unit", .{}),
                    .typ => try writer.print("[type] typ", .{}),
                    .integer => |integer| {
                        try writer.print("[type] integer: {}", .{integer});
                    },
                    .float => |float| {
                        try writer.print("[type] float: {}", .{float});
                    },
                    .boolean => try writer.print("[type] boolean", .{}),
                    .string => try writer.print("[type] string", .{}),
                    .enum_literal => try writer.print("[type] enum_literal", .{}),
                    .enumeration => |enumeration| {
                        try writer.print("[type] enumeration: ", .{});
                        const list = module.getList(enumeration.fields);
                        for (list.items) |field| {
                            const field_inst = module.getInstruction(field);
                            try writer.writeAll("    ");
                            try field_inst.formatExtra(module, writer);
                            try writer.writeAll("\n");
                        }
                    },
                    .structure => |structure| {
                        try writer.print("[type] structure: ", .{});
                        const list = module.getList(structure.fields);
                        for (list.items) |field| {
                            const field_inst = module.getInstruction(field);
                            try writer.writeAll("    ");
                            try field_inst.formatExtra(module, writer);
                            try writer.writeAll("\n");
                        }
                    },
                    .optional => |optional| {
                        const inner = module.getInstruction(optional);
                        try writer.print("[type] optional: ", .{});
                        try inner.formatExtra(module, writer);
                    },
                    .pointer => |pointer| {
                        try writer.print("[type] pointer (mutable: {}) to ", .{pointer.mutable});
                        const inner = module.getInstruction(pointer.typ);
                        try inner.formatExtra(module, writer);
                    },
                    .array => |array| {
                        try writer.print("[type] array size: {} of", .{array.size});
                        const inner = module.getInstruction(array.typ);
                        try inner.formatExtra(module, writer);
                    },
                    .slice => |slice| {
                        try writer.print("[type] slice (mutable: {}) of", .{slice.mutable});
                        const inner = module.getInstruction(slice.typ);
                        try inner.formatExtra(module, writer);
                    },
                    .function => |function| {
                        try writer.print("[type] function: ", .{});
                        const args_list = module.getList(function.args);
                        for (args_list.items) |arg| {
                            const arg_inst = module.getInstruction(arg);
                            try arg_inst.formatExtra(module, writer);
                            try writer.writeAll(", ");
                        }
                        try writer.writeAll(" -> ");
                        const inner = module.getInstruction(function.return_type);
                        try inner.formatExtra(module, writer);
                    },
                }
            },
            .structure_field => |*field| {
                try writer.print("[field] {s}: ", .{module.getSlice(u8, field.name)});
                const typ = module.getInstruction(field.typ);
                try typ.formatExtra(module, writer);
                if (field.default != .none) {
                    try writer.writeAll(" = ");
                    const default = module.getInstruction(field.default);
                    try default.formatExtra(module, writer);
                }
            },
            else => {
                try writer.print("{}", .{std.meta.activeTag(self)});
            },
        }
    }

    pub const EnumerationField = struct {
        name: DataIndex,
        value: u32,
    };

    pub const StructureField = struct {
        name: DataIndex,
        typ: TaggedInstructionIndex(Type),
        default: TaggedInstructionIndex(Value) = .none, // value
    };

    pub const Type = union(enum) {
        unit,
        typ,
        integer: IntegerKind,
        float: FloatKind,
        boolean,
        string,
        enum_literal,
        enumeration: struct {
            fields: TaggedListIndex(EnumerationField),
        },
        structure: struct {
            fields: TaggedListIndex(StructureField),
        },
        optional: TaggedInstructionIndex(Type),
        pointer: struct {
            mutable: bool,
            typ: TaggedInstructionIndex(Type),
        },
        array: struct {
            size: u32,
            typ: TaggedInstructionIndex(Type),
        },
        slice: struct {
            mutable: bool,
            typ: TaggedInstructionIndex(Type),
        },
        function: struct {
            args: TaggedListIndex(Type),
            return_type: TaggedInstructionIndex(Type),
        },
    };

    pub const FloatKind = enum {
        f32,
        f64,
    };

    pub const IntegerKind = enum {
        i8,
        i16,
        i32,
        i64,
        u8,
        u16,
        u32,
        u64,

        pub fn signed(self: @This()) bool {
            return switch (self) {
                .i8, .i16, .i32, .i64 => true,
                else => false,
            };
        }
    };

    /// something constant, maybe should express this in data but oh well
    pub const Value = union(ValueKind) {
        instruction: InstructionIndex,
        signed_integer: u64,
        unsigned_integer: u64,
        float: f64,
        boolean: bool,
        char: u32,
        string: DataIndex,
        enum_literal: DataIndex,
        array: struct {
            items: TaggedListIndex(Value),
            typ: TaggedInstructionIndex(Type),
        },
        // structure_literal: TaggedListIndex(Value),
        undefined,
        nil,
    };

    pub const ValueKind = enum {
        instruction,
        signed_integer,
        unsigned_integer,
        float,
        boolean,
        char,
        string,
        enum_literal,
        array,
        undefined,
        nil,
    };

    pub const Declaration = struct {
        // mut/let
        mutable: bool,
        // NAME
        identifier: DataIndex,
        // : TYPE
        typ: TaggedInstructionIndex(Type),
        // = EXPRESSION
        init: InstructionIndex = .none,
    };

    pub const Assignment = struct {
        // TARGET
        target: InstructionIndex,
        // = VALUE
        value: InstructionIndex,
    };

    pub const Function = struct {
        typ: TaggedInstructionIndex(Type), // Type.function
        // { BODY }
        body_block: TaggedInstructionIndex(Block) = .none,
    };

    pub const Block = struct {
        instructions: ListIndex,
    };

    pub const BinaryOperation = struct {
        op: Lexer.Token.Kind,
        left: InstructionIndex,
        right: InstructionIndex,
    };

    pub const UnaryOperation = struct {
        op: Lexer.Token.Kind,
        operand: InstructionIndex,
    };

    pub const Call = struct {
        operand: InstructionIndex,
        args: ListIndex,
    };

    pub const FieldAccess = struct {
        operand: InstructionIndex,
        field: DataIndex,
    };

    pub const SubscriptAccess = struct {
        operand: InstructionIndex,
        index: InstructionIndex,
    };

    pub const IfStatement = struct {
        condition: InstructionIndex,
        then_block: TaggedInstructionIndex(Block),
        else_block: TaggedInstructionIndex(Block) = .none,
    };

    pub const WhileStatement = struct {
        condition: InstructionIndex,
        body_block: TaggedInstructionIndex(Block),
    };

    pub const ForStatement = struct {
        condition: InstructionIndex,
        capture: ListIndex,
        body_block: TaggedInstructionIndex(Block),
    };

    pub const MatchStatement = struct {
        value: InstructionIndex,
        cases: TaggedListIndex(Case),
    };

    pub const Case = struct {
        pattern: InstructionIndex,
        block: TaggedInstructionIndex(Block),
    };

    pub const ReturnStatement = struct {
        value: InstructionIndex = .none,
    };

    pub const BreakStatement = struct {
        label: DataIndex = .none,
    };
};

pub const PrimitiveInstructions = struct {
    pub const list = [_]Instruction{
        .{ .typ = .unit },
        .{ .typ = .{ .integer = .u8 } },
        .{ .typ = .{ .integer = .u16 } },
        .{ .typ = .{ .integer = .u32 } },
        .{ .typ = .{ .integer = .u64 } },
        .{ .typ = .{ .integer = .i8 } },
        .{ .typ = .{ .integer = .i16 } },
        .{ .typ = .{ .integer = .i32 } },
        .{ .typ = .{ .integer = .i64 } },
        .{ .typ = .{ .float = .f32 } },
        .{ .typ = .{ .float = .f64 } },
        .{ .typ = .boolean },
        .{ .typ = .string },
        .{ .typ = .enum_literal },
        .{ .typ = .typ },
        .{ .value = .nil },
        .{ .value = .undefined },
    };
};

pub const ListIndex = enum(u32) { none = std.math.maxInt(u32), _ };
pub fn TaggedListIndex(comptime T: type) type {
    _ = T;
    return ListIndex;
}

pub const Module = struct {
    allocator: std.mem.Allocator,
    interner: Interner = .{},
    instructions: std.ArrayListUnmanaged(Instruction) = .{},
    lists: std.ArrayListUnmanaged(std.ArrayListUnmanaged(InstructionIndex)) = .{},

    pub fn init(allocator: std.mem.Allocator) Module {
        const self: Module = .{
            .allocator = allocator,
        };
        return self;
    }

    pub fn deinit(self: *Module) void {
        self.interner.deinit(self.allocator);
        self.instructions.deinit(self.allocator);
    }

    pub fn addInstruction(self: *Module, instruction: Instruction) !InstructionIndex {
        const index: InstructionIndex = @enumFromInt(self.instructions.items.len + InstructionIndex.max);
        try self.instructions.append(self.allocator, instruction);
        return index;
    }

    pub fn addValue(
        self: *Module,
        data: Instruction.Value,
    ) !InstructionIndex {
        return self.addInstruction(.{ .value = data });
    }

    pub fn addType(self: *Module, typ: Instruction.Type) !InstructionIndex {
        return self.addInstruction(.{ .typ = typ });
    }

    pub fn getInstruction(self: Module, index: InstructionIndex) Instruction {
        const number: usize = @intFromEnum(index);
        if (number >= InstructionIndex.max) {
            return self.instructions.items[number - InstructionIndex.max];
        }
        return PrimitiveInstructions.list[number];
    }

    pub fn addList(self: *Module, list: []const InstructionIndex) !ListIndex {
        const index: ListIndex = @enumFromInt(self.lists.items.len);
        try self.lists.append(self.allocator, std.ArrayListUnmanaged(Instruction).initCapacity(self.allocator, list.len));
        if (list.len > 0) {
            const got_list = self.getList(index);
            try got_list.appendSliceAssumeCapacity(self.allocator, list);
        }
        return index;
    }

    pub fn addListOwned(self: *Module, list: []InstructionIndex) !ListIndex {
        const index: ListIndex = @enumFromInt(self.lists.items.len);
        try self.lists.append(self.allocator, std.ArrayListUnmanaged(InstructionIndex).fromOwnedSlice(list));
        return index;
    }

    pub fn getList(self: Module, index: ListIndex) std.ArrayListUnmanaged(InstructionIndex) {
        return self.lists.items[@intFromEnum(index)];
    }

    pub fn intern(self: *Module, string: []const u8) !DataIndex {
        return self.interner.intern(self.allocator, string);
    }

    pub fn contains(self: Module, string: []const u8) bool {
        return self.interner.contains(string);
    }

    pub fn get(self: Module, comptime T: type, index: DataIndex) T {
        return self.interner.getAs(T, index);
    }

    pub fn getSlice(self: Module, comptime T: type, index: DataIndex) []const T {
        return self.interner.getSlice(T, index);
    }

    pub fn format(self: Module, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        for (self.instructions.items) |inst| {
            try inst.formatExtra(self, writer);
            _ = try writer.write("\n\n");
        }
    }
};

pub const Interner = struct {
    strings: std.StringArrayHashMapUnmanaged(void) = .{},

    pub fn init(allocator: std.mem.Allocator) Interner {
        return .{
            .allocator = allocator,
            .strings = .{},
            .data = .{},
        };
    }

    pub fn deinit(self: *Interner, allocator: std.mem.Allocator) void {
        var it = self.strings.iterator();
        while (it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
        }
        self.strings.deinit(allocator);
    }

    pub fn intern(self: *Interner, allocator: std.mem.Allocator, string: []const u8) !DataIndex {
        if (self.strings.getIndex(string)) |existing| {
            return @enumFromInt(existing);
        }

        const start = self.strings.count();
        const duped = try allocator.dupe(u8, string);
        try self.strings.put(allocator, duped, {});
        return @enumFromInt(start);
    }

    pub fn contains(self: *Interner, string: []const u8) bool {
        return self.strings.contains(string);
    }

    fn getRaw(self: Interner, index: DataIndex) []const u8 {
        return self.strings.entries.get(@intFromEnum(index)).key;
    }

    pub fn getAs(self: Interner, comptime T: type, index: DataIndex) T {
        return std.mem.bytesAsValue(T, self.getRaw(index));
    }

    pub fn getSlice(self: Interner, comptime T: type, index: DataIndex) []const T {
        return std.mem.bytesAsSlice(T, self.getRaw(index));
    }
};
