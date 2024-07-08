const std = @import("std");
const ast = @import("ast.zig");
const Reporter = @import("Reporter.zig");

pub const Error = error{
    OutOfMemory,
    Invalid,
};

pub const Context = struct {
    allocator: std.mem.Allocator,
    reporter: *Reporter,

    global_environment: Environment = .{
        .context = undefined,
    },
    environments: std.ArrayListUnmanaged(*Environment) = .{},
    values: std.ArrayListUnmanaged(Value) = .{},

    pub fn init(self: *Context) void {
        self.global_environment.context = self;
    }

    pub fn deinit(self: *Context) void {
        self.values.deinit(self.allocator);
        for (self.environments.items) |got| {
            got.deinit(self.allocator);
        }
        self.environments.deinit(self.allocator);
        self.global_environment.deinit(self.allocator);
    }

    const last_set_type_number: usize = @intFromEnum(Value.Kind.last_set_type);
    pub fn push(self: *Context, value: Value) !ValueIndex {
        try self.values.append(self.allocator, value);
        return @enumFromInt(self.values.items.len - 1 + last_set_type_number);
    }

    pub fn get(self: *Context, index: ValueIndex) Value {
        switch (@intFromEnum(index)) {
            0...(last_set_type_number - 1) => return Value{
                .data = .unit,
                .typ = index,
                .location = undefined,
                .mutable = false,
            },
            else => {
                return self.values.items[@intFromEnum(index) - last_set_type_number];
            },
        }
    }

    pub fn set(self: *Context, index: ValueIndex, value: Value) void {
        const number: usize = @intFromEnum(index);
        if (number < last_set_type_number) {
            return;
        }
        self.values.items[@intFromEnum(index) - last_set_type_number] = value;
    }

    pub fn resolveContainer(self: *Context, container: *ast.Container) Error!void {
        for (container.root_stmts.items) |statement| {
            try self.checkStatement(statement, &self.global_environment);
        }
    }

    pub fn checkStatement(self: *Context, statement: *ast.Statement, environment: *Environment) Error!void {
        switch (statement.variant) {
            .declaration => |declaration| {
                _ = try environment.define(self.allocator, declaration.identifier, .{
                    .data = .runtime_value,
                    .typ = @enumFromInt(14),
                    .location = declaration.initialiser.location,
                    .mutable = declaration.mutable,
                });
            },
            else => {},
        }
    }
};

pub const Environment = struct {
    context: *Context,
    parent: ?*Environment = null,
    values: std.StringArrayHashMapUnmanaged(ValueIndex) = .{},

    pub fn deinit(self: *Environment, allocator: std.mem.Allocator) void {
        self.values.deinit(allocator);
    }

    pub fn define(self: *Environment, allocator: std.mem.Allocator, name: []const u8, value: Value) !ValueIndex {
        const index = try self.context.push(value);
        try self.values.put(allocator, name, index);
        return index;
    }

    pub fn lookup(self: *Environment, name: []const u8) ?ValueIndex {
        return self.values.get(name);
    }

    pub fn resolve(self: *Environment, name: []const u8) ?ValueIndex {
        var current: ?*Environment = self;
        while (current) |got| {
            const value = got.lookup(name);
            if (value != null) {
                return value;
            }
            current = got.parent;
        }
        return null;
    }
};

pub const ValueIndex = enum(u32) {
    unit_type = 0,
    i8_type = 1,
    i16_type = 2,
    i32_type = 3,
    i64_type = 4,
    u8_type = 5,
    u16_type = 6,
    u32_type = 7,
    u64_type = 8,
    boolean_type = 9,
    f32_type = 10,
    f64_type = 11,
    string_type = 12,
    none = std.math.maxInt(u32),
    _,
};
pub const Value = struct {
    data: Data,
    typ: ValueIndex,
    location: ast.Location,
    mutable: bool,

    pub const Data = union(Kind) {
        unit_type,
        i8_type,
        i16_type,
        i32_type,
        i64_type,
        u8_type,
        u16_type,
        u32_type,
        u64_type,
        boolean_type,
        f32_type,
        f64_type,
        string_type,

        structure: Structure,
        enumeration: Enumeration,
        pointer_type: PointerType,
        slice_type: SliceType,
        optional_type: OptionalType,
        function_type: FunctionType,

        runtime_value,
        nil,
        unit,
        i8: i8,
        i16: i16,
        i32: i32,
        i64: i64,
        u8: u8,
        u16: u16,
        u32: u32,
        u64: u64,
        boolean: bool,
        f32: f32,
        f64: f64,
        string: []const u8,

        untyped_integer: i128,
        untyped_float: f128,

        structure_literal: StructureLiteral,
        enumeration_literal: EnumerationLiteral,
        array_literal: ArrayLiteral,

        pointer: Pointer,
        array: Array,
        slice: Slice,
        optional: Optional,
        function: Function,
    };

    pub const StructureLiteral = struct {
        fields: std.StringArrayHashMapUnmanaged(ValueIndex),
    };

    pub const EnumerationLiteral = struct {
        backing: ?ValueIndex = null,
        value: ValueIndex,
    };

    pub const ArrayLiteral = struct {
        values: std.ArrayListUnmanaged(ValueIndex),
    };

    pub const Structure = struct {
        fields: std.StringArrayHashMapUnmanaged(ValueIndex),
    };

    pub const Enumeration = struct {
        values: std.StringArrayHashMapUnmanaged(ValueIndex),
    };

    pub const Pointer = struct {
        address: ValueIndex,
    };

    pub const PointerType = struct {
        target: ValueIndex,
        mutable: bool,
    };

    pub const Array = struct {
        element: ValueIndex,
        length: ValueIndex,
    };

    pub const Slice = struct {
        address: ValueIndex,
    };

    pub const SliceType = struct {
        element: ValueIndex,
    };

    pub const Optional = struct {
        value: ValueIndex,
    };

    pub const OptionalType = struct {
        element: ValueIndex,
    };

    pub const Function = struct {
        typ: ValueIndex,
        block: ValueIndex,
    };

    pub const FunctionType = struct {
        generics: std.ArrayListUnmanaged([]const u8),
        parameters: std.ArrayListUnmanaged(ValueIndex),
        return_type: ValueIndex,
    };

    pub const Kind = enum(usize) {
        pub const last_set_type = Kind.string_type;

        unit_type = 0,

        i8_type,
        i16_type,
        i32_type,
        i64_type,
        u8_type,
        u16_type,
        u32_type,
        u64_type,
        boolean_type,
        f32_type,
        f64_type,
        string_type,

        structure,
        enumeration,
        pointer_type,
        slice_type,
        optional_type,
        function_type,

        runtime_value,
        nil,
        unit,
        i8,
        i16,
        i32,
        i64,
        u8,
        u16,
        u32,
        u64,
        boolean,
        f32,
        f64,
        string,

        untyped_integer,
        untyped_float,

        structure_literal,
        enumeration_literal,
        array_literal,

        pointer,
        array,
        slice,
        optional,
        function,

        pub fn isPrimitive(self: Kind) bool {
            return switch (self) {
                .unit, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .boolean, .f32, .f64, .string => true,
                else => false,
            };
        }

        pub fn isType(self: Kind) bool {
            return switch (self) {
                .structure, .enumeration, .pointer_type, .array, .slice_type, .optional_type, .function_type => true,
                else => false,
            };
        }
    };
};
