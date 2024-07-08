const std = @import("std");
const Lexer = @import("Lexer.zig");

pub const Position = struct {
    line: u32 = 0,
    column: u32 = 0,
    pub fn eql(self: Position, other: Position) bool {
        return self.line == other.line and self.column == other.column;
    }

    pub fn format(self: Position, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{}:{}", .{ self.line + 1, self.column + 1 });
    }
};

pub const Location = struct {
    file: []const u8 = &.{},
    begin: Position = .{},
    end: Position = .{},

    pub fn eql(self: Location, other: Location) bool {
        return self.begin.eql(other.begin) and self.end.eql(other.end);
    }

    pub fn initLength(begin: Position, length: u32) Location {
        return Location{
            .begin = begin,
            .end = .{ .line = begin.line, .column = begin.column + length },
        };
    }

    pub fn format(self: Location, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s}:{}", .{ self.file, self.begin });
    }

    pub fn merge(self: Location, other: Location) Location {
        return Location{
            .file = self.file,
            .begin = self.begin,
            .end = other.end,
        };
    }
};

pub const Container = struct {
    // Allocator used to allocate the root_stmts
    allocator: std.mem.Allocator,
    // Allocator used to allocate the nodes
    node_allocator: std.mem.Allocator,
    node_arena: std.heap.ArenaAllocator,
    root_stmts: std.ArrayListUnmanaged(*Statement),

    statements: u32 = 0,
    expressions: u32 = 0,

    pub fn init(
        self: *Container,
        allocator: std.mem.Allocator,
        node_allocator: std.mem.Allocator,
    ) void {
        self.* = .{
            .allocator = allocator,
            .node_allocator = undefined,
            .node_arena = std.heap.ArenaAllocator.init(node_allocator),
            .root_stmts = std.ArrayListUnmanaged(*Statement){},
        };
        self.node_allocator = self.node_arena.allocator();
    }

    pub fn deinit(self: *Container) void {
        self.node_arena.deinit();
        self.root_stmts.deinit(self.allocator);
    }

    pub fn pushRootStatement(self: *Container, node: *Statement) !void {
        try self.root_stmts.append(self.allocator, node);
    }

    pub fn allocExpression(self: *Container, expression: Expression) !*Expression {
        self.expressions += 1;
        const got = try self.node_allocator.create(Expression);
        got.* = expression;
        return got;
    }

    pub fn allocStatement(self: *Container, stmt: Statement) !*Statement {
        self.statements += 1;
        const got = try self.node_allocator.create(Statement);
        got.* = stmt;
        return got;
    }

    pub fn allocRootStatement(self: *Container, stmt: Statement) !*Statement {
        const got = try self.node_allocator.create(Statement);
        got.* = stmt;
        return try self.pushRootNode(got);
    }

    pub fn allocType(self: *Container, typ: Type) !*Type {
        const ptr = try self.node_allocator.create(Type);
        ptr.* = typ;
        return ptr;
    }
};

pub const PrimitiveType = enum(u8) {
    i8 = 0,
    i16 = 1,
    i32 = 2,
    i64 = 3,
    u8 = 4,
    u16 = 5,
    u32 = 6,
    u64 = 7,
    f32 = 8,
    f64 = 9,
    bool = 10,

    typ = 11, // type
    unit = 12, // void

    pub const Kind = enum {
        signed_integer,
        unsigned_integer,
        float,
        bool,
        special,
    };

    pub fn name(self: PrimitiveType) []const u8 {
        switch (self) {
            .i8 => return "i8",
            .i16 => return "i16",
            .i32 => return "i32",
            .i64 => return "i64",
            .u8 => return "u8",
            .u16 => return "u16",
            .u32 => return "u32",
            .u64 => return "u64",
            .f32 => return "f32",
            .f64 => return "f64",
            .bool => return "bool",
            .typ => return "type",
            .unit => return "unit",
        }
    }

    pub inline fn getKind(self: PrimitiveType) Kind {
        switch (self) {
            .i8, .i16, .i32, .i64 => return .signed_integer,
            .u8, .u16, .u32, .u64 => return .unsigned_integer,
            .f32, .f64 => return .float,
            .bool => return .bool,
            .typ, .unit => return .special,
        }
    }

    pub inline fn getSize(self: PrimitiveType) u8 {
        switch (self) {
            .i8, .u8 => return 8,
            .i16, .u16 => return 16,
            .i32, .u32 => return 32,
            .i64, .u64 => return 64,
            .f32 => return 32,
            .f64 => return 64,
            .bool => return 1,
            .typ => return 0,
            .unit => return 0,
        }
    }

    pub inline fn getCoercionRanking(self: PrimitiveType) u8 {
        switch (self) {
            .i8 => return 1,
            .u8 => return 0,
            .i16 => return 3,
            .u16 => return 2,
            .i32 => return 5,
            .u32 => return 4,
            .i64 => return 7,
            .u64 => return 6,
            .f32 => return 9,
            .f64 => return 8,
            .bool => return 0,
            .typ => return 0,
            .unit => return 0,
        }
    }

    pub fn asSigned(self: PrimitiveType) PrimitiveType {
        switch (self) {
            .u8 => return .i8,
            .u16 => return .i16,
            .u32 => return .i32,
            .u64 => return .i64,
            else => return self,
        }
    }

    pub fn asUnsigned(self: PrimitiveType) PrimitiveType {
        switch (self) {
            .i8 => return .u8,
            .i16 => return .u16,
            .i32 => return .u32,
            .i64 => return .u64,
            else => return self,
        }
    }

    pub fn isSigned(self: PrimitiveType) bool {
        switch (self) {
            .i8, .i16, .i32, .i64 => return true,
            else => return false,
        }
    }

    pub fn isInteger(self: PrimitiveType) bool {
        switch (self) {
            .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64 => return true,
            else => return false,
        }
    }

    pub fn isFloat(self: PrimitiveType) bool {
        switch (self) {
            .f32, .f64 => return true,
            else => return false,
        }
    }

    pub fn isBool(self: PrimitiveType) bool {
        return self == .bool;
    }

    pub fn isSpecial(self: PrimitiveType) bool {
        switch (self) {
            .typ, .unit => return true,
            else => return false,
        }
    }
};

pub const Field = struct {
    location: Location,
    attributes: []const *Expression,
    key: *Expression,
    typ: ?*const Type = null,
    initialiser: ?*Expression = null,
};

pub const Type = union(TypeKind) {
    structure: struct {
        location: Location,
        fields: []Field,
    },
    enumeration: struct {
        location: Location,
        fields: []Field,
    },
    expression: *Expression,
    primitive: PrimitiveType,
    optional: *const Type,
    pointer: struct {
        element: *const Type,
        mutable: bool,
    },
    array: struct {
        element: *const Type,
        length: ?*Expression = null,
    },
    slice: struct {
        element: *const Type,
        mutable: bool,
    },
    function: FunctionType,
};
pub const TypeKind = enum {
    structure,
    enumeration,
    expression,
    primitive,
    optional,
    pointer,
    array,
    slice,
    function,
};

pub const FunctionType = struct {
    generics: []Field,
    parameters: []Field,
    return_type: ?*const Type = null,
};

pub const MatchCase = struct {
    location: Location,
    /// null means it is the else case
    pattern: ?*Expression = null,
    body: *Expression,
};

pub const Expression = struct {
    location: Location,
    typ: ?Type = null,
    variant: union(ExpressionKind) {
        boolean_literal: bool,
        integer_literal: struct {
            value: u64,
            signed: bool,
        },
        float_literal: f64,
        string_literal: []const u8,
        enum_literal: []const u8,
        char_literal: u32,
        structure_literal: struct {
            // TODO: cleanup
            explicit_type: ?union(enum) {
                expression: *Expression,
                typ: Type,
            } = null,
            fields: []Field,
        },
        typ: *const Type,
        identifier: []const u8,
        binary: struct {
            op: Lexer.Token.Kind,
            left: *Expression,
            right: *Expression,
            // analyse in the type checker
            result_type: ?Type = null,
        },
        unary: struct {
            op: Lexer.Token.Kind,
            operand: *Expression,
            // analyse in the type checker
            result_type: ?Type = null,
        },
        call: struct {
            callee: *Expression,
            arguments: []*Expression,
            // analyse in the type checker
            result_type: ?Type = null,
        },
        subscript: struct {
            array: *Expression,
            index: *Expression,
            end_location: Location,
            // analyse in the type checker
            result_type: ?Type = null,
        },
        deref: struct {
            operand: *Expression,
            end_location: Location,
            // analyse in the type checker
            result_type: ?Type = null,
        },
        field: struct {
            record: *Expression,
            end_location: Location,
            field: []const u8,
            // analyse in the type checker
            result_type: ?Type = null,
        },
        function: struct {
            typ: *const Type,
            /// external functions have no body
            body: ?*Expression = null,
        },
        lambda: struct {
            capture: []Field,
            body: *Expression,
            // analyse in the type checker
            inferred_type: ?Type = null,
        },
        block: struct {
            label: ?[]const u8 = null,
            statements: []const *Statement,
            // analyse in the type checker
            result_type: ?Type = null,
        },
        pipeline: struct {
            stages: []const *Expression,
            // analyse in the type checker
            result_type: ?Type = null,
        },
        undefined: void,
        nil: void,
    },
};
pub const ExpressionKind = enum {
    boolean_literal,
    integer_literal,
    float_literal,
    string_literal,
    enum_literal,
    char_literal,
    structure_literal,
    typ,
    identifier,
    binary,
    unary,
    call,
    subscript,
    deref,
    field,
    function,
    lambda,
    block,
    pipeline,
    undefined,
    nil,
};

pub const Declaration = struct {
    public: bool = false,
    exported: bool = false,
    mutable: bool = false,
    identifier: []const u8,
    typ: ?*const Type = null,
    initialiser: *Expression,
};

pub const Statement = struct {
    location: Location,
    attributes: []const *Expression,
    variant: union(StatementKind) {
        expression: *Expression,
        @"if": struct {
            condition: *Expression,
            capture: ?[]Field = null,
            then_branch: *Expression,
            else_branch: ?*Expression = null,
            result_type: ?Type = null,
        },
        @"while": struct {
            condition: *Expression,
            body: *Expression,
        },
        @"for": struct {
            condition: ?*Expression = null,
            capture: ?[]Field = null,
            body: *Expression,
        },
        @"return": ?*Expression,
        @"break": struct {
            label: ?[]const u8,
            resolved_block: ?*Expression = null,
        },
        @"continue": struct {
            label: ?[]const u8,
            resolved_block: ?*Expression = null,
        },
        match: struct {
            expression: *Expression,
            cases: []const MatchCase,
            result_type: ?Type = null,
        },
        declaration: Declaration,
        assignment: struct {
            target: *Expression,
            value: *Expression,
            kind: Lexer.Token.Kind,
        },
    },
};
pub const StatementKind = enum {
    expression,
    @"if",
    @"while",
    @"for",
    @"return",
    @"break",
    @"continue",
    match,
    declaration,
    assignment,
};
