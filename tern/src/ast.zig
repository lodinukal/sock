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
};

pub const Container = struct {
    // Allocator used to allocate the root_nodes
    allocator: std.mem.Allocator,
    // Allocator used to allocate the nodes
    node_allocator: std.mem.Allocator,
    node_arena: std.heap.ArenaAllocator,
    root_nodes: std.ArrayListUnmanaged(*Node),

    pub fn init(
        allocator: std.mem.Allocator,
        node_allocator: std.mem.Allocator,
    ) Container {
        return Container{
            .allocator = allocator,
            .node_allocator = node_allocator,
            .node_arena = std.heap.ArenaAllocator.init(node_allocator),
            .root_nodes = std.ArrayListUnmanaged(*Node){},
        };
    }

    pub fn deinit(self: *Container) void {
        self.node_arena.deinit();
        self.root_nodes.deinit(self.allocator);
    }

    pub fn pushRootNode(self: *Container, node: *Node) !*Node {
        return try self.root_nodes.append(node);
    }

    pub fn allocNode(self: *Container, node: Node) !*Node {
        const ptr = try self.node_arena.allocator().create(Node);
        ptr.* = node;
        return ptr;
    }

    pub fn allocExpression(self: *Container, node: Node) !*Node.Expression {
        const got = try self.allocNode(node);
        return got.getExpressionPtr();
    }

    pub fn allocStatement(self: *Container, node: Node) !*Node.Statement {
        const got = try self.allocNode(node);
        return got.getStatementPtr();
    }

    pub fn allocRootNode(self: *Container, node: Node) !*Node {
        const ptr = try self.allocNode(node);
        return try self.pushRootNode(ptr);
    }

    pub fn allocType(self: *Container, typ: Node.Type) !*Node.Type {
        const ptr = try self.node_arena.allocator().create(Node.Type);
        ptr.* = typ;
        return ptr;
    }
};

pub const Node = struct {
    location: Location,
    variant: Variant,

    pub const Variant = union(Kind) {
        expression: Expression,
        statement: Statement,
    };

    pub const Kind = enum {
        expression,
        statement,
    };

    pub const PrimitiveType = enum {
        i8,
        i16,
        i32,
        i64,
        u8,
        u16,
        u32,
        u64,
        f32,
        f64,
        bool,
    };

    pub const Field = struct {
        attributes: []const *Expression,
        identifier: []const u8,
        type: ?*Type = null,
        initialiser: ?*Expression = null,
    };

    pub const Type = union(TypeKind) {
        @"struct": struct {
            fields: []Field,
        },
        @"enum": struct {
            backing_type: ?*Type = null,
            fields: []Field,
        },
        expression: *Expression,
        unit: void,
        primitive: PrimitiveType,
        pointer: *Type,
        array: struct {
            element: *Type,
            length: ?*Expression = null,
        },
        span: *Type,
        function: struct {
            parameters: []Field,
            return_type: []Field,
        },
    };
    pub const TypeKind = enum {
        @"struct",
        @"enum",
        expression,
        unit,
        primitive,
        pointer,
        array,
        span,
        function,
    };

    pub const Expression = union(ExpressionKind) {
        boolean_literal: bool,
        integer_literal: i128,
        float_literal: f64,
        string_literal: []const u8,
        char_literal: u32,
        structure_literal: []Field,
        type: *Type,
        identifier: []const u8,
        binary: struct {
            op: Lexer.Token.Kind,
            left: *Expression,
            right: *Expression,
        },
        unary: struct {
            op: Lexer.Token.Kind,
            operand: *Expression,
        },
        call: struct {
            callee: *Expression,
            arguments: []*Expression,
        },
        subscript: struct {
            array: *Expression,
            index: *Expression,
        },
        field: struct {
            record: *Expression,
            field: []const u8,
        },
        function: struct {
            typ: *Type,
            body: *Expression,
        },
        lambda: struct {
            capture: []Field,
            body: *Expression,
        },
        block: []const *Statement,

        pub fn node(self: *Expression) *Node {
            return @fieldParentPtr("variant", @as(*Variant, @alignCast(@fieldParentPtr("expression", self))));
        }
    };
    pub const ExpressionKind = enum {
        boolean_literal,
        integer_literal,
        float_literal,
        string_literal,
        char_literal,
        structure_literal,
        type,
        identifier,
        binary,
        unary,
        call,
        subscript,
        field,
        function,
        lambda,
        block,
    };

    pub const Declaration = struct {
        identifier: []const u8,
        type: ?*Type = null,
        mutable: bool = false,
        public: bool = false,
        exported: bool = false,
    };
    pub const Statement = struct {
        attributes: []const *Expression,
        variant: union(StatementKind) {
            expression: *Expression,
            @"if": struct {
                condition: *Expression,
                capture: ?[]Field = null,
                then_branch: *Expression,
                else_branch: ?*Expression = null,
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
            },
            @"continue": struct {
                label: ?[]const u8,
            },
            declaration: struct {
                declarations: []Declaration,
                initialiser: *Expression,
            },
            assignment: struct {
                target: *Expression,
                value: *Expression,
                kind: Lexer.Token.Kind,
            },
        },

        pub fn node(self: *Statement) *Node {
            return @fieldParentPtr("variant", @as(*Variant, @alignCast(@fieldParentPtr("statement", self))));
        }
    };
    pub const StatementKind = enum {
        expression,
        @"if",
        @"while",
        @"for",
        @"return",
        @"break",
        @"continue",
        declaration,
        assignment,
    };

    pub fn getStatementPtr(self: *Node) *Statement {
        return &self.variant.statement;
    }

    pub fn getExpressionPtr(self: *Node) *Expression {
        return &self.variant.expression;
    }
};
