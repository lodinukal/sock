const std = @import("std");
const ast = @import("ast.zig");
const Lexer = @import("Lexer.zig");

pub fn TaggedInstructionIndex(comptime T: type) type {
    _ = T;
    return InstructionIndex;
}
pub const InstructionIndex = enum(u32) { none = std.math.maxInt(u32), _ };
pub const StringIndex = enum(u32) { none = std.math.maxInt(u32), _ };
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

    pub const Type = union(enum) {
        unit,
        type: TaggedInstructionIndex(Type),
        integer: enum {
            i8,
            i16,
            i32,
            i64,
            u8,
            u16,
            u32,
            u64,

            pub fn signed(self: @This()) bool {
                switch (self) {
                    .i8, .i16, .i32, .i64 => true,
                    else => false,
                }
            }
        },
        float: enum {
            f32,
            f64,
        },
        boolean,
        string,
        enumeration: struct {
            count: u32,
            field_start: TaggedInstructionIndex(EnumerationField) = .none,
        },
        structure: struct {
            count: u32,
            field_start: TaggedInstructionIndex(StructureField) = .none,
        },
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
            args_count: u32 = 0,
            args_start: TaggedInstructionIndex(Type) = .none,
            return_type: TaggedInstructionIndex(Type),
        },

        pub const EnumerationField = struct {
            name: StringIndex,
            backing: TaggedInstructionIndex(Type) = .none,
            value: TaggedInstructionIndex(Value),
        };
        pub const StructureField = struct {
            name: StringIndex,
            typ: TaggedInstructionIndex(Type),
            default: TaggedInstructionIndex(Value) = .none, // value
        };
    };

    /// something constant, maybe should express this in data but oh well
    pub const Value = struct {
        typ: TaggedInstructionIndex(Type),
        data: StringIndex,
    };

    pub const Declaration = struct {
        // mut/let
        mutable: bool,
        // NAME
        identifier: StringIndex,
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
        instruction_count: u32 = 0,
        // { BODY }
        body_start: InstructionIndex = .none,
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
        args_count: u32,
        args_start: InstructionIndex = .none,
    };

    pub const FieldAccess = struct {
        operand: InstructionIndex,
        field: StringIndex,
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
        capture_count: u32 = 0,
        capture: InstructionIndex = .none,
        body_block: TaggedInstructionIndex(Block),
    };

    pub const MatchStatement = struct {
        value: InstructionIndex,
        case_count: u32 = 0,
        case_start: TaggedInstructionIndex(Case) = .none,
    };

    pub const Case = struct {
        pattern: InstructionIndex,
        block: TaggedInstructionIndex(Block),
    };

    pub const ReturnStatement = struct {
        value: InstructionIndex = .none,
    };

    pub const BreakStatement = struct {
        label: StringIndex = .none,
    };
};

pub const Module = struct { string_data: std.StringArrayHashMapUnmanaged(void) };
