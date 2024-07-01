const std = @import("std");
const Lexer = @import("Lexer.zig");
const ast = @import("ast.zig");

pub const ErrorSet = error{
    Unexpected,
    UnexpectedToken,
    ExpectedMutOrLet,
    ExpectedCommaOrPipe,
    ExpectedCommaOrCloseParen,
    ExpectedIdentifier,
    ExpectedIdentifierSameName,
    UnexpectedStatementStart,
    ExpectedStartOfBlock,
    ExpectedTokenOfKind,
    OutOfMemory,
    InvalidIntegerLiteral,
    FinishedParsing,
    InvalidTypeToParse,
};

pub const Error = struct {
    location: ast.Location,
    message: []const u8,
    fatal: bool = false,

    pub fn format(self: Error, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{}: {s}", .{ self.location, self.message });
    }
};

const Parser = @This();
allocator: std.mem.Allocator,
arena: std.heap.ArenaAllocator = undefined,
arena_allocator: std.mem.Allocator = undefined,
buffer: []const u8,
file_path: []const u8,
lexer: Lexer = undefined,
current_token: Lexer.Token = .{},
next_token: Lexer.Token = .{},
errors: std.ArrayListUnmanaged(Error) = undefined,
warning_count: u32 = 0,
error_count: u32 = 0,
container: *ast.Container, // not owned

in_pipeline: bool = false,

pub fn init(
    self: *Parser,
) ErrorSet!void {
    self.arena = std.heap.ArenaAllocator.init(self.allocator);
    self.arena_allocator = self.arena.allocator();
    self.lexer = Lexer.init(self.buffer, self.file_path);
    self.errors = std.ArrayListUnmanaged(Error){};
    try self.advance();
    try self.advance();
}

pub fn deinit(self: *Parser) void {
    self.arena.deinit();
}

// pub fn next(self: *Parser) ErrorSet!Lexer.Token {
//     // const decl = try self.parseDeclaration(.{});
//     // return decl;
//     return self.lexer.next();
// }

pub fn pushError(self: *Parser, location: ast.Location, comptime fmt: []const u8, args: anytype) ErrorSet!void {
    try self.errors.append(self.arena_allocator, .{
        .location = location,
        .message = try std.fmt.allocPrint(self.arena_allocator, fmt, args),
        .fatal = true,
    });
    self.error_count += 1;
}

pub fn pushErrorHere(self: *Parser, comptime fmt: []const u8, args: anytype) ErrorSet!void {
    try self.pushError(self.lexer.previous_location, fmt, args);
}

pub fn pushWarning(self: *Parser, location: ast.Location, comptime fmt: []const u8, args: anytype) ErrorSet!void {
    try self.errors.append(self.arena_allocator, .{
        .location = location,
        .message = try std.fmt.allocPrint(self.arena_allocator, fmt, args),
    });
    self.warning_count += 1;
}

pub fn pushWarningHere(self: *Parser, comptime fmt: []const u8, args: anytype) ErrorSet!void {
    try self.pushWarning(self.lexer.previous_location, fmt, args);
}

pub fn advance(self: *Parser) ErrorSet!void {
    self.current_token = self.next_token;
    self.next_token = try self.lexer.next();
}

pub fn peek(self: Parser, n: usize) ErrorSet!Lexer.Token {
    if (n == 0) {
        return self.current_token;
    }
    if (n == 1) {
        return self.next_token;
    }
    var copy_lexer = self.lexer;
    var token = self.current_token;
    for (0..n) |_| {
        token = try copy_lexer.next();
    }
    return token;
}

pub fn currentTokenIsKind(self: Parser, kind: Lexer.Token.Kind) bool {
    return self.current_token.kind == kind;
}

pub fn nextTokenIsKind(self: Parser, kind: Lexer.Token.Kind) bool {
    return self.next_token.kind == kind;
}

pub fn expectCurrentTokenIsKind(self: *Parser, kind: Lexer.Token.Kind) ErrorSet!void {
    if (!self.currentTokenIsKind(kind)) {
        try self.pushErrorHere("expected token of kind '{}', got '{}'", .{
            kind,
            self.current_token.kind,
        });
        return error.ExpectedTokenOfKind;
    }
}

pub fn expectNextTokenIsKind(self: *Parser, kind: Lexer.Token.Kind) ErrorSet!void {
    if (!self.nextTokenIsKind(kind)) {
        try self.pushErrorHere("expected token of kind '{}', got '{}'", .{
            kind,
            self.next_token.kind,
        });
        return error.ExpectedTokenOfKind;
    }
}

pub fn consumeIdentifier(self: *Parser, str: []const u8) ErrorSet!void {
    if (!self.currentTokenIsKind(.identifier)) {
        try self.pushErrorHere("expected identifier, got '{}'", .{self.current_token.kind});
        return error.ExpectedIdentifier;
    }
    if (std.mem.eql(u8, str, self.current_token.data)) {
        try self.pushErrorHere("expected identifier '{s}', got '{s}'", .{ str, self.current_token.data });
        return error.ExpectedIdentifierSameName;
    }
    try self.advance();
}

pub fn consumeKind(self: *Parser, kind: Lexer.Token.Kind) ErrorSet!void {
    if (!self.currentTokenIsKind(kind)) {
        try self.pushErrorHere("expected token of kind '{}', got '{}'", .{
            kind,
            self.current_token.kind,
        });
        return error.ExpectedTokenOfKind;
    }
    try self.advance();
}

pub fn parseTopLevel(self: *Parser) ErrorSet!*ast.Statement {
    while (self.currentTokenIsKind(.comment)) {
        try self.consumeKind(.comment);
    }
    if (self.current_token.kind == .eof) {
        return error.FinishedParsing;
    }
    const statement = try self.parseStatement();
    try self.container.pushRootStatement(statement);
    return statement;
}

pub fn parseType(self: *Parser) ErrorSet!*ast.Type {
    switch (self.current_token.kind) {
        .@"struct" => return try self.parseStructType(),
        .question => {
            try self.consumeKind(.question);
            const inner = try self.parseType();
            return try self.container.allocType(.{ .optional = inner });
        },
        .star => {
            try self.consumeKind(.star);
            const mutable = if (self.currentTokenIsKind(.mut)) blk: {
                try self.consumeKind(.mut);
                break :blk true;
            } else false;
            const inner = try self.parseType();
            return try self.container.allocType(.{ .pointer = .{
                .element = inner,
                .mutable = mutable,
            } });
        },
        .open_bracket => {
            try self.consumeKind(.open_bracket);
            if (self.currentTokenIsKind(.close_bracket)) {
                try self.consumeKind(.close_bracket);
                const mutable = if (self.currentTokenIsKind(.mut)) blk: {
                    try self.consumeKind(.mut);
                    break :blk true;
                } else false;
                const inner = try self.parseType();
                return try self.container.allocType(.{ .slice = .{
                    .element = inner,
                    .mutable = mutable,
                } });
            }
            const length = try self.parseExpression();
            try self.consumeKind(.close_bracket);
            const inner = try self.parseType();
            return try self.container.allocType(.{ .array = .{
                .element = inner,
                .length = length,
            } });
        },
        .identifier => {
            const identifier = self.current_token.data;
            try self.expectCurrentTokenIsKind(.identifier);

            if (std.meta.stringToEnum(ast.PrimitiveType, identifier)) |primitive_type| {
                try self.consumeKind(.identifier);
                return try self.container.allocType(.{ .primitive = primitive_type });
            }

            return try self.container.allocType(.{ .expression = try self.parseExpression() });
        },
        .@"fn" => {
            try self.consumeKind(.@"fn");
            return try self.parseFunctionType();
        },
        else => {
            try self.pushErrorHere("unexpected token '{}'", .{self.current_token.kind});
            return error.UnexpectedToken;
        },
    }
}

pub fn parseFunctionType(self: *Parser) ErrorSet!*ast.Type {
    try self.consumeKind(.open_paren);
    const parameters = try self.parseFieldList(.{
        .type_requirement = .required,
        .value_requirement = .allow,
        .allow_attributes = true,
    }, .close_paren);
    try self.consumeKind(.close_paren);
    // TODO: parse modifiers
    const return_type: ?*ast.Type = if (self.currentTokenIsKind(.arrow)) blk: {
        try self.consumeKind(.arrow);
        break :blk try self.parseType();
    } else null;

    return try self.container.allocType(.{ .function = .{
        .parameters = parameters,
        .return_type = return_type,
    } });
}

pub fn tryParseAttributes(self: *Parser) ErrorSet![]const *ast.Expression {
    var attributes = std.ArrayListUnmanaged(*ast.Expression){};
    while (self.currentTokenIsKind(.at)) {
        try self.consumeKind(.at);
        try attributes.append(self.container.node_allocator, try self.parseExpression());
    }
    return attributes.items;
}

pub const FieldParseInfo = struct {
    pub const Requirement = enum { allow, disallow, required };
    type_requirement: Requirement = .disallow,
    value_requirement: Requirement = .disallow,
    allow_attributes: bool = false,
    key_type: enum { identifier, expression } = .identifier,
};
pub fn parseField(self: *Parser, info: FieldParseInfo) ErrorSet!ast.Field {
    // @ATTRIBUTES
    const attributes = try self.tryParseAttributes();
    // KEY
    const key = if (info.key_type == .expression) try self.parseExpression() else blk: {
        if (!self.currentTokenIsKind(.identifier)) {
            try self.pushErrorHere("expected identifier, got '{}'", .{self.current_token.kind});
            return error.ExpectedIdentifier;
        }
        const data = self.current_token.data;
        try self.consumeKind(.identifier);
        break :blk try self.container.allocExpression(.{
            .location = self.current_token.location,
            .variant = .{ .identifier = data },
        });
    };
    // :
    const got_type: ?*ast.Type =
        if (self.currentTokenIsKind(.colon))
    blk: {
        if (info.type_requirement == .disallow) {
            try self.pushErrorHere("unexpected token ':', you can't specify a type here", .{});
            return error.UnexpectedToken;
        }
        // TYPE
        try self.consumeKind(.colon);
        break :blk try self.parseType();
    } else blk: {
        if (info.type_requirement == .required) {
            try self.pushErrorHere("expected type here (by token ':'), got '{}'", .{self.current_token.kind});
            return error.ExpectedTokenOfKind;
        }
        break :blk null;
    };
    // =
    const got_default: ?*ast.Expression =
        if (self.currentTokenIsKind(.equal))
    blk: {
        if (info.value_requirement == .disallow) {
            try self.pushErrorHere("unexpected token '=', you can't specify a default value here", .{});
            return error.UnexpectedToken;
        }
        // DEFAULT
        try self.consumeKind(.equal);
        break :blk try self.parseExpression();
    } else blk: {
        if (info.value_requirement == .required) {
            try self.pushErrorHere("expected value here (by token '='), got '{}'", .{self.current_token.kind});
            return error.ExpectedTokenOfKind;
        }
        break :blk null;
    };
    return .{
        .attributes = attributes,
        .key = key,
        .type = got_type,
        .initialiser = got_default,
    };
}

pub fn parseFieldList(self: *Parser, info: FieldParseInfo, closer: Lexer.Token.Kind) ErrorSet![]ast.Field {
    var fields = std.ArrayListUnmanaged(ast.Field){};
    while (!self.currentTokenIsKind(closer)) {
        const field = try self.parseField(info);
        try fields.append(self.container.node_allocator, field);
        if (!self.currentTokenIsKind(closer)) {
            try self.consumeKind(.comma);
        }
    }
    return fields.items;
}

pub fn parseStructType(self: *Parser) ErrorSet!*ast.Type {
    // struct
    try self.consumeKind(.@"struct");
    // {
    try self.consumeKind(.open_brace);
    // FIELDS
    const fields = try self.parseFieldList(.{
        .type_requirement = .allow,
        .value_requirement = .allow,
        .allow_attributes = true,
    }, .close_brace);
    // }
    try self.consumeKind(.close_brace);
    return try self.container.allocType(.{ .@"struct" = .{
        .fields = fields,
    } });
}

pub fn parseStatement(self: *Parser) ErrorSet!*ast.Statement {
    // ATTRIBUTES
    const attributes = try self.tryParseAttributes();
    switch (self.current_token.kind) {
        .@"if" => return try self.parseIfStatement(attributes),
        .@"while" => return try self.parseWhileStatement(attributes),
        .@"for" => return try self.parseForStatement(attributes),
        .@"return" => return try self.parseReturnStatement(attributes),
        .@"break" => return try self.parseBreakStatement(attributes),
        .@"continue" => return try self.parseContinueStatement(attributes),
        .match => return try self.parseMatchStatement(attributes),
        .mut, .let, .@"pub", .@"export", .@"fn" => return try self.parseDeclaration(attributes),
        else => {
            const expression = try self.parseExpression();
            // peek ahead to check for assignment
            if (self.current_token.kind.isAssignmentOperation()) {
                const using_token = self.current_token.kind;
                try self.consumeKind(self.current_token.kind);
                return try self.parseAssignmentStatement(
                    attributes,
                    expression,
                    using_token,
                );
            }
            return try self.container.allocStatement(.{
                .location = self.current_token.location,
                .attributes = attributes,
                .variant = .{ .expression = expression },
            });
        },
    }
}

// pub fn parseFunctionStatement(self: *Parser, attributes: []const *ast.Expression) ErrorSet!*ast.Statement {
//     const start = self.current_token.location;

//     // pub
//     const is_public = if (self.currentTokenIsKind(.@"pub")) blk: {
//         try self.consumeKind(.@"pub");
//         break :blk true;
//     } else false;
//     // exported
//     const is_exported = if (self.currentTokenIsKind(.@"export")) blk: {
//         try self.consumeKind(.@"export");
//         break :blk true;
//     } else false;

//     try self.consumeKind(.@"fn");
//     try self.expectCurrentTokenIsKind(.identifier);
//     const identifier = self.current_token.data;
//     try self.consumeKind(.identifier);

//     const function_type = try self.parseFunctionType();
//     const body = try self.parseBlockExpression(.{});
//     const declaration = try self.container.node_allocator.alloc(ast.Declaration, 1);
//     declaration[0] = .{
//         .identifier = identifier,
//         .public = is_public,
//         .exported = is_exported,
//     };
//     return try self.container.allocStatement(.{
//         .location = start,
//         .attributes = attributes,
//         .variant = .{ .declaration = .{
//             .declarations = declaration,
//             .initialiser = try self.container.allocExpression(.{
//                 .location = start,
//                 .variant = .{ .function = .{
//                     .typ = function_type,
//                     .body = body,
//                 } },
//             }),
//         } },
//     });
// }

pub fn parseCapture(self: *Parser) ErrorSet![]ast.Field {
    // |
    try self.consumeKind(.pipe);
    var captures = std.ArrayListUnmanaged(ast.Field){};
    while (!self.currentTokenIsKind(.pipe)) {
        const capture = try self.parseField(.{
            .type_requirement = .allow,
        });
        try captures.append(self.container.node_allocator, capture);
        if (!self.currentTokenIsKind(.pipe) and !self.currentTokenIsKind(.comma)) {
            try self.pushErrorHere("expected ',' or '|', got '{}'", .{self.current_token.kind});
            return error.ExpectedCommaOrPipe;
        }
        if (!self.currentTokenIsKind(.pipe)) {
            try self.consumeKind(.comma);
        }
    }
    // |
    try self.consumeKind(.pipe);
    return captures.items;
}

pub fn parseIfStatement(self: *Parser, attributes: []const *ast.Expression) ErrorSet!*ast.Statement {
    const start = self.current_token.location;
    // if
    try self.consumeKind(.@"if");
    // (
    try self.consumeKind(.open_paren);
    // CONDITION
    const condition = try self.parseExpression();
    // )
    try self.consumeKind(.close_paren);
    // |CAPTURE|
    const capture: ?[]ast.Field = if (self.nextTokenIsKind(.pipe)) try self.parseCapture() else null;
    // {BLOCK} / STATEMENT
    const then_branch = try self.parseBlockExpression(.{});
    // ELSE
    const else_branch: ?*ast.Expression =
        if (self.currentTokenIsKind(.@"else"))
    blk: {
        // else
        try self.consumeKind(.@"else");
        // ELSE
        break :blk try self.parseBlockExpression(.{});
    } else null;
    return try self.container.allocStatement(.{
        .location = start,
        .attributes = attributes,
        .variant = .{
            .@"if" = .{
                .condition = condition,
                .capture = capture,
                .then_branch = then_branch,
                .else_branch = else_branch,
            },
        },
    });
}

pub fn parseWhileStatement(self: *Parser, attributes: []const *ast.Expression) ErrorSet!*ast.Statement {
    // while
    try self.consumeKind(.@"while");
    // (
    try self.consumeKind(.open_paren);
    // CONDITION
    const condition = try self.parseExpression();
    // )
    try self.consumeKind(.close_paren);
    // {BLOCK}
    const body = try self.parseBlockExpression(.{});
    return try self.container.allocStatement(.{
        .location = self.current_token.location,
        .attributes = attributes,
        .variant = .{ .@"while" = .{
            .condition = condition,
            .body = body,
        } },
    });
}

pub fn parseForStatement(self: *Parser, attributes: []const *ast.Expression) ErrorSet!*ast.Statement {
    // for
    try self.consumeKind(.@"for");
    // (
    try self.consumeKind(.open_paren);
    // CONDITION
    const condition = try self.parseExpression();
    // )
    try self.consumeKind(.close_paren);
    // |CAPTURE|
    const capture: ?[]ast.Field = if (self.nextTokenIsKind(.pipe)) try self.parseCapture() else null;
    // {BLOCK}
    const body = try self.parseBlockExpression(.{});
    return try self.container.allocStatement(.{
        .location = self.current_token.location,
        .attributes = attributes,
        .variant = .{ .@"for" = .{
            .condition = condition,
            .capture = capture,
            .body = body,
        } },
    });
}

pub fn parseReturnStatement(self: *Parser, attributes: []const *ast.Expression) ErrorSet!*ast.Statement {
    // return
    try self.consumeKind(.@"return");
    // EXPRESSION
    const expression: ?*ast.Expression =
        if (!self.currentTokenIsKind(.close_brace))
    blk: {
        break :blk try self.parseExpression();
    } else null;
    return try self.container.allocStatement(.{
        .location = self.current_token.location,
        .attributes = attributes,
        .variant = .{ .@"return" = expression },
    });
}

pub fn tryParseLabel(self: *Parser, definition: bool) ErrorSet!?[]const u8 {
    const peek_zero = try self.peek(0);
    const peek_one = try self.peek(1);
    if (definition) {
        if (peek_zero.kind != .identifier) {
            return null;
        }
        // peek to check if it's a label definition
        if (peek_one.kind != .colon) {
            return null;
        }
        // IDENTIFIER
        const label = self.current_token.data;
        try self.consumeKind(.identifier);
        // :
        try self.consumeKind(.colon);
        return label;
    } else {
        if (peek_zero.kind != .colon) {
            return null;
        }
        if (peek_one.kind != .identifier) {
            return null;
        }
        // :
        try self.consumeKind(.colon);
        // IDENTIFIER
        const label = self.current_token.data;
        try self.consumeKind(.identifier);
        return label;
    }
}

pub fn parseBreakStatement(self: *Parser, attributes: []const *ast.Expression) ErrorSet!*ast.Statement {
    // break
    try self.consumeKind(.@"break");
    // :label
    const label = try self.tryParseLabel(false);
    return try self.container.allocStatement(.{
        .location = self.current_token.location,
        .attributes = attributes,
        .variant = .{ .@"break" = .{ .label = label } },
    });
}

pub fn parseContinueStatement(self: *Parser, attributes: []const *ast.Expression) ErrorSet!*ast.Statement {
    // continue
    try self.consumeKind(.@"continue");
    // :label
    const label = try self.tryParseLabel(false);
    return try self.container.allocStatement(.{
        .location = self.current_token.location,
        .attributes = attributes,
        .variant = .{ .@"continue" = .{ .label = label } },
    });
}

pub fn parseMatchStatement(self: *Parser, attributes: []const *ast.Expression) ErrorSet!*ast.Statement {
    const start = self.current_token.location;
    // match
    try self.consumeKind(.match);
    // (
    try self.consumeKind(.open_paren);
    // EXPRESSION
    const expression = try self.parseExpression();
    // )
    try self.consumeKind(.close_paren);
    // {
    try self.consumeKind(.open_brace);
    // CASES
    var cases = std.ArrayListUnmanaged(ast.MatchCase){};
    while (!self.currentTokenIsKind(.close_brace)) {
        const location = self.current_token.location;
        // PATTERN/else
        const pattern: ?*ast.Expression = if (self.currentTokenIsKind(.@"else")) blk: {
            try self.consumeKind(.@"else");
            break :blk null;
        } else try self.parseExpression();
        // =>
        try self.consumeKind(.double_arrow);
        // BODY
        const body = try self.parseBlockExpression(.{});
        try cases.append(self.container.node_allocator, .{
            .location = location,
            .pattern = pattern,
            .body = body,
        });
        if (!self.currentTokenIsKind(.close_brace)) {
            try self.consumeKind(.comma);
        }
    }
    // }
    try self.consumeKind(.close_brace);
    return try self.container.allocStatement(.{
        .location = start,
        .attributes = attributes,
        .variant = .{ .match = .{
            .expressions = expression,
            .cases = cases.items,
        } },
    });
}

pub const StatementDeclarationModifiers = struct {
    @"pub": bool = false,
    exported: bool = false,
};
pub fn parseDeclaration(self: *Parser, attributes: []const *ast.Expression) ErrorSet!*ast.Statement {
    const start = self.current_token.location;
    var lhs = std.ArrayListUnmanaged(ast.Declaration){};
    // pub
    const is_public = if (self.currentTokenIsKind(.@"pub")) blk: {
        try self.consumeKind(.@"pub");
        break :blk true;
    } else false;
    // exported
    const is_exported = if (self.currentTokenIsKind(.@"export")) blk: {
        try self.consumeKind(.@"export");
        break :blk true;
    } else false;

    const is_function_declaraction = self.currentTokenIsKind(.@"fn");
    if (is_function_declaraction) {
        try self.consumeKind(.@"fn");
        try self.expectCurrentTokenIsKind(.identifier);
        const identifier = self.current_token.data;
        try self.consumeKind(.identifier);

        try lhs.append(self.container.node_allocator, .{
            .mutable = false,
            .identifier = identifier,
            .exported = is_exported,
            .public = is_public,
        });
    } else {
        while (!self.currentTokenIsKind(.equal)) {
            // mut/let
            if (!self.currentTokenIsKind(.mut) and !self.currentTokenIsKind(.let)) {
                try self.pushErrorHere("expected 'mut' or 'let', got '{}'", .{self.current_token.kind});
                return error.ExpectedMutOrLet;
            }
            const mutable = self.currentTokenIsKind(.mut);
            try self.consumeKind(if (mutable) .mut else .let);
            // IDENTIFIER
            try self.expectCurrentTokenIsKind(.identifier);
            const identifier = self.current_token.data;
            try self.consumeKind(.identifier);
            // :
            const got_type = if (self.currentTokenIsKind(.colon)) blk: {
                try self.consumeKind(.colon);
                break :blk try self.parseType();
            } else null;

            try lhs.append(self.container.node_allocator, .{
                .mutable = mutable,
                .identifier = identifier,
                .type = got_type,
                .exported = is_exported,
                .public = is_public,
            });

            if (!self.currentTokenIsKind(.comma)) {
                break;
            }
            try self.consumeKind(.comma);
        }

        // =
        try self.expectCurrentTokenIsKind(.equal);
        try self.consumeKind(.equal);
    }

    // EXPRESSION
    const got_initialiser = if (is_function_declaraction) try self.parseFunctionExpression(true) else try self.parseExpression();
    return try self.container.allocStatement(.{
        .location = start,
        .attributes = attributes,
        .variant = .{ .declaration = .{
            .declarations = lhs.items,
            .initialiser = got_initialiser,
        } },
    });
}

pub fn parseAssignmentStatement(
    self: *Parser,
    attributes: []const *ast.Expression,
    assign_to: *ast.Expression,
    kind: Lexer.Token.Kind,
) ErrorSet!*ast.Statement {
    // EXPRESSION
    const expression = try self.parseExpression();
    return try self.container.allocStatement(.{
        .location = self.current_token.location,
        .attributes = attributes,
        .variant = .{ .assignment = .{
            .target = assign_to,
            .value = expression,
            .kind = kind,
        } },
    });
}

pub fn parseExpressionStatement(self: *Parser, attributes: []const *ast.Expression) ErrorSet!*ast.Statement {
    const start = self.current_token.location;
    const expression = try self.parseExpression();
    return try self.container.allocStatement(.{
        .location = start,
        .attributes = attributes,
        .variant = .{ .expression = expression },
    });
}

pub fn parseFunctionExpression(self: *Parser, keyword_consumed: bool) ErrorSet!*ast.Expression {
    const start = self.current_token.location;
    const function_type = if (keyword_consumed) try self.parseFunctionType() else try self.parseType();

    // if there is no open brace, then its a function type
    if (!self.currentTokenIsKind(.open_brace) and !self.currentTokenIsKind(.triple_dash)) {
        return try self.container.allocExpression(.{
            .location = start,
            .variant = .{ .type = function_type },
        });
    }

    const body = if (self.currentTokenIsKind(.triple_dash)) blk: {
        try self.consumeKind(.triple_dash);
        break :blk null;
    } else try self.parseBlockExpression(.{
        .statement_requirement = .multiple,
    });

    std.debug.print("body: {?}\n", .{body});

    return try self.container.allocExpression(.{
        .location = start,
        .variant = .{ .function = .{
            .typ = function_type,
            .body = body,
        } },
    });
}

pub fn parseCaptureLambdaExpression(self: *Parser) ErrorSet!*ast.Expression {
    const start = self.current_token.location;
    const capture = try self.parseCapture();
    const body = try self.parseBlockExpression(.{
        .statement_requirement = .one,
    });
    return try self.container.allocExpression(.{
        .location = start,
        .variant = .{ .lambda = .{
            .capture = capture,
            .body = body,
        } },
    });
}

pub const ParseBlockInfo = struct {
    statement_requirement: enum { one, any, multiple } = .any,
};
pub fn parseBlockExpression(self: *Parser, info: ParseBlockInfo) ErrorSet!*ast.Expression {
    const start = self.current_token.location;
    var statements = std.ArrayListUnmanaged(*ast.Statement){};
    const statement_requirement = info.statement_requirement;
    // {
    var multiple_statements = false;
    if (self.currentTokenIsKind(.open_brace)) {
        if (statement_requirement == .one) {
            try self.pushErrorHere("expected one statement, got start of a block", .{});
            return error.UnexpectedStatementStart;
        }
        try self.consumeKind(.open_brace);
        multiple_statements = true;
    } else if (statement_requirement == .multiple) {
        try self.pushErrorHere("expected start of block, got '{}'", .{self.current_token.kind});
        return error.ExpectedStartOfBlock;
    }
    while (true) {
        // }
        if (self.currentTokenIsKind(.close_brace)) {
            try self.consumeKind(.close_brace);
            break;
        }
        const statement = try self.parseStatement();
        try statements.append(self.container.node_allocator, statement);
        if (!multiple_statements) {
            break;
        }
    }
    return try self.container.allocExpression(.{
        .location = start,
        .variant = .{
            .block = .{
                .statements = statements.items,
            },
        },
    });
}

pub fn parseExpression(self: *Parser) ErrorSet!*ast.Expression {
    return try self.parseBinaryExpression(0, false);
}

pub fn parseBinaryExpression(self: *Parser, min_precedence: usize, turn_to_block: bool) ErrorSet!*ast.Expression {
    var left = try self.parseUnaryExpression(turn_to_block);
    while (self.current_token.kind.isBinaryOperation() and
        self.current_token.kind.precedence() >= min_precedence)
    {
        const operator = self.current_token.kind;
        try self.consumeKind(operator);

        const start = self.current_token.location;
        const right = try self.parseBinaryExpression(operator.precedence() + 1, operator.shouldTurnToBlock());
        left = try self.container.allocExpression(.{
            .location = start,
            .variant = .{ .binary = .{
                .left = left,
                .op = operator,
                .right = right,
            } },
        });
    }
    return left;
}

pub fn parseUnaryExpression(self: *Parser, turn_to_block: bool) ErrorSet!*ast.Expression {
    if (self.current_token.kind.isUnaryOperation()) {
        const op = self.current_token.kind;
        try self.consumeKind(op);
        const start = self.current_token.location;
        const operand = try self.parseUnaryExpression(turn_to_block);
        return try self.container.allocExpression(.{
            .location = start,
            .variant = .{ .unary = .{
                .op = op,
                .operand = operand,
            } },
        });
    } else {
        return try self.parsePrimaryExpression(turn_to_block);
    }
}

pub fn parsePrimaryExpression(self: *Parser, turn_to_block: bool) ErrorSet!*ast.Expression {
    const start = self.current_token.location;
    if (turn_to_block) {
        return try self.parseBlockExpression(.{});
    }
    switch (self.current_token.kind) {
        .open_paren => {
            try self.consumeKind(.open_paren);
            const expr = try self.parseExpression();
            try self.consumeKind(.close_paren);
            return expr;
        },
        .identifier => {
            return try self.parseIdentifierExpression();
        },
        .triple_dash => {
            try self.consumeKind(.triple_dash);
            return try self.container.allocExpression(.{ .location = start, .variant = .undefined });
        },
        .integer => {
            const got = try self.container.allocExpression(.{ .location = start, .variant = .{
                .integer_literal = std.fmt.parseInt(i128, self.current_token.data, 0) catch {
                    try self.pushErrorHere(
                        "invalid integer literal '{s}'",
                        .{self.current_token.data},
                    );
                    return error.InvalidIntegerLiteral;
                },
            } });
            try self.consumeKind(.integer);
            return got;
        },
        .float => {
            const got = try self.container.allocExpression(.{ .location = start, .variant = .{
                .float_literal = std.fmt.parseFloat(f64, self.current_token.data) catch {
                    try self.pushErrorHere(
                        "invalid float literal '{s}'",
                        .{self.current_token.data},
                    );
                    return error.InvalidIntegerLiteral;
                },
            } });
            try self.consumeKind(.float);
            return got;
        },
        .char => {
            const got = try self.container.allocExpression(.{ .location = start, .variant = .{
                .char_literal = @intCast(self.current_token.data[0]),
            } });
            try self.consumeKind(.char);
            return got;
        },
        .string => {
            const got = try self.container.allocExpression(.{ .location = start, .variant = .{
                .string_literal = self.current_token.data,
            } });
            try self.consumeKind(.string);
            return got;
        },
        .true, .false => {
            const got = try self.container.allocExpression(.{ .location = start, .variant = .{
                .boolean_literal = self.current_token.kind == .true,
            } });
            try self.consumeKind(self.current_token.kind);
            return got;
        },
        // types
        .@"struct",
        .@"enum",
        .question,
        .star,
        .open_bracket,
        => {
            const got = try self.parseType();
            return try self.container.allocExpression(.{ .location = start, .variant = .{
                .type = got,
            } });
        },
        .@"fn" => {
            return try self.parseFunctionExpression(false);
        },
        .pipe => {
            return try self.parseCaptureLambdaExpression();
        },
        .open_brace => {
            return try self.parseBlockExpression(.{});
        },
        .dot => {
            try self.consumeKind(.dot);
            switch (self.current_token.kind) {
                .open_brace => {
                    return try self.parseStructureLiteral();
                },
                // enum literal
                .identifier => {
                    const identifier = self.current_token.data;
                    try self.consumeKind(.identifier);
                    return try self.container.allocExpression(.{
                        .location = start,
                        .variant = .{ .enum_literal = identifier },
                    });
                },
                else => {
                    try self.pushErrorHere("unexpected token '{}'", .{self.current_token.kind});
                    return error.UnexpectedToken;
                },
            }
        },
        else => {
            try self.pushErrorHere("unexpected token '{}'", .{self.current_token.kind});
            return error.UnexpectedToken;
        },
    }
}

pub fn parseStructureLiteral(self: *Parser) ErrorSet!*ast.Expression {
    const start = self.current_token.location;
    try self.consumeKind(.open_brace);
    const fields = try self.parseFieldList(.{
        .type_requirement = .disallow,
        .value_requirement = .allow,
        .allow_attributes = false,
        .key_type = .expression,
    }, .close_brace);
    try self.consumeKind(.close_brace);
    return try self.container.allocExpression(.{ .location = start, .variant = .{
        .structure_literal = fields,
    } });
}

pub fn parseIdentifierExpression(self: *Parser) ErrorSet!*ast.Expression {
    const start = self.current_token.location;
    try self.expectCurrentTokenIsKind(.identifier);
    const identifier = self.current_token.data;
    try self.consumeKind(.identifier);

    const expr = try self.container.allocExpression(.{ .location = start, .variant = .{
        .identifier = identifier,
    } });

    return try self.parseExpressionChain(expr);
}

pub fn parseCallExpression(self: *Parser, expression: *ast.Expression) ErrorSet!*ast.Expression {
    const start = self.current_token.location;
    try self.expectCurrentTokenIsKind(.open_paren);
    try self.consumeKind(.open_paren);
    var arguments = std.ArrayListUnmanaged(*ast.Expression){};
    while (!self.currentTokenIsKind(.close_paren)) {
        const argument = try self.parseExpression();
        try arguments.append(self.container.node_allocator, argument);
        if (!self.currentTokenIsKind(.close_paren) and !self.currentTokenIsKind(.comma)) {
            try self.pushErrorHere("expected ',' or ')', got '{}'", .{self.current_token.kind});
            return error.ExpectedCommaOrCloseParen;
        }
        if (!self.currentTokenIsKind(.close_paren)) {
            try self.consumeKind(.comma);
        }
    }
    try self.consumeKind(.close_paren);
    const expr = try self.container.allocExpression(.{ .location = start, .variant = .{
        .call = .{
            .callee = expression,
            .arguments = arguments.items,
        },
    } });

    return try self.parseExpressionChain(expr);
}

pub fn parseSubscriptExpression(self: *Parser, expression: *ast.Expression) ErrorSet!*ast.Expression {
    const start = self.current_token.location;
    try self.consumeKind(.open_bracket);
    const index = try self.parseExpression();
    try self.consumeKind(.close_bracket);

    const expr = try self.container.allocExpression(.{ .location = start, .variant = .{
        .subscript = .{
            .array = expression,
            .index = index,
        },
    } });

    return try self.parseExpressionChain(expr);
}

pub fn parseSelectorExpression(self: *Parser, expression: *ast.Expression) ErrorSet!*ast.Expression {
    const start = self.current_token.location;
    try self.consumeKind(.dot);
    try self.expectCurrentTokenIsKind(.identifier);
    const field = self.current_token.data;
    try self.consumeKind(.identifier);
    const expr = try self.container.allocExpression(.{ .location = start, .variant = .{
        .field = .{
            .record = expression,
            .field = field,
        },
    } });

    return try self.parseExpressionChain(expr);
}

pub fn parsePipelineExpression(self: *Parser, expression: *ast.Expression) ErrorSet!*ast.Expression {
    self.in_pipeline = true;
    defer self.in_pipeline = false;
    const start = self.current_token.location;
    // |>
    try self.consumeKind(.pipeline);
    var stages = try std.ArrayListUnmanaged(*ast.Expression).initCapacity(self.container.node_allocator, 2);
    try stages.append(self.container.node_allocator, expression);

    while (true) {
        const stage = try self.parseExpression();
        try stages.append(self.container.node_allocator, stage);
        std.debug.print("stage: {}\n", .{stage.variant.call.callee.location});
        if (!self.currentTokenIsKind(.pipeline)) {
            break;
        }
        try self.consumeKind(.pipeline);
    }

    const expr = try self.container.allocExpression(.{ .location = start, .variant = .{ .pipeline = .{
        .stages = stages.items,
    } } });

    return try self.parseExpressionChain(expr);
}

pub fn parseExpressionChain(self: *Parser, inner_expression: *ast.Expression) ErrorSet!*ast.Expression {
    switch (self.current_token.kind) {
        .open_paren => return try self.parseCallExpression(inner_expression),
        .open_bracket => return try self.parseSubscriptExpression(inner_expression),
        .dot => return try self.parseSelectorExpression(inner_expression),
        else => {
            if (!self.in_pipeline and self.currentTokenIsKind(.pipeline)) {
                return try self.parsePipelineExpression(inner_expression);
            }
            return inner_expression;
        },
    }
}
