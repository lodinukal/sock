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

pub fn parseTopLevel(self: *Parser) ErrorSet!*ast.Node {
    if (self.current_token.kind == .eof) {
        return error.FinishedParsing;
    }
    if (self.currentTokenIsKind(.type))
        return try self.parseTypeDeclaration();
    return (try self.parseStatement()).node();
}

// type IDENTIFIER = TYPE
pub fn parseTypeDeclaration(self: *Parser) ErrorSet!*ast.Node {
    // type
    try self.consumeKind(.type);
    // IDENTIFIER
    try self.expectCurrentTokenIsKind(.identifier);
    const identifier = self.current_token.data;
    try self.consumeKind(.identifier);
    // =
    try self.consumeKind(.equal);
    // TYPE
    const got_type = try self.parseType();

    // const backing_type = if (kind == .@"enum" and self.currentTokenIsKind(.open_paren)) blk: {

    // };
    return try self.container.allocNode(.{
        .location = self.current_token.location,
        .variant = .{
            .statement = .{
                .attributes = &.{},
                .variant = .{
                    .type_declaration = .{
                        .exported = false,
                        .identifier = identifier,
                        .type = got_type,
                    },
                },
            },
        },
    });
}

pub fn parseType(self: *Parser) ErrorSet!*ast.Node.Type {
    switch (self.current_token.kind) {
        .@"struct" => return try self.parseStructType(),
        .star => {
            try self.consumeKind(.star);
            const inner = try self.parseType();
            return try self.container.allocType(.{ .pointer = inner });
        },
        .open_bracket => {
            try self.consumeKind(.open_bracket);
            if (self.currentTokenIsKind(.close_bracket)) {
                try self.consumeKind(.close_bracket);
                const inner = try self.parseType();
                return try self.container.allocType(.{ .span = inner });
            }
            unreachable;
            // const inner = try self.parseType();
            // try self.consumeKind(.close_bracket);
            // return try self.container.allocType(.{ .array = inner });
        },
        .identifier => {
            const identifier = self.current_token.data;
            try self.expectCurrentTokenIsKind(.identifier);

            if (std.meta.stringToEnum(ast.Node.PrimitiveType, identifier)) |primitive_type| {
                try self.consumeKind(.identifier);
                return try self.container.allocType(.{ .primitive = primitive_type });
            }

            return try self.container.allocType(.{ .expression = try self.parseExpression() });
        },
        else => {
            try self.pushErrorHere("unexpected token '{}'", .{self.current_token.kind});
            return error.UnexpectedToken;
        },
    }
}

const FieldParseInfo = struct {
    allow_type: bool = false,
    allow_default: bool = false,
    allow_attributes: bool = false,
};
pub fn parseField(self: *Parser, info: FieldParseInfo) ErrorSet!ast.Node.Field {
    // @ATTRIBUTES
    // const attributes = try self.parseAttributes();
    // IDENTIFIER
    try self.expectCurrentTokenIsKind(.identifier);
    const identifier = self.current_token.data;
    try self.consumeKind(.identifier);
    // :
    const got_type: ?*ast.Node.Type =
        if (self.currentTokenIsKind(.colon))
    blk: {
        if (!info.allow_type) {
            try self.pushErrorHere("unexpected token ':', you can't specify a type here", .{});
            return error.UnexpectedToken;
        }
        // TYPE
        try self.consumeKind(.colon);
        break :blk try self.parseType();
    } else null;
    // =
    const got_default: ?*ast.Node.Expression =
        if (self.currentTokenIsKind(.equal))
    blk: {
        if (!info.allow_default) {
            try self.pushErrorHere("unexpected token '=', you can't specify a default value here", .{});
            return error.UnexpectedToken;
        }
        // DEFAULT
        try self.consumeKind(.equal);
        break :blk try self.parseExpression();
    } else null;
    return .{
        .attributes = &.{},
        .identifier = identifier,
        .type = got_type,
        .initialiser = got_default,
    };
}

pub fn parseFieldList(self: *Parser, info: FieldParseInfo, closer: Lexer.Token.Kind) ErrorSet![]ast.Node.Field {
    var fields = std.ArrayListUnmanaged(ast.Node.Field){};
    while (!self.currentTokenIsKind(closer)) {
        const field = try self.parseField(info);
        try fields.append(self.arena_allocator, field);
        std.debug.print("{}\n", .{field});
        if (!self.currentTokenIsKind(closer)) {
            try self.consumeKind(.comma);
        }
    }
    return fields.items;
}

pub fn parseStructType(self: *Parser) ErrorSet!*ast.Node.Type {
    // struct
    try self.consumeKind(.@"struct");
    // {
    try self.consumeKind(.open_brace);
    // FIELDS
    const fields = try self.parseFieldList(.{
        .allow_type = true,
        .allow_default = true,
        .allow_attributes = true,
    }, .close_brace);
    // }
    try self.consumeKind(.close_brace);
    return try self.container.allocType(.{ .@"struct" = .{
        .fields = fields,
    } });
}

pub fn parseStatement(self: *Parser) ErrorSet!*ast.Node.Statement {
    // ATTRIBUTES
    // const attributes = try self.parseAttributes();
    const attributes: []const *ast.Node.Expression = &.{};
    switch (self.current_token.kind) {
        .@"if" => return try self.parseIfStatement(attributes),
        .@"while" => return try self.parseWhileStatement(attributes),
        .@"for" => return try self.parseForStatement(attributes),
        .@"return" => return try self.parseReturnStatement(attributes),
        .@"break" => return try self.parseBreakStatement(attributes),
        .@"continue" => return try self.parseContinueStatement(attributes),
        .mut => return try self.parseDeclaration(attributes),
        .let => return try self.parseDeclaration(attributes),
        else => return try self.parseExpressionStatement(attributes),
    }
}

pub fn parseCapture(self: *Parser) ErrorSet![]ast.Node.Field {
    // |
    try self.consumeKind(.pipe);
    var captures = std.ArrayListUnmanaged(ast.Node.Field){};
    while (!self.currentTokenIsKind(.pipe)) {
        const capture = try self.parseField(.{
            .allow_type = true,
        });
        try captures.append(self.arena_allocator, capture);
        if (!self.currentTokenIsKind(.pipe) or !self.currentTokenIsKind(.comma)) {
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

pub fn parseIfStatement(self: *Parser, attributes: []const *ast.Node.Expression) ErrorSet!*ast.Node.Statement {
    // if
    try self.consumeKind(.@"if");
    // (
    try self.consumeKind(.open_paren);
    // CONDITION
    const condition = try self.parseExpression();
    // )
    try self.consumeKind(.close_paren);
    // |CAPTURE|
    const capture: ?[]ast.Node.Field = if (self.nextTokenIsKind(.pipe)) try self.parseCapture() else null;
    // {BLOCK} / STATEMENT
    const then_branch = try self.parseBlockExpression();
    // ELSE
    const else_branch: ?*ast.Node.Expression =
        if (self.currentTokenIsKind(.@"else"))
    blk: {
        // else
        try self.consumeKind(.@"else");
        // ELSE
        break :blk try self.parseBlockExpression();
    } else null;
    return try self.container.allocStatement(.{
        .location = self.current_token.location,
        .variant = .{ .statement = .{
            .attributes = attributes,
            .variant = .{ .@"if" = .{
                .condition = condition,
                .capture = capture,
                .then_branch = then_branch,
                .else_branch = else_branch,
            } },
        } },
    });
}

pub fn parseWhileStatement(self: *Parser, attributes: []const *ast.Node.Expression) ErrorSet!*ast.Node.Statement {
    // while
    try self.consumeKind(.@"while");
    // (
    try self.consumeKind(.open_paren);
    // CONDITION
    const condition = try self.parseExpression();
    // )
    try self.consumeKind(.close_paren);
    // {BLOCK}
    const body = try self.parseBlockExpression();
    return try self.container.allocStatement(.{
        .location = self.current_token.location,
        .variant = .{ .statement = .{
            .attributes = attributes,
            .variant = .{ .@"while" = .{
                .condition = condition,
                .body = body,
            } },
        } },
    });
}

pub fn parseForStatement(self: *Parser, attributes: []const *ast.Node.Expression) ErrorSet!*ast.Node.Statement {
    // for
    try self.consumeKind(.@"for");
    // (
    try self.consumeKind(.open_paren);
    // CONDITION
    const condition = try self.parseExpression();
    // )
    try self.consumeKind(.close_paren);
    // |CAPTURE|
    const capture: ?[]ast.Node.Field = if (self.nextTokenIsKind(.pipe)) try self.parseCapture() else null;
    // {BLOCK}
    const body = try self.parseBlockExpression();
    return try self.container.allocStatement(.{
        .location = self.current_token.location,
        .variant = .{ .statement = .{
            .attributes = attributes,
            .variant = .{ .@"for" = .{
                .condition = condition,
                .capture = capture,
                .body = body,
            } },
        } },
    });
}

pub fn parseReturnStatement(self: *Parser, attributes: []const *ast.Node.Expression) ErrorSet!*ast.Node.Statement {
    // return
    try self.consumeKind(.@"return");
    // EXPRESSION
    const expression: ?*ast.Node.Expression =
        if (!self.currentTokenIsKind(.close_brace))
    blk: {
        break :blk try self.parseExpression();
    } else null;
    return try self.container.allocStatement(.{
        .location = self.current_token.location,
        .variant = .{ .statement = .{
            .attributes = attributes,
            .variant = .{ .@"return" = expression },
        } },
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

pub fn parseBreakStatement(self: *Parser, attributes: []const *ast.Node.Expression) ErrorSet!*ast.Node.Statement {
    // break
    try self.consumeKind(.@"break");
    // :label
    const label = try self.tryParseLabel(false);
    return try self.container.allocStatement(.{
        .location = self.current_token.location,
        .variant = .{ .statement = .{
            .attributes = attributes,
            .variant = .{ .@"break" = .{ .label = label } },
        } },
    });
}

pub fn parseContinueStatement(self: *Parser, attributes: []const *ast.Node.Expression) ErrorSet!*ast.Node.Statement {
    // continue
    try self.consumeKind(.@"continue");
    // :label
    const label = try self.tryParseLabel(false);
    return try self.container.allocStatement(.{
        .location = self.current_token.location,
        .variant = .{ .statement = .{
            .attributes = attributes,
            .variant = .{ .@"continue" = .{ .label = label } },
        } },
    });
}

pub fn parseDeclaration(self: *Parser, attributes: []const *ast.Node.Expression) ErrorSet!*ast.Node.Statement {
    var lhs = std.ArrayListUnmanaged(ast.Node.Declaration){};
    while (!self.currentTokenIsKind(.equal)) {
        // @ATTRIBUTES
        // const attributes = try self.parseAttributes();
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

        try lhs.append(self.arena_allocator, .{
            .mutable = mutable,
            .identifier = identifier,
            .type = got_type,
        });

        if (!self.currentTokenIsKind(.comma)) {
            break;
        }
        try self.consumeKind(.comma);
    }

    // =
    try self.expectCurrentTokenIsKind(.equal);
    try self.consumeKind(.equal);
    // EXPRESSION
    const got_initialiser = try self.parseExpression();
    return try self.container.allocStatement(.{
        .location = self.current_token.location,
        .variant = .{ .statement = .{
            .attributes = attributes,
            .variant = .{ .declaration = .{
                .declarations = lhs.items,
                .initialiser = got_initialiser,
            } },
        } },
    });
}

pub fn parseExpressionStatement(self: *Parser, attributes: []const *ast.Node.Expression) ErrorSet!*ast.Node.Statement {
    const expression = try self.parseBlockExpression();
    return try self.container.allocStatement(.{
        .location = self.current_token.location,
        .variant = .{ .statement = .{
            .attributes = attributes,
            .variant = .{ .expression = expression },
        } },
    });
}

pub fn parseBlockExpression(self: *Parser) ErrorSet!*ast.Node.Expression {
    var statements = std.ArrayListUnmanaged(*ast.Node.Statement){};
    // {
    var multiple_statements = false;
    if (self.currentTokenIsKind(.open_brace)) {
        try self.consumeKind(.open_brace);
        multiple_statements = true;
    }
    while (true) {
        const statement = try self.parseStatement();
        try statements.append(self.arena_allocator, statement);
        if (!multiple_statements or self.currentTokenIsKind(.close_brace)) {
            break;
        }
    }
    // }
    if (multiple_statements) {
        try self.consumeKind(.close_brace);
    }
    return try self.container.allocExpression(.{
        .location = self.current_token.location,
        .variant = .{ .expression = .{
            .block = statements.items,
        } },
    });
}

pub fn parseExpression(self: *Parser) ErrorSet!*ast.Node.Expression {
    return try self.parseBinaryExpression(0);
}

pub fn parseBinaryExpression(self: *Parser, min_precedence: usize) ErrorSet!*ast.Node.Expression {
    var left = try self.parseUnaryExpression();
    while (self.current_token.kind.isBinaryOperation() and
        self.current_token.kind.precedence() >= min_precedence)
    {
        const operator = self.current_token.kind;
        try self.consumeKind(operator);

        const right = try self.parseBinaryExpression(operator.precedence() + 1);
        left = try self.container.allocExpression(.{
            .location = self.current_token.location,
            .variant = .{ .expression = .{ .binary = .{
                .left = left,
                .op = operator,
                .right = right,
            } } },
        });
    }
    return left;
}

pub fn parseUnaryExpression(self: *Parser) ErrorSet!*ast.Node.Expression {
    if (self.current_token.kind.isUnaryOperation()) {
        const op = self.current_token.kind;
        try self.consumeKind(op);
        const operand = try self.parseUnaryExpression();
        return try self.container.allocExpression(.{
            .location = self.current_token.location,
            .variant = .{ .expression = .{ .unary = .{
                .op = op,
                .operand = operand,
            } } },
        });
    } else {
        return try self.parsePrimaryExpression();
    }
}

pub fn parsePrimaryExpression(self: *Parser) ErrorSet!*ast.Node.Expression {
    switch (self.current_token.kind) {
        .open_bracket => {
            try self.consumeKind(.open_bracket);
            const expr = try self.parseExpression();
            try self.consumeKind(.close_bracket);
            return expr;
        },
        .identifier => {
            return try self.parseIdentifierExpression();
        },
        .integer => {
            const got = try self.container.allocExpression(.{ .location = self.current_token.location, .variant = .{ .expression = .{
                .integer_literal = std.fmt.parseInt(i128, self.current_token.data, 0) catch {
                    try self.pushErrorHere(
                        "invalid integer literal '{s}'",
                        .{self.current_token.data},
                    );
                    return error.InvalidIntegerLiteral;
                },
            } } });
            try self.consumeKind(.integer);
            return got;
        },
        .float => {
            const got = try self.container.allocExpression(.{ .location = self.current_token.location, .variant = .{ .expression = .{
                .float_literal = std.fmt.parseFloat(f64, self.current_token.data) catch {
                    try self.pushErrorHere(
                        "invalid float literal '{s}'",
                        .{self.current_token.data},
                    );
                    return error.InvalidIntegerLiteral;
                },
            } } });
            try self.consumeKind(.float);
            return got;
        },
        .char => {
            const got = try self.container.allocExpression(.{ .location = self.current_token.location, .variant = .{ .expression = .{
                .char_literal = @intCast(self.current_token.data[0]),
            } } });
            try self.consumeKind(.char);
            return got;
        },
        .string => {
            const got = try self.container.allocExpression(.{ .location = self.current_token.location, .variant = .{ .expression = .{
                .string_literal = self.current_token.data,
            } } });
            try self.consumeKind(.string);
            return got;
        },
        .true, .false => {
            const got = try self.container.allocExpression(.{ .location = self.current_token.location, .variant = .{ .expression = .{
                .boolean_literal = self.current_token.kind == .true,
            } } });
            try self.consumeKind(self.current_token.kind);
            return got;
        },
        else => {
            try self.pushErrorHere("unexpected token '{}'", .{self.current_token.kind});
            return error.UnexpectedToken;
        },
    }
}

pub fn parseIdentifierExpression(self: *Parser) ErrorSet!*ast.Node.Expression {
    try self.expectCurrentTokenIsKind(.identifier);
    const identifier = self.current_token.data;
    try self.consumeKind(.identifier);

    const expr = try self.container.allocExpression(.{ .location = self.current_token.location, .variant = .{ .expression = .{
        .identifier = identifier,
    } } });

    switch (self.current_token.kind) {
        .open_paren => return try self.parseCallExpression(expr),
        .open_bracket => return try self.parseSubscriptExpression(expr),
        .dot => return try self.parseSelectorExpression(expr),
        else => return expr,
    }
}

pub fn parseCallExpression(self: *Parser, expression: *ast.Node.Expression) ErrorSet!*ast.Node.Expression {
    try self.expectCurrentTokenIsKind(.open_paren);
    try self.consumeKind(.open_paren);
    var arguments = std.ArrayListUnmanaged(*ast.Node.Expression){};
    while (!self.currentTokenIsKind(.close_paren)) {
        const argument = try self.parseExpression();
        try arguments.append(self.arena_allocator, argument);
        if (!self.currentTokenIsKind(.close_paren) or !self.currentTokenIsKind(.comma)) {
            try self.pushErrorHere("expected ',' or ')', got '{}'", .{self.current_token.kind});
            return error.ExpectedCommaOrCloseParen;
        }
        if (!self.currentTokenIsKind(.close_paren)) {
            try self.consumeKind(.comma);
        }
    }
    try self.consumeKind(.close_paren);
    return try self.container.allocExpression(.{ .location = self.current_token.location, .variant = .{ .expression = .{
        .call = .{
            .callee = expression,
            .arguments = arguments.items,
        },
    } } });
}

pub fn parseSubscriptExpression(self: *Parser, expression: *ast.Node.Expression) ErrorSet!*ast.Node.Expression {
    try self.expectCurrentTokenIsKind(.open_bracket);
    const index = try self.parseExpression();
    try self.consumeKind(.close_bracket);
    return try self.container.allocExpression(.{ .location = self.current_token.location, .variant = .{ .expression = .{
        .subscript = .{
            .array = expression,
            .index = index,
        },
    } } });
}

pub fn parseSelectorExpression(self: *Parser, expression: *ast.Node.Expression) ErrorSet!*ast.Node.Expression {
    try self.expectCurrentTokenIsKind(.dot);
    try self.consumeKind(.dot);
    try self.expectCurrentTokenIsKind(.identifier);
    const field = self.current_token.data;
    try self.consumeKind(.identifier);
    return try self.container.allocExpression(.{ .location = self.current_token.location, .variant = .{ .expression = .{
        .field = .{
            .record = expression,
            .field = field,
        },
    } } });
}
