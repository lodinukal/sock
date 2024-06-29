const std = @import("std");
const ast = @import("ast.zig");

pub const Token = struct {
    location: ast.Location = .{},
    data: []const u8 = "",
    kind: Kind = .eof,

    pub const Kind = enum {
        eof,
        comment,

        identifier,
        integer,
        float,
        string,
        char,
        err_string,
        err_char,

        // Punctuation
        open_paren, // (
        close_paren, // )
        open_brace, // {
        close_brace, // }
        open_bracket, // [
        close_bracket, // ]
        comma, // ,
        colon, // :
        dot, // .
        arrow, // ->
        at, // @

        // Operators
        plus, // +
        minus, // -
        star, // \*
        slash, // /
        slash_slash, // //
        percent, // %
        ampersand, // &
        pipe, // |
        caret, // ^
        tilde, // ~
        bang, // \!
        question, // \?
        less, // <
        greater, // >
        equal, // =
        equal_equal, // ==
        bang_equal, // \!=
        less_equal, // <=
        greater_equal, // >=
        less_less, // <<
        greater_greater, // >>
        plus_equal, // +=
        minus_equal, // -=
        star_equal, // \*=
        slash_equal, // /=
        slash_slash_equal, // //=
        percent_equal, // %=
        ampersand_equal, // &=
        pipe_equal, // \|=
        caret_equal, // ^=
        less_less_equal, // <<=
        greater_greater_equal, // >>=

        // Keywords
        @"fn",
        @"if",
        @"else",
        @"while",
        @"for",
        @"return",
        @"break",
        @"continue",
        @"struct",
        @"enum",
        @"export",
        @"pub",
        @"and",
        @"or",
        mut,
        let,
        true,
        false,

        pub fn format(self: Kind, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            const value = switch (self) {
                .eof => "EOF",
                .comment => "comment",
                .identifier => "identifier",
                .integer => "integer",
                .float => "float",
                .string => "string",
                .char => "char",
                .err_string => "err_string",
                .err_char => "err_char",
                .open_paren => "(",
                .close_paren => ")",
                .open_brace => "{",
                .close_brace => "}",
                .open_bracket => "[",
                .close_bracket => "]",
                .comma => ",",
                .colon => ":",
                .dot => ".",
                .arrow => "->",
                .at => "@",
                .plus => "+",
                .minus => "-",
                .star => "*",
                .slash => "/",
                .slash_slash => "//",
                .percent => "%",
                .ampersand => "&",
                .pipe => "|",
                .caret => "^",
                .tilde => "~",
                .bang => "!",
                .question => "?",
                .less => "<",
                .greater => ">",
                .equal => "=",
                .equal_equal => "==",
                .bang_equal => "!=",
                .less_equal => "<=",
                .greater_equal => ">=",
                .less_less => "<<",
                .greater_greater => ">>",
                .plus_equal => "+=",
                .minus_equal => "-=",
                .star_equal => "*=",
                .slash_equal => "/=",
                .slash_slash_equal => "//=",
                .percent_equal => "%=",
                .ampersand_equal => "&=",
                .pipe_equal => "|=",
                .caret_equal => "^=",
                .less_less_equal => "<<=",
                .greater_greater_equal => ">>=",
                .@"fn" => "fn",
                .@"if" => "if",
                .@"else" => "else",
                .@"while" => "while",
                .@"for" => "for",
                .@"return" => "return",
                .@"break" => "break",
                .@"continue" => "continue",
                .@"struct" => "struct",
                .@"enum" => "enum",
                .@"export" => "export",
                .@"pub" => "pub",
                .@"and" => "and",
                .@"or" => "or",
                .mut => "mut",
                .let => "let",
                .true => "true",
                .false => "false",
            };
            try writer.writeAll(value);
        }

        pub fn isKeyword(self: Kind) bool {
            return switch (self) {
                .@"fn",
                .@"if",
                .@"else",
                .@"while",
                .@"for",
                .@"return",
                .@"break",
                .@"continue",
                .@"struct",
                .@"enum",
                .@"export",
                .@"pub",
                .@"and",
                .@"or",
                .mut,
                .let,
                .true,
                .false,
                => true,
                else => false,
            };
        }

        pub fn isBinaryOperation(self: Kind) bool {
            return switch (self) {
                .plus,
                .minus,
                .star,
                .slash,
                .slash_slash,
                .percent,
                .ampersand,
                .pipe,
                .caret,
                .less,
                .greater,
                .equal_equal,
                .bang_equal,
                .less_equal,
                .greater_equal,
                .less_less,
                .greater_greater,
                .@"and",
                .@"or",
                => true,
                else => false,
            };
        }

        pub fn isAssignmentOperation(self: Kind) bool {
            return switch (self) {
                .plus_equal,
                .minus_equal,
                .star_equal,
                .slash_equal,
                .slash_slash_equal,
                .percent_equal,
                .ampersand_equal,
                .pipe_equal,
                .caret_equal,
                .less_less_equal,
                .greater_greater_equal,
                => true,
                else => false,
            };
        }

        pub fn isUnaryOperation(self: Kind) bool {
            return switch (self) {
                .minus,
                .tilde,
                .bang,
                => true,
                else => false,
            };
        }

        pub fn precedence(self: Kind) u32 {
            return switch (self) {
                .@"or" => 1,
                .@"and" => 2,
                .pipe => 3,
                .caret => 4,
                .ampersand => 5,
                .equal_equal, .bang_equal => 6,
                .less, .greater, .less_equal, .greater_equal => 7,
                .less_less, .greater_greater => 8,
                .plus, .minus => 9,
                .star, .slash, .slash_slash, .percent => 10,
                else => 0,
            };
        }
    };

    pub fn format(self: Token, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (self.data.len > 0) {
            try writer.print("{}: {}({s})", .{ self.location, self.kind, self.data });
        } else {
            try writer.print("{}: {}", .{ self.location, self.kind });
        }
    }
};

pub fn stringAsKeyword(str: []const u8) ?Token.Kind {
    const as_kind = std.meta.stringToEnum(Token.Kind, str) orelse return null;
    return if (as_kind.isKeyword()) as_kind else null;
}

const Lexer = @This();
buffer: []const u8,
file_path: []const u8 = "",

offset: u32 = 0,
line: u32 = 0,
line_offset: u32 = 0,

token: Token = .{},
previous_location: ast.Location = .{},

pub fn init(buffer: []const u8, file_path: []const u8) Lexer {
    return Lexer{ .buffer = buffer, .file_path = file_path };
}

pub fn deinit(_: *Lexer) void {}

pub fn next(self: *Lexer) !Token {
    while (std.ascii.isWhitespace(self.peek(0)))
        self.consumeAny();
    self.previous_location = self.token.location;
    self.token = try self.readNext();

    return self.token;
}

fn isNewline(ch: u8) bool {
    return ch == '\n';
}

pub fn nextLine(self: *Lexer) void {
    while (self.peek(0) != 0 and self.peek(0) != 'r' and !isNewline(self.peek(0)))
        try self.consume();
    self.next();
}

pub fn lookahead(self: *Lexer) Token {
    const current_offset = self.offset;
    const current_line = self.line;
    const current_offset_line = self.line_offset;
    const current_token = self.token;
    const current_previous_location = self.previous_location;
    defer {
        self.offset = current_offset;
        self.line = current_line;
        self.line_offset = current_offset_line;
        self.token = current_token;
        self.previous_location = current_previous_location;
    }

    return self.next();
}

pub inline fn peek(self: Lexer, n: u32) u8 {
    return if (self.offset + n < self.buffer.len) self.buffer[self.offset + n] else 0;
}

pub inline fn position(self: Lexer) ast.Position {
    return .{
        .line = self.line,
        .column = self.offset - self.line_offset,
    };
}

pub inline fn consume(self: *Lexer) !void {
    if (isNewline(self.buffer[self.offset])) {
        return error.Unexpected;
    }
    self.offset += 1;
}

pub inline fn consumeAny(self: *Lexer) void {
    if (isNewline(self.buffer[self.offset])) {
        self.line += 1;
        self.line_offset = self.offset + 1;
    }
    self.offset += 1;
}

pub fn readComment(self: *Lexer) !Token {
    const start = self.position();
    std.debug.assert(self.peek(0) == '/');
    const is_multiline = self.peek(1) == '*';
    try self.consume();
    try self.consume();

    const start_offset = self.offset;
    if (is_multiline) {
        while (self.peek(0) != 0) {
            if (self.peek(0) == '*' and self.peek(1) == '/') {
                try self.consume();
                try self.consume();
                return Token{
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                    .data = self.buffer[start_offset..self.offset],
                    .kind = .comment,
                };
            }
            try self.consume();
        }
        return error.Unexpected;
    } else {
        while (self.peek(0) != 0 and !isNewline(self.peek(0)))
            try self.consume();

        return Token{
            .location = .{
                .file = self.file_path,
                .begin = start,
                .end = self.position(),
            },
            .data = self.buffer[start_offset..self.offset],
            .kind = .comment,
        };
    }
}

pub fn readBackslashInString(self: *Lexer) !void {
    std.debug.assert(self.peek(0) == '\\');
    try self.consume();
    switch (self.peek(0)) {
        '\r' => {
            try self.consume();
            if (self.peek(0) == '\n') {
                self.consumeAny();
            }
        },
        0 => {},
        'z' => {
            try self.consume();
            while (std.ascii.isWhitespace(self.peek(0)))
                self.consumeAny();
        },
        else => {
            self.consumeAny();
        },
    }
}

pub fn readStringOrCharacterLiteral(self: *Lexer) !Token {
    const start = self.position();

    const delimiter = self.peek(0);
    const is_character = delimiter == '\'';
    const is_multiline_string = delimiter == '`';
    std.debug.assert(delimiter == '"' or is_character or is_multiline_string);
    try self.consume();

    const start_offset = self.offset;

    var single_character_consumed = false;
    walker: while (self.peek(0) != delimiter) {
        if (single_character_consumed and is_character) {
            return .{
                .location = .{
                    .file = self.file_path,
                    .begin = start,
                    .end = self.position(),
                },
                .kind = .err_char,
                .data = self.buffer[start_offset..self.offset],
            };
        }
        switch (self.peek(0)) {
            0, '\r', '\n' => {
                if (is_multiline_string) {
                    self.consumeAny();
                    continue :walker;
                }
                return .{
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                    .kind = .err_string,
                    .data = self.buffer[start_offset..self.offset],
                };
            },
            '\\' => {
                try self.readBackslashInString();
            },
            else => {
                try self.consume();
            },
        }
        single_character_consumed = true;
    }
    try self.consume();
    return .{
        .location = .{
            .file = self.file_path,
            .begin = start,
            .end = self.position(),
        },
        .kind = .string,
        .data = self.buffer[start_offset .. self.offset - 1],
    };
}

pub fn readNumber(self: *Lexer, start: ast.Position, start_offset: u32) !Token {
    std.debug.assert(std.ascii.isDigit(self.peek(0)));

    var has_point = false;
    while (true) {
        try self.consume();
        const got_point = self.peek(0) == '.';
        has_point = has_point or got_point;
        if (!(std.ascii.isDigit(self.peek(0)) or got_point or self.peek(0) == '_')) {
            break;
        }
    }

    if (std.ascii.toLower(self.peek(0)) == 'e') {
        try self.consume();

        if (self.peek(0) == '+' or self.peek(0) == '-') {
            try self.consume();
        }
    }

    while (std.ascii.isAlphanumeric(self.peek(0)) or std.ascii.isDigit(self.peek(0)) or self.peek(0) == '_') {
        try self.consume();
    }

    return .{
        .location = .{
            .file = self.file_path,
            .begin = start,
            .end = self.position(),
        },
        .kind = if (has_point) .float else .integer,
        .data = self.buffer[start_offset..self.offset],
    };
}

pub fn readIdentifier(self: *Lexer) ![]const u8 {
    std.debug.assert(std.ascii.isAlphabetic(self.peek(0)) or self.peek(0) == '_' or self.peek(0) == '@');

    const start_offset = self.offset;

    while (true) {
        try self.consume();
        if (!(std.ascii.isAlphanumeric(self.peek(0)) or self.peek(0) == '_')) {
            break;
        }
    }

    return self.buffer[start_offset..self.offset];
}

pub fn readNext(self: *Lexer) !Token {
    const start = self.position();

    switch (self.peek(0)) {
        0 => return .{
            .kind = .eof,
            .location = .{
                .file = self.file_path,
                .begin = start,
                .end = start,
            },
        },
        '(' => {
            try self.consume();
            return .{
                .kind = .open_paren,
                .location = .{
                    .file = self.file_path,
                    .begin = start,
                    .end = self.position(),
                },
            };
        },
        ')' => {
            try self.consume();
            return .{
                .kind = .close_paren,
                .location = .{
                    .file = self.file_path,
                    .begin = start,
                    .end = self.position(),
                },
            };
        },
        '{' => {
            try self.consume();
            return .{
                .kind = .open_brace,
                .location = .{
                    .file = self.file_path,
                    .begin = start,
                    .end = self.position(),
                },
            };
        },
        '}' => {
            try self.consume();
            return .{
                .kind = .close_brace,
                .location = .{
                    .file = self.file_path,
                    .begin = start,
                    .end = self.position(),
                },
            };
        },
        '[' => {
            try self.consume();
            return .{
                .kind = .open_bracket,
                .location = .{
                    .file = self.file_path,
                    .begin = start,
                    .end = self.position(),
                },
            };
        },
        ']' => {
            try self.consume();
            return .{
                .kind = .close_bracket,
                .location = .{
                    .file = self.file_path,
                    .begin = start,
                    .end = self.position(),
                },
            };
        },
        ',' => {
            try self.consume();
            return .{
                .kind = .comma,
                .location = .{
                    .file = self.file_path,
                    .begin = start,
                    .end = self.position(),
                },
            };
        },
        ':' => {
            try self.consume();
            return .{
                .kind = .colon,
                .location = .{
                    .file = self.file_path,
                    .begin = start,
                    .end = self.position(),
                },
            };
        },
        '.' => {
            try self.consume();
            return .{
                .kind = .dot,
                .location = .{
                    .file = self.file_path,
                    .begin = start,
                    .end = self.position(),
                },
            };
        },
        '+' => {
            try self.consume();
            if (self.peek(0) == '=') {
                try self.consume();
                return .{
                    .kind = .plus_equal,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            } else {
                return .{
                    .kind = .plus,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            }
        },
        '-' => {
            try self.consume();
            if (self.peek(0) == '>') {
                try self.consume();
                return .{
                    .kind = .arrow,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            } else if (self.peek(0) == '=') {
                try self.consume();
                return .{
                    .kind = .minus_equal,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            } else {
                return .{
                    .kind = .minus,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            }
        },
        '*' => {
            try self.consume();
            if (self.peek(0) == '=') {
                try self.consume();
                return .{
                    .kind = .star_equal,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            } else {
                return .{
                    .kind = .star,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            }
        },
        '/' => {
            if (self.peek(1) == '/' or self.peek(1) == '*') {
                return try self.readComment();
            } else {
                try self.consume();
                if (self.peek(0) == '=') {
                    try self.consume();
                    return .{
                        .kind = .slash_equal,
                        .location = .{
                            .file = self.file_path,
                            .begin = start,
                            .end = self.position(),
                        },
                    };
                } else if (self.peek(0) == '/') {
                    if (self.peek(1) == '=') {
                        try self.consume();
                        try self.consume();
                        return .{
                            .kind = .slash_slash_equal,
                            .location = .{
                                .file = self.file_path,
                                .begin = start,
                                .end = self.position(),
                            },
                        };
                    } else {
                        try self.consume();
                        return .{
                            .kind = .slash_slash,
                            .location = .{
                                .file = self.file_path,
                                .begin = start,
                                .end = self.position(),
                            },
                        };
                    }
                } else {
                    return .{
                        .kind = .slash,
                        .location = .{
                            .file = self.file_path,
                            .begin = start,
                            .end = self.position(),
                        },
                    };
                }
            }
        },
        '%' => {
            try self.consume();
            if (self.peek(0) == '=') {
                try self.consume();
                return .{
                    .kind = .percent_equal,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            } else {
                return .{
                    .kind = .percent,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            }
        },
        '&' => {
            try self.consume();
            if (self.peek(0) == '=') {
                try self.consume();
                return .{
                    .kind = .ampersand_equal,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            } else {
                return .{
                    .kind = .ampersand,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            }
        },
        '|' => {
            try self.consume();
            if (self.peek(0) == '=') {
                try self.consume();
                return .{
                    .kind = .pipe_equal,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            } else {
                return .{
                    .kind = .pipe,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            }
        },
        '^' => {
            try self.consume();
            if (self.peek(0) == '=') {
                try self.consume();
                return .{
                    .kind = .caret_equal,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            } else {
                return .{
                    .kind = .caret,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            }
        },
        '~' => {
            try self.consume();
            return .{
                .kind = .tilde,
                .location = .{
                    .file = self.file_path,
                    .begin = start,
                    .end = self.position(),
                },
            };
        },
        '!' => {
            try self.consume();
            if (self.peek(0) == '=') {
                try self.consume();
                return .{
                    .kind = .bang_equal,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            } else {
                return .{
                    .kind = .bang,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            }
        },
        '?' => {
            try self.consume();
            return .{
                .kind = .question,
                .location = .{
                    .file = self.file_path,
                    .begin = start,
                    .end = self.position(),
                },
            };
        },
        '<' => {
            try self.consume();
            if (self.peek(0) == '<') {
                try self.consume();
                if (self.peek(0) == '=') {
                    try self.consume();
                    return .{
                        .kind = .less_less_equal,
                        .location = .{
                            .file = self.file_path,
                            .begin = start,
                            .end = self.position(),
                        },
                    };
                } else {
                    return .{
                        .kind = .less_less,
                        .location = .{
                            .file = self.file_path,
                            .begin = start,
                            .end = self.position(),
                        },
                    };
                }
            } else if (self.peek(0) == '=') {
                try self.consume();
                return .{
                    .kind = .less_equal,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            } else {
                return .{
                    .kind = .less,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            }
        },
        '>' => {
            try self.consume();
            if (self.peek(0) == '>') {
                try self.consume();
                if (self.peek(0) == '=') {
                    try self.consume();
                    return .{
                        .kind = .greater_greater_equal,
                        .location = .{
                            .file = self.file_path,
                            .begin = start,
                            .end = self.position(),
                        },
                    };
                } else {
                    return .{
                        .kind = .greater_greater,
                        .location = .{
                            .file = self.file_path,
                            .begin = start,
                            .end = self.position(),
                        },
                    };
                }
            } else if (self.peek(0) == '=') {
                try self.consume();
                return .{
                    .kind = .greater_equal,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            } else {
                return .{
                    .kind = .greater,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            }
        },
        '=' => {
            try self.consume();
            if (self.peek(0) == '=') {
                try self.consume();
                return .{
                    .kind = .equal_equal,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            } else {
                return .{
                    .kind = .equal,
                    .location = .{
                        .file = self.file_path,
                        .begin = start,
                        .end = self.position(),
                    },
                };
            }
        },
        '@' => {
            try self.consume();
            return .{
                .kind = .at,
                .location = .{
                    .file = self.file_path,
                    .begin = start,
                    .end = self.position(),
                },
            };
        },
        '"', '\'', '`' => return try self.readStringOrCharacterLiteral(),
        else => {
            if (std.ascii.isDigit(self.peek(0))) {
                return try self.readNumber(start, self.offset);
            } else if (std.ascii.isAlphabetic(self.peek(0)) or self.peek(0) == '_') {
                const name = try self.readIdentifier();
                if (stringAsKeyword(name)) |keyword| {
                    return .{
                        .kind = keyword,
                        .location = .{
                            .file = self.file_path,
                            .begin = start,
                            .end = self.position(),
                        },
                    };
                } else {
                    return .{
                        .kind = .identifier,
                        .location = .{
                            .file = self.file_path,
                            .begin = start,
                            .end = self.position(),
                        },
                        .data = name,
                    };
                }

                // TODO: check utf8 here
            } else {
                return error.Unexpected;
            }
        },
    }
}
