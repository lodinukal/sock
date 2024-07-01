const std = @import("std");
const ast = @import("ast.zig");
const Lexer = @import("Lexer.zig");

const Self = @This();

allocator: std.mem.Allocator = undefined,
node_area: std.heap.ArenaAllocator = undefined,
node_allocator: std.mem.Allocator = undefined,
container: *ast.Container = undefined,
symbols: SymbolTable = undefined,

pub fn init(self: *Self, allocator: std.mem.Allocator, container: *ast.Container) !void {
    self.* = .{
        .allocator = allocator,
        .node_area = std.heap.ArenaAllocator.init(allocator),
        .node_allocator = self.node_area.allocator(),
        .container = container,
        .symbols = SymbolTable.init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.node_area.deinit();
}

pub const SymbolTable = struct {
    scopes: std.ArrayListUnmanaged(Scope),
    current_scope: ?ScopeIndex = null,

    pub fn init(allocator: std.mem.Allocator) !SymbolTable {
        var self: SymbolTable = .{
            .scopes = try std.ArrayListUnmanaged(Scope).initCapacity(allocator, 1),
        };
        self.scopes.items[0] = .{};
        return self;
    }

    pub fn deinit(self: *SymbolTable, allocator: std.mem.Allocator) void {
        for (self.scopes.items) |*scope| {
            scope.deinit(allocator);
        }
        self.scopes.deinit(allocator);
    }

    pub fn enterScope(self: *SymbolTable, allocator: std.mem.Allocator) !void {
        const scope = try self.scopes.addOne(allocator);
        scope.* = .{
            .parent = self.current_scope,
        };
        const index = self.scopes.items.len - 1;
        const current_scope_object: *Scope = self.getScope(self.current_scope) catch return error.OutOfRange;
        scope.next = current_scope_object.child;
        current_scope_object.child = @enumFromInt(index);
        self.current_scope = @enumFromInt(index);
    }

    pub fn exitScope(self: *SymbolTable) void {
        const current_scope = self.current_scope orelse return;
        const current_scope_object: *Scope = self.getScope(current_scope) catch return;
        self.current_scope = current_scope_object.parent;
    }

    pub const AddInfo = struct {
        typ: *ast.Type,
        mutable: bool,
        declaration: *ast.Statement,
    };
    pub fn add(self: *SymbolTable, allocator: std.mem.Allocator, name: []const u8, info: AddInfo) !void {
        if (self.scopes.items.len == 0) {
            try self.enterScope();
        }

        var scope = &self.scopes.items[self.scopes.items.len - 1];
        const symbol = try allocator.create(Symbol);
        symbol.* = .{
            .name = name,
            .typ = info.typ,
            .mutable = info.mutable,
            .declaration = info.declaration,
        };
        try scope.put(name, symbol);
    }

    pub fn lookup(self: *SymbolTable, name: []const u8) ?*Symbol {
        var current_scope = self.current_scope orelse return null;
        var scope_object: ?*Scope = self.getScope(current_scope) catch return null;
        while (scope_object) |scope| {
            if (scope.lookup(name)) |symbol| {
                return symbol;
            }
            current_scope = scope.next orelse break;
            scope_object = self.getScope(current_scope) catch break;
        }
        return null;
    }

    pub fn getScope(self: *SymbolTable, index: ScopeIndex) !*Scope {
        const as_index = @intFromEnum(index);
        if (self.scopes.items.len <= as_index) {
            return error.OutOfRange;
        }
        return &self.scopes.items[as_index];
    }
};

const Scope = struct {
    symbols: std.StringHashMapUnmanaged(Symbol) = .{},
    parent: ?ScopeIndex = null,
    child: ?ScopeIndex = null,
    next: ?ScopeIndex = null,

    pub fn deinit(self: *Scope, allocator: std.mem.Allocator) void {
        self.symbols.deinit(allocator);
    }

    pub fn lookup(self: *Scope, name: []const u8) ?*Symbol {
        return self.symbols.get(name);
    }

    pub fn put(self: *Scope, allocator: std.mem.Allocator, name: []const u8, symbol: *Symbol) !void {
        try self.symbols.put(allocator, name, symbol);
    }

    pub fn getDepth(self: *Scope, table: *SymbolTable) usize {
        var depth: usize = 0;
        var current: ?*const Scope = self;
        while (current) |scope| {
            depth += 1;
            current = table.getScope(scope.parent) catch break;
        }
        return depth;
    }
};

pub const ScopeIndex = enum(usize) {
    root,
    _,
};

pub const Symbol = struct {
    name: []const u8,
    typ: ?*ast.Type = null,
    mutable: bool = false,
    declaration: *ast.Statement,
};
