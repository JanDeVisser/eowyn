//
// Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
//
// SPDX-License-Identifier: MIT
//

const std = @import("std");
const Allocator = std.mem.Allocator;

const grm = @import("grammar.zig");
const lxr = @import("lexer.zig");
const grmparser = @import("grammarparser.zig");
const parse = @import("parser.zig");

const Parser = parse.Parser;
const Token = lxr.Token;
const Grammar = grm.Grammar;
const Rule = grm.Rule;
const Sequence = grm.Sequence;
const Symbol = grm.Symbol;
const GrammarParser = grmparser.GrammarParser;

const eowyn_grammar: []const u8 = @embedFile("eowyn.grammar");

pub const OperatorCtx = struct {
    pub fn hash(this: @This(), op: lxr.Token.Kind) u64 {
        _ = this;
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHashStrat(&hasher, op, .Deep);
        return hasher.final();
    }

    pub fn eql(_: @This(), a: lxr.Token.Kind, b: lxr.Token.Kind) bool {
        return a.eql(b);
    }
};

const BinaryOperator = enum {
    Add,
    BinaryAnd,
    BinaryOr,
    BinaryXor,
    Divide,
    Equal,
    Greater,
    GreaterEqual,
    LeftShift,
    Less,
    LessEqual,
    LogicalAnd,
    LogicalOr,
    Modulo,
    Multiply,
    NotEqual,
    RightShift,
    Subtract,

    pub fn format(this: BinaryOperator, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        for (BinaryOperatorMap.operators) |op| {
            if (op.op == this) {
                switch (op.token) {
                    .Symbol => |c| try w.print("{c}", .{c}),
                    .Keyword => |s| try w.print("{s}", .{s}),
                    else => unreachable,
                }
                return;
            }
        }
        try w.writeAll("??");
    }
};

const BinaryOperatorMap = struct {
    op: BinaryOperator,
    token: lxr.Token.Kind,

    const operators = [_]BinaryOperatorMap{
        .{ .op = .Add, .token = .{ .Symbol = '+' } },
        .{ .op = .BinaryAnd, .token = .{ .Symbol = '&' } },
        .{ .op = .BinaryOr, .token = .{ .Symbol = '|' } },
        .{ .op = .BinaryXor, .token = .{ .Symbol = '^' } },
        .{ .op = .Divide, .token = .{ .Symbol = '/' } },
        .{ .op = .Equal, .token = .{ .Keyword = "==" } },
        .{ .op = .Greater, .token = .{ .Symbol = '>' } },
        .{ .op = .GreaterEqual, .token = .{ .Keyword = ">=" } },
        .{ .op = .LeftShift, .token = .{ .Keyword = "<<" } },
        .{ .op = .Less, .token = .{ .Symbol = '<' } },
        .{ .op = .LessEqual, .token = .{ .Keyword = "<=" } },
        .{ .op = .LogicalAnd, .token = .{ .Keyword = "&&" } },
        .{ .op = .LogicalOr, .token = .{ .Keyword = "||" } },
        .{ .op = .Modulo, .token = .{ .Symbol = '%' } },
        .{ .op = .Multiply, .token = .{ .Symbol = '*' } },
        .{ .op = .NotEqual, .token = .{ .Symbol = '+' } },
        .{ .op = .RightShift, .token = .{ .Keyword = ">>" } },
        .{ .op = .Subtract, .token = .{ .Symbol = '-' } },
    };
};

const UnaryOperator = enum {
    Idempotent,
    Invert,
    LogicalNegate,
    Negate,

    pub fn format(this: UnaryOperator, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        for (UnaryOperatorMap.operators) |op| {
            if (op.op == this) {
                switch (op.token) {
                    .Symbol => |c| try w.print("{c}", .{c}),
                    .Keyword => |s| try w.print("{s}", .{s}),
                    else => unreachable,
                }
                return;
            }
        }
        try w.writeAll("??");
    }
};

const UnaryOperatorMap = struct {
    op: UnaryOperator,
    token: lxr.Token.Kind,

    const operators = [_]UnaryOperatorMap{
        .{ .op = .Idempotent, .token = .{ .Symbol = '+' } },
        .{ .op = .Invert, .token = .{ .Symbol = '~' } },
        .{ .op = .LogicalNegate, .token = .{ .Symbol = '!' } },
        .{ .op = .Negate, .token = .{ .Symbol = '-' } },
    };
};

const BinaryExpression = struct {
    left: NodeReference,
    op: BinaryOperator,
    right: NodeReference,

    pub fn format(this: BinaryExpression, parser: *EowynParser, w: anytype) !void {
        try w.print("{} {} {}", .{ parser.get_node(this.left), this.op, parser.get_node(this.right) });
    }
};

const Block = struct {
    statements: std.ArrayList(NodeReference),

    pub fn deinit(this: *Block) void {
        this.statements.deinit();
    }

    pub fn format(this: Block, parser: *EowynParser, w: anytype) !void {
        for (this.statements.items) |stmt| {
            try w.print("{}\n", .{parser.get_node(stmt)});
        }
    }
};

const Function = struct {
    declaration: NodeReference,
    implementation: NodeReference,

    pub fn format(this: Function, parser: *EowynParser, w: anytype) !void {
        try w.print("{} {{\n{}}}\n", .{ parser.get_node(this.declaration), parser.get_node(this.implementation) });
    }
};

const FunctionCall = struct {
    name: []const u8,
    arguments: std.ArrayList(NodeReference),

    pub fn deinit(this: *FunctionCall) void {
        this.arguments.deinit();
    }

    pub fn format(this: FunctionCall, parser: *EowynParser, w: anytype) !void {
        try w.print("{s}(", .{this.name});
        var first = true;
        for (this.arguments.items) |a| {
            if (!first) {
                try w.print(", ", .{});
            }
            try w.print("{}", .{parser.get_node(a)});
            first = false;
        }
        try w.print(")", .{});
    }
};

const FunctionDecl = struct {
    name: []const u8,
    parameters: std.ArrayList(NodeReference),

    pub fn deinit(this: *FunctionDecl) void {
        this.parameters.deinit();
    }

    pub fn format(this: FunctionDecl, parser: *EowynParser, w: anytype) !void {
        try w.print("func {s}(", .{this.name});
        var first = true;
        for (this.parameters.items) |p| {
            if (!first) {
                try w.print(", ", .{});
            }
            try w.print("{}", .{parser.get_node(p)});
            first = false;
        }
        try w.print(")", .{});
    }
};

const If = struct {
    condition: NodeReference,
    true_statement: NodeReference,
    false_statement: ?NodeReference,

    pub fn format(this: If, parser: *EowynParser, w: anytype) !void {
        try w.print("if {s} {{\n", .{parser.get_node(this.condition)});
        try w.print("{s}", .{parser.get_node(this.true_statement)});
        if (this.false_statement) |false_stmt| {
            try w.print("}} else {{\n", .{});
            try w.print("{s}", .{parser.get_node(false_stmt)});
        }
        try w.print("}}", .{});
    }
};

const Label = struct {
    label: []const u8,

    pub fn format(this: Label, _: *EowynParser, w: anytype) !void {
        try w.print("#{s}", .{this.label});
    }
};

const Loop = struct {
    label: ?NodeReference = null,
    statement: NodeReference,

    pub fn format(this: Loop, parser: *EowynParser, w: anytype) !void {
        if (this.label) |label| {
            try w.print("{} ", .{parser.get_node(label)});
        }
        try w.print("loop {{\n", .{});
        try w.print("{s}", .{parser.get_node(this.statement)});
        try w.print("}}", .{});
    }
};

const Parameter = struct {
    name: []const u8,

    pub fn format(this: Parameter, _: *EowynParser, w: anytype) !void {
        try w.print("{s}", .{this.name});
    }
};

const Program = struct {
    declarations: std.StringArrayHashMap(NodeReference),

    pub fn deinit(this: *Program) void {
        this.declarations.deinit();
    }

    pub fn format(this: Program, parser: *EowynParser, w: anytype) !void {
        for (this.declarations.values()) |decl| {
            try w.print("{}\n", .{parser.get_node(decl)});
        }
    }
};

const Subscript = struct {
    subscripts: std.ArrayList(NodeReference),

    pub fn deinit(this: *Subscript) void {
        this.subscripts.deinit();
    }

    pub fn format(this: Subscript, parser: *EowynParser, w: anytype) !void {
        try w.print("[", .{});
        var first = true;
        for (this.subscripts.items) |a| {
            if (!first) {
                try w.print(", ", .{});
            }
            try w.print("{}", .{parser.get_node(a)});
            first = false;
        }
        try w.print("]", .{});
    }
};

const UnaryExpression = struct {
    op: UnaryOperator,
    operand: NodeReference,

    pub fn format(this: UnaryExpression, parser: *EowynParser, w: anytype) !void {
        try w.print("{}{}", .{ this.op, parser.get_node(this.operand) });
    }
};

const ASTNodeKind = enum {
    BinaryExpression,
    Block,
    BoolConstant,
    FloatConstant,
    Function,
    FunctionCall,
    FunctionDecl,
    Identifier,
    If,
    IntConstant,
    Label,
    Loop,
    Parameter,
    Program,
    QString,
    StartBlock,
    Subscript,
    UnaryExpression,
};

const ASTNodeImpl = union(ASTNodeKind) {
    BinaryExpression: BinaryExpression,
    Block: Block,
    BoolConstant: bool,
    FloatConstant: f64,
    Function: Function,
    FunctionCall: FunctionCall,
    FunctionDecl: FunctionDecl,
    Identifier: []const u8,
    If: If,
    IntConstant: u64,
    Label: Label,
    Loop: Loop,
    Parameter: Parameter,
    Program: Program,
    QString: []const u8,
    StartBlock: void,
    Subscript: Subscript,
    UnaryExpression: UnaryExpression,
};

pub const NodeReference = usize;

pub fn Impl(comptime E: type) type {
    if (@typeInfo(E) != .@"enum") {
        @compileError("Expected enum type, found '" ++ @typeName(ExpressionKind) ++ "'");
    }
    if (E == ASTNodeKind) {
        return ASTNodeImpl;
    }
    const e = @typeInfo(E).@"enum";
    var union_flds: [e.fields.len]std.builtin.Type.UnionField = undefined;
    inline for (e.fields, 0..) |fld, ix| {
        union_flds[ix].name = fld.name;
        const f = blk: {
            for (std.meta.fields(ASTNodeImpl)) |ast_fld| {
                if (std.mem.eql(u8, ast_fld.name, fld.name)) {
                    break :blk ast_fld;
                }
            }
            @compileError("Union tag type for '" ++ fld.name ++ "' not found");
        };
        union_flds[ix] = f;
    }
    const decls = [_]std.builtin.Type.Declaration{};
    return @Type(.{
        .@"union" = .{
            .tag_type = E,
            .fields = &union_flds,
            .decls = &decls,
            .layout = .auto,
        },
    });
}

pub fn Node(comptime E: type) type {
    const U: type = Impl(E);
    return struct {
        parser: *EowynParser = undefined,
        ref: NodeReference = 0,
        location: lxr.Location = .{},
        impl: Union,

        const Self = @This();
        const Enum = E;
        const Union = U;

        pub fn init(src: anytype) !Self {
            const S = @TypeOf(src);
            const s_type: std.builtin.Type.Struct = if (@typeInfo(S) == .@"struct") @typeInfo(S).@"struct" else if (@typeInfo(S) == .pointer and @typeInfo(@typeInfo(S).pointer.child) == .@"struct" and @typeInfo(S).pointer.size == .One) @typeInfo(@typeInfo(S).pointer.child).@"struct" else @compileError("Expected struct type, found '" ++ @typeName(S) ++ "'");
            comptime ref_blk: {
                for (s_type.fields) |fld| {
                    if (std.mem.eql(u8, fld.name, "ref") and fld.type == NodeReference) {
                        break :ref_blk;
                    }
                }
                @compileError("Struct '" ++ @typeName(S) ++ "' has no 'ref' field");
            }
            comptime loc_blk: {
                for (s_type.fields) |fld| {
                    if (std.mem.eql(u8, fld.name, "location") and fld.type == lxr.Location) {
                        break :loc_blk;
                    }
                }
                @compileError("Struct '" ++ @typeName(S) ++ "' has no 'location' field");
            }
            comptime impl_blk: {
                for (s_type.fields) |fld| {
                    if (std.mem.eql(u8, fld.name, "impl") and @typeInfo(fld.type) == .@"union") {
                        break :impl_blk;
                    }
                }
                @compileError("Struct '" ++ @typeName(S) ++ "' has no 'impl' field");
            }
            var impl: Union = undefined;
            assign_blk: {
                inline for (std.meta.fields(U)) |my_fld| {
                    if (std.mem.eql(u8, @tagName(src.impl), my_fld.name)) {
                        impl = @unionInit(Union, my_fld.name, @field(src.impl, my_fld.name));
                        break :assign_blk;
                    }
                }
                std.debug.print("source tag value '{s}' not in target enum '{s}'", .{ @tagName(src.impl), @typeName(E) });
            }
            return .{
                .parser = src.parser,
                .ref = src.ref,
                .location = src.location,
                .impl = impl,
            };
        }

        pub fn deinit(this: *Self) void {
            blk: inline for (std.meta.fields(Union)) |fld| {
                if (std.mem.eql(u8, @tagName(this.impl), fld.name)) {
                    switch (@typeInfo(fld.type)) {
                        .@"struct" => if (std.meta.hasFn(fld.type, "deinit")) @field(this.impl, fld.name).deinit(),
                        else => {},
                    }
                    break :blk;
                }
            }
        }

        pub fn format(this: Self, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            blk: inline for (std.meta.fields(Union)) |fld| {
                if (std.mem.eql(u8, @tagName(this.impl), fld.name)) {
                    switch (@typeInfo(fld.type)) {
                        .pointer => |p| {
                            if (p.child == u8 and p.size == .Slice) {
                                try w.print("{s}", .{@field(this.impl, fld.name)});
                            }
                            if (p.size == .One and @typeInfo(p.child) == .@"struct") {
                                try @field(this.impl, fld.name).format(this.parser, w);
                            }
                        },
                        .bool => try w.print("{s}", .{if (@field(this.impl, fld.name)) "true" else "false"}),
                        .@"struct" => try @field(this.impl, fld.name).format(this.parser, w),
                        .void => {},
                        else => try w.print("{}", .{@field(this.impl, fld.name)}),
                    }
                    break :blk;
                }
            }
        }
    };
}

const ASTNode = Node(ASTNodeKind);

const ExpressionKind = enum {
    BinaryExpression,
    BoolConstant,
    FloatConstant,
    FunctionCall,
    Identifier,
    IntConstant,
    QString,
    UnaryExpression,
};

const Expression = Node(ExpressionKind);

const EowynParser = struct {
    allocator: Allocator,
    token_stack: std.ArrayList(lxr.Token),
    node_stack: std.ArrayList(NodeReference),
    node_cache: std.ArrayList(ASTNode),

    pub fn get(parser: *parse.Parser) *EowynParser {
        return @as(*EowynParser, @alignCast(@ptrCast(parser.impl orelse @panic("EowynParser not initialized"))));
    }

    pub fn init(allocator: Allocator, _: Grammar) EowynParser {
        return .{
            .allocator = allocator,
            .token_stack = std.ArrayList(lxr.Token).init(allocator),
            .node_stack = std.ArrayList(NodeReference).init(allocator),
            .node_cache = std.ArrayList(ASTNode).init(allocator),
        };
    }

    pub fn deinit(this: *EowynParser) void {
        this.token_stack.deinit();
        this.node_stack.deinit();
        for (this.node_cache) |*n| {
            n.deinit();
        }
        this.node_cache.deinit();
    }

    pub fn startup(this: *EowynParser) void {
        this.node_stack.clearRetainingCapacity();
        for (this.node_cache.items) |*n| {
            n.deinit();
        }
        this.node_cache.clearRetainingCapacity();
    }

    pub fn cleanup(this: *EowynParser) void {
        if (this.node_stack.items.len > 0) {
            std.debug.print("Stack not empty on cleanup\n", .{});
        }
    }

    fn pop_token(this: *EowynParser) Token {
        return if (this.token_stack.popOrNull()) |token| token else @panic("pop_token(): stack underflow");
    }

    fn push_token(this: *EowynParser, node: Token) void {
        this.token_stack.append(node) catch @panic("Out of memory");
    }

    fn peek_node(this: EowynParser) *ASTNode {
        this.dump_node_stack("peek_node");
        const ref = this.node_stack.getLastOrNull() orelse @panic("Node stack underflow");
        return this.get_node(ref);
    }

    fn pop_node(this: *EowynParser) *ASTNode {
        this.dump_node_stack("pop_node");
        const ref = if (this.node_stack.popOrNull()) |node| node else @panic("pop_node(): stack underflow");
        return this.get_node(ref);
    }

    fn pop_node_one_of(this: *EowynParser, comptime K: type) *ASTNode {
        this.dump_node_stack("pop_typed_node");
        const ref = if (this.node_stack.popOrNull()) |node| node else @panic("pop_node(): stack underflow");
        return this.get_node_one_of(K, ref);
    }

    fn pop_typed_node(this: *EowynParser, comptime kind: ASTNodeKind) *ASTNode {
        this.dump_node_stack("pop_typed_node");
        const ref = if (this.node_stack.popOrNull()) |node| node else @panic("pop_node(): stack underflow");
        return this.get_typed_node(kind, ref);
    }

    fn pop_typed_node_or_null(this: *EowynParser, comptime kind: ASTNodeKind) ?*ASTNode {
        this.dump_node_stack("pop_typed_node_or_null");
        const ref = this.node_stack.getLastOrNull() orelse @panic("pop_typed_node_or_null(): stack underflow");
        const ret = this.get_typed_node_or_null(kind, ref);
        if (ret != null) {
            _ = this.node_stack.pop();
        }
        return ret;
    }

    fn push_node(this: *EowynParser, node: ASTNode) NodeReference {
        const ref = this.cache_node(node);
        this.node_stack.append(ref) catch @panic("Out of memory");
        this.dump_node_stack("push_node");
        return ref;
    }

    fn peek_and_assert(this: *EowynParser, comptime kind: ASTNodeKind) *ASTNode {
        this.dump_node_stack("peek_and_assert");
        const ref = this.node_stack.getLastOrNull() orelse @panic("Node stack underflow");
        const ret = this.get_node(ref);
        if (ret.impl != kind) {
            std.debug.panic("Top of stack has invalid node type '{s}'", .{@tagName(ret.impl)});
        }
        return ret;
    }

    fn peek_kind(this: *EowynParser) ASTNodeKind {
        this.dump_node_stack("peek_kind");
        const l = this.node_stack.items.len;
        if (l == 0) {
            @panic("peek_kind(): stack underflow");
        }
        const ix = this.node_stack.items[l - 1];
        return this.node_cache.items[ix].impl;
    }

    fn cache_node(this: *EowynParser, node: ASTNode) NodeReference {
        var n = node;
        n.ref = this.node_cache.items.len;
        n.parser = this;
        this.node_cache.append(n) catch @panic("Out of memory");
        return n.ref;
    }

    fn get_node(this: EowynParser, ref: NodeReference) *ASTNode {
        return &this.node_cache.items[ref];
    }

    fn get_node_one_of(this: EowynParser, comptime K: type, ref: NodeReference) *ASTNode {
        const ret = &this.node_cache.items[ref];
        inline for (@typeInfo(K).@"enum".fields) |k| {
            if (std.mem.eql(u8, @tagName(ret.impl), k.name)) return ret;
        }
        std.debug.panic("Node {} has invalid node type '{s}'", .{ ref, @tagName(ret.impl) });
    }

    fn get_typed_node(this: EowynParser, comptime kind: ASTNodeKind, ref: NodeReference) *ASTNode {
        const ret = &this.node_cache.items[ref];
        return if (ret.impl == kind) ret else std.debug.panic("Node {} has invalid node type '{s}'", .{ ref, @tagName(ret.impl) });
    }

    fn get_typed_node_or_null(this: EowynParser, comptime kind: ASTNodeKind, ref: NodeReference) ?*ASTNode {
        return if (this.node_cache.items[ref].impl == kind) &this.node_cache.items[ref] else null;
    }

    fn dump_node_stack(this: EowynParser, caption: []const u8) void {
        if (true) {
            return;
        }
        std.debug.print("Stack {s}: ", .{caption});
        var first = true;
        for (this.node_stack.items) |ix| {
            const node = &this.node_cache.items[ix];
            if (!first) {
                std.debug.print(" | ", .{});
            }
            first = false;
            std.debug.print("{s}", .{@tagName(node.impl)});
        }
        std.debug.print("\n", .{});
    }
};

const P = Parser(EowynParser);

export fn push_current_token(this: *P) callconv(.C) void {
    this.impl.push_token(this.last_token);
}

export fn push_bookmark(this: *P) callconv(.C) void {
    this.impl.push_token(Token.bookmark());
}

export fn eowyn_make_identifier(this: *P) callconv(.C) void {
    _ = this.impl.push_node(ASTNode{
        .location = this.last_token.location,
        .impl = .{
            .Identifier = this.last_token.text,
        },
    });
}

export fn eowyn_make_qstring(this: *P) callconv(.C) void {
    if (this.last_token.kind != .String) {
        std.debug.panic("Expected quoted string, got '{s}'", .{@tagName(this.last_token.kind)});
    }
    _ = this.impl.push_node(ASTNode{
        .location = this.last_token.location,
        .impl = .{
            .QString = this.last_token.text,
        },
    });
}

export fn eowyn_make_int(this: *P) callconv(.C) void {
    if (this.last_token.kind != .Number or this.last_token.kind.Number == .Float) {
        std.debug.panic("Expected integer number, got '{s}'", .{@tagName(this.last_token.kind)});
    }
    _ = this.impl.push_node(ASTNode{
        .location = this.last_token.location,
        .impl = .{
            .IntConstant = std.fmt.parseUnsigned(u64, this.last_token.text, 0) catch std.debug.panic("Could not parse integer number '{s}'", .{this.last_token.text}),
        },
    });
}

export fn eowyn_make_float(this: *P) callconv(.C) void {
    if (this.last_token.kind != .Number or this.last_token.kind.Number != .Float) {
        std.debug.panic("Expected floating point number, got '{s}'", .{@tagName(this.last_token.kind)});
    }
    _ = this.impl.push_node(ASTNode{
        .location = this.last_token.location,
        .impl = .{
            .FloatConstant = std.fmt.parseFloat(f64, this.last_token.text) catch std.debug.panic("Could not parse floating point number '{s}'", .{this.last_token.text}),
        },
    });
}

export fn eowyn_make_bool(this: *P, data: ?*grm.Value) callconv(.C) void {
    const val: *grm.Value = if (data) |v| v else std.debug.panic("Expected boolean value as argument", .{});
    const bool_val = switch (val.*) {
        .Bool => |b| b,
        else => std.debug.panic("Expected Bool value, got '{}'", .{val}),
    };
    _ = this.impl.push_node(ASTNode{
        .location = this.last_token.location,
        .impl = .{
            .BoolConstant = bool_val,
        },
    });
}

export fn eowyn_make_binary_expression(this: *P) callconv(.C) void {
    const op_token = this.impl.pop_token();
    const operator = blk: {
        for (BinaryOperatorMap.operators) |o| {
            if (o.token.eql(op_token.kind)) {
                break :blk o.op;
            }
        }
        std.debug.panic("Unknown operator '{s}'", .{op_token.text});
    };
    const right = this.impl.pop_node().ref;
    const left = this.impl.pop_node().ref;
    _ = this.impl.push_node(ASTNode{
        .location = this.last_token.location,
        .impl = .{
            .BinaryExpression = .{
                .left = left,
                .op = operator,
                .right = right,
            },
        },
    });
}

export fn eowyn_make_unary_expression(this: *P) callconv(.C) void {
    const op_token = this.impl.pop_token();
    const operator = blk: {
        for (UnaryOperatorMap.operators) |o| {
            if (o.token.eql(op_token.kind)) {
                break :blk o.op;
            }
        }
        std.debug.panic("Unknown operator '{s}'", .{op_token.text});
    };
    const operand = this.impl.pop_node().ref;
    _ = this.impl.push_node(ASTNode{
        .location = this.last_token.location,
        .impl = .{
            .UnaryExpression = .{
                .op = operator,
                .operand = operand,
            },
        },
    });
}

export fn eowyn_make_parameter(this: *P) callconv(.C) void {
    _ = this.impl.push_node(ASTNode{
        .location = this.last_token.location,
        .impl = .{
            .Parameter = .{
                .name = this.last_token.text,
            },
        },
    });
}

export fn eowyn_make_function_decl(this: *P) callconv(.C) void {
    var params = std.ArrayList(NodeReference).init(this.allocator);
    while (this.impl.pop_typed_node_or_null(.Parameter)) |param| {
        params.append(param.ref) catch @panic("Out of memory");
    }
    std.mem.reverse(usize, params.items);
    const name = this.impl.pop_typed_node(.Identifier);
    _ = this.impl.push_node(.{
        .location = this.last_token.location,
        .impl = .{
            .FunctionDecl = .{
                .name = name.impl.Identifier,
                .parameters = params,
            },
        },
    });
}

export fn eowyn_make_function(this: *P) callconv(.C) void {
    const block = this.impl.pop_typed_node(.Block);
    const decl = this.impl.pop_typed_node(.FunctionDecl);
    const func_ref = this.impl.cache_node(ASTNode{
        .location = decl.location,
        .impl = .{
            .Function = .{
                .declaration = decl.ref,
                .implementation = block.ref,
            },
        },
    });
    var program = this.impl.peek_and_assert(.Program);
    program.impl.Program.declarations.put(decl.impl.FunctionDecl.name, func_ref) catch @panic("Out of memory");
}

export fn eowyn_start_block(this: *P) callconv(.C) void {
    _ = this.impl.push_node(ASTNode{
        .location = this.last_token.location,
        .impl = .StartBlock,
    });
}

export fn eowyn_finish_block(this: *P) callconv(.C) void {
    var stmts = std.ArrayList(NodeReference).init(this.allocator);
    while (this.impl.peek_kind() != .StartBlock) {
        const stmt = this.impl.pop_node();
        stmts.append(stmt.ref) catch std.debug.panic("Out of memory", .{});
    }
    std.mem.reverse(usize, stmts.items);
    const start = this.impl.pop_typed_node(.StartBlock);
    _ = this.impl.push_node(.{
        .location = start.location,
        .impl = .{
            .Block = .{
                .statements = stmts,
            },
        },
    });
}

export fn eowyn_finish_if(this: *P) callconv(.C) void {
    var nodes = std.ArrayList(NodeReference).init(this.allocator);
    while (this.impl.peek_kind() != .StartBlock) {
        const node = this.impl.pop_node();
        nodes.append(node.ref) catch std.debug.panic("Out of memory", .{});
    }
    std.mem.reverse(usize, nodes.items);
    const start = this.impl.pop_typed_node(.StartBlock);
    if (nodes.items.len < 2 or nodes.items.len > 3) {
        std.debug.panic("Invalid number of nodes in if statement: got {}, need 2 or 3", .{nodes.items.len});
    }
    _ = this.impl.push_node(.{
        .location = start.location,
        .impl = .{
            .If = .{
                .condition = nodes.items[0],
                .true_statement = nodes.items[1],
                .false_statement = if (nodes.items.len == 3) nodes.items[2] else null,
            },
        },
    });
}

export fn eowyn_make_label(this: *P) callconv(.C) void {
    _ = this.impl.push_node(ASTNode{
        .location = this.last_token.location,
        .impl = .{
            .Label = .{
                .label = this.last_token.text,
            },
        },
    });
}

export fn eowyn_make_loop(this: *P) callconv(.C) void {
    const statement = this.impl.pop_node();
    const label = this.impl.pop_typed_node_or_null(.Label);

    _ = this.impl.push_node(ASTNode{
        .location = this.last_token.location,
        .impl = .{
            .Loop = .{
                .label = if (label) |lbl| lbl.ref else null,
                .statement = statement.ref,
            },
        },
    });
}

export fn eowyn_make_function_call(this: *P) callconv(.C) void {
    var args = std.ArrayList(NodeReference).init(this.allocator);
    while (this.impl.peek_kind() != .StartBlock) {
        const expr = this.impl.pop_node_one_of(ExpressionKind);
        args.append(expr.ref) catch std.debug.panic("Out of memory", .{});
    }
    std.mem.reverse(usize, args.items);
    _ = this.impl.pop_typed_node(.StartBlock);
    const name = this.impl.pop_typed_node(.Identifier);
    _ = this.impl.push_node(.{
        .location = name.location,
        .impl = .{
            .FunctionCall = .{
                .name = name.impl.Identifier,
                .arguments = args,
            },
        },
    });
}

export fn eowyn_program(this: *P) callconv(.C) void {
    _ = this.impl.push_node(ASTNode{
        .location = .{
            .pos = 0,
            .line = 0,
            .col = 0,
        },
        .impl = .{
            .Program = .{
                .declarations = std.StringArrayHashMap(NodeReference).init(this.allocator),
            },
        },
    });
}

export fn eowyn_pop_program(this: *P) callconv(.C) void {
    const p = this.impl.pop_typed_node(.Program);
    std.debug.print("{}\n", .{p});
}

const expr_test =
    \\func main() {
    \\  println("Hello, World!");
    \\  println(42);
    \\  println(3.14);
    \\  println(true);
    \\  println(34 + 35);
    \\  println(-34 + 35);
    \\}
;

const if_test =
    \\func main() {
    \\  if (foo(x)) {
    \\    println("ok");
    \\  }
    \\}
    \\
;

const if_else_test =
    \\func main() {
    \\  if (foo(32)) {
    \\    println("ok");
    \\  } else {
    \\    println("not ok");
    \\  }
    \\}
    \\
;

const loop_test =
    \\func main() {
    \\  #blk loop {
    \\    println("ok");
    \\  }
    \\}
    \\
;

const tests = &[_][]const u8{
    expr_test,
    if_test,
    if_else_test,
    loop_test,
};

pub fn main() !void {
    var gp = try GrammarParser.init(std.heap.c_allocator, eowyn_grammar);
    var grammar = Grammar.init(std.heap.c_allocator);
    try gp.parse(&grammar);
    // grammar.dump();
    var p = P.init(std.heap.c_allocator, grammar);

    for (tests) |t| {
        try p.parse(t);
    }
}
