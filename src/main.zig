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

const Block = struct {
    statements: std.ArrayList(ASTNode),

    pub fn deinit(this: *Block) void {
        for (this.statements.items) |*stmt| {
            stmt.deinit();
        }
        this.statements.deinit();
    }

    pub fn format(this: Block, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        for (this.statements.items) |stmt| {
            try w.print("{}\n", .{stmt});
        }
    }
};

const Function = struct {
    declaration: FunctionDecl,
    implementation: Block,

    pub fn deinit(this: *Function) void {
        this.declaration.deinit();
        this.implementation.deinit();
    }

    pub fn format(this: Function, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        try w.print("{} {{\n{}}}\n", .{ this.declaration, this.implementation });
    }
};

const FunctionCall = struct {
    name: []const u8,
    arguments: std.ArrayList(Expression),

    pub fn deinit(this: *FunctionDecl) void {
        for (this.arguments.items) |*a| {
            a.deinit();
        }
        this.arguments.deinit();
    }

    pub fn format(this: FunctionCall, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        try w.print("{s}(", .{this.name});
        var first = true;
        for (this.arguments.items) |a| {
            if (!first) {
                try w.print(", ", .{});
            }
            try w.print("{}", .{a});
            first = false;
        }
        try w.print(")", .{});
    }
};

const FunctionDecl = struct {
    name: []const u8,
    parameters: std.ArrayList(Parameter),

    pub fn deinit(this: *FunctionDecl) void {
        for (this.parameters.items) |*p| {
            p.deinit();
        }
        this.parameters.deinit();
    }

    pub fn format(this: FunctionDecl, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        try w.print("func {s}(", .{this.name});
        var first = true;
        for (this.parameters.items) |p| {
            if (!first) {
                try w.print(", ", .{});
            }
            try w.print("{}", .{p});
            first = false;
        }
        try w.print(")", .{});
    }
};

const Parameter = struct {
    name: []const u8,

    pub fn deinit(_: *Parameter) void {}

    pub fn format(this: Parameter, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        try w.print("{s}", .{this.name});
    }
};

const Program = struct {
    declarations: std.StringArrayHashMap(ASTNode),

    pub fn deinit(this: *Program) void {
        for (this.declarations.items) |*decl| {
            decl.deinit();
        }
        this.declarations.deinit();
    }

    pub fn format(this: Program, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        for (this.declarations.values()) |decl| {
            try w.print("{}\n", .{decl});
        }
    }
};

const ExpressionKind = enum {
    FloatConstant,
    FunctionCall,
    IntConstant,
    QString,
};

const ExpressionImpl = union(ExpressionKind) {
    FloatConstant: f64,
    FunctionCall: FunctionCall,
    IntConstant: u64,
    QString: []const u8,

    pub fn deinit(this: *ExpressionImpl) void {
        switch (this) {
            .FunctionCall => |*call| call.deinit(),
            else => {},
        }
    }

    pub fn format(this: ExpressionImpl, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        switch (this) {
            .FloatConstant => |f| try w.print("{}", .{f}),
            .FunctionCall => |call| try w.print("{}", .{call}),
            .IntConstant => |i| try w.print("{}", .{i}),
            .QString => |qstring| try w.print("{s}", .{qstring}),
        }
    }
};

const Expression = struct {
    location: lxr.Location,
    impl: ExpressionImpl,

    pub fn deinit(this: *Expression) void {
        this.impl.deinit();
    }

    pub fn format(this: Expression, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        try w.print("{}", .{this.impl});
    }
};

const ASTNodeKind = enum {
    Block,
    FloatConstant,
    Function,
    FunctionCall,
    FunctionDecl,
    Identifier,
    IntConstant,
    Parameter,
    Program,
    QString,
};

const ASTNodeImpl = union(ASTNodeKind) {
    Block: Block,
    FloatConstant: f64,
    Function: Function,
    FunctionCall: FunctionCall,
    FunctionDecl: FunctionDecl,
    Identifier: []const u8,
    IntConstant: u64,
    Parameter: Parameter,
    Program: Program,
    QString: []const u8,

    pub fn deinit(this: *ASTNodeImpl) void {
        switch (this) {
            .Block => |*block| block.deinit(),
            .Function => |*f| f.deinit(),
            .FunctionCall => |*call| call.deinit(),
            .FunctionDecl => |*decl| decl.deinit(),
            .Parameter => |*param| param.deinit(),
            .Program => |*program| program.deinit(),
            else => {},
        }
    }

    pub fn format(this: ASTNodeImpl, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        switch (this) {
            .Block => |block| try w.print("{}", .{block}),
            .FloatConstant => |f| try w.print("{}", .{f}),
            .Function => |f| try w.print("{}", .{f}),
            .FunctionCall => |call| try w.print("{}", .{call}),
            .FunctionDecl => |decl| try w.print("{}", .{decl}),
            .Identifier, .QString => |s| try w.print("{s}", .{s}),
            .IntConstant => |i| try w.print("{}", .{i}),
            .Parameter => |param| try w.print("{}", .{param}),
            .Program => |program| try w.print("{}", .{program}),
        }
    }
};

const ASTNode = struct {
    location: lxr.Location,
    impl: ASTNodeImpl,

    pub fn deinit(this: *ASTNode) void {
        this.impl.deinit();
    }

    pub fn format(this: ASTNode, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        try w.print("{}", .{this.impl});
    }
};

const EowynParser = struct {
    allocator: Allocator,
    token_stack: std.ArrayList(lxr.Token),
    node_stack: std.ArrayList(ASTNode),

    pub fn get(parser: *parse.Parser) *EowynParser {
        return @as(*EowynParser, @alignCast(@ptrCast(parser.impl orelse @panic("EowynParser not initialized"))));
    }

    pub fn init(allocator: Allocator, _: Grammar) EowynParser {
        return .{
            .allocator = allocator,
            .token_stack = std.ArrayList(lxr.Token).init(allocator),
            .node_stack = std.ArrayList(ASTNode).init(allocator),
        };
    }

    pub fn deinit(this: *EowynParser) void {
        this.token_stack.deinit();
        this.node_stack.deinit();
    }

    pub fn startup(this: *EowynParser) void {
        this.node_stack.clearRetainingCapacity();
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

    fn peek_node(this: EowynParser) ASTNode {
        this.dump_node_stack("peek_node");
        return this.node_stack.getLastOrNull() orelse @panic("Node stack underflow");
    }

    fn pop_node(this: *EowynParser) ASTNode {
        this.dump_node_stack("pop_node");
        return if (this.node_stack.popOrNull()) |node| node else @panic("pop_node(): stack underflow");
    }

    fn pop_node_one_of(this: *EowynParser, comptime kinds: []ASTNodeKind) ASTNode {
        this.dump_node_stack("pop_typed_node");
        const ret = if (this.node_stack.popOrNull()) |node| node else @panic("pop_node(): stack underflow");
        for (kinds) |k| {
            if (ret.impl == k) return ret;
        }
        std.debug.panic("Top of stack has invalid node type '{s}'", .{@tagName(ret.impl)});
    }

    fn pop_typed_node(this: *EowynParser, comptime kind: ASTNodeKind) ASTNode {
        this.dump_node_stack("pop_typed_node");
        const ret = if (this.node_stack.popOrNull()) |node| node else @panic("pop_typed_node(): stack underflow");
        return if (ret.impl == kind) ret else std.debug.panic("Top of stack has invalid node type '{s}'", .{@tagName(ret.impl)});
    }

    fn pop_typed_node_or_null(this: *EowynParser, comptime kind: ASTNodeKind) ?ASTNode {
        this.dump_node_stack("pop_typed_node_or_null");
        const l = this.node_stack.items.len;
        if (l == 0) {
            @panic("pop_typed_node_or_null(): stack underflow");
        }
        return if (this.node_stack.items[l - 1].impl == kind) this.node_stack.pop() else null;
    }

    fn push_node(this: *EowynParser, node: ASTNode) void {
        this.node_stack.append(node) catch @panic("Out of memory");
        this.dump_node_stack("push_node");
    }

    fn peek_and_assert(this: *EowynParser, comptime kind: ASTNodeKind) *ASTNode {
        this.dump_node_stack("peek_and_assert");
        const l = this.node_stack.items.len;
        if (l == 0) {
            @panic("peek_and_assert(): stack underflow");
        }
        if (this.node_stack.items[l - 1].impl != kind) {
            std.debug.panic("Top of stack has invalid node type '{s}'", .{@tagName(this.node_stack.items[l - 1].impl)});
        }
        return &this.node_stack.items[l - 1];
    }

    fn peek_kind(this: *EowynParser) ASTNodeKind {
        this.dump_node_stack("peek_kind");
        const l = this.node_stack.items.len;
        if (l == 0) {
            @panic("peek_kind(): stack underflow");
        }
        return this.node_stack.items[l - 1].impl;
    }

    fn dump_node_stack(this: EowynParser, caption: []const u8) void {
        std.debug.print("Stack {s}: ", .{caption});
        var first = true;
        for (this.node_stack.items) |node| {
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

export fn push_identifier(this: *P) callconv(.C) void {
    this.impl.push_node(ASTNode{
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
    this.impl.push_node(ASTNode{
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
    this.impl.push_node(ASTNode{
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
    this.impl.push_node(ASTNode{
        .location = this.last_token.location,
        .impl = .{
            .FloatConstant = std.fmt.parseFloat(f64, this.last_token.text) catch std.debug.panic("Could not parse floating point number '{s}'", .{this.last_token.text}),
        },
    });
}

export fn eowyn_make_parameter(this: *P) callconv(.C) void {
    this.impl.push_node(ASTNode{
        .location = this.last_token.location,
        .impl = .{
            .Parameter = .{
                .name = this.last_token.text,
            },
        },
    });
}

export fn eowyn_make_function_decl(this: *P) callconv(.C) void {
    var params = std.ArrayList(Parameter).init(this.allocator);
    while (this.impl.pop_typed_node_or_null(.Parameter)) |param| {
        params.append(param.impl.Parameter) catch @panic("Out of memory");
    }
    std.mem.reverse(Parameter, params.items);
    const name = this.impl.pop_typed_node(.Identifier);
    this.impl.push_node(.{
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
    const func = ASTNode{
        .location = decl.location,
        .impl = .{
            .Function = .{
                .declaration = decl.impl.FunctionDecl,
                .implementation = block.impl.Block,
            },
        },
    };
    var program = this.impl.pop_typed_node(.Program);
    program.impl.Program.declarations.put(decl.impl.FunctionDecl.name, func) catch @panic("Out of memory");
    this.impl.push_node(program);
}

export fn eowyn_make_block(this: *P) callconv(.C) void {
    this.impl.push_node(ASTNode{ .location = this.last_token.location, .impl = .{ .Block = .{
        .statements = std.ArrayList(ASTNode).init(this.allocator),
    } } });
}

export fn eowyn_add_statement(this: *P) callconv(.C) void {
    const stmt = this.impl.pop_node();
    this.impl.peek_and_assert(.Block).impl.Block.statements.append(stmt) catch @panic("Out of memory");
}

export fn eowyn_make_function_call(this: *P) callconv(.C) void {
    var args = std.ArrayList(Expression).init(this.allocator);
    while (this.impl.peek_kind() != .Identifier) {
        const expr = this.impl.pop_node();
        args.append(.{
            .location = expr.location,
            .impl = switch (expr.impl) {
                .FloatConstant => |f| .{ .FloatConstant = f },
                .FunctionCall => |call| .{ .FunctionCall = call },
                .IntConstant => |i| .{ .IntConstant = i },
                .QString => |s| .{ .QString = s },
                else => std.debug.panic("Invalid expression type '{s}'", .{@tagName(expr.impl)}),
            },
        }) catch @panic("Out of memory");
    }
    std.mem.reverse(Expression, args.items);
    const name = this.impl.pop_typed_node(.Identifier);
    this.impl.push_node(.{
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
    this.impl.push_node(ASTNode{
        .location = .{
            .pos = 0,
            .line = 0,
            .col = 0,
        },
        .impl = .{
            .Program = .{
                .declarations = std.StringArrayHashMap(ASTNode).init(this.allocator),
            },
        },
    });
}

export fn eowyn_pop_program(this: *P) callconv(.C) void {
    const p = this.impl.pop_typed_node(.Program);
    std.debug.print("{}\n", .{p});
}

pub fn main() !void {
    var gp = try GrammarParser.init(std.heap.c_allocator, eowyn_grammar);
    var grammar = Grammar.init(std.heap.c_allocator);
    try gp.parse(&grammar);
    var p = P.init(std.heap.c_allocator, grammar);
    try p.parse(
        \\func main() {
        \\  println("Hello, World!");
        \\  println(42);
        \\  println(3.14);
        \\}
        \\
    );
}
