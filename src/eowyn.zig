//
// Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
//
// SPDX-License-Identifier: MIT
//

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const grm = @import("grammar.zig");
const lxr = @import("lexer.zig");
const node = @import("node.zig");
const op = @import("operator.zig");
const parse = @import("parser.zig");

const Grammar = grm.Grammar;
const NodeReference = node.NodeReference;
const Token = lxr.Token;

pub const eowyn_grammar: []const u8 = @embedFile("eowyn.grammar");

pub const ASTNodeKind = enum {
    ArrayType,
    AssignmentExpression,
    BasicType,
    BinaryExpression,
    Block,
    BoolConstant,
    ConstDecl,
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
    PointerType,
    Program,
    QString,
    Return,
    StartBlock,
    Subscript,
    UnaryExpression,
    VarDecl,
};

pub const ASTNodeImpl = union(ASTNodeKind) {
    ArrayType: ast.ArrayType,
    AssignmentExpression: ast.AssignmentExpression,
    BasicType: []const u8,
    BinaryExpression: ast.BinaryExpression,
    Block: ast.Block,
    BoolConstant: bool,
    ConstDecl: ast.ConstDecl,
    FloatConstant: f64,
    Function: ast.Function,
    FunctionCall: ast.FunctionCall,
    FunctionDecl: ast.FunctionDecl,
    Identifier: []const u8,
    If: ast.If,
    IntConstant: u64,
    Label: ast.Label,
    Loop: ast.Loop,
    Parameter: ast.Parameter,
    PointerType: ast.PointerType,
    Program: ast.Program,
    QString: []const u8,
    Return: ast.Return,
    StartBlock: void,
    Subscript: ast.Subscript,
    UnaryExpression: ast.UnaryExpression,
    VarDecl: ast.VarDecl,
};

pub const ASTBase = struct {
    parser: *EowynParser = undefined,
    ref: NodeReference = 0,
    location: lxr.Location = .{},
};

pub const ASTNode = node.Node(ASTBase, ASTNodeImpl, ASTNodeKind);

pub const ExpressionKind = enum {
    AssignmentExpression,
    BinaryExpression,
    BoolConstant,
    FloatConstant,
    FunctionCall,
    Identifier,
    IntConstant,
    QString,
    UnaryExpression,
};

pub const Expression = ast.Node(ASTBase, ASTNodeImpl, ExpressionKind);

pub const TypeKind = enum {
    ArrayType,
    BasicType,
    PointerType,
};

pub const TypeSpec = ast.Node(ASTBase, ASTNodeImpl, ExpressionKind);

pub const EowynParser = struct {
    allocator: Allocator,
    log: bool = false,
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

    pub fn pop_token(this: *EowynParser) Token {
        return if (this.token_stack.popOrNull()) |token| token else @panic("pop_token(): stack underflow");
    }

    pub fn push_token(this: *EowynParser, t: Token) void {
        this.token_stack.append(t) catch @panic("Out of memory");
    }

    pub fn peek_node(this: EowynParser) *ASTNode {
        this.dump_node_stack("peek_node");
        const ref = this.node_stack.getLastOrNull() orelse @panic("Node stack underflow");
        return this.get_node(ref);
    }

    pub fn pop_node(this: *EowynParser) *ASTNode {
        this.dump_node_stack("pop_node");
        const ref = if (this.node_stack.popOrNull()) |n| n else @panic("pop_node(): stack underflow");
        return this.get_node(ref);
    }

    pub fn pop_node_one_of(this: *EowynParser, comptime K: type) *ASTNode {
        this.dump_node_stack("pop_typed_node");
        const ref = if (this.node_stack.popOrNull()) |n| n else @panic("pop_node(): stack underflow");
        return this.get_node_one_of(K, ref);
    }

    pub fn pop_typed_node(this: *EowynParser, comptime kind: ASTNodeKind) *ASTNode {
        this.dump_node_stack("pop_typed_node");
        const ref = if (this.node_stack.popOrNull()) |n| n else @panic("pop_node(): stack underflow");
        return this.get_typed_node(kind, ref);
    }

    pub fn pop_typed_node_or_null(this: *EowynParser, comptime kind: ASTNodeKind) ?*ASTNode {
        this.dump_node_stack("pop_typed_node_or_null");
        const ref = this.node_stack.getLastOrNull() orelse @panic("pop_typed_node_or_null(): stack underflow");
        const ret = this.get_typed_node_or_null(kind, ref);
        if (ret != null) {
            _ = this.node_stack.pop();
        }
        return ret;
    }

    pub fn push_node(this: *EowynParser, n: ASTNode) NodeReference {
        const ref = this.cache_node(n);
        this.node_stack.append(ref) catch @panic("Out of memory");
        this.dump_node_stack("push_node");
        return ref;
    }

    pub fn peek_and_assert(this: *EowynParser, comptime kind: ASTNodeKind) *ASTNode {
        this.dump_node_stack("peek_and_assert");
        const ref = this.node_stack.getLastOrNull() orelse @panic("Node stack underflow");
        const ret = this.get_node(ref);
        if (ret.impl != kind) {
            std.debug.panic("Top of stack has invalid node type '{s}'", .{@tagName(ret.impl)});
        }
        return ret;
    }

    pub fn peek_kind(this: *EowynParser) ASTNodeKind {
        this.dump_node_stack("peek_kind");
        const l = this.node_stack.items.len;
        if (l == 0) {
            @panic("peek_kind(): stack underflow");
        }
        const ix = this.node_stack.items[l - 1];
        return this.node_cache.items[ix].impl;
    }

    pub fn cache_node(this: *EowynParser, n: ASTNode) NodeReference {
        var n_ = n;
        n_.base.ref = this.node_cache.items.len;
        n_.base.parser = this;
        this.node_cache.append(n_) catch @panic("Out of memory");
        return n_.base.ref;
    }

    pub fn get_node(this: EowynParser, ref: NodeReference) *ASTNode {
        return &this.node_cache.items[ref];
    }

    pub fn get_node_one_of(this: EowynParser, comptime K: type, ref: NodeReference) *ASTNode {
        const ret = &this.node_cache.items[ref];
        inline for (@typeInfo(K).@"enum".fields) |k| {
            if (std.mem.eql(u8, @tagName(ret.impl), k.name)) return ret;
        }
        std.debug.panic("Node {} has invalid node type '{s}'", .{ ref, @tagName(ret.impl) });
    }

    pub fn get_typed_node(this: EowynParser, comptime kind: ASTNodeKind, ref: NodeReference) *ASTNode {
        const ret = &this.node_cache.items[ref];
        return if (ret.impl == kind) ret else std.debug.panic("Node {} has invalid node type '{s}'", .{ ref, @tagName(ret.impl) });
    }

    pub fn get_typed_node_or_null(this: EowynParser, comptime kind: ASTNodeKind, ref: NodeReference) ?*ASTNode {
        return if (this.node_cache.items[ref].impl == kind) &this.node_cache.items[ref] else null;
    }

    pub fn dump_node_stack(this: EowynParser, caption: []const u8) void {
        if (!this.log) {
            return;
        }
        std.debug.print("Stack {s}: ", .{caption});
        var first = true;
        for (this.node_stack.items) |ix| {
            const n = &this.node_cache.items[ix];
            if (!first) {
                std.debug.print(" | ", .{});
            }
            first = false;
            std.debug.print("{s}", .{@tagName(n.impl)});
        }
        std.debug.print("\n", .{});
    }
};

pub const P = parse.Parser(EowynParser);

export fn push_current_token(this: *P) void {
    this.impl.push_token(this.last_token);
}

export fn push_bookmark(this: *P) void {
    this.impl.push_token(Token.bookmark());
}

export fn eowyn_make_identifier(this: *P) void {
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = this.last_token.location },
        .impl = .{
            .Identifier = this.last_token.text,
        },
    });
}

export fn eowyn_make_qstring(this: *P) void {
    if (this.last_token.kind != .String) {
        std.debug.panic("Expected quoted string, got '{s}'", .{@tagName(this.last_token.kind)});
    }
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = this.last_token.location },
        .impl = .{
            .QString = this.last_token.text,
        },
    });
}

export fn eowyn_make_int(this: *P) void {
    if (this.last_token.kind != .Number or this.last_token.kind.Number == .Float) {
        std.debug.panic("Expected integer number, got '{s}'", .{@tagName(this.last_token.kind)});
    }
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = this.last_token.location },
        .impl = .{
            .IntConstant = std.fmt.parseUnsigned(u64, this.last_token.text, 0) catch std.debug.panic("Could not parse integer number '{s}'", .{this.last_token.text}),
        },
    });
}

export fn eowyn_make_float(this: *P) void {
    if (this.last_token.kind != .Number or this.last_token.kind.Number != .Float) {
        std.debug.panic("Expected floating point number, got '{s}'", .{@tagName(this.last_token.kind)});
    }
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = this.last_token.location },
        .impl = .{
            .FloatConstant = std.fmt.parseFloat(f64, this.last_token.text) catch std.debug.panic("Could not parse floating point number '{s}'", .{this.last_token.text}),
        },
    });
}

export fn eowyn_make_bool(this: *P, data: ?*grm.Value) void {
    const val: *grm.Value = if (data) |v| v else std.debug.panic("Expected boolean value as argument", .{});
    const bool_val = switch (val.*) {
        .Bool => |b| b,
        else => std.debug.panic("Expected Bool value, got '{}'", .{val}),
    };
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = this.last_token.location },
        .impl = .{
            .BoolConstant = bool_val,
        },
    });
}

export fn eowyn_make_binary_expression(this: *P) void {
    const op_token = this.impl.pop_token();
    const operator = blk: {
        for (op.BinaryOperatorMap.operators) |o| {
            if (o.token.eql(op_token.kind)) {
                break :blk o.op;
            }
        }
        std.debug.panic("Unknown operator '{s}'", .{op_token.text});
    };
    const right = this.impl.pop_node().base.ref;
    const left = this.impl.pop_node();
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = left.base.location },
        .impl = .{
            .BinaryExpression = .{
                .left = left.base.ref,
                .op = operator,
                .right = right,
            },
        },
    });
}

export fn eowyn_make_assignment_expression(this: *P) void {
    const op_token = this.impl.pop_token();
    const operator = op: {
        switch (op_token.kind) {
            .Symbol => |s| {
                if (s == '=') {
                    break :op op.BinaryOperator.Assign;
                }
                std.debug.panic("Operator '{s}' is not an assignment operator", .{op_token.text});
            },
            .Keyword => |kw| {
                const map = map: {
                    if (std.mem.endsWith(u8, kw, "=")) {
                        if (kw.len == 2) {
                            break :map op.BinaryOperatorMap.get(.{ .Symbol = kw[0] }) orelse std.debug.panic("Operator '{s}' is not an assignment operator", .{op_token.text});
                        }
                        break :map op.BinaryOperatorMap.get(.{ .Keyword = kw[0 .. kw.len - 1] }) orelse std.debug.panic("Operator '{s}' is not an assignment operator", .{op_token.text});
                    }
                    std.debug.panic("Operator '{s}' is not an assignment operator", .{op_token.text});
                };
                if (!map.assignment) {
                    std.debug.panic("Operator '{s}' is not an assignment operator", .{op_token.text});
                }
                break :op map.op;
            },
            else => std.debug.panic("Unknown operator '{s}'", .{op_token.text}),
        }
    };
    const right = this.impl.pop_node().base.ref;
    const left = this.impl.pop_node();
    switch (left.impl) {
        .Identifier => {},
        .UnaryExpression => |u| {
            if (u.op != .Deref) {
                std.debug.panic("Expected lvalue, got '{}'", .{left});
            }
        },
        else => std.debug.panic("Expected lvalue, got '{}'", .{left}),
    }
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = left.base.location },
        .impl = .{
            .AssignmentExpression = .{
                .left = left.base.ref,
                .op = operator,
                .right = right,
            },
        },
    });
}

export fn eowyn_make_const_decl(this: *P) void {
    const expr = this.impl.pop_node_one_of(ExpressionKind).base.ref;
    const ident = this.impl.pop_typed_node(.Identifier);
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = ident.base.location },
        .impl = .{
            .ConstDecl = .{
                .type = null,
                .name = ident.impl.Identifier,
                .value = expr,
            },
        },
    });
}

export fn eowyn_make_var_decl(this: *P) void {
    var nodes = std.ArrayList(NodeReference).init(this.allocator);
    while (this.impl.peek_kind() != .StartBlock) {
        const n = this.impl.pop_node();
        nodes.append(n.base.ref) catch std.debug.panic("Out of memory", .{});
    }
    std.mem.reverse(usize, nodes.items);
    _ = this.impl.pop_typed_node(.StartBlock);

    const ident = this.impl.get_node(nodes.items[0]);
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = ident.base.location },
        .impl = .{
            .VarDecl = .{
                .type = null,
                .name = ident.impl.Identifier,
                .value = if (nodes.items.len == 2) nodes.items[1] else null,
            },
        },
    });
}

export fn eowyn_make_unary_expression(this: *P) void {
    const op_token = this.impl.pop_token();
    const operator = blk: {
        for (op.UnaryOperatorMap.operators) |o| {
            if (o.token.eql(op_token.kind)) {
                break :blk o.op;
            }
        }
        std.debug.panic("Unknown operator '{s}'", .{op_token.text});
    };
    const operand = this.impl.pop_node().base.ref;
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = op_token.location },
        .impl = .{
            .UnaryExpression = .{
                .op = operator,
                .operand = operand,
            },
        },
    });
}

export fn eowyn_make_type(this: *P) void {
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = this.last_token.location },
        .impl = .{
            .BasicType = this.last_token.text,
        },
    });
}

export fn eowyn_make_array_type(this: *P) void {
    const element_type = this.impl.pop_node_one_of(TypeKind).base.ref;
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = this.last_token.location },
        .impl = .{
            .ArrayType = .{
                .element_type = element_type,
            },
        },
    });
}

export fn eowyn_make_pointer_type(this: *P) void {
    const element_type = this.impl.pop_node_one_of(TypeKind).base.ref;
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = this.last_token.location },
        .impl = .{
            .PointerType = .{
                .element_type = element_type,
            },
        },
    });
}

export fn eowyn_make_parameter(this: *P) void {
    const param_type = this.impl.pop_node_one_of(TypeKind).base.ref;
    const ident = this.impl.pop_typed_node(.Identifier);
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = this.last_token.location },
        .impl = .{
            .Parameter = .{
                .name = ident.impl.Identifier,
                .type = param_type,
            },
        },
    });
}

fn make_function_decl_(this: *P, return_type: ?NodeReference) void {
    var params = std.ArrayList(NodeReference).init(this.allocator);
    while (this.impl.pop_typed_node_or_null(.Parameter)) |param| {
        params.append(param.base.ref) catch @panic("Out of memory");
    }
    std.mem.reverse(usize, params.items);
    const name = this.impl.pop_typed_node(.Identifier);
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = name.base.location },
        .impl = .{
            .FunctionDecl = .{
                .name = name.impl.Identifier,
                .parameters = params,
                .return_type = return_type,
            },
        },
    });
}

export fn eowyn_make_function_decl(this: *P) void {
    make_function_decl_(this, this.impl.pop_node_one_of(TypeKind).base.ref);
}

export fn eowyn_make_void_function_decl(this: *P) void {
    make_function_decl_(this, null);
}

export fn eowyn_make_function(this: *P) void {
    const block = this.impl.pop_typed_node(.Block);
    const decl = this.impl.pop_typed_node(.FunctionDecl);
    const func_ref = this.impl.cache_node(ASTNode{
        .base = .{ .location = decl.base.location },
        .impl = .{
            .Function = .{
                .declaration = decl.base.ref,
                .implementation = block.base.ref,
            },
        },
    });
    var program = this.impl.peek_and_assert(.Program);
    program.impl.Program.declarations.put(decl.impl.FunctionDecl.name, func_ref) catch @panic("Out of memory");
}

export fn eowyn_start_block(this: *P) void {
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = this.last_token.location },
        .impl = .StartBlock,
    });
}

export fn eowyn_finish_block(this: *P) void {
    var stmts = std.ArrayList(NodeReference).init(this.allocator);
    while (this.impl.peek_kind() != .StartBlock) {
        const stmt = this.impl.pop_node();
        stmts.append(stmt.base.ref) catch std.debug.panic("Out of memory", .{});
    }
    std.mem.reverse(usize, stmts.items);
    const start = this.impl.pop_typed_node(.StartBlock);
    _ = this.impl.push_node(.{
        .base = .{ .location = start.base.location },
        .impl = .{
            .Block = .{
                .statements = stmts,
            },
        },
    });
}

export fn eowyn_finish_if(this: *P) void {
    var nodes = std.ArrayList(NodeReference).init(this.allocator);
    while (this.impl.peek_kind() != .StartBlock) {
        const n = this.impl.pop_node();
        nodes.append(n.base.ref) catch std.debug.panic("Out of memory", .{});
    }
    std.mem.reverse(usize, nodes.items);
    const start = this.impl.pop_typed_node(.StartBlock);
    if (nodes.items.len < 2 or nodes.items.len > 3) {
        std.debug.panic("Invalid number of nodes in if statement: got {}, need 2 or 3", .{nodes.items.len});
    }
    _ = this.impl.push_node(.{
        .base = .{ .location = start.base.location },
        .impl = .{
            .If = .{
                .condition = nodes.items[0],
                .true_statement = nodes.items[1],
                .false_statement = if (nodes.items.len == 3) nodes.items[2] else null,
            },
        },
    });
}

export fn eowyn_make_label(this: *P) void {
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = this.last_token.location },
        .impl = .{
            .Label = .{
                .label = this.last_token.text,
            },
        },
    });
}

export fn eowyn_make_loop(this: *P) void {
    const statement = this.impl.pop_node();
    const label = this.impl.pop_typed_node_or_null(.Label);

    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = this.last_token.location },
        .impl = .{
            .Loop = .{
                .label = if (label) |lbl| lbl.base.ref else null,
                .statement = statement.base.ref,
            },
        },
    });
}

export fn eowyn_make_return_with_value(this: *P) void {
    const value = this.impl.pop_node_one_of(ExpressionKind);
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = this.last_token.location },
        .impl = .{
            .Return = .{
                .expr = value.base.ref,
            },
        },
    });
}

export fn eowyn_make_void_return(this: *P) void {
    _ = this.impl.push_node(ASTNode{
        .base = .{ .location = this.last_token.location },
        .impl = .{
            .Return = .{
                .expr = null,
            },
        },
    });
}

export fn eowyn_make_function_call(this: *P) void {
    var args = std.ArrayList(NodeReference).init(this.allocator);
    while (this.impl.peek_kind() != .StartBlock) {
        const expr = this.impl.pop_node_one_of(ExpressionKind);
        args.append(expr.base.ref) catch std.debug.panic("Out of memory", .{});
    }
    std.mem.reverse(usize, args.items);
    _ = this.impl.pop_typed_node(.StartBlock);
    const name = this.impl.pop_typed_node(.Identifier);
    _ = this.impl.push_node(.{
        .base = .{ .location = name.base.location },
        .impl = .{
            .FunctionCall = .{
                .name = name.impl.Identifier,
                .arguments = args,
            },
        },
    });
}

export fn eowyn_program(this: *P) void {
    _ = this.impl.push_node(ASTNode{
        .base = .{},
        .impl = .{
            .Program = .{
                .declarations = std.StringArrayHashMap(NodeReference).init(this.allocator),
            },
        },
    });
}

export fn eowyn_pop_program(this: *P) void {
    const p = this.impl.pop_typed_node(.Program);
    std.debug.print("{}\n", .{p});
}
