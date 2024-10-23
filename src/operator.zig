//
// Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
//
// SPDX-License-Identifier: MIT
//

const std = @import("std");
const Allocator = std.mem.Allocator;

const lxr = @import("lexer.zig");

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

pub const BinaryOperator = enum {
    Add,
    Assign,
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

    pub fn map(op: BinaryOperator) BinaryOperatorMap {
        for (BinaryOperatorMap.operators) |m| {
            if (m.op == op) {
                return m;
            }
        }
        unreachable;
    }
};

pub const BinaryOperatorMap = struct {
    op: BinaryOperator,
    assignment: bool = true,
    token: lxr.Token.Kind,

    pub fn get(t: lxr.Token.Kind) ?BinaryOperatorMap {
        for (BinaryOperatorMap.operators) |op| {
            if (op.token.eql(t)) {
                return op;
            }
        }
        return null;
    }

    pub const operators = [_]BinaryOperatorMap{
        .{ .op = .Add, .token = .{ .Symbol = '+' } },
        .{ .op = .Assign, .token = .{ .Symbol = '=' }, .assignment = false },
        .{ .op = .BinaryAnd, .token = .{ .Symbol = '&' } },
        .{ .op = .BinaryOr, .token = .{ .Symbol = '|' } },
        .{ .op = .BinaryXor, .token = .{ .Symbol = '^' } },
        .{ .op = .Divide, .token = .{ .Symbol = '/' } },
        .{ .op = .Equal, .token = .{ .Keyword = "==" }, .assignment = false },
        .{ .op = .Greater, .token = .{ .Symbol = '>' }, .assignment = false },
        .{ .op = .GreaterEqual, .token = .{ .Keyword = ">=" }, .assignment = false },
        .{ .op = .LeftShift, .token = .{ .Keyword = "<<" } },
        .{ .op = .Less, .token = .{ .Symbol = '<' }, .assignment = false },
        .{ .op = .LessEqual, .token = .{ .Keyword = "<=" }, .assignment = false },
        .{ .op = .LogicalAnd, .token = .{ .Keyword = "&&" }, .assignment = false },
        .{ .op = .LogicalOr, .token = .{ .Keyword = "||" }, .assignment = false },
        .{ .op = .Modulo, .token = .{ .Symbol = '%' } },
        .{ .op = .Multiply, .token = .{ .Symbol = '*' } },
        .{ .op = .NotEqual, .token = .{ .Keyword = "==" }, .assignment = false },
        .{ .op = .RightShift, .token = .{ .Keyword = ">>" } },
        .{ .op = .Subtract, .token = .{ .Symbol = '-' } },
    };
};

pub const UnaryOperator = enum {
    AddressOf,
    Deref,
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

pub const UnaryOperatorMap = struct {
    op: UnaryOperator,
    token: lxr.Token.Kind,

    pub const operators = [_]UnaryOperatorMap{
        .{ .op = .AddressOf, .token = .{ .Symbol = '&' } },
        .{ .op = .Deref, .token = .{ .Symbol = '*' } },
        .{ .op = .Idempotent, .token = .{ .Symbol = '+' } },
        .{ .op = .Invert, .token = .{ .Symbol = '~' } },
        .{ .op = .LogicalNegate, .token = .{ .Symbol = '!' } },
        .{ .op = .Negate, .token = .{ .Symbol = '-' } },
    };
};
