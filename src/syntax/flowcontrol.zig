const std = @import("std");
const node = @import("../node.zig");

pub const Break = struct {
    label: ?[]const u8 = null,
};

pub const Continue = struct {
    label: ?[]const u8 = null,
};

pub const Defer = struct {
    statement: node.pSyntaxNode,
};

pub const Error = struct {
    expression: node.pSyntaxNode,
};

pub const ForStatement = struct {
    range_variable: []const u8,
    range_expr: node.pSyntaxNode,
    statement: node.pSyntaxNode,
};

pub const IfStatement = struct {
    condition: node.pSyntaxNode,
    if_branch: node.pSyntaxNode,
    else_branch: ?node.pSyntaxNode,
};

pub const LoopStatement = struct {
    label: ?[]const u8,
    statement: node.pSyntaxNode,
};

pub const Return = struct {
    expression: node.pSyntaxNode,
};
pub const Yield = struct {
    label: ?[]const u8 = null,
    expression: node.pSyntaxNode,
};
