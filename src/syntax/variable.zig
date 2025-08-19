const std = @import("std");
const node = @import("../node.zig");

pub const Const = struct {
    declaration: node.pSyntaxNode,
};

pub const Identifier = struct {
    identifier: []const u8,
};

pub const Var = struct {
    declaration: node.pSyntaxNode,
};

pub const VariableDeclaration = struct {
    name: []const u8,
    type_spec: ?node.pSyntaxNode,
    initializer: ?node.pSyntaxNode,
};
