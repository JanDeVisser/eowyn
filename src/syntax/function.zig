const std = @import("std");
const node = @import("../node.zig");

pub const ExternLink = struct {
    link_name: []const u8,
};

pub const FunctionDefinition = struct {
    name: []const u8,
    signature: node.pSyntaxNode,
    implementation: node.pSyntaxNode,
};

pub const FunctionSignature = struct {
    name: []const u8,
    parameters: node.SyntaxNodes,
    return_type: node.pSyntaxNode,
};

pub const Parameter = struct {
    name: []const u8,
    type_spec: node.pSyntaxNode,
};
