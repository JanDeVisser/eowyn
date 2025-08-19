const std = @import("std");
const node = @import("../node.zig");

pub const Import = struct {
    file_name: []const u8,
};

pub const Public = struct {
    name: []const u8,
    decl: node.pSyntaxNode,
};
