const std = @import("std");
const node = @import("../node.zig");

pub const Block = struct {
    allocator: std.mem.Allocator,
    statements: node.SyntaxNodes,
};

pub const Void = struct {};
