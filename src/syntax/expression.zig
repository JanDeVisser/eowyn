const std = @import("std");
const node = @import("../node.zig");
const operator = @import("../operator.zig");

const Operator = operator.Operator;
const pSyntaxNode = node.pSyntaxNode;

pub const BinaryExpression = struct {
    lhs: pSyntaxNode,
    op: Operator,
    rhs: pSyntaxNode,
};

pub const UnaryExpression = struct {
    op: Operator,
    operand: pSyntaxNode,
};
