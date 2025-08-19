const std = @import("std");
const lexer = @import("lexer.zig");
const operator = @import("operator.zig");

const block = @import("syntax/block.zig");
const constant = @import("syntax/constant.zig");
const expression = @import("syntax/expression.zig");
const flowcontrol = @import("syntax/flowcontrol.zig");
const import = @import("syntax/import.zig");
const preprocess = @import("syntax/preprocess.zig");
const typespec = @import("syntax/typespec.zig");
const variable = @import("syntax/variable.zig");

const Operator = operator.Operator;
pub const pSyntaxNode = usize;
pub const SyntaxNodes = std.ArrayList(pSyntaxNode);

pub const NodeValueTag = enum {
    BinaryExpression,
    Block,
    BoolConstant,
    Const,
    Error,
    Identifier,
    Insert,
    Nullptr,
    Number,
    Public,
    QuotedString,
    Return,
    Struct,
    StructField,
    TypeSpecification,
    UnaryExpression,
    VariableDeclaration,
    Void,
    Yield,
};

pub const NodeValue = union(NodeValueTag) {
    BinaryExpression: expression.BinaryExpression,
    Block: block.Block,
    BoolConstant: constant.BoolConstant,
    Const: variable.Const,
    Error: flowcontrol.Error,
    Identifier: variable.Identifier,
    Insert: preprocess.Insert,
    Nullptr: constant.Nullptr,
    Number: constant.Number,
    Public: import.Public,
    QuotedString: constant.QuotedString,
    Return: flowcontrol.Return,
    Struct: typespec.Struct,
    StructField: typespec.StructField,
    TypeSpecification: typespec.TypeSpecification,
    UnaryExpression: expression.UnaryExpression,
    VariableDeclaration: variable.VariableDeclaration,
    Void: block.Void,
    Yield: flowcontrol.Yield,
};

pub const Node = struct {
    tree: *Tree,
    index: pSyntaxNode,
    location: lexer.TokenLocation,
    value: NodeValue,
};

pub const Tree = struct {
    allocator: std.mem.Allocator,
    nodes: std.ArrayList(Node),

    pub fn init(allocator: std.mem.Allocator) Tree {
        return .{
            .allocator = allocator,
            .nodes = std.ArrayList(Node).init(allocator),
        };
    }

    pub fn deinit(this: *Tree) void {
        this.nodes.deinit();
    }

    pub fn add(this: *Tree, comptime Tag: NodeValueTag, location: lexer.TokenLocation, value: anytype) *Node {
        var node_value: NodeValue = undefined;
        @field(node_value, @tagName(Tag)) = value;
        const newnode = Node{
            .tree = this,
            .index = this.nodes.items.len,
            .location = location,
            .value = node_value,
        };
        this.nodes.append(newnode);
        return &this.nodes.items[this.nodes.items.len - 1];
    }

    pub fn get(this: *const Tree, ix: pSyntaxNode) *Node {
        std.debug.assert(ix < this.nodes.items.len);
        return &this.nodes.items[ix];
    }
};
