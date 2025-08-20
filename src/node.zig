const std = @import("std");
const fatal = @import("fatal.zig");
const lexer = @import("lexer.zig");
const operator = @import("operator.zig");

const block = @import("syntax/block.zig");
const constant = @import("syntax/constant.zig");
const expression = @import("syntax/expression.zig");
const flowcontrol = @import("syntax/flowcontrol.zig");
const function = @import("syntax/function.zig");
const import = @import("syntax/import.zig");
const preprocess = @import("syntax/preprocess.zig");
const typespec = @import("syntax/typespec.zig");
const variable = @import("syntax/variable.zig");

const Operator = operator.Operator;
pub const pSyntaxNode = usize;
pub const SyntaxNodes = std.ArrayList(pSyntaxNode);
const ign = fatal.ignore;

pub const NodeValueTag = enum {
    BinaryExpression,
    Block,
    BoolConstant,
    Break,
    Const,
    Continue,
    Defer,
    Embed,
    Enum,
    EnumValue,
    Error,
    ExternLink,
    ForStatement,
    FunctionDefinition,
    FunctionSignature,
    Identifier,
    IfStatement,
    Include,
    Insert,
    LoopStatement,
    Nullptr,
    Number,
    Parameter,
    Public,
    QuotedString,
    Return,
    Struct,
    StructField,
    TypeSpecification,
    UnaryExpression,
    Var,
    VariableDeclaration,
    Void,
    WhileStatement,
    Yield,
};

pub const NodeValue = union(NodeValueTag) {
    BinaryExpression: expression.BinaryExpression,
    Block: block.Block,
    BoolConstant: constant.BoolConstant,
    Break: flowcontrol.Break,
    Const: variable.Const,
    Continue: flowcontrol.Continue,
    Defer: flowcontrol.Defer,
    Embed: preprocess.Embed,
    Enum: typespec.Enum,
    EnumValue: typespec.EnumValue,
    Error: flowcontrol.Error,
    ExternLink: function.ExternLink,
    ForStatement: flowcontrol.ForStatement,
    FunctionDefinition: function.FunctionDefinition,
    FunctionSignature: function.FunctionSignature,
    Identifier: variable.Identifier,
    IfStatement: flowcontrol.IfStatement,
    Include: preprocess.Include,
    Insert: preprocess.Insert,
    LoopStatement: flowcontrol.LoopStatement,
    Nullptr: constant.Nullptr,
    Number: constant.Number,
    Parameter: function.Parameter,
    Public: import.Public,
    QuotedString: constant.QuotedString,
    Return: flowcontrol.Return,
    Struct: typespec.Struct,
    StructField: typespec.StructField,
    TypeSpecification: typespec.TypeSpecification,
    UnaryExpression: expression.UnaryExpression,
    Var: variable.Var,
    VariableDeclaration: variable.VariableDeclaration,
    Void: block.Void,
    WhileStatement: flowcontrol.WhileStatement,
    Yield: flowcontrol.Yield,
};

pub const Node = struct {
    tree: *Tree,
    index: pSyntaxNode,
    location: lexer.TokenLocation,
    value: NodeValue,

    pub fn print(this: *const Node, writer: anytype) void {
        writer.print("{}: {s} ", .{ this.index, @tagName(this.value) }) catch ign();
        switch (this.value) {
            .BinaryExpression => |expr| writer.print("{} {s} {}", .{ expr.lhs, @tagName(expr.op), expr.rhs }) catch ign(),
            .Block => |b| for (b.statements.items) |stmt| writer.print("{} ", .{stmt}) catch ign(),
            .FunctionDefinition => |fd| writer.print("signature {} implementation {}", .{ fd.signature, fd.implementation }) catch ign(),
            .FunctionSignature => |sig| {
                writer.print("{s}(", .{sig.name}) catch ign();
                for (sig.parameters.items) |param| {
                    writer.print("{} ", .{param}) catch ign();
                }
                writer.print(") {}", .{sig.return_type}) catch ign();
            },
            .Identifier => |id| writer.print("{s}", .{id.identifier}) catch ign(),
            .Parameter => |p| writer.print("{s} {}", .{ p.name, p.type_spec }) catch ign(),
            .TypeSpecification => |ts| {
                writer.print("{s} ", .{@tagName(ts.description)}) catch ign();
                switch (ts.description) {
                    .TypeName => |name| writer.print("{s}", .{name.name}) catch ign(),
                    else => {},
                }
            },
            else => {},
        }
        writer.writeAll("\n") catch ign();
    }
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

    pub fn add(this: *Tree, comptime tag: NodeValueTag, location: lexer.TokenLocation, value: anytype) *Node {
        const newnode = Node{
            .tree = this,
            .index = this.nodes.items.len,
            .location = location,
            .value = @unionInit(NodeValue, @tagName(tag), value),
        };
        this.nodes.append(newnode) catch fatal.oom();
        return &this.nodes.items[this.nodes.items.len - 1];
    }

    pub fn get(this: *const Tree, ix: pSyntaxNode) *Node {
        std.debug.assert(ix < this.nodes.items.len);
        return &this.nodes.items[ix];
    }
};
