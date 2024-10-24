//
// Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
//
// SPDX-License-Identifier: MIT
//

const std = @import("std");
const Allocator = std.mem.Allocator;

const eowyn = @import("eowyn.zig");
const node = @import("node.zig");
const op = @import("operator.zig");

const ASTBase = eowyn.ASTBase;
const BinaryOperator = op.BinaryOperator;
const EowynParser = eowyn.EowynParser;
const NodeReference = node.NodeReference;

pub const ArrayType = struct {
    element_type: NodeReference,
    size: ?NodeReference = null,

    pub fn format(this: ArrayType, base: ASTBase, w: anytype) !void {
        if (this.size) |size| {
            try w.print("[{}]{}", .{ base.parser.get_node(size), base.parser.get_node(this.element_type) });
        } else {
            try w.print("[]{}", .{base.parser.get_node(this.element_type)});
        }
    }
};

pub const AssignmentExpression = struct {
    left: NodeReference,
    op: BinaryOperator,
    right: NodeReference,

    pub fn format(this: AssignmentExpression, base: ASTBase, w: anytype) !void {
        try w.print("{} {} {}", .{ base.parser.get_node(this.left), this.op, base.parser.get_node(this.right) });
    }
};

pub const BinaryExpression = struct {
    left: NodeReference,
    op: BinaryOperator,
    right: NodeReference,

    pub fn format(this: BinaryExpression, base: ASTBase, w: anytype) !void {
        try w.print("{} {} {}", .{ base.parser.get_node(this.left), this.op, base.parser.get_node(this.right) });
    }
};

pub const Block = struct {
    statements: std.ArrayList(NodeReference),

    pub fn deinit(this: *Block) void {
        this.statements.deinit();
    }

    pub fn format(this: Block, base: ASTBase, w: anytype) !void {
        for (this.statements.items) |stmt| {
            try w.print("{}\n", .{base.parser.get_node(stmt)});
        }
    }
};

pub const ConstDecl = struct {
    name: []const u8,
    type: ?NodeReference,
    value: NodeReference,

    pub fn format(this: ConstDecl, base: ASTBase, w: anytype) !void {
        if (this.type) |t| {
            try w.print("const {s}: {}", .{ this.name, base.parser.get_node(t) });
        } else {
            try w.print("const {s}", .{this.name});
        }
        try w.print(" = {}", .{base.parser.get_node(this.value)});
    }
};

pub const Function = struct {
    declaration: NodeReference,
    implementation: NodeReference,

    pub fn format(this: Function, base: ASTBase, w: anytype) !void {
        try w.print("{} {{\n{}}}\n", .{ base.parser.get_node(this.declaration), base.parser.get_node(this.implementation) });
    }
};

pub const FunctionCall = struct {
    name: []const u8,
    arguments: std.ArrayList(NodeReference),

    pub fn deinit(this: *FunctionCall) void {
        this.arguments.deinit();
    }

    pub fn format(this: FunctionCall, base: ASTBase, w: anytype) !void {
        try w.print("{s}(", .{this.name});
        var first = true;
        for (this.arguments.items) |a| {
            if (!first) {
                try w.print(", ", .{});
            }
            try w.print("{}", .{base.parser.get_node(a)});
            first = false;
        }
        try w.print(")", .{});
    }
};

pub const FunctionDecl = struct {
    name: []const u8,
    parameters: std.ArrayList(NodeReference),
    return_type: ?NodeReference,

    pub fn deinit(this: *FunctionDecl) void {
        this.parameters.deinit();
    }

    pub fn format(this: FunctionDecl, base: ASTBase, w: anytype) !void {
        try w.print("func {s}(", .{this.name});
        var first = true;
        for (this.parameters.items) |p| {
            if (!first) {
                try w.print(", ", .{});
            }
            try w.print("{}", .{base.parser.get_node(p)});
            first = false;
        }
        try w.writeAll(")");
        if (this.return_type) |rt| {
            try w.print(" {}", .{base.parser.get_node(rt)});
        }
    }
};

pub const If = struct {
    condition: NodeReference,
    true_statement: NodeReference,
    false_statement: ?NodeReference,

    pub fn format(this: If, base: ASTBase, w: anytype) !void {
        try w.print("if {s} {{\n", .{base.parser.get_node(this.condition)});
        try w.print("{s}", .{base.parser.get_node(this.true_statement)});
        if (this.false_statement) |false_stmt| {
            try w.print("}} else {{\n", .{});
            try w.print("{s}", .{base.parser.get_node(false_stmt)});
        }
        try w.print("}}", .{});
    }
};

pub const Label = struct {
    label: []const u8,

    pub fn format(this: Label, _: ASTBase, w: anytype) !void {
        try w.print("#{s}", .{this.label});
    }
};

pub const Loop = struct {
    label: ?NodeReference = null,
    statement: NodeReference,

    pub fn format(this: Loop, base: ASTBase, w: anytype) !void {
        if (this.label) |label| {
            try w.print("{} ", .{base.parser.get_node(label)});
        }
        try w.print("loop {{\n", .{});
        try w.print("{s}", .{base.parser.get_node(this.statement)});
        try w.print("}}", .{});
    }
};

pub const Parameter = struct {
    name: []const u8,
    type: NodeReference,

    pub fn format(this: Parameter, base: ASTBase, w: anytype) !void {
        try w.print("{s}: {}", .{ this.name, base.parser.get_node(this.type) });
    }
};

pub const PointerType = struct {
    element_type: NodeReference,

    pub fn format(this: PointerType, base: ASTBase, w: anytype) !void {
        try w.print("*{}", .{base.parser.get_node(this.element_type)});
    }
};

pub const Program = struct {
    declarations: std.StringArrayHashMap(NodeReference),

    pub fn deinit(this: *Program) void {
        this.declarations.deinit();
    }

    pub fn format(this: Program, base: ASTBase, w: anytype) !void {
        for (this.declarations.values()) |decl| {
            try w.print("{}\n", .{base.parser.get_node(decl)});
        }
    }
};

pub const Return = struct {
    expr: ?NodeReference,

    pub fn format(this: Return, base: ASTBase, w: anytype) !void {
        try w.writeAll("return");
        if (this.expr) |t| {
            try w.print(" {}", .{base.parser.get_node(t)});
        }
    }
};

pub const Subscript = struct {
    subscripts: std.ArrayList(NodeReference),

    pub fn deinit(this: *Subscript) void {
        this.subscripts.deinit();
    }

    pub fn format(this: Subscript, base: ASTBase, w: anytype) !void {
        try w.print("[", .{});
        var first = true;
        for (this.subscripts.items) |a| {
            if (!first) {
                try w.print(", ", .{});
            }
            try w.print("{}", .{base.parser.get_node(a)});
            first = false;
        }
        try w.print("]", .{});
    }
};

pub const UnaryExpression = struct {
    op: op.UnaryOperator,
    operand: NodeReference,

    pub fn format(this: UnaryExpression, base: ASTBase, w: anytype) !void {
        try w.print("{}{}", .{ this.op, base.parser.get_node(this.operand) });
    }
};

pub const VarDecl = struct {
    name: []const u8,
    type: ?NodeReference,
    value: ?NodeReference,

    pub fn format(this: VarDecl, base: ASTBase, w: anytype) !void {
        if (this.type) |t| {
            try w.print("var {s}: {}", .{ this.name, base.parser.get_node(t) });
        } else {
            try w.print("var {s}", .{this.name});
        }
        if (this.value) |value| {
            try w.print(" = {}", .{base.parser.get_node(value)});
        }
    }
};
