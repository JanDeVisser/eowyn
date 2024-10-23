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

const BinaryOperator = op.BinaryOperator;
const EowynParser = eowyn.EowynParser;
const NodeReference = node.NodeReference;

pub const BuiltinType = enum {
    Bool,
    Float,
    Int,
    String,
    Void,

    pub fn get(t: []const u8) ?BuiltinType {
        if (std.mem.eql(u8, t, "bool")) {
            return .Bool;
        } else if (std.mem.eql(u8, t, "float")) {
            return .Float;
        } else if (std.mem.eql(u8, t, "int")) {
            return .Int;
        } else if (std.mem.eql(u8, t, "string")) {
            return .String;
        } else if (std.mem.eql(u8, t, "void")) {
            return .Void;
        }
        return null;
    }

    pub fn format(this: BuiltinType, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        switch (this) {
            .Bool => try w.print("bool", .{}),
            .Float => try w.print("float", .{}),
            .Int => try w.print("int", .{}),
            .String => try w.print("string", .{}),
            .Void => try w.print("void", .{}),
        }
    }
};

pub const ArrayType = struct {
    element_type: NodeReference,
    size: ?NodeReference = null,

    pub fn format(this: ArrayType, parser: *EowynParser, w: anytype) !void {
        if (this.size) |size| {
            try w.print("[{}]{}", .{ parser.get_node(size), parser.get_node(this.element_type) });
        } else {
            try w.print("[]{}", .{parser.get_node(this.element_type)});
        }
    }
};

pub const AssignmentExpression = struct {
    left: NodeReference,
    op: BinaryOperator,
    right: NodeReference,

    pub fn format(this: AssignmentExpression, parser: *EowynParser, w: anytype) !void {
        try w.print("{} {} {}", .{ parser.get_node(this.left), this.op, parser.get_node(this.right) });
    }
};

pub const BinaryExpression = struct {
    left: NodeReference,
    op: BinaryOperator,
    right: NodeReference,

    pub fn format(this: BinaryExpression, parser: *EowynParser, w: anytype) !void {
        try w.print("{} {} {}", .{ parser.get_node(this.left), this.op, parser.get_node(this.right) });
    }
};

pub const Block = struct {
    statements: std.ArrayList(NodeReference),

    pub fn deinit(this: *Block) void {
        this.statements.deinit();
    }

    pub fn format(this: Block, parser: *EowynParser, w: anytype) !void {
        for (this.statements.items) |stmt| {
            try w.print("{}\n", .{parser.get_node(stmt)});
        }
    }
};

pub const Function = struct {
    declaration: NodeReference,
    implementation: NodeReference,

    pub fn format(this: Function, parser: *EowynParser, w: anytype) !void {
        try w.print("{} {{\n{}}}\n", .{ parser.get_node(this.declaration), parser.get_node(this.implementation) });
    }
};

pub const FunctionCall = struct {
    name: []const u8,
    arguments: std.ArrayList(NodeReference),

    pub fn deinit(this: *FunctionCall) void {
        this.arguments.deinit();
    }

    pub fn format(this: FunctionCall, parser: *EowynParser, w: anytype) !void {
        try w.print("{s}(", .{this.name});
        var first = true;
        for (this.arguments.items) |a| {
            if (!first) {
                try w.print(", ", .{});
            }
            try w.print("{}", .{parser.get_node(a)});
            first = false;
        }
        try w.print(")", .{});
    }
};

pub const FunctionDecl = struct {
    name: []const u8,
    parameters: std.ArrayList(NodeReference),
    return_type: NodeReference,

    pub fn deinit(this: *FunctionDecl) void {
        this.parameters.deinit();
    }

    pub fn format(this: FunctionDecl, parser: *EowynParser, w: anytype) !void {
        try w.print("func {s}(", .{this.name});
        var first = true;
        for (this.parameters.items) |p| {
            if (!first) {
                try w.print(", ", .{});
            }
            try w.print("{}", .{parser.get_node(p)});
            first = false;
        }
        try w.print(") {}", .{parser.get_node(this.return_type)});
    }
};

pub const If = struct {
    condition: NodeReference,
    true_statement: NodeReference,
    false_statement: ?NodeReference,

    pub fn format(this: If, parser: *EowynParser, w: anytype) !void {
        try w.print("if {s} {{\n", .{parser.get_node(this.condition)});
        try w.print("{s}", .{parser.get_node(this.true_statement)});
        if (this.false_statement) |false_stmt| {
            try w.print("}} else {{\n", .{});
            try w.print("{s}", .{parser.get_node(false_stmt)});
        }
        try w.print("}}", .{});
    }
};

pub const Label = struct {
    label: []const u8,

    pub fn format(this: Label, _: *EowynParser, w: anytype) !void {
        try w.print("#{s}", .{this.label});
    }
};

pub const Loop = struct {
    label: ?NodeReference = null,
    statement: NodeReference,

    pub fn format(this: Loop, parser: *EowynParser, w: anytype) !void {
        if (this.label) |label| {
            try w.print("{} ", .{parser.get_node(label)});
        }
        try w.print("loop {{\n", .{});
        try w.print("{s}", .{parser.get_node(this.statement)});
        try w.print("}}", .{});
    }
};

pub const Parameter = struct {
    name: []const u8,
    type: NodeReference,

    pub fn format(this: Parameter, parser: *EowynParser, w: anytype) !void {
        try w.print("{s}: {}", .{ this.name, parser.get_node(this.type) });
    }
};

pub const PointerType = struct {
    element_type: NodeReference,

    pub fn format(this: PointerType, parser: *EowynParser, w: anytype) !void {
        try w.print("*{}", .{parser.get_node(this.element_type)});
    }
};

pub const Program = struct {
    declarations: std.StringArrayHashMap(NodeReference),

    pub fn deinit(this: *Program) void {
        this.declarations.deinit();
    }

    pub fn format(this: Program, parser: *EowynParser, w: anytype) !void {
        for (this.declarations.values()) |decl| {
            try w.print("{}\n", .{parser.get_node(decl)});
        }
    }
};

pub const Subscript = struct {
    subscripts: std.ArrayList(NodeReference),

    pub fn deinit(this: *Subscript) void {
        this.subscripts.deinit();
    }

    pub fn format(this: Subscript, parser: *EowynParser, w: anytype) !void {
        try w.print("[", .{});
        var first = true;
        for (this.subscripts.items) |a| {
            if (!first) {
                try w.print(", ", .{});
            }
            try w.print("{}", .{parser.get_node(a)});
            first = false;
        }
        try w.print("]", .{});
    }
};

pub const UnaryExpression = struct {
    op: op.UnaryOperator,
    operand: NodeReference,

    pub fn format(this: UnaryExpression, parser: *EowynParser, w: anytype) !void {
        try w.print("{}{}", .{ this.op, parser.get_node(this.operand) });
    }
};
