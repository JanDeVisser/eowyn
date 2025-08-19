const std = @import("std");

const node = @import("../node.zig");

pub const pTypeSpecification = node.pSyntaxNode;

pub const EnumValue = struct {
    label: []const u8,
    value: ?node.pSyntaxNode,
    payload: ?pTypeSpecification,
};

pub const Enum = struct {
    name: []const u8,
    values: node.SyntaxNodes,
    underlying: ?pTypeSpecification,
};

pub const StructField = struct {
    label: []const u8,
    field_type: node.pSyntaxNode,
};

pub const StructFields = std.ArrayList(node.pSyntaxNode);

pub const Struct = struct {
    name: []const u8,
    fields: StructFields,
};

pub const TypeDescription = union(enum) {
    TypeName: struct {
        name: []const u8,
        arguments: std.ArrayList(pTypeSpecification),
    },
    Reference: struct {
        referencing: pTypeSpecification,
    },
    Slice: struct {
        slice_of: pTypeSpecification,
    },
    ZeroTerminatedArray: struct {
        array_of: pTypeSpecification,
    },
    Array: struct {
        array_of: pTypeSpecification,
        size: usize,
    },
    DynArray: struct {
        array_of: pTypeSpecification,
    },
    Optional: struct {
        optional_of: pTypeSpecification,
    },
    Error: struct {
        success_type: pTypeSpecification,
        error_type: pTypeSpecification,
    },
};

pub const TypeSpecification = struct {
    description: TypeDescription,
};
