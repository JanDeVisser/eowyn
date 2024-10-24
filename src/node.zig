//
// Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
//
// SPDX-License-Identifier: MIT
//

const std = @import("std");

pub const NodeReference = usize;

pub fn Impl(comptime ImplMap: type, comptime E: type) type {
    if (@typeInfo(ImplMap) != .@"union") {
        @compileError("Expected union type, found '" ++ @typeName(ImplMap) ++ "'");
    }
    if (@typeInfo(E) != .@"enum") {
        @compileError("Expected enum type, found '" ++ @typeName(E) ++ "'");
    }
    if (E == @typeInfo(ImplMap).@"union".tag_type) {
        return ImplMap;
    }
    const e = @typeInfo(E).@"enum";
    var union_flds: [e.fields.len]std.builtin.Type.UnionField = undefined;
    inline for (e.fields, 0..) |fld, ix| {
        union_flds[ix].name = fld.name;
        const f = blk: {
            for (std.meta.fields(ImplMap)) |base_fld| {
                if (std.mem.eql(u8, base_fld.name, fld.name)) {
                    break :blk base_fld;
                }
            }
            @compileError("Union tag type for '" ++ fld.name ++ "' not found");
        };
        union_flds[ix] = f;
    }
    const decls = [_]std.builtin.Type.Declaration{};
    return @Type(.{
        .@"union" = .{
            .tag_type = E,
            .fields = &union_flds,
            .decls = &decls,
            .layout = .auto,
        },
    });
}

pub fn Node(comptime NodeBase: type, comptime ImplMap: type, comptime E: type) type {
    const U: type = Impl(ImplMap, E);
    return struct {
        base: NodeBase,
        impl: Union,

        const Self = @This();
        const Enum = E;
        const Union = U;

        pub fn init(src: anytype) !Self {
            const S = @TypeOf(src);
            const s_type: std.builtin.Type.Struct = if (@typeInfo(S) == .@"struct") @typeInfo(S).@"struct" else if (@typeInfo(S) == .pointer and @typeInfo(@typeInfo(S).pointer.child) == .@"struct" and @typeInfo(S).pointer.size == .One) @typeInfo(@typeInfo(S).pointer.child).@"struct" else @compileError("Expected struct type, found '" ++ @typeName(S) ++ "'");
            comptime base_blk: {
                for (s_type.fields) |fld| {
                    if (std.mem.eql(u8, fld.name, "base")) {
                        if (fld.type == NodeBase) {
                            break :base_blk;
                        }
                        @compileError("Struct '" ++ @typeName(S) ++ "' has a 'base' field of the wrong type '" ++ @typeName(fld.type) ++ "'");
                    }
                }
                @compileError("Struct '" ++ @typeName(S) ++ "' has no 'base' field");
            }
            comptime impl_blk: {
                for (s_type.fields) |fld| {
                    if (std.mem.eql(u8, fld.name, "impl") and @typeInfo(fld.type) == .@"union") {
                        break :impl_blk;
                    }
                }
                @compileError("Struct '" ++ @typeName(S) ++ "' has no 'impl' field");
            }
            var impl: Union = undefined;
            assign_blk: {
                inline for (std.meta.fields(U)) |my_fld| {
                    if (std.mem.eql(u8, @tagName(src.impl), my_fld.name)) {
                        impl = @unionInit(Union, my_fld.name, @field(src.impl, my_fld.name));
                        break :assign_blk;
                    }
                }
                std.debug.print("source tag value '{s}' not in target enum '{s}'", .{ @tagName(src.impl), @typeName(E) });
            }
            return .{
                .base = src.base,
                .impl = impl,
            };
        }

        pub fn deinit(this: *Self) void {
            blk: inline for (std.meta.fields(Union)) |fld| {
                if (std.mem.eql(u8, @tagName(this.impl), fld.name)) {
                    switch (@typeInfo(fld.type)) {
                        .@"struct" => if (std.meta.hasFn(fld.type, "deinit")) @field(this.impl, fld.name).deinit(),
                        else => {},
                    }
                    break :blk;
                }
            }
        }

        pub fn format(this: Self, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            blk: inline for (std.meta.fields(Union)) |fld| {
                if (std.mem.eql(u8, @tagName(this.impl), fld.name)) {
                    switch (@typeInfo(fld.type)) {
                        .pointer => |p| {
                            if (p.child == u8 and p.size == .Slice) {
                                try w.print("{s}", .{@field(this.impl, fld.name)});
                            }
                            if (p.size == .One and @typeInfo(p.child) == .@"struct") {
                                try @field(this.impl, fld.name).format(this.base, w);
                            }
                        },
                        .bool => try w.print("{s}", .{if (@field(this.impl, fld.name)) "true" else "false"}),
                        .@"struct" => try @field(this.impl, fld.name).format(this.base, w),
                        .void => {},
                        else => try w.print("{}", .{@field(this.impl, fld.name)}),
                    }
                    break :blk;
                }
            }
        }
    };
}
