const std = @import("std");

pub extern "c" fn dlopen(path: ?[*:0]const u8, mode: std.c.RTLD) ?*anyopaque;

pub const Resolver = struct {
    lib: ?[]const u8,
    prefix: ?[]const u8,

    pub fn init(lib: ?[]const u8, prefix: ?[]const u8) Resolver {
        return .{
            .lib = lib,
            .prefix = prefix,
        };
    }

    fn try_resolve(this: Resolver, comptime FncType: type, lib_name: ?[]const u8, func_name: []const u8) !?FncType {
        var buf: [1024]u8 = undefined;
        const l = blk: {
            const image: ?[*:0]const u8 = if (lib_name) |l| try std.fmt.bufPrintZ(&buf, "{s}.dylib", .{l}) else null;
            break :blk dlopen(image, .{}) orelse return error.NoSuchLibrary;
        };
        const funcZ = try std.fmt.bufPrintZ(&buf, "{s}", .{func_name});
        if (std.c.dlsym(l, funcZ)) |f| {
            return @as(FncType, @alignCast(@ptrCast(f)));
        }
        if (this.prefix) |p| {
            const prefixed_funcZ = try std.fmt.bufPrintZ(&buf, "{s}{s}", .{ p, func_name });
            if (std.c.dlsym(l, prefixed_funcZ)) |f| {
                return @as(FncType, @alignCast(@ptrCast(f)));
            }
        }
        return null;
    }

    pub fn resolve(this: Resolver, comptime FncType: type, name: []const u8) !?FncType {
        const lib_name = if (std.mem.indexOfScalar(u8, name, ':')) |ix| name[0..ix] else null;
        const func_name = name[(if (lib_name) |l| l.len + 1 else 0)..];
        if (lib_name) |_| {
            return try this.try_resolve(FncType, lib_name, func_name);
        }
        if (try this.try_resolve(FncType, null, func_name)) |f| {
            return f;
        }
        if (this.lib) |_| {
            if (try this.try_resolve(FncType, this.lib, func_name)) |f| {
                return f;
            }
        }
        return error.NoSuchFunction;
    }
};

// -- TESTING ----------------------------------------------------------------

const testlib = @cImport(@cInclude("/Users/jan/projects/eowyn/testlib.h"));

const Fnc = *fn (arg: *anyopaque) c_ulong;

export fn local_function(_: *anyopaque) c_ulong {
    return 42;
}
