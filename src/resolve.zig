//
// Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
//
// SPDX-License-Identifier: MIT
//

const std = @import("std");

pub extern "c" fn dlopen(path: ?[*:0]const u8, mode: std.c.RTLD) ?*anyopaque;

pub const Resolver = struct {
    allocator: std.mem.Allocator,
    lib: ?[]const u8,
    prefix: ?[]const u8,

    const HandleCache = struct {
        allocator: std.mem.Allocator,
        app_image: ?*anyopaque = null,
        handles: std.StringHashMap(*anyopaque),

        pub fn init(allocator: std.mem.Allocator) HandleCache {
            return .{
                .allocator = allocator,
                .handles = std.StringHashMap(*anyopaque).init(allocator),
            };
        }

        pub fn deinit(this: *HandleCache) void {
            for (this.handles.iterator()) |e| {
                _ = std.c.dlclose(e.value_ptr.*);
                this.allocator.free(e.key_ptr.*);
            }
            if (this.app_image != null) {
                _ = std.c.dlclose(this.app_image.?);
            }
            this.handles.deinit();
        }

        fn get(this: *HandleCache, lib_name: ?[]const u8) !*anyopaque {
            return blk: {
                if (lib_name) |l| {
                    if (this.handles.get(l)) |h| {
                        break :blk h;
                    }
                    const image = try std.fmt.allocPrintZ(this.allocator, "{s}.dylib", .{l});
                    defer this.allocator.free(image);
                    const h = dlopen(image, .{}) orelse return error.NoSuchLibrary;
                    try this.handles.put(try this.allocator.dupe(u8, image), h);
                    break :blk h;
                } else {
                    break :blk this.app_image orelse app_image_blk: {
                        this.app_image = dlopen(null, .{}) orelse return error.NoSuchLibrary;
                        break :app_image_blk this.app_image.?;
                    };
                }
            };
        }
    };

    var handle_cache: ?HandleCache = null;

    pub fn init(allocator: std.mem.Allocator, lib: ?[]const u8, prefix: ?[]const u8) Resolver {
        return .{
            .allocator = allocator,
            .lib = lib,
            .prefix = prefix,
        };
    }

    pub fn deinit(_: *Resolver) void {}

    fn get_handle(this: Resolver, lib_name: ?[]const u8) !*anyopaque {
        if (handle_cache == null) {
            handle_cache = HandleCache.init(this.allocator);
        }
        if (handle_cache) |*cache| {
            return try cache.get(lib_name);
        } else {
            unreachable;
        }
    }

    fn try_resolve(this: Resolver, comptime FncType: type, lib_name: ?[]const u8, func_name: []const u8) !?FncType {
        const l = try this.get_handle(lib_name);
        const funcZ = try std.fmt.allocPrintZ(this.allocator, "{s}", .{func_name});
        defer this.allocator.free(funcZ);
        if (std.c.dlsym(l, funcZ)) |f| {
            return @as(FncType, @alignCast(@ptrCast(f)));
        }
        if (this.prefix) |p| {
            const prefixed_funcZ = try std.fmt.allocPrintZ(this.allocator, "{s}{s}", .{ p, func_name });
            defer this.allocator.free(prefixed_funcZ);
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
        std.debug.print("Could not resolve '{s}'\n", .{name});
        return error.NoSuchFunction;
    }
};

// -- TESTING ----------------------------------------------------------------

const testlib = @cImport(@cInclude("/Users/jan/projects/eowyn/testlib.h"));

const Fnc = *fn (arg: *anyopaque) c_ulong;

export fn local_function(_: *anyopaque) c_ulong {
    return 42;
}
