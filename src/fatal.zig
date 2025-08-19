const std = @import("std");

pub fn oom() noreturn {
    fatal("Out of memory", .{});
}

pub fn fatal(comptime msg: []const u8, args: anytype) noreturn {
    std.debug.print("Fatal error: ", .{});
    std.debug.print(msg, args);
    std.debug.print("\n", .{});
    std.debug.print("Stack trace:\n", .{});
    std.debug.dumpCurrentStackTrace(null);
    std.debug.print("\n", .{});
    std.process.exit(1);
}

pub fn panic(msg: []const u8) noreturn {
    fatal("Panic: {s}", .{msg});
}
