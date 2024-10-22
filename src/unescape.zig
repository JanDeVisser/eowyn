//
// Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
//
// SPDX-License-Identifier: MIT
//

const std = @import("std");

pub fn unescape(alloc: std.mem.Allocator, s: []const u8) !?[]const u8 {
    if (std.mem.indexOfScalar(u8, s, '\\')) |_| {
        const ret = try alloc.alloc(u8, s.len);
        var esc = false;
        var hexcode_start: ?usize = null;
        var tix: usize = 0;
        for (s, 0..) |ch, six| {
            if (hexcode_start) |start| {
                if (std.ascii.isDigit(ch)) {
                    continue;
                }
                const code = std.fmt.parseInt(u8, s[start..six], 16) catch unreachable;
                ret[tix] = code;
                tix += 1;
                hexcode_start = null;
                continue;
            }
            if (!esc and ch == '\\') {
                esc = true;
                continue;
            }
            var escaped = ch;
            if (esc) {
                esc = false;
                switch (ch) {
                    'x', 'X' => {
                        hexcode_start = six + 1;
                        continue;
                    },
                    'n' => escaped = '\n',
                    't' => escaped = '\t',
                    'r' => escaped = '\r',
                    else => escaped = ch,
                }
            }
            ret[tix] = escaped;
            tix += 1;
        }
        if (esc or hexcode_start != null) {
            return error.EscapeSyntaxError;
        }
        return ret[0..tix];
    }
    return null;
}

test "No escapes" {
    const s = "Hello, World!";
    try std.testing.expect(try unescape(std.heap.c_allocator, s) == null);
}

test "Escape newline" {
    const s = "Hello, World!\\n";
    try std.testing.expectEqualStrings("Hello, World!\n", try unescape(std.heap.c_allocator, s) orelse "Fail");
}
