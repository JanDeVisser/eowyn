//
// Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
//
// SPDX-License-Identifier: MIT
//

const std = @import("std");
const Allocator = std.mem.Allocator;

const set = @import("hashset.zig");
const unesc = @import("unescape.zig");

pub const Location = struct {
    pos: u64 = 0,
    line: u64 = 0,
    col: u64 = 0,
};

pub const Token = struct {
    location: Location,
    raw_text: []const u8,
    text: []const u8,
    kind: Kind,

    pub fn bookmark() Token {
        return init(.Null, "");
    }

    pub fn init(kind: Kind, text: []const u8) Token {
        return .{
            .location = .{},
            .kind = kind,
            .raw_text = text,
            .text = text,
        };
    }

    pub fn is_bookmark(this: Token) bool {
        return this.kind == .Null;
    }

    pub fn decode(s: []const u8, lexer: Lexer) !Token {
        var v = s;
        const k: KindTag = if (std.mem.indexOfScalar(u8, s, ':')) |colon| std.meta.stringToEnum(KindTag, blk: {
            v = s[colon + 1 ..];
            break :blk s[0..colon];
        }) orelse return error.InvalidKind else .Identifier;
        return switch (k) {
            .Null => init(.Null, v),
            .EOF => init(.EOF, v),
            .Identifier => init(.Identifier, v),
            .Keyword => init(.{ .Keyword = if (lexer.config.keywords.has(v)) v else return error.UnknownKeyword }, v),
            .Newline => init(.Newline, v),
            .Number => number: {
                const t: NumberType = if (std.mem.indexOfScalar(u8, v, ':')) |colon| std.meta.stringToEnum(NumberType, number_type: {
                    const type_str = v[0..colon];
                    v = s[colon + 1 ..];
                    break :number_type type_str;
                }) orelse return error.InvalidNumberType else if (std.mem.indexOfScalar(u8, v, '.')) |_| .Float else .Int;
                break :number init(.{ .Number = t }, v);
            },
            .String => init(.{
                .String = if (v.len > 0) v[0] else '"',
            }, v),
            .Symbol => init(.{
                .Symbol = v[0],
            }, v),
            .Whitespace => init(.Whitespace, v),
            .Comment => unreachable,
        };
    }

    pub fn format(this: Token, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        try w.print("{s} [{}]", .{ this.text, this.kind });
    }

    pub const KindTag = enum(usize) {
        Null = 0,
        EOF = 1,
        Identifier = 2,
        Keyword = 3,
        Newline = 4,
        Number = 5,
        String = 6,
        Symbol = 7,
        Whitespace = 8,
        Comment = 9,
    };

    pub const NumberType = enum {
        Binary,
        Float,
        Hex,
        Int,
    };

    pub const Kind = union(KindTag) {
        Null: void,
        EOF: void,
        Identifier: void,
        Keyword: []const u8,
        Newline: void,
        Number: NumberType,
        String: u8,
        Symbol: u8,
        Whitespace: void,
        Comment: []const u8,

        pub fn format(this: Kind, comptime fmt: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            if (std.mem.eql(u8, fmt, "s")) {
                switch (this) {
                    .Keyword => |k| try w.print("\"{s}\"", .{k}),
                    .String, .Symbol => |c| try w.print("'{c}'", .{c}),
                    .Number => |t| try w.print("#{s}", .{@tagName(t)}),
                    .Comment => |marker| try w.print("{s}", .{marker}),
                    else => try w.print("'{s}'", .{@tagName(this)}),
                }
            } else {
                switch (this) {
                    .Keyword => |k| try w.print(".{{ .Keyword = \"{s}\" }}", .{k}),
                    .String, .Symbol => |c| try w.print(".{{ .{s} = '{c}' }}", .{ @tagName(this), c }),
                    .Number => |t| try w.print(".{{ .Number = .{s} }}", .{@tagName(t)}),
                    .Comment => |marker| try w.print(".{{ .Comment = \"{s}\" }}", .{marker}),
                    else => try w.print(".{s}", .{@tagName(this)}),
                }
            }
        }

        pub fn eql(this: Kind, other: Kind) bool {
            if (@intFromEnum(this) != @intFromEnum(other)) return false;
            return switch (this) {
                .Comment => |s| std.mem.eql(u8, s, other.Comment),
                .Keyword => |s| std.mem.eql(u8, s, other.Keyword),
                .Number => |t| t == other.Number,
                .String => |c| c == other.String,
                .Symbol => |c| c == other.Symbol,
                else => true,
            };
        }
    };
};

pub const Config = struct {
    allocator: Allocator,
    comment: Comment,
    keywords: Keywords,
    number: Number,
    identifier: struct {
        on: bool = true,
    },

    qstring: struct {
        on: bool = true,
        quotes: []const u8 = "\"'`",
    },

    whitespace: struct {
        on: bool = true,
        ignore_ws: bool = false,
        ignore_nl: bool = false,

        const Self = @This();

        pub fn configure(this: *Self, key: []const u8, _: ?[]const u8) !void {
            if (std.ascii.eqlIgnoreCase(key, "ignoreall")) {
                this.ignore_nl = true;
                this.ignore_ws = true;
            }
            if (std.ascii.eqlIgnoreCase(key, "ignorews")) {
                this.ignore_ws = true;
            }
            if (std.ascii.eqlIgnoreCase(key, "ignorenl")) {
                this.ignore_nl = true;
            }
        }
    },

    const Keywords = struct {
        on: bool = true,
        keywords: set.StringSet,

        const MatchResult = enum {
            NoMatch,
            ExactMatch,
            Prefix,
            PrefixAndExact,
            MatchLost,
        };

        pub fn init(allocator: Allocator) Keywords {
            return .{
                .keywords = set.StringSet.init(allocator),
            };
        }

        pub fn deinit(this: *Keywords) void {
            this.keywords.deinit();
        }

        pub fn configure(this: *Keywords, key: []const u8, value: ?[]const u8) !void {
            if (std.ascii.eqlIgnoreCase(key, "kw")) {
                const v = std.mem.trim(u8, value orelse return error.MalformedLexerConfigString, " \t");
                _ = try this.keywords.add(v);
            }
        }

        pub fn has(this: Keywords, kw: []const u8) bool {
            return this.keywords.contains(kw);
        }

        pub fn add(this: *Keywords, kw: []const u8) !void {
            _ = try this.keywords.add(kw);
        }

        pub fn addAll(this: *Keywords, kw_s: []const []const u8) !void {
            for (kw_s) |kw| {
                _ = try this.add(kw);
            }
        }

        pub fn match(this: Keywords, text: []const u8) MatchResult {
            var prefix_matches: i32 = 0;
            var lost_matches: i32 = 0;
            var matched = false;
            var it = this.keywords.iterator();
            while (it.next()) |kw| {
                if (std.mem.eql(u8, text, kw)) {
                    matched = true;
                } else if (text.len < kw.len) {
                    if (std.mem.startsWith(u8, kw, text)) {
                        prefix_matches += 1;
                    }
                } else {
                    if (std.mem.startsWith(u8, text, kw)) {
                        lost_matches += 1;
                    }
                }
            }
            if (!matched and prefix_matches == 0 and lost_matches == 0) {
                return .NoMatch;
            }
            if (!matched and prefix_matches > 0) {
                return .Prefix;
            }
            if (matched) {
                if (prefix_matches == 0) {
                    return .ExactMatch;
                }
                return .PrefixAndExact;
            }
            std.debug.assert(!matched and prefix_matches == 0 and lost_matches != 0);
            return .MatchLost;
        }
    };

    const Comment = struct {
        on: bool = true,
        ignore: bool = false,
        hashpling: bool = true,
        block_marker: std.ArrayList(BlockMarker),
        eol_marker: std.ArrayList([]const u8),

        const BlockMarker = struct {
            start: []const u8,
            end: []const u8,
        };

        pub fn init(allocator: Allocator) Comment {
            return .{
                .block_marker = std.ArrayList(Comment.BlockMarker).init(allocator),
                .eol_marker = std.ArrayList([]const u8).init(allocator),
            };
        }

        pub fn deinit(this: *Comment) void {
            this.block_marker.deinit();
            this.eol_marker.deinit();
        }

        pub fn configure(this: *Comment, key: []const u8, value: ?[]const u8) !void {
            if (std.ascii.eqlIgnoreCase(key, "marker")) {
                const v = std.mem.trim(u8, value orelse return error.MalformedLexerConfigString, " \t");
                if (std.mem.indexOfScalar(u8, v, ' ')) |ix| {
                    try this.block_marker.append(.{
                        .start = v[0..ix],
                        .end = std.mem.trim(u8, v[ix + 1 ..], " \t"),
                    });
                } else {
                    try this.eol_marker.append(v);
                }
            }
        }

        fn scan(this: Comment, lexer: *Lexer) ?Token {
            if (lexer.pos == 0 and this.hashpling and std.mem.startsWith(u8, lexer.source, "#!")) {
                const len = if (std.mem.indexOfScalar(u8, lexer.source, '\n')) |p| p else lexer.source.len;
                const ret = lexer.buildToken(len, .{ .Comment = "#!" });
                lexer.line = 1;
                lexer.col = 0;
                return ret;
            }
            for (this.eol_marker.items) |marker| {
                if (std.mem.startsWith(u8, lexer.source, marker)) {
                    const len = if (std.mem.indexOfScalar(u8, lexer.source, '\n')) |p| p else lexer.source.len;
                    const ret = lexer.buildToken(len, .{ .Comment = marker });
                    lexer.line += 1;
                    lexer.col = 0;
                    return ret;
                }
            }
            for (this.block_marker.items) |marker| {
                if (std.mem.startsWith(u8, lexer.source, marker.start)) {
                    const len = if (std.mem.indexOf(u8, lexer.source, marker.end)) |p| p + marker.end.len else lexer.source.len;
                    const ret = lexer.buildToken(len, .{ .Comment = marker.start });
                    const newlines = std.mem.count(u8, lexer.source[0..len], "\n");
                    if (newlines > 0) {
                        lexer.line += newlines;
                        lexer.col = len - std.mem.lastIndexOfScalar(u8, lexer.source[0..len], '\n').?;
                    }
                    return ret;
                }
            }
            return null;
        }
    };

    const Number = struct {
        on: bool = true,
        signed: bool = false,
        float: bool = false,
        binary: bool = false,
        hex: bool = false,

        fn scan(this: Number, lexer: *Lexer) ?Token {
            var t: Token.NumberType = .Int;
            var p: usize = 0;
            if (lexer.source.len > 0 and lexer.source[0] == '+' or lexer.source[0] == '-') {
                if (!this.signed) {
                    return null;
                }
                p = 1;
            }
            var digit_found = false;
            if (p < lexer.source.len and lexer.source[p] == 0) {
                p += 1;
                if (p < lexer.source.len and (lexer.source[p] == 'x' or lexer.source[p] == 'X')) {
                    if (!this.hex) {
                        return lexer.buildToken(p, .{ .Number = .Int });
                    }
                    p += 1;
                } else if (p < lexer.source.len and (lexer.source[p] == 'b' or lexer.source[p] == 'B')) {
                    if (!this.binary) {
                        return lexer.buildToken(p, .{ .Number = .Int });
                    }
                    p += 1;
                } else {
                    digit_found = true;
                }
            }
            while (p < lexer.source.len and std.ascii.isDigit(lexer.source[p])) {
                digit_found = true;
                p += 1;
            }
            if (this.float and p < lexer.source.len and t == .Int and lexer.source[p] == '.') {
                p += 1;
                t = .Float;
                while (p < lexer.source.len and std.ascii.isDigit(lexer.source[p])) {
                    digit_found = true;
                    p += 1;
                }
            }
            if (digit_found) {
                return lexer.buildToken(p, .{ .Number = t });
            }
            return null;
        }
    };

    pub fn init(allocator: Allocator) Config {
        return .{
            .allocator = allocator,
            .comment = Comment.init(allocator),
            .identifier = .{},
            .keywords = Keywords.init(allocator),
            .number = .{},
            .qstring = .{},
            .whitespace = .{},
        };
    }

    pub fn deinit(this: *Config) void {
        this.keywords.deinit();
        this.comment.deinit();
    }

    fn config_scanner(this: *Config, comptime scanner: std.builtin.Type.StructField, config: []const u8) !void {
        var c = config;
        // std.debug.print("Scanner {s}\n", .{scanner.name});
        while (c.len > 0) {
            const eq_ix = if (std.mem.indexOfAny(u8, c, "=;")) |eq| eq else c.len;
            const was_eq = if (eq_ix < c.len) c[eq_ix] == '=' else false;
            const key = std.mem.trim(u8, c[0..eq_ix], " \t");
            c = c[(if (eq_ix < c.len) eq_ix + 1 else c.len)..];
            const value = if (!was_eq) null else blk: {
                const semi_ix = if (std.mem.indexOfScalar(u8, c, ';')) |semi| semi else c.len;
                const trimmed = std.mem.trim(u8, c[0..semi_ix], " \t");
                c = c[(if (semi_ix < c.len) semi_ix + 1 else c.len)..];
                break :blk trimmed;
            };
            configure: {
                // std.debug.print("key: {s} value {s}\n", .{ key, if (value) |v| v else "null" });
                inline for (@typeInfo(scanner.type).@"struct".fields) |fld| {
                    if (std.ascii.eqlIgnoreCase(fld.name, key)) {
                        // std.debug.print("Godim\n", .{});
                        switch (@typeInfo(fld.type)) {
                            .bool => {
                                @field(@field(this, scanner.name), fld.name) = if (value) |v| std.ascii.eqlIgnoreCase(v, "true") else true;
                                break :configure;
                            },
                            .pointer => |p| {
                                if (p.child == u8 and p.size == .Slice) {
                                    @field(@field(this, scanner.name), fld.name) = if (value) |v| v else "";
                                    break :configure;
                                }
                            },
                            else => {},
                        }
                    }
                }
                if (std.meta.hasFn(scanner.type, "configure")) {
                    try @field(this, scanner.name).configure(key, value);
                }
            }
        }
    }

    pub fn configure(this: *Config, scanner: []const u8, scanner_config: ?[]const u8) !void {
        inline for (@typeInfo(Config).@"struct".fields) |fld| {
            if (std.ascii.eqlIgnoreCase(fld.name, scanner)) {
                if (@typeInfo(fld.type) == .@"struct") {
                    if (std.meta.fieldIndex(fld.type, "on")) |_| {
                        @field(@field(this, fld.name), "on") = true;
                    }
                    if (scanner_config) |sc| {
                        try this.config_scanner(fld, sc);
                    }
                }
            }
        }
    }
};

pub const Lexer = struct {
    allocator: Allocator,
    config: Config,
    source: []const u8,
    ignore: set.HashSet(Token.KindTag, null),
    current: ?Token = null,
    pos: u64 = 0,
    line: u64 = 0,
    col: u64 = 0,
    exhausted: bool = false,
    unescaped_strings: std.ArrayList([]const u8),

    pub fn init(allocator: Allocator, config: Config, source: []const u8) !Lexer {
        var ignore = set.HashSet(Token.KindTag, null).init(allocator);
        if (config.whitespace.on) {
            if (config.whitespace.ignore_nl) {
                _ = try ignore.add(.Newline);
            }
            if (config.whitespace.ignore_ws) {
                _ = try ignore.add(.Whitespace);
            }
        }
        if (config.comment.ignore) {
            _ = try ignore.add(.Comment);
        }
        return .{
            .allocator = allocator,
            .config = config,
            .source = source,
            .ignore = ignore,
            .unescaped_strings = std.ArrayList([]const u8).init(allocator),
        };
    }

    pub fn deinit(this: *Lexer) void {
        this.ignore.deinit();
        for (this.unescaped_strings.items) |s| {
            this.allocator.free(s);
        }
        this.unescaped_strings.deinit();
    }

    fn unescape(this: *Lexer, s: []const u8) []const u8 {
        if (unesc.unescape(this.allocator, s) catch return s) |unescaped| {
            this.unescaped_strings.append(unescaped) catch @panic("Out of memory");
            return unescaped;
        }
        return s;
    }

    fn buildToken(this: *Lexer, length: u64, kind: Token.Kind) Token {
        this.current = Token{
            .location = .{
                .pos = this.pos,
                .line = this.line,
                .col = this.col,
            },
            .raw_text = if (this.source.len == 0 or length == 0) "" else this.source[0..length],
            .text = this.unescape(if (this.source.len == 0 or length == 0) "" else this.source[0..length]),
            .kind = kind,
        };
        return this.current.?;
    }

    pub fn advance(this: *Lexer) void {
        // std.debug.print("advance() ", .{});
        const token = this.current orelse unreachable;
        this.pos += token.raw_text.len;
        this.source = this.source[token.raw_text.len..];
        this.current = null;
    }

    pub fn next(this: *Lexer) ?Token {
        if (this.peek_next()) |t| {
            this.advance();
            return t;
        }
        return null;
    }

    pub fn peek_next(this: *Lexer) ?Token {
        return blk: {
            while (true) {
                if (this.peek()) |t| {
                    if (!this.ignore.contains(t.kind)) {
                        break :blk t;
                    }
                    this.advance();
                } else {
                    break :blk null;
                }
            }
        };
    }

    pub fn peek(this: *Lexer) ?Token {
        if (this.current) |t| {
            return t;
        }
        if (this.exhausted) {
            return null;
        }
        if (this.source.len == 0) {
            this.exhausted = true;
            return this.buildToken(0, .EOF);
        }
        if (this.source[0] == '\n') {
            const ret = this.buildToken(1, .Newline);
            this.line += 1;
            this.col = 0;
            if (this.config.whitespace.on) {
                return ret;
            }
        }
        if (this.config.comment.on) {
            if (this.config.comment.scan(this)) |t| {
                return t;
            }
        }
        if (std.ascii.isWhitespace(this.source[0])) {
            var p: u64 = 1;
            while (p < this.source.len and std.ascii.isWhitespace(this.source[p])) {
                p += 1;
            }
            return this.buildToken(p, .Whitespace);
        }
        if (this.config.number.on) {
            if (this.config.number.scan(this)) |t| {
                return t;
            }
        }
        if (this.config.qstring.on) {
            if (std.mem.indexOfScalar(u8, this.config.qstring.quotes, this.source[0])) |q| {
                const quote = this.config.qstring.quotes[q];
                var p: u64 = 1;
                while (p < this.source.len and this.source[p] != quote) {
                    p += if (this.source[p] == '\\') 2 else 1;
                }
                if (p < this.source.len) {
                    p += 1;
                }
                return this.buildToken(p, .{ .String = quote });
            }
        }
        if (std.ascii.isAlphabetic(this.source[0]) or this.source[0] == '_') {
            var p: u64 = 1;
            while (p < this.source.len and (std.ascii.isAlphanumeric(this.source[p]) or this.source[p] == '_')) {
                p += 1;
            }
            if (this.config.keywords.on) {
                switch (this.config.keywords.match(this.source[0..p])) {
                    .ExactMatch, .PrefixAndExact => return this.buildToken(p, .{ .Keyword = this.source[0..p] }),
                    else => {},
                }
            }
            if (this.config.identifier.on) {
                return this.buildToken(p, .Identifier);
            }
        }
        if (this.config.keywords.on) {
            var matched: ?usize = null;
            for (1..this.source.len) |l| {
                switch (this.config.keywords.match(this.source[0..l])) {
                    .ExactMatch => return this.buildToken(l, .{ .Keyword = this.source[0..l] }),
                    .NoMatch => break,
                    .Prefix => {},
                    .PrefixAndExact => matched = l,
                    .MatchLost => return this.buildToken(l, .{ .Keyword = this.source[0 .. matched orelse unreachable] }),
                }
            }
        }
        return this.buildToken(1, .{ .Symbol = this.source[0] });
    }

    pub fn accept_keyword(this: *Lexer, keyword: []const u8) bool {
        if (this.peek_next()) |t| {
            switch (t.kind) {
                .Keyword => |kw| {
                    if (std.mem.eql(u8, keyword, kw)) {
                        this.advance();
                        return true;
                    }
                },
                else => {},
            }
        }
        return false;
    }

    pub fn expect_identifier(this: *Lexer) !Token {
        if (this.peek_next()) |t| {
            if (t.kind == .Identifier) {
                this.advance();
                return t;
            }
        }
        return error.ExpectedIdentifier;
    }

    pub fn accept_identifier(this: *Lexer) ?Token {
        if (this.peek_next()) |t| {
            if (t.kind == .Identifier) {
                this.advance();
                return t;
            }
        }
        return null;
    }

    pub fn expect_symbol(this: *Lexer, symbol: u8) !void {
        if (this.peek_next()) |t| {
            if (t.kind == .Symbol and t.kind.Symbol == symbol) {
                this.advance();
                return;
            }
        }
        return error.ExpectedSymbol;
    }

    pub fn accept_symbol(this: *Lexer, symbol: u8) bool {
        if (this.peek_next()) |t| {
            if (t.kind == .Symbol and t.kind.Symbol == symbol) {
                this.advance();
                return true;
            }
        }
        return false;
    }
};

test "Config" {
    var config = Config.init(std.heap.c_allocator);
    try config.configure("qstring", "quotes=%$");
    try std.testing.expectEqualStrings("%$", config.qstring.quotes);
    try config.configure("qstring", "quotes=12;quotes=34");
    try std.testing.expectEqualStrings("34", config.qstring.quotes);
}

fn test_tokenize_string(comptime expected: comptime_int, s: []const u8) !void {
    var lexer = try Lexer.init(std.heap.c_allocator, Config.init(std.heap.c_allocator), s);
    var count: usize = 0;
    // std.debug.print("\n", .{});
    while (lexer.peek_next()) |t| {
        _ = t;
        // std.debug.print("{}: {s}\n", .{ count, t });
        count += 1;
        lexer.advance();
    }
    try std.testing.expectEqual(expected, count);
}

fn test_count_tokens(config: Config, comptime tag: Token.KindTag, comptime expected: comptime_int, s: []const u8) !void {
    var lexer = try Lexer.init(std.heap.c_allocator, config, s);
    var count: usize = 0;
    // std.debug.print("\n", .{});
    while (lexer.peek_next()) |t| {
        // std.debug.print("{}: {s}\n", .{ count, t });
        count += if (t.kind == tag) 1 else 0;
        lexer.advance();
    }
    try std.testing.expectEqual(expected, count);
}

test "Create Lexer" {
    const lexer = try Lexer.init(std.heap.c_allocator, Config.init(std.heap.c_allocator),
        \\#include <stdio.h>
        \\
        \\int main(int argc, char **argv) {
        \\    printf("Hello, World!\n");
        \\    return 0;
        \\}
        \\
    );
    _ = lexer;
}

test "Get first token" {
    var lexer = try Lexer.init(std.heap.c_allocator, Config.init(std.heap.c_allocator),
        \\#include <stdio.h>
        \\
        \\int main(int argc, char **argv) {
        \\    printf("Hello, World!\n");
        \\    return 0;
        \\}
        \\
    );
    if (lexer.peek_next()) |t| {
        try std.testing.expect(t.kind == .Symbol);
        try std.testing.expect(t.kind.Symbol == '#');
    } else {
        try std.testing.expect(false);
    }
}

test "Newlines" {
    try test_tokenize_string(4,
        \\X"A\nB"X
    );
}

test "Tokenize strings" {
    try test_tokenize_string(10,
        \\"Hello" 'World' `!` "Hello,\nWorld!" "Hello, World!\n"
    );
}

test "Get all tokens" {
    try test_tokenize_string(44,
        \\#include <stdio.h>
        \\
        \\int main(int argc, char **argv) {
        \\    printf("Hello, World!\n");
        \\    return 0;
        \\}
        \\
    );
}

test "Keywords" {
    var config = Config.init(std.heap.c_allocator);
    try config.keywords.addAll(&[_][]const u8{
        "int",
        "char",
        "return",
    });
    try test_count_tokens(config, .Keyword, 4,
        \\#include <stdio.h>
        \\
        \\int main(int argc, char **argv) {
        \\    printf("Hello, World!\n");
        \\    return 0;
        \\}
        \\
    );
}

test "Get all tokens with comments" {
    var config = Config.init(std.heap.c_allocator);
    try config.configure("comment", "marker=/* */;marker=//");
    try test_count_tokens(config, .Comment, 2,
        \\#include <stdio.h>
        \\
        \\// This is a comment
        \\
        \\int main(int argc, char **argv) {
        \\    /* This is a comment */
        \\    printf("Hello, World!\n");
        \\    return 0;
        \\}
        \\
    );
}
