const std = @import("std");
const Allocator = std.mem.Allocator;

const set = @import("hashset.zig");
const unesc = @import("unescape.zig");

pub const Keyword = usize;

pub const Keywords = struct {
    keywords: []const []const u8 = &[0][]const u8{},

    const MatchResult = union(enum) {
        NoMatch: void,
        ExactMatch: Keyword,
        Prefix: void,
        PrefixAndExact: Keyword,
        MatchLost: void,
    };

    pub fn init(comptime kw_s: []const []const u8) Keywords {
        return .{
            .keywords = kw_s,
        };
    }

    pub fn match(this: Keywords, text: []const u8) MatchResult {
        var prefix_matches: i32 = 0;
        var lost_matches: i32 = 0;
        var matched: ?Keyword = null;
        for (this.keywords, 0..) |kw, ix| {
            if (std.mem.eql(u8, text, kw)) {
                matched = ix;
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
        if (matched == null and prefix_matches == 0 and lost_matches == 0) {
            return .NoMatch;
        }
        if (matched == null and prefix_matches > 0) {
            return .Prefix;
        }
        if (matched) |m| {
            if (prefix_matches == 0) {
                return .{
                    .ExactMatch = m,
                };
            }
            return .{
                .PrefixAndExact = m,
            };
        }
        std.debug.assert(matched == null and prefix_matches == 0 and lost_matches != 0);
        return .MatchLost;
    }
};

pub const Operator = struct {
    id: Id,
    precedence: u8,
    associative: enum {
        Left,
        Right,
    } = .Left,

    pub const Id = union(enum) {
        Keyword: usize,
        Symbol: u8,
    };
};

pub const Operators = struct {
    operators: []const Operator = &[0]Operator{},

    pub fn init(comptime ops: []const Operator) Operators {
        return .{
            .operators = ops,
        };
    }

    pub fn from_token(this: Operators, token: Token) ?Operator {
        for (this.operators) |op| {
            switch (op.id) {
                .Keyword => |kw| if (token.payload == .Keyword and token.payload.Keyword == kw) return op,
                .Symbol => |s| if (token.payload == .Symbol and token.payload.Symbol == s) return op,
                else => {},
            }
        }
        return null;
    }
};

pub const Token = struct {
    pos: u64 = 0,
    line: u64 = 0,
    col: u64 = 0,
    raw_text: []const u8,
    text: []const u8,
    kind: Kind,

    fn init(kind: Kind, text: []const u8) Token {
        return .{
            .kind = kind,
            .raw_text = text,
            .text = text,
        };
    }

    pub fn decode(s: []const u8, lexer: Lexer) !Token {
        var v = s;
        const kind_str: []const u8 = if (std.mem.indexOfScalar(u8, s, ':')) |colon| blk: {
            v = s[colon + 1 ..];
            break :blk s[0..colon];
        } else "Identifier";
        const k: KindTag = @enumFromInt(blk: {
            inline for (@typeInfo(KindTag).@"enum".fields) |fld| {
                if (std.ascii.eqlIgnoreCase(kind_str, fld.name)) {
                    break :blk fld.value;
                }
            }
            break :blk @intFromEnum(KindTag.Identifier);
        });
        return switch (k) {
            .Null => init(.Null, v),
            .EOF => init(.EOF, v),
            .Identifier => init(.Identifier, v),
            .Keyword => init(.{
                .Keyword = blk: {
                    for (lexer.config.keywords.keywords.keywords, 0..) |kw, ix| {
                        if (std.ascii.eqlIgnoreCase(kw, v)) {
                            break :blk ix;
                        }
                    }
                    return error.UnknownKeyword;
                },
            }, v),
            .Newline => init(.Newline, v),
            .Number => init(.Number, v),
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

    pub const Kind = union(KindTag) {
        Null: void,
        EOF: void,
        Identifier: void,
        Keyword: Keyword,
        Newline: void,
        Number: void,
        String: u8,
        Symbol: u8,
        Whitespace: void,
        Comment: []const u8,

        pub fn format(this: Kind, comptime fmt: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            if (std.mem.eql(u8, fmt, "s")) {
                switch (this) {
                    .Keyword => |k| try w.print("\"{}\"", .{k}),
                    .String, .Symbol => |c| try w.print("'{c}'", .{c}),
                    .Comment => |marker| try w.print("{s}", .{marker}),
                    else => try w.print("'{s}'", .{@tagName(this)}),
                }
            } else {
                switch (this) {
                    .Keyword => |k| try w.print(".{{ .Keyword = {} }}", .{k}),
                    .String, .Symbol => |c| try w.print(".{{ .{s} = '{c}' }}", .{ @tagName(this), c }),
                    .Comment => |marker| try w.print(".{{ .{s} = \"{s}\" }}", .{ @tagName(this), marker }),
                    else => try w.print(".{s}", .{@tagName(this)}),
                }
            }
        }
    };
};

pub const Config = struct {
    allocator: Allocator,
    comment: Comment,
    identifier: struct {
        on: bool = true,

        const Self = @This();

        fn configure(this: *Self, key: []const u8, value: ?[]const u8) !void {
            _ = this;
            _ = key;
            _ = value;
        }
    },

    keywords: struct {
        on: bool = true,
        keywords: Keywords = .{},
        operators: Operators = .{},

        const Self = @This();

        fn configure(this: *Self, key: []const u8, value: ?[]const u8) !void {
            _ = this;
            _ = key;
            _ = value;
        }
    },

    number: struct {
        on: bool = true,
        signed: bool = true,

        const Self = @This();

        fn configure(this: *Self, key: []const u8, value: ?[]const u8) !void {
            _ = this;
            _ = key;
            _ = value;
        }
    },

    qstring: struct {
        on: bool = true,
        quotes: []const u8 = "\"'`",

        const Self = @This();

        fn configure(this: *Self, key: []const u8, value: ?[]const u8) !void {
            _ = this;
            _ = key;
            _ = value;
        }
    },

    whitespace: struct {
        on: bool = true,
        ignore_ws: bool = false,
        ignore_nl: bool = false,

        const Self = @This();

        fn configure(this: *Self, key: []const u8, value: ?[]const u8) !void {
            _ = this;
            _ = key;
            _ = value;
        }
    },

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
    };

    pub fn init(allocator: Allocator) Config {
        return .{
            .allocator = allocator,
            .comment = .{
                .block_marker = std.ArrayList(Comment.BlockMarker).init(allocator),
                .eol_marker = std.ArrayList([]const u8).init(allocator),
            },
            .identifier = .{},
            .keywords = .{},
            .number = .{},
            .qstring = .{},
            .whitespace = .{},
        };
    }

    fn config_scanner(this: *Config, comptime scanner: std.builtin.Type.StructField, config: []const u8) !void {
        var c = config;
        while (c.len > 0) {
            const eq_ix = if (std.mem.indexOfScalar(u8, c, '=')) |eq| eq else c.len;
            const key = std.mem.trim(u8, c[0..eq_ix], " \t");
            c = if (eq_ix < c.len) c[eq_ix + 1 ..] else "";
            const value: ?[]const u8 = if (c.len == 0) null else blk: {
                var ix: usize = 0;
                while (ix < c.len and c[ix] != ';') {
                    var all_whitespace = true;
                    switch (c[ix]) {
                        '"' => {
                            ix += 1;
                            if (all_whitespace) {
                                const start_ix = ix;
                                while (ix < c.len and c[ix] != '"') {
                                    ix += if (c[ix] == '\\') 2 else 1;
                                }
                                const v = if (ix < c.len) c[start_ix..ix] else return error.MalformedLexerConfigString;
                                ix += 1;
                                while (ix < c.len and c[ix] != ';') {
                                    if (c[ix] != ' ' and c[ix] != '\t') {
                                        return error.MalformedLexerConfigString;
                                    }
                                    ix += 1;
                                }
                                c = if (ix < c.len - 1) c[ix + 1 ..] else "";
                                break :blk v;
                            }
                            all_whitespace = false;
                        },
                        '\\' => {
                            all_whitespace = false;
                            ix += 2;
                        },
                        ' ', '\t' => ix += 1,
                        else => {
                            all_whitespace = false;
                            ix += 1;
                        },
                    }
                }
                const v = if (ix < c.len) c[0..ix] else c;
                c = if (ix < c.len - 1) c[ix + 1 ..] else "";
                break :blk std.mem.trim(u8, v, " \t");
            };
            configure: {
                inline for (@typeInfo(scanner.type).@"struct".fields) |fld| {
                    if (std.ascii.eqlIgnoreCase(fld.name, key)) {
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
        this.filter.deinit();
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
            .pos = this.pos,
            .line = this.line,
            .col = this.col,
            .raw_text = if (this.source.len == 0 or length == 0) "" else this.source[0..length],
            .text = this.unescape(if (this.source.len == 0 or length == 0) "" else this.source[0..length]),
            .kind = kind,
        };
        return this.current.?;
    }

    fn scan_comment(this: *Lexer) ?Token {
        if (this.pos == 0 and this.config.comment.hashpling and std.mem.startsWith(u8, this.source, "#!")) {
            const len = if (std.mem.indexOfScalar(u8, this.source, '\n')) |p| p else this.source.len;
            const ret = this.buildToken(len, .{ .Comment = "#!" });
            this.line = 1;
            this.col = 0;
            return ret;
        }
        for (this.config.comment.eol_marker.items) |marker| {
            if (std.mem.startsWith(u8, this.source, marker)) {
                const len = if (std.mem.indexOfScalar(u8, this.source, '\n')) |p| p else this.source.len;
                const ret = this.buildToken(len, .{ .Comment = marker });
                this.line += 1;
                this.col = 0;
                return ret;
            }
        }
        for (this.config.comment.block_marker.items) |marker| {
            if (std.mem.startsWith(u8, this.source, marker.start)) {
                const len = if (std.mem.indexOf(u8, this.source, marker.end)) |p| p + marker.end.len else this.source.len;
                const ret = this.buildToken(len, .{ .Comment = marker.start });
                const newlines = std.mem.count(u8, this.source[0..len], "\n");
                if (newlines > 0) {
                    this.line += newlines;
                    this.col = len - std.mem.lastIndexOfScalar(u8, this.source[0..len], '\n').?;
                }
                return ret;
            }
        }
        return null;
    }

    pub fn advance(this: *Lexer) void {
        // std.debug.print("advance() ", .{});
        const token = this.current orelse unreachable;
        this.pos += token.text.len;
        this.source = this.source[token.raw_text.len..];
        this.current = null;
    }

    pub fn next(this: *Lexer) ?Token {
        if (this.peek_next()) |_| {
            this.advance();
            return;
        }
        return null;
    }

    pub fn peek_next(this: *Lexer) ?Token {
        return blk: {
            while (true) {
                if (this.peek()) |t| {
                    if (!this.ignore.contains(t.kind)) {
                        // std.debug.print("peek_next() {} ", .{t});
                        break :blk t;
                    }
                    this.advance();
                } else {
                    // std.debug.print("peek_next() NULL! ", .{});
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
            if (this.scan_comment()) |t| {
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
            if (std.ascii.isDigit(this.source[0]) or (this.config.number.signed and (this.source[0] == '+' or this.source[0] == '-'))) {
                var p: u64 = 1;
                while (p < this.source.len and std.ascii.isDigit(this.source[p])) {
                    p += 1;
                }
                return this.buildToken(p, .Number);
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
                switch (this.config.keywords.keywords.match(this.source[0..p])) {
                    .ExactMatch => |kw| return this.buildToken(p, .{ .Keyword = kw }),
                    .PrefixAndExact => |kw| return this.buildToken(this.config.keywords.keywords.keywords[kw].len, .{ .Keyword = kw }),
                    else => {},
                }
            }
            if (this.config.identifier.on) {
                return this.buildToken(p, .Identifier);
            }
        }
        if (this.config.keywords.on) {
            var matched: ?Keyword = null;
            for (1..this.source.len) |l| {
                switch (this.config.keywords.keywords.match(this.source[0..l])) {
                    .ExactMatch => |kw| return this.buildToken(l, .{ .Keyword = kw }),
                    .NoMatch => break,
                    .Prefix => {},
                    .PrefixAndExact => |kw| matched = kw,
                    .MatchLost => return this.buildToken(l, .{ .Keyword = matched orelse unreachable }),
                }
            }
        }
        return this.buildToken(1, .{ .Symbol = this.source[0] });
    }

    pub fn accept_keyword(this: *Lexer, keyword: Keyword) bool {
        if (this.peek_next()) |t| {
            switch (t.kind) {
                .Keyword => |kw| {
                    if (keyword == kw) {
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
    try config.configure("qstring", "quotes=\"@#\"");
    try std.testing.expectEqualStrings("@#", config.qstring.quotes);
    try config.configure("qstring", "quotes=\"12\";quotes=34");
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
    config.keywords.keywords.keywords = &[_][]const u8{
        "int",
        "char",
        "return",
    };
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
