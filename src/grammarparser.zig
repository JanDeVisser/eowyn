const std = @import("std");
const hashset = @import("hashset.zig");
const lxr = @import("lexer.zig");
const grm = @import("grammar.zig");

const GrammarParser = struct {
    allocator: std.mem.Allocator,
    lexer: lxr.Lexer,
    grammar: grm.Grammar,
    keywords: std.StringHashMap(usize),

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !GrammarParser {
        var ret = GrammarParser{
            .allocator = allocator,
            .lexer = try lxr.Lexer.init(allocator, source),
            .grammar = try grm.Grammar.init(allocator),
            .keywords = std.StringHashMap(usize).init(allocator),
        };
        try ret.lexer.ignore.add(.Whitespace);
        try ret.lexer.ignore.add(.Newline);
        try ret.lexer.ignore.add(.Comment);
        return ret;
    }

    pub fn deinit(this: *GrammarParser) void {
        this.keywords.deinit();
    }

    fn grammar_config(this: *GrammarParser) !void {
        try this.lexer.expect_symbol('%');
        while (this.lexer.peek_next()) |t| {
            switch (t.kind) {
                .Identifier => {
                    const name = t.text;
                    this.lexer.advance();
                    try this.lexer.expect_symbol(':');
                    if (this.lexer.peek_next()) |v| {
                        const value: []const u8 = switch (v.kind) {
                            .Identifier, .Number, .Keyword => v.text,
                            .String => std.mem.trim(u8, v.text[1 .. v.text.len - 1], " \t"),
                            else => return error.MalformedConfigSection,
                        };
                        try this.grammar.configure(name, value);
                        this.lexer.advance();
                    } else {
                        return error.MalformedConfigSection;
                    }
                },
                .Symbol => |c| {
                    if (c != '%') {
                        return error.MalformedConfigSection;
                    }
                    this.lexer.advance();
                    return;
                },
                else => return error.MalformedConfigSection,
            }
        }
        return error.MalformedConfigSection;
    }

    fn parse_value(this: *GrammarParser) !grm.Value {
        if (this.lexer.peek_next()) |t| {
            this.lexer.advance();
            return try grm.Value.decode(this.lexer, switch (t.kind) {
                .Identifier => t.text,
                .String => t.text[1 .. t.text.len - 1],
                else => return error.MalformedActionData,
            });
        }
        return error.MalformedActionData;
    }

    fn parse_actions(this: *GrammarParser, rule: *grm.Rule) !void {
        try this.lexer.expect_symbol('[');
        while (this.lexer.peek_next()) |t| {
            switch (t.kind) {
                .Symbol => |c| {
                    switch (c) {
                        ']' => {
                            this.lexer.advance();
                            return;
                        },
                        else => return error.MalformedAction,
                    }
                },
                .Identifier => {
                    const name = t.text;
                    this.lexer.advance();
                    const data: ?grm.Value = if (this.lexer.accept_symbol(':')) try this.parse_value() else null;
                    try rule.entries.append(.{ .Action = try grm.GrammarAction.init(this.allocator, this.grammar.resolver(), name, data) });
                },
                else => return error.MalformedAction,
            }
        }
        return error.MalformedAction;
    }

    fn parse_non_terminal(this: *GrammarParser) !void {
        const name = try this.lexer.expect_identifier();
        try this.lexer.expect_symbol(':');
        try this.lexer.expect_symbol('=');
        var nt = grm.NonTerminal.init(this.allocator, name.text);
        var rule = grm.Rule.init(this.allocator);
        while (this.lexer.peek_next()) |t| {
            switch (t.kind) {
                .Symbol => |c| {
                    switch (c) {
                        '[' => try this.parse_actions(&rule),
                        ';' => break,
                        '|' => {
                            try nt.rules.append(rule);
                            rule = grm.Rule.init(this.allocator);
                            this.lexer.advance();
                        },
                        else => {
                            try rule.entries.append(.{ .Terminal = .{ .Symbol = c } });
                            this.lexer.advance();
                        },
                    }
                },
                .Identifier => {
                    try rule.entries.append(.{ .NonTerminal = t.text });
                    this.lexer.advance();
                },
                .String => |q| {
                    switch (q) {
                        '\'' => {
                            switch (t.text[1]) {
                                'i' => try rule.entries.append(.{ .Terminal = .Identifier }),
                                'd' => try rule.entries.append(.{ .Terminal = .Number }),
                                '"', '\'' => try rule.entries.append(.{ .Terminal = .{ .String = t.text[1] } }),
                                else => try rule.entries.append(.{ .Terminal = .{ .Symbol = t.text[1] } }),
                            }
                            this.lexer.advance();
                        },
                        '"' => {
                            const kw = t.text[1 .. t.text.len - 1];
                            const code = this.keywords.get(kw) orelse blk: {
                                const new_code = this.keywords.count();
                                try this.keywords.put(kw, new_code);
                                break :blk new_code;
                            };
                            try rule.entries.append(.{ .Terminal = .{ .Keyword = code } });
                            this.lexer.advance();
                        },
                        else => return error.MalformedProduction,
                    }
                },
                .EOF => break,
                else => return error.MalformedProduction,
            }
        }
        try nt.rules.append(rule);
        try this.grammar.non_terminals.put(nt.name, nt);
        this.lexer.advance();
    }

    pub fn parse(this: *GrammarParser) !void {
        while (this.lexer.peek_next()) |t| {
            switch (t.kind) {
                .Symbol => |c| {
                    switch (c) {
                        '%' => try this.grammar_config(),
                        else => return error.UnexpectedSymbol,
                    }
                },
                .Identifier => try this.parse_non_terminal(),
                .EOF => return,
                else => return error.UnexpectedToken,
            }
        }
    }
};

export fn init(target: *anyopaque, data: ?*grm.Value) void {
    _ = target;
    std.debug.print("init({})\n", .{data.?});
}

export fn done(target: *anyopaque, data: ?*grm.Value) void {
    _ = target;
    std.debug.print("done({})\n", .{data.?});
}

export fn stmt_start(target: *anyopaque, data: ?*grm.Value) void {
    _ = target;
    std.debug.print("stmt_start({})\n", .{data.?});
}

export fn stmt_end(target: *anyopaque, data: ?*grm.Value) void {
    _ = target;
    std.debug.print("stmt_end({})\n", .{data.?});
}

test "Grammar Parser" {
    var gp = try GrammarParser.init(std.heap.c_allocator,
        \\ 
        \\program    := [ init ] statements [ done ]
        \\           ;
        \\
        \\statements := [ stmt_start ] statement [ stmt_end ] statements
        \\           |
        \\           ;
        \\
    );
    try gp.parse();
    std.debug.print("{}", .{gp.grammar});
}
