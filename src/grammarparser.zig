const std = @import("std");
const hashset = @import("hashset.zig");
const lxr = @import("lexer.zig");
const grm = @import("grammar.zig");

const Grammar = grm.Grammar;

pub const GrammarParser = struct {
    allocator: std.mem.Allocator,
    lexer: lxr.Lexer,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !GrammarParser {
        var config = lxr.Config.init(allocator);
        config.number.signed = false;
        try config.comment.eol_marker.append("//");
        try config.keywords.addAll(&[_][]const u8{
            "#binary",
            "#float",
            "#hex",
            "#ident",
            "#int",
        });
        var ret = GrammarParser{
            .allocator = allocator,
            .lexer = try lxr.Lexer.init(allocator, config, source),
        };
        _ = try ret.lexer.ignore.add(.Whitespace);
        _ = try ret.lexer.ignore.add(.Newline);
        _ = try ret.lexer.ignore.add(.Comment);
        return ret;
    }

    pub fn deinit(this: *GrammarParser) void {
        this.lexer.deinit();
    }

    fn grammar_config(this: *GrammarParser, grammar: *Grammar) !void {
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
                        try grammar.configure(name, value);
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
            std.debug.print("parse_value {}\n", .{t});
            this.lexer.advance();
            var buf: [1024]u8 = undefined;
            return try grm.Value.decode(this.lexer, switch (t.kind) {
                .Identifier => t.text,
                .String => t.text[1 .. t.text.len - 1],
                .Number => |number_type| try std.fmt.bufPrint(&buf, "{s}:{s}", .{ @tagName(number_type), t.text }),
                else => return error.MalformedActionData,
            });
        }
        return error.MalformedActionData;
    }

    fn parse_actions(this: *GrammarParser, grammar: *Grammar, seq: *grm.Sequence) !void {
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
                    std.debug.print("name {s} next {}\n", .{ name, this.lexer.peek_next().? });
                    const data: ?grm.Value = if (this.lexer.accept_symbol(':')) try this.parse_value() else null;
                    try seq.symbols.append(.{ .Action = try grm.GrammarAction.init(this.allocator, grammar.resolver, name, data) });
                },
                else => return error.MalformedAction,
            }
        }
        return error.MalformedAction;
    }

    fn parse_non_terminal(this: *GrammarParser, grammar: *Grammar) !void {
        const name = try this.lexer.expect_identifier();
        try this.lexer.expect_symbol(':');
        try this.lexer.expect_symbol('=');
        var rule = grm.Rule.init(grammar, name.text);
        var seq = grm.Sequence.init(grammar);
        while (this.lexer.peek_next()) |t| {
            switch (t.kind) {
                .Symbol => |c| {
                    switch (c) {
                        '[' => try this.parse_actions(grammar, &seq),
                        ';' => break,
                        '|' => {
                            try rule.sequences.append(seq);
                            seq = grm.Sequence.init(grammar);
                            this.lexer.advance();
                        },
                        else => {
                            try seq.symbols.append(.{ .Terminal = .{ .Symbol = c } });
                            this.lexer.advance();
                        },
                    }
                },
                .Identifier => {
                    try seq.symbols.append(.{ .NonTerminal = t.text });
                    this.lexer.advance();
                },
                .Keyword => |kw| {
                    if (std.mem.eql(u8, kw, "#ident")) {
                        try seq.symbols.append(.{ .Terminal = .Identifier });
                    } else if (std.mem.eql(u8, kw, "#int")) {
                        try seq.symbols.append(.{ .Terminal = .{ .Number = .Int } });
                    } else if (std.mem.eql(u8, kw, "#hex")) {
                        try seq.symbols.append(.{ .Terminal = .{ .Number = .Hex } });
                    } else if (std.mem.eql(u8, kw, "#binary")) {
                        try seq.symbols.append(.{ .Terminal = .{ .Number = .Binary } });
                    } else if (std.mem.eql(u8, kw, "#float")) {
                        try seq.symbols.append(.{ .Terminal = .{ .Number = .Float } });
                    } else {
                        std.debug.panic("Unexpected keyword '{s}'", .{kw});
                    }
                    this.lexer.advance();
                },
                .String => |q| {
                    switch (q) {
                        '\'' => {
                            switch (t.text[1]) {
                                '"', '\'' => try seq.symbols.append(.{ .Terminal = .{ .String = t.text[1] } }),
                                else => try seq.symbols.append(.{ .Terminal = .{ .Symbol = t.text[1] } }),
                            }
                            this.lexer.advance();
                        },
                        '"' => {
                            const kw = t.text[1 .. t.text.len - 1];
                            try grammar.lexer.keywords.add(kw);
                            try seq.symbols.append(.{ .Terminal = .{ .Keyword = kw } });
                            this.lexer.advance();
                        },
                        else => return error.MalformedProduction,
                    }
                },
                .EOF => break,
                else => {
                    std.debug.print("{s}\n", .{t});
                    return error.MalformedProduction;
                },
            }
        }
        try rule.sequences.append(seq);
        try grammar.rules.put(rule.non_terminal, rule);
        if (grammar.rules.count() == 1) {
            std.debug.assert(grammar.entry_point == null);
            grammar.entry_point = rule.non_terminal;
        }
        this.lexer.advance();
    }

    pub fn parse(this: *GrammarParser, grammar: *Grammar) !void {
        while (this.lexer.peek_next()) |t| {
            switch (t.kind) {
                .Symbol => |c| {
                    switch (c) {
                        '%' => try this.grammar_config(grammar),
                        else => return error.UnexpectedSymbol,
                    }
                },
                .Identifier => try this.parse_non_terminal(grammar),
                .EOF => break,
                else => return error.UnexpectedToken,
            }
        }
        try grammar.build_parse_table();
    }
};

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
    var grammar = Grammar.init(std.heap.c_allocator);
    try gp.parse(&grammar);
    std.debug.print("\n{}", .{grammar});
}

test "Firsts" {
    var gp = try GrammarParser.init(std.heap.c_allocator,
        \\
        \\E          := T Eopt
        \\           ;
        \\
        \\Eopt       := '+' T Eopt
        \\           |  '-' T Eopt
        \\           |
        \\           ;
        \\
        \\T          := F Topt
        \\           ;
        \\
        \\Topt       := '*' F Topt
        \\           |  '/' F Topt
        \\           |
        \\           ;
        \\
        \\F          := 'd'
        \\           |  '(' E ')'
        \\           ;
        \\
    );
    var grammar = Grammar.init(std.heap.c_allocator);
    try gp.parse(&grammar);
    try grammar.build_firsts();
    std.debug.print("\n", .{});
    for (grammar.rules.values()) |*rule| {
        std.debug.print("Firsts {s}: {}\n", .{ rule.non_terminal, rule.firsts });
    }
}

test "Follows" {
    var gp = try GrammarParser.init(std.heap.c_allocator,
        \\
        \\E          := T Eopt ;
        \\Eopt       := '+' T Eopt |  '-' T Eopt | ;
        \\T          := F Topt ;
        \\Topt       := '*' F Topt |  '/' F Topt | ;
        \\F          := 'd' |  '(' E ')' ;
        \\
    );
    var grammar = Grammar.init(std.heap.c_allocator);
    try gp.parse(&grammar);
    try grammar.build_firsts();
    try grammar.build_follows();
    std.debug.print("\n", .{});
    for (grammar.rules.values()) |*rule| {
        std.debug.print("Follows {s}: {}\n", .{ rule.non_terminal, rule.follows });
    }
}
