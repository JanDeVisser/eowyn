const std = @import("std");

const Allocator = std.mem.Allocator;

const lxr = @import("lexer.zig");
const grm = @import("grammar.zig");

const Set = grm.Set;
const Resolver = grm.Resolver;
const Lexer = lxr.Lexer;
const LexerConfig = lxr.Config;
const Token = lxr.Token;
const Grammar = grm.Grammar;
const Rule = grm.Rule;
const Sequence = grm.Sequence;
const Symbol = grm.Symbol;

const Parser = struct {
    allocator: Allocator,
    grammar: Grammar,
    prod_stack: std.ArrayList(Symbol),
    last_token: Token,

    pub fn init(allocator: Allocator, grammar: Grammar) Parser {
        return .{
            .allocator = allocator,
            .grammar = grammar,
            .prod_stack = std.ArrayList(Symbol).init(allocator),
            .last_token = Token.init(.Null, ""),
        };
    }

    pub fn parse(this: *Parser, source: []const u8) !void {
        this.prod_stack.clearRetainingCapacity();
        std.debug.print("\n", .{});
        try this.prod_stack.append(.{ .NonTerminal = this.grammar.entry_point orelse @panic("No entry point") });
        var lexer = try Lexer.init(this.allocator, this.grammar.lexer, source);
        while (lexer.next()) |token| {
            this.last_token = token;
            if (token.kind == .EOF) {
                while (this.prod_stack.items.len > 0) {
                    const s = this.prod_stack.pop();
                    switch (s) {
                        .NonTerminal => |nt| {
                            const rule = this.grammar.rules.get(nt) orelse unreachable;
                            if (!rule.firsts.contains(.Empty)) {
                                return error.SyntaxError;
                            }
                        },
                        .Action => |*action| {
                            try action.call(this);
                        },
                        else => return error.SyntaxError,
                    }
                }
                return;
            }
            std.debug.print("{}\n", .{token});
            var consumed = false;
            while (!consumed) {
                std.debug.print("  ", .{});
                for (this.prod_stack.items) |s| {
                    std.debug.print("{} ", .{s});
                }
                std.debug.print("\n", .{});
                if (this.prod_stack.popOrNull()) |s| {
                    switch (s) {
                        .NonTerminal => |nt| {
                            const rule = this.grammar.rules.get(nt) orelse unreachable;
                            if (rule.parse_table.get(Symbol{ .Terminal = token.kind })) |seq_id| {
                                const seq: Sequence = rule.sequences.items[seq_id];
                                std.mem.reverse(Symbol, seq.symbols.items);
                                for (seq.symbols.items) |entry| {
                                    try this.prod_stack.append(entry);
                                }
                                std.mem.reverse(Symbol, seq.symbols.items);
                            } else {
                                std.debug.print("Unexpected token '{}'\n", .{token});
                                return error.SyntaxError;
                            }
                        },
                        .Terminal => |t| {
                            if (!std.meta.eql(t, token.kind)) {
                                std.debug.print("Expected '{}', got '{}'\n", .{ t, token.kind });
                                return error.SyntaxError;
                            }
                            consumed = true;
                        },
                        .Action => |*action| {
                            try action.call(this);
                        },
                        else => {},
                    }
                } else {
                    std.debug.print("Production stack underflow\n", .{});
                    return error.SyntaxError;
                }
            }
        }
    }
};

test "Parse" {
    const grammar = try grm.build_test_grammar();
    var parser = Parser.init(std.heap.c_allocator, grammar);
    try parser.parse("(5*8)+(2*1)");
}
