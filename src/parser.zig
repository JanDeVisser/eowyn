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

pub fn Parser(comptime T: type) type {
    return struct {
        allocator: Allocator,
        grammar: Grammar,
        prod_stack: std.ArrayList(Symbol),
        last_token: Token,
        impl: T,

        const P = @This();
        const ParserFnc = *fn (this: *P) callconv(.C) void;

        pub fn init(allocator: Allocator, grammar: Grammar) @This() {
            return .{
                .allocator = allocator,
                .grammar = grammar,
                .prod_stack = std.ArrayList(Symbol).init(allocator),
                .last_token = Token.init(.Null, ""),
                .impl = T.init(allocator, grammar),
            };
        }

        pub fn deinit(this: *P) void {
            this.impl.deinit();
            this.prod_stack.deinit();
        }

        pub fn parse(this: *P, source: []const u8) !void {
            this.prod_stack.clearRetainingCapacity();
            // std.debug.print("\n", .{});
            // this.grammar.dump_parse_table();
            // std.debug.print("\n", .{});
            try this.prod_stack.append(.{ .NonTerminal = this.grammar.entry_point orelse @panic("No entry point") });
            var lexer = try Lexer.init(this.allocator, this.grammar.lexer, source);
            defer lexer.deinit();
            this.impl.startup();
            defer this.impl.cleanup();
            while (lexer.next()) |token| {
                this.last_token = token;
                // std.debug.print("{}\n", .{token});
                var consumed = false;
                var done = false;
                while (!done) {
                    blk: {
                        // std.debug.print("  ", .{});
                        // for (this.prod_stack.items) |s| {
                        //     std.debug.print("{} ", .{s});
                        // }
                        // std.debug.print("\n", .{});
                        if (this.prod_stack.popOrNull()) |s| {
                            switch (s) {
                                .NonTerminal => |nt| {
                                    if (consumed) {
                                        try this.prod_stack.append(s);
                                        done = true;
                                        break :blk;
                                    }
                                    const rule = this.grammar.rules.get(nt) orelse unreachable;
                                    if (rule.parse_table.get(Symbol{ .Terminal = token.kind })) |seq_id| {
                                        const seq: Sequence = rule.sequences.items[seq_id];
                                        std.mem.reverse(Symbol, seq.symbols.items);
                                        for (seq.symbols.items) |entry| {
                                            try this.prod_stack.append(entry);
                                        }
                                        std.mem.reverse(Symbol, seq.symbols.items);
                                    } else if (token.kind != .EOF) {
                                        std.debug.print("Unexpected token '{}'\n", .{token});
                                        rule.dump_parse_table();
                                        return error.SyntaxError;
                                    }
                                },
                                .Terminal => |t| {
                                    if (consumed) {
                                        try this.prod_stack.append(s);
                                        done = true;
                                        break :blk;
                                    }
                                    if (!t.eql(token.kind)) {
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
                            if (token.kind == .EOF) {
                                return;
                            }
                            std.debug.print("Production stack underflow\n", .{});
                            return error.SyntaxError;
                        }
                    }
                }
            }
        }
    };
}

test "Parse" {
    const grammar = try grm.build_test_grammar();
    var parser = Parser.init(std.heap.c_allocator, grammar);
    try parser.parse("(5*8)+(2*1)");
}
