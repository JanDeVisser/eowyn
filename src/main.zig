const std = @import("std");
const Allocator = std.mem.Allocator;

const grm = @import("grammar.zig");
const lxr = @import("lexer.zig");

const Grammar = grm.Grammar;
const Rule = grm.Rule;
const Sequence = grm.Sequence;
const Symbol = grm.Symbol;

fn add_rule(grammar: *Grammar, nt: []const u8, symbols: []const []const u8) !*Rule {
    var r = Rule.init(grammar, nt);
    if (symbols.len > 0) {
        var seq = Sequence.init(r.grammar);
        for (symbols) |s| {
            try seq.symbols.append(.{ .NonTerminal = s });
        }
        try r.sequences.append(seq);
    }
    try grammar.rules.put(r.non_terminal, r);
    return grammar.rules.getPtr(r.non_terminal) orelse unreachable;
}

fn add_sequence(rule: *Rule, symbols: []const Symbol) !void {
    var seq = Sequence.init(rule.grammar);
    for (symbols) |s| {
        try seq.symbols.append(s);
    }
    try rule.sequences.append(seq);
}

fn build_test_grammar() !Grammar {
    _ =
        \\E          := T Eopt ;
        \\Eopt       := '+' T Eopt |  '-' T Eopt | ;
        \\T          := F Topt ;
        \\Topt       := '*' F Topt |  '/' F Topt | ;
        \\F          := 'd' |  '(' E ')' ;
        \\
    ;

    var grammar = try Grammar.init(std.heap.c_allocator);
    _ = try add_rule(&grammar, "E", &[_][]const u8{ "T", "Eopt" });
    var rule = try add_rule(&grammar, "Eopt", &[_][]const u8{});
    try add_sequence(rule, &[_]Symbol{ .{ .Terminal = .{ .Symbol = '+' } }, .{ .NonTerminal = "T" }, .{ .NonTerminal = "Eopt" } });
    try add_sequence(rule, &[_]Symbol{ .{ .Terminal = .{ .Symbol = '-' } }, .{ .NonTerminal = "T" }, .{ .NonTerminal = "Eopt" } });
    try add_sequence(rule, &[_]Symbol{});
    _ = try add_rule(&grammar, "T", &[_][]const u8{ "F", "Topt" });
    rule = try add_rule(&grammar, "Topt", &[_][]const u8{});
    try add_sequence(rule, &[_]Symbol{ .{ .Terminal = .{ .Symbol = '*' } }, .{ .NonTerminal = "F" }, .{ .NonTerminal = "Topt" } });
    try add_sequence(rule, &[_]Symbol{ .{ .Terminal = .{ .Symbol = '/' } }, .{ .NonTerminal = "F" }, .{ .NonTerminal = "Topt" } });
    try add_sequence(rule, &[_]Symbol{});
    rule = try add_rule(&grammar, "F", &[_][]const u8{});
    try add_sequence(rule, &[_]Symbol{.{ .Terminal = .Number }});
    try add_sequence(rule, &[_]Symbol{ .{ .Terminal = .{ .Symbol = '(' } }, .{ .NonTerminal = "E" }, .{ .Terminal = .{ .Symbol = ')' } } });
    _ = try grammar.build_parse_table();
    return grammar;
}

pub fn main() !void {
    const grammar = try build_test_grammar();
    try std.testing.expect(try grammar.check_LL1());
}
