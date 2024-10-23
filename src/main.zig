//
// Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
//
// SPDX-License-Identifier: MIT
//

const std = @import("std");
const Allocator = std.mem.Allocator;

const eowyn = @import("eowyn.zig");
const grm = @import("grammar.zig");
const lxr = @import("lexer.zig");
const grmparser = @import("grammarparser.zig");
const parse = @import("parser.zig");

const Parser = parse.Parser;
const Token = lxr.Token;
const Grammar = grm.Grammar;
const Rule = grm.Rule;
const Sequence = grm.Sequence;
const Symbol = grm.Symbol;
const GrammarParser = grmparser.GrammarParser;

const expr_test =
    \\func main() {
    \\  println("Hello, World!");
    \\  println(42);
    \\  println(3.14);
    \\  println(true);
    \\  println(34 + 35);
    \\  println(-34 + 35);
    \\}
;

const if_test =
    \\func main() {
    \\  if (foo(x)) {
    \\    println("ok");
    \\  }
    \\}
    \\
;

const if_else_test =
    \\func main() {
    \\  if (foo(32)) {
    \\    println("ok");
    \\  } else {
    \\    println("not ok");
    \\  }
    \\}
    \\
;

const loop_test =
    \\func main() {
    \\  #blk loop {
    \\    println("ok");
    \\  }
    \\}
    \\
;

const tests = &[_][]const u8{
    expr_test,
    if_test,
    if_else_test,
    loop_test,
};

pub fn main() !void {
    var gp = try GrammarParser.init(std.heap.c_allocator, eowyn.eowyn_grammar);
    var grammar = Grammar.init(std.heap.c_allocator);
    try gp.parse(&grammar);
    // grammar.dump();
    var p = eowyn.P.init(std.heap.c_allocator, grammar);

    for (tests) |t| {
        try p.parse(t);
    }
}
