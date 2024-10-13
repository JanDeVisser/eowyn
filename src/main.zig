const std = @import("std");
const Allocator = std.mem.Allocator;

const grm = @import("grammar.zig");
const lxr = @import("lexer.zig");
const grmparser = @import("grammarparser.zig");
const parser = @import("parser.zig");

const Grammar = grm.Grammar;
const Rule = grm.Rule;
const Sequence = grm.Sequence;
const Symbol = grm.Symbol;
const GrammarParser = grmparser.GrammarParser;

const eowyn_grammar: []const u8 = @embedFile("eowyn.grammar");

const calc_grammar =
    \\%
    \\  lexer: "number: signed=false" 
    \\  parser: "init: init"
    \\  parser: "deinit: deinit"
    \\  parser: "startup: startup"
    \\  parser: "cleanup: cleanup"
    \\%
    \\Main       := [init] E [cleanup];
    \\E          := T Eopt ;
    \\Eopt       := '+' T [add] Eopt |  '-' T [subtract] Eopt | ;
    \\T          := F Topt ;
    \\Topt       := '*' F [multiply] Topt |  '/' F [divide] Topt | ;
    \\F          := 'd' [push_number] |  '(' E ')' ;
    \\
;

fn pop(this: *parser.Parser) i64 {
    var stack: *std.ArrayList(grm.Value) = @alignCast(@ptrCast(this.impl orelse @panic("Stack not initialized")));
    return if (stack.popOrNull()) |v| blk: {
        switch (v) {
            .Int => |i| break :blk i,
            else => std.debug.panic("Invalid value '{}' on stack", .{v}),
        }
    } else {
        @panic("pop(): stack underflow");
    };
}

fn push(this: *parser.Parser, i: i64) void {
    var stack: *std.ArrayList(grm.Value) = @alignCast(@ptrCast(this.impl orelse @panic("Stack not initialized")));
    stack.append(.{
        .Int = i,
    }) catch @panic("Out of memory");
}

export fn init(this: *parser.Parser) callconv(.C) void {
    this.impl = this.allocator.create(std.ArrayList(grm.Value)) catch @panic("Out of memory");
    const stack: *std.ArrayList(grm.Value) = @alignCast(@ptrCast(this.impl));
    stack.* = std.ArrayList(grm.Value).init(this.allocator);
}

export fn deinit(this: *parser.Parser) callconv(.C) void {
    std.debug.print("{}\n", .{pop(this)});
    const stack: *std.ArrayList(grm.Value) = @alignCast(@ptrCast(this.impl));
    this.allocator.destroy(stack);
}

export fn startup(this: *parser.Parser) void {
    const stack: *std.ArrayList(grm.Value) = @alignCast(@ptrCast(this.impl));
    stack.clearRetainingCapacity();
}

export fn cleanup(this: *parser.Parser) void {
    const stack: *std.ArrayList(grm.Value) = @alignCast(@ptrCast(this.impl));
    if (stack.popOrNull()) |v| {
        switch (v) {
            .Int => |i| std.debug.print("{}\n", .{i}),
            else => {},
        }
    }
    if (stack.items.len > 0) {
        @panic("Stack not empty on cleanup");
    }
}

export fn push_number(this: *parser.Parser, _: ?*grm.Value) callconv(.C) void {
    switch (this.last_token.kind) {
        .Number => {
            const v: grm.Value = .{ .Int = std.fmt.parseInt(i64, this.last_token.text, 0) catch @panic("Could not parse number") };
            var stack: *std.ArrayList(grm.Value) = @alignCast(@ptrCast(this.impl orelse @panic("Stack not initialized")));
            stack.append(v) catch @panic("Out of memory");
        },
        else => std.debug.panic("Invalid token '{}' in push_number", .{this.last_token}),
    }
}

export fn add(this: *parser.Parser, _: ?*grm.Value) callconv(.C) void {
    push(this, pop(this) + pop(this));
}

export fn subtract(this: *parser.Parser, _: ?*grm.Value) callconv(.C) void {
    // Second operand sits on top of the stack so we have to switch the
    // subtraction:
    push(this, -pop(this) + pop(this));
}

export fn multiply(this: *parser.Parser, _: ?*grm.Value) callconv(.C) void {
    push(this, pop(this) * pop(this));
}

export fn divide(this: *parser.Parser, _: ?*grm.Value) callconv(.C) void {
    // Denominator sits on top of the stack so we have to switch the
    // divide:
    const denominator = pop(this);
    push(this, @divFloor(pop(this), denominator));
}

pub fn main() !void {
    var gp = try GrammarParser.init(std.heap.c_allocator, calc_grammar);
    var grammar = Grammar.init(std.heap.c_allocator);
    try gp.parse(&grammar);
    var p = parser.Parser.init(std.heap.c_allocator, grammar);
    try p.parse("(5*8)+(2*1)");
}
