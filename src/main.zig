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

fn pop(this: *parser.Parser) i64 {
    var stack: *std.ArrayList(grm.Value) = @alignCast(@ptrCast(this.impl orelse @panic("Stack not initialized")));
    return if (stack.popOrNull()) |v| blk: {
        switch (v) {
            .Int => |i| break :blk i,
            else => @panic("Invalid value on stack"),
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
        // else => std.debug.panic("Invalid token '{s}' in push_number", .{this.last_token}),
        else => @panic("Invalid token in push_number"),
    }
}

pub fn main() !void {
    var gp = try GrammarParser.init(std.heap.c_allocator, eowyn_grammar);
    var grammar = Grammar.init(std.heap.c_allocator);
    try gp.parse(&grammar);
    var p = parser.Parser.init(std.heap.c_allocator, grammar);
    try p.parse(
        \\func main() {
        \\  x = 42;
        \\}
        \\
    );
}
