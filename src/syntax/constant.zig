const std = @import("std");
const lexer = @import("../lexer.zig");

pub const Number = struct {
    number: []const u8,
    number_type: lexer.NumberType,
};

pub const BoolConstant = struct {
    value: bool,
};

pub const Nullptr = struct {};

pub const QuotedString = struct {
    string: []const u8,
    quote_type: lexer.QuoteType,
};
