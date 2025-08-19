const std = @import("std");
const lexer = @import("lexer.zig");
const fatal = @import("fatal.zig");

pub const Precedence = u16;

pub const EowynKeyword = enum {
    AssignAnd,
    AssignDecrement,
    AssignDivide,
    AssignIncrement,
    AssignModulo,
    AssignMultiply,
    AssignOr,
    AssignShiftLeft,
    AssignShiftRight,
    AssignXor,
    Break,
    Cast,
    Const,
    Continue,
    Defer,
    Else,
    Embed,
    Enum,
    Equals,
    Error,
    ExternLink,
    False,
    For,
    Func,
    GreaterEqual,
    If,
    Import,
    Include,
    LessEqual,
    LogicalAnd,
    LogicalOr,
    Loop,
    NotEqual,
    Null,
    Public,
    Range,
    Return,
    ShiftLeft,
    ShiftRight,
    Sizeof,
    Struct,
    True,
    Var,
    While,
    Yield,

    const Self = @This();
    var strings: ?std.AutoHashMap(Self, []const u8) = null;
    pub fn match(s: []const u8) ?lexer.KeywordMatch(Self) {
        if (strings == null) {
            strings = std.AutoHashMap(Self, []const u8).init(std.heap.c_allocator);
            strings.?.put(.AssignAnd, "&=") catch fatal.oom();
            strings.?.put(.AssignDecrement, "-=") catch fatal.oom();
            strings.?.put(.AssignDivide, "/=") catch fatal.oom();
            strings.?.put(.AssignIncrement, "+=") catch fatal.oom();
            strings.?.put(.AssignModulo, "%=") catch fatal.oom();
            strings.?.put(.AssignMultiply, "*=") catch fatal.oom();
            strings.?.put(.AssignOr, "|=") catch fatal.oom();
            strings.?.put(.AssignShiftLeft, "<<=") catch fatal.oom();
            strings.?.put(.AssignShiftRight, ">>=") catch fatal.oom();
            strings.?.put(.AssignXor, "^=") catch fatal.oom();
            strings.?.put(.Equals, "==") catch fatal.oom();
            strings.?.put(.ExternLink, "->") catch fatal.oom();
            strings.?.put(.GreaterEqual, ">=") catch fatal.oom();
            strings.?.put(.LessEqual, "<=") catch fatal.oom();
            strings.?.put(.LogicalAnd, "&&") catch fatal.oom();
            strings.?.put(.LogicalOr, "||") catch fatal.oom();
            strings.?.put(.NotEqual, "!=") catch fatal.oom();
            strings.?.put(.ShiftLeft, "<<") catch fatal.oom();
            strings.?.put(.ShiftRight, ">>") catch fatal.oom();
        }
        return lexer.match_enum(Self, s, strings);
    }
};

pub const Position = enum {
    Prefix,
    Infix,
    Postfix,
    Closing,
};

pub const Associativity = enum {
    Left,
    Right,
};

pub const BinaryOperator = enum {
    Add,
    AddressOf,
    BinaryAnd,
    BinaryInvert,
    BinaryOr,
    BinaryXor,
    Call,
    Cast,
    Divide,
    Equals,
    Greater,
    GreaterEqual,
    Idempotent,
    Length,
    Less,
    LessEqual,
    LogicalAnd,
    LogicalInvert,
    LogicalOr,
    MemberAccess,
    Modulo,
    Multiply,
    Negate,
    NotEqual,
    Range,
    Sequence,
    ShiftLeft,
    ShiftRight,
    Sizeof,
    Subscript,
    Subtract,
};

pub const AssignmentOperator = enum {
    Assign,
    AssignAnd,
    AssignDecrement,
    AssignDivide,
    AssignIncrement,
    AssignModulo,
    AssignMultiply,
    AssignOr,
    AssignShiftLeft,
    AssignShiftRight,
    AssignXor,
};

pub const Operator = enum {
    Add,
    AddressOf,
    BinaryAnd,
    BinaryInvert,
    BinaryOr,
    BinaryXor,
    Call,
    Cast,
    Divide,
    Equals,
    Greater,
    GreaterEqual,
    Idempotent,
    Length,
    Less,
    LessEqual,
    LogicalAnd,
    LogicalInvert,
    LogicalOr,
    MemberAccess,
    Modulo,
    Multiply,
    Negate,
    NotEqual,
    Range,
    Sequence,
    ShiftLeft,
    ShiftRight,
    Sizeof,
    Subscript,
    Subtract,

    Assign,
    AssignAnd,
    AssignDecrement,
    AssignDivide,
    AssignIncrement,
    AssignModulo,
    AssignMultiply,
    AssignOr,
    AssignShiftLeft,
    AssignShiftRight,
    AssignXor,
};

pub const OperatorSymbol = union(enum) {
    Char: u32,
    Keyword: EowynKeyword,
};

pub const OperatorDef = struct {
    op: Operator,
    sym: OperatorSymbol,
    precedence: Precedence,
    position: Position = .Infix,
    associativity: Associativity = .Left,
};

pub const BindingPower = struct {
    left: i32,
    right: i32,
};

pub const operators = [_]OperatorDef{
    .{ .Add, '+', 11 },
    .{ .AddressOf, '&', 14, .Prefix, .Right },
    .{ .Assign, '=', 1, .Infix, .Right },
    .{ .AssignAnd, .AssignAnd, 1, .Infix, .Right },
    .{ .AssignDecrement, .AssignDecrement, 1, .Infix, .Right },
    .{ .AssignDivide, .AssignDivide, 1, .Infix, .Right },
    .{ .AssignIncrement, .AssignIncrement, 1, .Infix, .Right },
    .{ .AssignModulo, .AssignModulo, 1, .Infix, .Right },
    .{ .AssignMultiply, .AssignMultiply, 1, .Infix, .Right },
    .{ .AssignOr, .AssignOr, 1, .Infix, .Right },
    .{ .AssignShiftLeft, .AssignShiftLeft, 1, .Infix, .Right },
    .{ .AssignShiftRight, .AssignShiftRight, 1, .Infix, .Right },
    .{ .AssignXor, .AssignXor, 1, .Infix, .Right },
    .{ .BinaryInvert, '~', 14, .Prefix, .Right },
    .{ .Call, '(', 15 },
    .{ .Call, ')', 15, .Closing },
    .{ .Cast, .Cast, 14 },
    .{ .Divide, '/', 12 },
    .{ .Equals, .Equals, 8 },
    .{ .Greater, '>', 8 },
    .{ .GreaterEqual, .GreaterEqual, 8 },
    .{ .Idempotent, '+', 14, .Prefix, .Right },
    .{ .Length, '#', 9, .Prefix, .Right },
    .{ .Less, '<', 8 },
    .{ .LessEqual, .LessEqual, 8 },
    .{ .LogicalInvert, '!', 14, .Prefix, .Right },
    .{ .MemberAccess, '.', 15 },
    .{ .Modulo, '%', 12 },
    .{ .Multiply, '*', 12 },
    .{ .Negate, '-', 14, .Prefix, .Right },
    .{ .NotEqual, .NotEqual, 8 },
    .{ .Range, .Range, 2 },
    .{ .Sequence, ',', 1 },
    .{ .ShiftLeft, .ShiftLeft, 10 },
    .{ .ShiftRight, .ShiftRight, 10 },
    .{ .Sizeof, .Sizeof, 9, .Prefix, .Right },
    .{ .Subscript, '[', 15, .Postfix },
    .{ .Subscript, ']', 15, .Closing },
    .{ .Subtract, '-', 11 },
};

pub fn binding_power(op: OperatorDef) BindingPower {
    switch (op.position) {
        .Infix => switch (op.associativity) {
            .Left => return .{ op.precedence * 2 - 1, op.precedence * 2 },
            .Right => return .{ op.precedence * 2, op.precedence * 2 - 1 },
        },
        .Prefix => return .{ -1, op.precedence * 2 - 1 },
        .Postfix => return .{ op.precedence * 2 - 1, -1 },
        .Closing => return .{ -1, -1 },
        else => unreachable,
    }
}
