const std = @import("std");
const lexer = @import("lexer.zig");
const fatal = @import("fatal.zig");

pub const Precedence = i32;

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
            strings.?.put(.Break, "break") catch fatal.oom();
            strings.?.put(.Cast, "cast") catch fatal.oom();
            strings.?.put(.Const, "const") catch fatal.oom();
            strings.?.put(.Continue, "continue") catch fatal.oom();
            strings.?.put(.Defer, "defer") catch fatal.oom();
            strings.?.put(.Else, "else") catch fatal.oom();
            strings.?.put(.Embed, "@embed") catch fatal.oom();
            strings.?.put(.Enum, "enum") catch fatal.oom();
            strings.?.put(.Equals, "==") catch fatal.oom();
            strings.?.put(.Error, "error") catch fatal.oom();
            strings.?.put(.ExternLink, "->") catch fatal.oom();
            strings.?.put(.False, "false") catch fatal.oom();
            strings.?.put(.For, "for") catch fatal.oom();
            strings.?.put(.Func, "func") catch fatal.oom();
            strings.?.put(.GreaterEqual, ">=") catch fatal.oom();
            strings.?.put(.If, "if") catch fatal.oom();
            strings.?.put(.Import, "import") catch fatal.oom();
            strings.?.put(.Include, "include") catch fatal.oom();
            strings.?.put(.LessEqual, "<=") catch fatal.oom();
            strings.?.put(.LogicalAnd, "&&") catch fatal.oom();
            strings.?.put(.LogicalOr, "||") catch fatal.oom();
            strings.?.put(.Loop, "loop") catch fatal.oom();
            strings.?.put(.NotEqual, "!=") catch fatal.oom();
            strings.?.put(.Null, "null") catch fatal.oom();
            strings.?.put(.Public, "public") catch fatal.oom();
            strings.?.put(.Range, "..") catch fatal.oom();
            strings.?.put(.Return, "return") catch fatal.oom();
            strings.?.put(.ShiftLeft, "<<") catch fatal.oom();
            strings.?.put(.ShiftRight, ">>") catch fatal.oom();
            strings.?.put(.Sizeof, "sizeof") catch fatal.oom();
            strings.?.put(.Struct, "struct") catch fatal.oom();
            strings.?.put(.True, "true") catch fatal.oom();
            strings.?.put(.Var, "var") catch fatal.oom();
            strings.?.put(.While, "while") catch fatal.oom();
            strings.?.put(.Yield, "yield") catch fatal.oom();
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
    .{ .op = .Add, .sym = .{ .Char = '+' }, .precedence = 11 },
    .{ .op = .Assign, .sym = .{ .Char = '=' }, .precedence = 1, .position = .Infix, .associativity = .Right },
    .{ .op = .AssignAnd, .sym = .{ .Keyword = .AssignAnd }, .precedence = 1, .position = .Infix, .associativity = .Right },
    .{ .op = .AssignDecrement, .sym = .{ .Keyword = .AssignDecrement }, .precedence = 1, .position = .Infix, .associativity = .Right },
    .{ .op = .AssignDivide, .sym = .{ .Keyword = .AssignDivide }, .precedence = 1, .position = .Infix, .associativity = .Right },
    .{ .op = .AddressOf, .sym = .{ .Char = '&' }, .precedence = 14, .position = .Prefix, .associativity = .Right },
    .{ .op = .AssignIncrement, .sym = .{ .Keyword = .AssignIncrement }, .precedence = 1, .position = .Infix, .associativity = .Right },
    .{ .op = .AssignModulo, .sym = .{ .Keyword = .AssignModulo }, .precedence = 1, .position = .Infix, .associativity = .Right },
    .{ .op = .AssignMultiply, .sym = .{ .Keyword = .AssignMultiply }, .precedence = 1, .position = .Infix, .associativity = .Right },
    .{ .op = .AssignOr, .sym = .{ .Keyword = .AssignOr }, .precedence = 1, .position = .Infix, .associativity = .Right },
    .{ .op = .AssignShiftLeft, .sym = .{ .Keyword = .AssignShiftLeft }, .precedence = 1, .position = .Infix, .associativity = .Right },
    .{ .op = .AssignShiftRight, .sym = .{ .Keyword = .AssignShiftRight }, .precedence = 1, .position = .Infix, .associativity = .Right },
    .{ .op = .AssignXor, .sym = .{ .Keyword = .AssignXor }, .precedence = 1, .position = .Infix, .associativity = .Right },
    .{ .op = .BinaryInvert, .sym = .{ .Char = '~' }, .precedence = 14, .position = .Prefix, .associativity = .Right },
    .{ .op = .Call, .sym = .{ .Char = '(' }, .precedence = 15 },
    .{ .op = .Call, .sym = .{ .Char = ')' }, .precedence = 15, .position = .Closing },
    .{ .op = .Cast, .sym = .{ .Keyword = .Cast }, .precedence = 14 },
    .{ .op = .Divide, .sym = .{ .Char = '/' }, .precedence = 12 },
    .{ .op = .Equals, .sym = .{ .Keyword = .Equals }, .precedence = 8 },
    .{ .op = .Greater, .sym = .{ .Char = '>' }, .precedence = 8 },
    .{ .op = .GreaterEqual, .sym = .{ .Keyword = .GreaterEqual }, .precedence = 8 },
    .{ .op = .Idempotent, .sym = .{ .Char = '+' }, .precedence = 14, .position = .Prefix, .associativity = .Right },
    .{ .op = .Length, .sym = .{ .Char = '#' }, .precedence = 9, .position = .Prefix, .associativity = .Right },
    .{ .op = .Less, .sym = .{ .Char = '<' }, .precedence = 8 },
    .{ .op = .LessEqual, .sym = .{ .Keyword = .LessEqual }, .precedence = 8 },
    .{ .op = .LogicalInvert, .sym = .{ .Char = '!' }, .precedence = 14, .position = .Prefix, .associativity = .Right },
    .{ .op = .MemberAccess, .sym = .{ .Char = '.' }, .precedence = 15 },
    .{ .op = .Modulo, .sym = .{ .Char = '%' }, .precedence = 12 },
    .{ .op = .Multiply, .sym = .{ .Char = '*' }, .precedence = 12 },
    .{ .op = .Negate, .sym = .{ .Char = '-' }, .precedence = 14, .position = .Prefix, .associativity = .Right },
    .{ .op = .NotEqual, .sym = .{ .Keyword = .NotEqual }, .precedence = 8 },
    .{ .op = .Range, .sym = .{ .Keyword = .Range }, .precedence = 2 },
    .{ .op = .Sequence, .sym = .{ .Char = ',' }, .precedence = 1 },
    .{ .op = .ShiftLeft, .sym = .{ .Keyword = .ShiftLeft }, .precedence = 10 },
    .{ .op = .ShiftRight, .sym = .{ .Keyword = .ShiftRight }, .precedence = 10 },
    .{ .op = .Sizeof, .sym = .{ .Keyword = .Sizeof }, .precedence = 9, .position = .Prefix, .associativity = .Right },
    .{ .op = .Subscript, .sym = .{ .Char = '[' }, .precedence = 15, .position = .Postfix },
    .{ .op = .Subscript, .sym = .{ .Char = ']' }, .precedence = 15, .position = .Closing },
    .{ .op = .Subtract, .sym = .{ .Char = '-' }, .precedence = 11 },
};

pub fn binding_power(op: OperatorDef) BindingPower {
    switch (op.position) {
        .Infix => switch (op.associativity) {
            .Left => return .{ .left = op.precedence * 2 - 1, .right = op.precedence * 2 },
            .Right => return .{ .left = op.precedence * 2, .right = op.precedence * 2 - 1 },
        },
        .Prefix => return .{ .left = -1, .right = op.precedence * 2 - 1 },
        .Postfix => return .{ .left = op.precedence * 2 - 1, .right = -1 },
        .Closing => return .{ .left = -1, .right = -1 },
    }
}
