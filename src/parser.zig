const std = @import("std");
const fatal = @import("fatal.zig");
const node = @import("node.zig");
const lx = @import("lexer.zig");
const op = @import("operator.zig");

const block = @import("syntax/block.zig");
const constant = @import("syntax/constant.zig");
const expression = @import("syntax/expression.zig");
const flowcontrol = @import("syntax/flowcontrol.zig");
const function = @import("syntax/function.zig");
const import = @import("syntax/import.zig");
const preprocess = @import("syntax/preprocess.zig");
const typespec = @import("syntax/typespec.zig");
const variable = @import("syntax/variable.zig");

const Node = node.Node;
const Tree = node.Tree;
const pSyntaxNode = node.pSyntaxNode;
const SyntaxNodes = node.SyntaxNodes;

pub const Parser = struct {
    pub const LexerTypes = lx.LexerTypes(op.EowynKeyword);

    pub const Scanners = LexerTypes.ScannerPack(&[_]type{
        LexerTypes.RawScanner("@begin", "@end"),
        LexerTypes.CScannerPack,
    });

    pub const Lexer = lx.Lexer(LexerTypes, Scanners);
    pub const Token = Lexer.Token;
    pub const TokenLocation = lx.TokenLocation;
    pub const TokenKind = lx.TokenKind;

    pub const ParserError = struct {
        location: TokenLocation,
        message: []const u8,
    };

    allocator: std.mem.Allocator,
    text: []const u8,
    lexer: Lexer,
    namespaces: std.ArrayList(node.Node),
    pass: i32 = 0,
    tree: node.Tree,
    errors: std.ArrayList(ParserError),

    pub fn init(allocator: std.mem.Allocator, text: []const u8) Parser {
        var ret = Parser{
            .allocator = allocator,
            .text = text,
            .lexer = Lexer.init(allocator),
            .namespaces = std.ArrayList(node.Node).init(allocator),
            .tree = node.Tree.init(allocator),
            .errors = std.ArrayList(ParserError).init(allocator),
        };
        ret.lexer.push_source(text);
        return ret;
    }

    pub fn deinit(this: *Parser) void {
        this.errors.deinit();
        this.tree.deinit();
        this.namespaces.deinit();
        this.lexer.deinit();
    }

    pub fn append(this: *Parser, location: TokenLocation, comptime fmt: []const u8, args: anytype) void {
        const err = ParserError{
            .location = location,
            .message = std.fmt.allocPrint(this.allocator, fmt, args) catch fatal.oom(),
        };
        this.errors.append(err) catch fatal.oom();
    }

    pub fn parse(this: *Parser) ?*Node {
        var statements = SyntaxNodes.init(this.allocator);
        const t = this.parse_statements(&statements);
        std.debug.assert(t.matches(.EndOfFile));
        switch (statements.items.len) {
            0 => return null,
            1 => return &this.tree.nodes.items[statements.items[0]],
            else => return this.tree.add(
                .Block,
                this.tree.get(statements.items[0]).location.merge(this.tree.get(statements.getLast()).location),
                block.Block{ .allocator = this.allocator, .statements = statements },
            ),
        }
    }

    pub fn parse_statements(this: *Parser, statements: *SyntaxNodes) LexerTypes.Token {
        while (true) {
            const t: Token = this.lexer.peek();
            if (t.matches(.EndOfFile) or t.matches_symbol('}')) {
                _ = this.lexer.lex();
                return t;
            }
            if (this.parse_statement()) |s| {
                statements.append(s.index) catch fatal.oom();
            }
        }
    }

    pub fn parse_module_statements(this: *Parser, statements: *SyntaxNodes) Token {
        while (true) {
            const t = this.lexer.peek();
            if (t.matches(.EndOfFile) or t.matches_symbol('}')) {
                _ = this.lexer.lex();
                return t;
            }
            if (this.parse_module_level_statement()) |s| {
                statements.append(s.index);
            }
        }
    }

    pub fn parse_module_level_statement(this: *Parser) ?*Node {
        const t = this.lexer.peek();
        switch (t.value) {
            .EndOfFile => {
                this.append(t, "Unexpected end of file", .{});
                return null;
            },
            .Identifier => {
                _ = this.lexer.lex();
                this.lexer.expect_symbol(':') catch {
                    this.append(this.lexer.current_location, "Expected variable declaration", .{});
                    return null;
                };
                return parse_statement();
            },
            .Keyword => |kw| {
                switch (kw) {
                    .Const => return this.parse_const(),
                    .Enum => return this.parse_enum(),
                    .Func => return this.parse_func(),
                    .Import => return this.parse_import(),
                    .Include => return this.parse_include(),
                    .Public => return this.parse_public(),
                    .Struct => return this.parse_struct(),
                    .Var => return this.parse_var(),
                    else => {},
                }
            },
            else => {},
        }
        _ = this.lexer.lex();
        this.append(t, "Unexpected token `{s}`", .{text_of(t)});
        return null;
    }

    pub fn parse_statement(this: *Parser) ?*Node {
        const t = this.lexer.peek();
        switch (t.value) {
            .EndOfFile => {
                this.append(this.lexer.location, "Unexpected end of file", .{});
                return null;
            },
            .Identifier => {
                if (this.lexer.cursor > 2 and
                    this.lexer.tokens.items[this.lexer.cursor - 1].matches_symbol(':') and
                    this.lexer.tokens.items[this.lexer.cursor - 2].matches(.Identifier))
                {
                    // This is the type of a variable decl:
                    return this.parse_var_decl();
                }
                _ = this.lexer.lex();
                if (this.lexer.matches_symbol(':')) {
                    _ = this.lexer.lex();
                    return this.parse_statement();
                }
                this.lexer.push_back();
                return this.parse_expression(0);
            },
            .Number, .QuotedString => return this.parse_expression(0),
            .Keyword => |kw| {
                switch (kw) {
                    .Break, .Continue => return this.parse_break_continue(),
                    .Const => return this.parse_const(),
                    .Defer => return this.parse_defer(),
                    .Embed => return this.parse_embed(),
                    .Enum => return this.parse_enum(),
                    .Error => return this.parse_return_error(),
                    .For => return this.parse_for(),
                    .Func => return this.parse_func(),
                    .If => return this.parse_if(),
                    .Include => return this.parse_include(),
                    .Loop => return this.parse_loop(),
                    .Return => return this.parse_return_error(),
                    .Struct => return this.parse_struct(),
                    .Var => return this.parse_var(),
                    .While => return this.parse_while(),
                    .Yield => return this.parse_yield(),
                    else => {
                        this.append(t.location, "Unexpected keyword `{s}` parsing statement", .{this.text_of(t)});
                        _ = this.lexer.lex();
                        return null;
                    },
                }
            },
            .Symbol => |sym| {
                switch (sym) {
                    ';' => return this.tree.add(.Void, t.location, .{}),
                    '{' => {
                        _ = this.lexer.lex();
                        var new_block = SyntaxNodes.init(this.allocator);
                        const end_token = this.parse_statements(&new_block);
                        if (!end_token.matches_symbol('}')) {
                            this.append(t.location, "Unexpected end of block", .{});
                            return null;
                        } else {
                            if (new_block.items.len == 0) {
                                return this.tree.add(.Void, t.location.merge(end_token.location), block.Void{});
                            }
                            return this.tree.add(
                                .Block,
                                t.location.merge(end_token.location),
                                block.Block{ .allocator = this.allocator, .statements = new_block },
                            );
                        }
                    },
                    '=' => {
                        if (this.lexer.cursor > 2 and
                            this.lexer.tokens.items[this.lexer.cursor - 1].matches_symbol(':') and
                            this.lexer.tokens.items[this.lexer.cursor - 2].matches(.Identifier))
                        {
                            // This is the '=' of a variable decl with implied type:
                            return this.parse_var_decl();
                        }
                        if (this.parse_expression(0)) |expr| {
                            return expr;
                        }
                        this.append(t.location, "Unexpected symbol `{c}`", .{@as(u8, @truncate(sym))});
                        _ = this.lexer.lex();
                        return null;
                    },
                    else => {
                        if (this.parse_expression(0)) |expr| {
                            return expr;
                        }
                        this.append(t.location, "Unexpected symbol `{c}`", .{@as(u8, @truncate(sym))});
                        _ = this.lexer.lex();
                        return null;
                    },
                }
            },
            .Raw => {
                const raw = t.raw_text();
                std.debug.assert(std.mem.eql(u8, raw.marker, "@begin"));
                _ = this.lexer.lex();
                if (raw.end) |_| {
                    return this.tree.add(
                        .Insert,
                        t.location,
                        preprocess.Insert{ .script_text = this.text_of(t) },
                    );
                } else {
                    this.append(t.location, "Unclosed `@insert` block", .{});
                    return null;
                }
            },
            else => {
                _ = this.lexer.lex();
                this.append(t.location, "Unexpected token `{s}`", .{this.text_of(t)});
                return null;
            },
        }
    }

    pub fn text_at(this: *const Parser, start: usize, end: ?usize) []const u8 {
        if (start < this.text.len) {
            if (end) |e| {
                return this.text[start..e];
            } else {
                return this.text[start..];
            }
        }
        return "";
    }

    pub fn text_of(this: *const Parser, token: Token) []const u8 {
        return this.text_at_location(token.location);
    }

    pub fn text_at_location(this: *const Parser, location: lx.TokenLocation) []const u8 {
        return this.text_at_index(location.index, location.length);
    }

    pub fn text_at_index(this: *const Parser, index: usize, length: usize) []const u8 {
        if (index < this.text.len) {
            return this.text[index .. index + length];
        }
        return "";
    }

    pub fn parse_primary(this: *Parser) ?*Node {
        const token = this.lexer.peek();
        var ret: ?*Node = null;
        switch (token.value) {
            .Number => {
                ret = this.tree.add(
                    .Number,
                    token.location,
                    constant.Number{
                        .number = this.text_of(token),
                        .number_type = token.number_type(),
                    },
                );
                _ = this.lexer.lex();
            },
            .QuotedString => {
                _ = this.lexer.lex();
                if (token.quote_type() == .SingleQuote and token.location.length != 1) {
                    this.append(token.location, "Single quoted string should contain exactly one character", .{});
                    return null;
                }
                ret = this.tree.add(
                    .QuotedString,
                    token.location,
                    constant.QuotedString{ .string = this.text_of(token), .quote_type = token.quote_type() },
                );
            },
            .Identifier => {
                _ = this.lexer.lex();
                // const bm = this.lexer.bookmark();
                // if (this.lexer.accept_symbol('<')) {
                //     TypeSpecifications specs;
                //     while (true) {
                //         const spec = parse_type();
                //         if (spec == null) {
                //             break;
                //         }
                //         specs.push_back(spec);
                //         if (this.lexer.accept_symbol('>')) {
                //             return this.tree.add(StampedIdentifier, token.location + this.lexer.location, this.text_of(token), specs);
                //         }
                //         if (!this.lexer.accept_symbol(',')) {
                //             break;
                //         }
                //     }
                // }
                // this.lexer.push_back(bm);
                ret = this.tree.add(.Identifier, token.location, variable.Identifier{ .identifier = this.text_of(token) });
            },
            .Keyword => |kw| {
                switch (kw) {
                    .Embed => return this.parse_embed(),
                    .Include => return this.parse_include(),
                    .False => {
                        _ = this.lexer.lex();
                        return this.tree.add(.BoolConstant, token.location, constant.BoolConstant{ .value = false });
                    },
                    .True => {
                        _ = this.lexer.lex();
                        return this.tree.add(.BoolConstant, token.location, constant.BoolConstant{ .value = true });
                    },
                    .Null => {
                        _ = this.lexer.lex();
                        return this.tree.add(.Nullptr, token.location, constant.Nullptr{});
                    },
                    else => {},
                }
                const op_maybe = this.check_prefix_op();
                if (op_maybe) |operator| {
                    const bp = op.binding_power(operator);
                    const op_token = this.lexer.lex();
                    const operand = (if (operator.op == .Sizeof) this.parse_type() else this.parse_expression(bp.right)) orelse {
                        this.append(token.location, "Expected operand following prefix operator '{s}'", .{@tagName(operator.op)});
                        return null;
                    };
                    ret = this.tree.add(
                        .UnaryExpression,
                        op_token.location.merge(operand.location),
                        expression.UnaryExpression{ .op = operator.op, .operand = operand.index },
                    );
                } else {
                    this.append(token.location, "Unexpected keyword '{s}' parsing primary expression", .{@tagName(kw)});
                    return null;
                }
            },
            .Symbol => |sym| {
                if (sym == '(') {
                    _ = this.lexer.lex();
                    if (this.lexer.accept_symbol(')')) {
                        return this.tree.add(.Void, token.location, block.Void{});
                    }
                    ret = this.parse_expression(0);
                    this.lexer.expect_symbol(')') catch {
                        this.append(token.location, "Expected ')'", .{});
                        return null;
                    };
                } else {
                    if (this.check_prefix_op()) |operator| {
                        const bp = op.binding_power(operator);
                        const op_token = this.lexer.lex();
                        const operand = this.parse_expression(bp.right) orelse {
                            this.append(token.location, "Expected operand following prefix operator '{s}'", .{@tagName(operator.op)});
                            return null;
                        };
                        ret = this.tree.add(
                            .UnaryExpression,
                            op_token.location.merge(operand.location),
                            expression.UnaryExpression{ .op = operator.op, .operand = operand.index },
                        );
                    } else {
                        this.append(token.location, "Unexpected token {s} `{s}`", .{ @tagName(token.value), this.text_of(token) });
                        ret = null;
                    }
                }
            },
            else => {
                this.append(token.location, "Unexpected token {s} `{s}`", .{ @tagName(token.value), this.text_of(token) });
                ret = null;
            },
        }
        if (ret == null) {
            this.append(token.location, "Expected primary expression", .{});
        }
        return ret;
    }

    // Shamelessly stolen from here:
    // https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    pub fn parse_expression(this: *Parser, min_prec: op.Precedence) ?*Node {
        var lhs = this.parse_primary() orelse return null;
        while (!this.lexer.matches(.EndOfFile) and this.check_op()) {
            if (this.check_postfix_op()) |operator| {
                const bp = op.binding_power(operator);
                if (bp.left < min_prec) {
                    break;
                }
                if (operator.op == .Subscript) {
                    _ = this.lexer.lex();
                    const rhs = this.parse_expression(0) orelse {
                        this.append(this.lexer.peek().location, "Expected subscript expression", .{});
                        return null;
                    };
                    this.lexer.expect_symbol(']') catch {
                        this.append(this.lexer.current.location, "Expected ']'", .{});
                        return null;
                    };
                    lhs = this.tree.add(
                        .BinaryExpression,
                        lhs.location.merge(rhs.location),
                        expression.BinaryExpression{ .lhs = lhs.index, .op = operator.op, .rhs = rhs.index },
                    );
                } else {
                    lhs = this.tree.add(
                        .UnaryExpression,
                        lhs.location.merge(this.lexer.peek().location),
                        expression.UnaryExpression{ .op = operator.op, .operand = lhs.index },
                    );
                    _ = this.lexer.lex();
                }
                continue;
            }
            if (this.check_binop()) |operator| {
                const bp = op.binding_power(operator);
                if (bp.left < min_prec) {
                    break;
                }
                if (operator.op == .Call) {
                    // Don't lex the '(' so parse_primary will return a
                    // single expression, probably a binop with op = ','.
                    const param_list = this.parse_primary() orelse {
                        this.append(lhs.location, "Could not parse function call argument list", .{});
                        return null;
                    };
                    lhs = this.tree.add(
                        .BinaryExpression,
                        lhs.location.merge(param_list.location),
                        expression.BinaryExpression{ .lhs = lhs.index, .op = .Call, .rhs = param_list.index },
                    );
                } else {
                    _ = this.lexer.lex();
                    const rhs = (if (operator.op == .Cast) this.parse_type() else this.parse_expression(bp.right)) orelse return null;
                    lhs = this.tree.add(
                        .BinaryExpression,
                        lhs.location.merge(rhs.location),
                        expression.BinaryExpression{ .lhs = lhs.index, .op = operator.op, .rhs = rhs.index },
                    );
                }
                continue;
            }
            break;
        }
        return lhs;
    }

    pub fn check_op(this: *Parser) bool {
        const token = this.lexer.peek();
        if (!token.matches(.Symbol) and !token.matches(.Keyword)) {
            return false;
        }
        for (op.operators) |operator| {
            switch (operator.sym) {
                .Char => |ch| if (token.matches_symbol(ch)) return true,
                .Keyword => |kw| if (token.matches_keyword(kw)) return true,
            }
        }
        return false;
    }

    pub fn check_binop(this: *Parser) ?op.OperatorDef {
        return this.check_op_by_position(.Infix);
    }

    pub fn check_prefix_op(this: *Parser) ?op.OperatorDef {
        return this.check_op_by_position(.Prefix);
    }

    pub fn check_postfix_op(this: *Parser) ?op.OperatorDef {
        return this.check_op_by_position(.Postfix);
    }

    fn check_op_by_position(this: *Parser, pos: op.Position) ?op.OperatorDef {
        const token = this.lexer.peek();
        if (!token.matches(.Symbol) and !token.matches(.Keyword)) {
            return null;
        }
        for (op.operators) |operator| {
            if (operator.position != pos) {
                continue;
            }
            switch (operator.sym) {
                .Char => |ch| if (token.matches_symbol(ch)) return operator,
                .Keyword => |kw| if (token.matches_keyword(kw)) return operator,
            }
        }
        return null;
    }

    fn parse_type(this: *Parser) ?*Node {
        const t = this.lexer.peek();
        if (this.lexer.accept_symbol('&')) {
            if (this.parse_type()) |typ| {
                return this.tree.add(
                    .TypeSpecification,
                    t.location.merge(typ.location),
                    typespec.TypeSpecification{ .description = .{ .Reference = typ.index } },
                );
            }
            return null;
        }
        if (this.lexer.accept_symbol('[')) {
            if (this.lexer.accept_symbol(']')) {
                if (this.parse_type()) |typ| {
                    return this.tree.add(
                        .TypeSpecification,
                        t.location.merge(typ.location),
                        typespec.TypeSpecification{ .description = .{ .Slice = typ.index } },
                    );
                }
                return null;
            }
            if (this.lexer.accept_symbol('0')) {
                this.lexer.expect_symbol(']') catch {
                    this.append(this.lexer.location, "Expected `]` to close `[0`", .{});
                    return null;
                };
                if (this.parse_type()) |typ| {
                    return this.tree.add(
                        .TypeSpecification,
                        t.location.merge(typ.location),
                        typespec.TypeSpecification{ .description = .{ .ZeroTerminatedArray = typ.index } },
                    );
                }
                return null;
            }
            if (this.lexer.accept_symbol('*')) {
                this.lexer.expect_symbol(']') catch {
                    this.append(this.lexer.location, "Expected `]` to close `[*`", .{});
                    return null;
                };
                if (this.parse_type()) |typ| {
                    return this.tree.add(
                        .TypeSpecification,
                        t.location.merge(typ.location),
                        typespec.TypeSpecification{ .description = .{ .DynArray = typ.index } },
                    );
                }
                return null;
            }
            const res = this.lexer.expect(.Number) catch {
                this.append(this.lexer.location, "Expected array size, `0`, or `]`", .{});
                return null;
            };
            if (res.number_type() == .Decimal) {
                this.append(res.location, "Array size must be integer", .{});
                return null;
            } else {
                this.lexer.expect_symbol(']') catch {
                    this.append(this.lexer.location, "Expected `]` to close array descriptor", .{});
                    return null;
                };
                const size = std.fmt.parseUnsigned(u64, this.text_of(res), 0) catch unreachable;
                if (this.parse_type()) |typ| {
                    return this.tree.add(
                        .TypeSpecification,
                        t.location.merge(typ.location),
                        typespec.TypeSpecification{ .description = .{ .Array = .{ .array_of = typ.index, .size = size } } },
                    );
                }
                return null;
            }
        }

        const name = this.lexer.expect_identifier() catch {
            this.append(this.lexer.location, "Expected type name", .{});
            return null;
        };
        var arguments = std.ArrayList(pSyntaxNode).init(this.allocator);
        if (this.lexer.accept_symbol('<')) {
            while (true) {
                if (this.lexer.accept_symbol('>')) {
                    break;
                }
                const arg = this.parse_type() orelse {
                    this.append(this.lexer.location, "Expected template type specification", .{});
                    return null;
                };
                arguments.append(arg.index) catch fatal.oom();
                if (this.lexer.accept_symbol('>')) {
                    break;
                }
                this.lexer.expect_symbol(',') catch {
                    this.append(this.lexer.location, "Expected `,` or `>`", .{});
                    return null;
                };
            }
        }
        var the_type = this.tree.add(
            .TypeSpecification,
            name.location.merge(this.lexer.location),
            typespec.TypeSpecification{ .description = .{ .TypeName = .{ .name = this.text_of(name), .arguments = arguments } } },
        );
        if (this.lexer.accept_symbol('?')) {
            the_type = this.tree.add(
                .TypeSpecification,
                name.location.merge(this.lexer.location),
                typespec.TypeSpecification{ .description = .{ .Optional = the_type.index } },
            );
        }
        if (this.lexer.accept_symbol('/')) {
            const error_type = this.parse_type() orelse {
                this.append(this.lexer.location, "Expected error type", .{});
                return null;
            };
            return this.tree.add(
                .TypeSpecification,
                name.location.merge(this.lexer.location),
                typespec.TypeSpecification{ .description = .{ .Error = .{ .success_type = the_type.index, .error_type = error_type.index } } },
            );
        }
        return the_type;
    }

    fn parse_const(this: *Parser) ?*Node {
        const t = this.lexer.peek();
        std.debug.assert(t.matches_keyword(.Const));
        _ = this.lexer.lex();
        const decl = this.parse_var_decl() orelse return null;
        return this.tree.add(.Const, t.location.merge(decl.location), variable.Const{ .declaration = decl.index });
    }

    fn parse_break_continue(this: *Parser) ?*Node {
        const kw = this.lexer.lex();
        std.debug.assert(kw.matches_keyword(.Break) or kw.matches_keyword(.Continue));
        var label: ?[]const u8 = null;
        if (this.lexer.accept_symbol(':')) {
            const lbl = this.lexer.expect_identifier() catch {
                this.append(kw.location, "Expected label name after `:`", .{});
                return null;
            };
            label = this.text_of(lbl);
        }
        if (kw.matches_keyword(.Break)) {
            return this.tree.add(.Break, kw.location, flowcontrol.Break{ .label = label });
        }
        return this.tree.add(.Continue, kw.location, flowcontrol.Continue{ .label = label });
    }

    fn parse_embed(this: *Parser) ?*Node {
        const kw = this.lexer.lex();
        this.lexer.expect_symbol('(') catch {
            this.append(this.lexer.location, "Expected `(`", .{});
            return null;
        };
        const file_name = this.lexer.expect(.QuotedString) catch {
            this.append(this.lexer.location, "Expected embed file name as quoted string", .{});
            return null;
        };
        var fname = this.text_of(file_name);
        fname = fname[1 .. fname.len - 1];
        this.lexer.expect_symbol(')') catch {
            this.append(this.lexer.location, "Expected `)`", .{});
            return null;
        };
        return this.tree.add(
            .Embed,
            kw.location.merge(this.lexer.location),
            preprocess.Embed{ .file_name = fname },
        );
    }

    fn parse_defer(this: *Parser) ?*Node {
        const kw = this.lexer.lex();
        const stmt = this.parse_statement() orelse {
            this.append(kw.location, "Could not parse defer statement", .{});
            return null;
        };
        return this.tree.add(
            .Defer,
            kw.location.merge(stmt.location),
            flowcontrol.Defer{ .statement = stmt.index },
        );
    }

    fn parse_enum(this: *Parser) ?*Node {
        const enum_token = this.lexer.lex();
        std.debug.assert(enum_token.matches_keyword(.Enum));

        const name = this.lexer.expect_identifier() catch {
            this.append(this.lexer.location, "Expected enum name", .{});
            return null;
        };
        var underlying: ?pSyntaxNode = null;
        if (this.lexer.accept_symbol(':')) {
            const underlying_type = this.parse_type() orelse {
                this.append(this.lexer.location, "Expected underlying type after `:`", .{});
                return null;
            };
            underlying = underlying_type.index;
        }
        this.lexer.expect_symbol('{') catch {
            this.append(this.lexer.location, "Expected `{{`", .{});
            return null;
        };
        var values = SyntaxNodes.init(this.allocator);
        while (!this.lexer.accept_symbol('}')) {
            const label = this.lexer.expect_identifier() catch {
                this.append(this.lexer.location, "Expected enum value label", .{});
                return null;
            };
            var payload: ?pSyntaxNode = null;
            if (this.lexer.accept_symbol('(')) {
                const payload_type = this.parse_type() orelse {
                    this.append(this.lexer.location, "Expected enum value payload type", .{});
                    return null;
                };
                payload = payload_type.index;
                this.lexer.expect_symbol(')') catch {
                    this.append(this.lexer.location, "Expected `)` to close enum value payload type", .{});
                    return null;
                };
            }
            var value_node: ?*Node = null;
            if (this.lexer.accept_symbol('=')) {
                const value = this.lexer.peek();
                if (!value.matches(.Number) or value.number_type() == .Decimal) {
                    this.append(value.location, "Expected enum value", .{}); // Make better
                    return null;
                }
                _ = this.lexer.lex();
                value_node = this.tree.add(
                    .Number,
                    value.location,
                    constant.Number{ .number = this.text_of(value), .number_type = value.number_type() },
                );
            }
            values.append(this.tree.add(
                .EnumValue,
                label.location.merge(this.lexer.location),
                typespec.EnumValue{ .label = this.text_of(label), .value = if (value_node) |n| n.index else null, .payload = payload },
            ).index) catch fatal.oom();
            if (!this.lexer.accept_symbol(',') and !this.lexer.accept_symbol('}')) {
                this.append(this.lexer.location, "Expected `,` or `}}`", .{});
                return null;
            }
        }
        return this.tree.add(
            .Enum,
            enum_token.location.merge(this.lexer.location),
            typespec.Enum{ .name = this.text_of(name), .values = values, .underlying = underlying },
        );
    }

    fn parse_for(this: *Parser) ?*Node {
        var label: ?[]const u8 = null;
        var location: TokenLocation = undefined;
        if (this.lexer.cursor > 1 and
            this.lexer.tokens.items[this.lexer.cursor - 1].matches_symbol(':') and
            this.lexer.tokens.items[this.lexer.cursor - 2].matches(.Identifier))
        {
            const lbl = this.lexer.tokens.items[this.lexer.cursor - 2];
            label = this.text_of(lbl);
            location = lbl.location;
        }
        const for_token = this.lexer.lex();
        std.debug.assert(for_token.matches_keyword(.For));
        if (label == null) {
            location = for_token.location;
        }

        const var_name = this.lexer.expect_identifier() catch {
            this.append(this.lexer.location, "Expected `for` range variable name", .{});
            return null;
        };
        var token = this.lexer.peek();
        if (token.matches(.Identifier) and std.mem.eql(u8, this.text_of(token), "in")) {
            _ = this.lexer.lex();
        }
        token = this.lexer.peek();
        const range = this.parse_expression(0) orelse {
            this.append(token.location, "Error parsing `for` range", .{});
            return null;
        };
        token = this.lexer.peek();
        const stmt = this.parse_statement() orelse {
            this.append(token.location, "Error parsing `for` block", .{});
            return null;
        };
        return this.tree.add(
            .ForStatement,
            location.merge(stmt.location),
            flowcontrol.ForStatement{ .range_variable = this.text_of(var_name), .range_expr = range.index, .statement = stmt.index },
        );
    }

    fn parse_func(this: *Parser) ?*Node {
        const func = this.lexer.lex();
        const name = blk: {
            const id = this.lexer.expect_identifier() catch {
                this.append(this.lexer.location, "Expected function name", .{});
                return null;
            };
            break :blk this.text_of(id);
        };
        var generics = SyntaxNodes.init(this.allocator);
        if (this.lexer.accept_symbol('<')) {
            while (true) {
                if (this.lexer.accept_symbol('>')) {
                    break;
                }
                const generic_name = this.lexer.expect_identifier() catch {
                    this.append(this.lexer.location, "Expected generic type parameter name", .{});
                    return null;
                };
                generics.append(this.tree.add(
                    .Identifier,
                    generic_name.location,
                    variable.Identifier{ .identifier = this.text_of(generic_name) },
                ).index) catch fatal.oom();
                if (this.lexer.accept_symbol('>')) {
                    break;
                }
                this.lexer.expect_symbol(',') catch {
                    this.append(this.lexer.location, "Expected `,` in function signature generic list", .{});
                    return null;
                };
            }
        }
        this.lexer.expect_symbol('(') catch {
            this.append(this.lexer.location, "Expected '(' in function definition", .{});
            return null;
        };
        var params = SyntaxNodes.init(this.allocator);
        while (true) {
            if (this.lexer.accept_symbol(')')) {
                break;
            }
            const param_name = this.lexer.expect_identifier() catch {
                this.append(this.lexer.location, "Expected parameter name", .{});
                return null;
            };
            this.lexer.expect_symbol(':') catch {
                this.append(this.lexer.location, "Expected ':' in function parameter declaration", .{});
                return null;
            };
            const param_type = this.parse_type() orelse {
                this.append(this.lexer.location, "Expected parameter type", .{});
                return null;
            };

            params.append(this.tree.add(
                .Parameter,
                param_name.location.merge(param_type.location),
                function.Parameter{ .name = this.text_of(param_name), .type_spec = param_type.index },
            ).index) catch fatal.oom();
            if (this.lexer.accept_symbol(')')) {
                break;
            }
            this.lexer.expect_symbol(',') catch {
                this.append(this.lexer.location, "Expected `,` or `)`in function signature", .{});
                return null;
            };
        }
        const return_type = this.parse_type() orelse {
            this.append(this.lexer.location, "Expected return type", .{});
            return null;
        };
        const signature = this.tree.add(
            .FunctionSignature,
            func.location.merge(return_type.location),
            function.FunctionSignature{ .name = name, .parameters = params, .return_type = return_type.index },
        );
        const impl = blk: {
            if (this.lexer.accept_keyword(.ExternLink)) {
                const link: Token = this.lexer.expect(.QuotedString) catch {
                    this.append(this.lexer.location, "Expected extern function name", .{});
                    return null;
                };
                if (link.quote_type() != .DoubleQuote) {
                    this.append(this.lexer.location, "Expected extern function name as double quoted string", .{});
                    return null;
                }
                var link_name = this.text_of(link);
                if (link_name.len <= 2) {
                    this.append(link.location, "Invalid extern function name", .{});
                    return null;
                }
                link_name = link_name[1 .. link_name.len - 1];
                break :blk this.tree.add(
                    .ExternLink,
                    link.location,
                    function.ExternLink{ .link_name = link_name },
                );
            } else {
                break :blk this.parse_statement() orelse {
                    this.append(this.lexer.location, "Error parsing function body", .{});
                    return null;
                };
            }
        };
        return this.tree.add(
            .FunctionDefinition,
            signature.location.merge(impl.location),
            function.FunctionDefinition{ .name = name, .signature = signature.index, .implementation = impl.index },
        );
    }

    fn parse_if(this: *Parser) ?*Node {
        const if_token = this.lexer.lex();
        std.debug.assert(if_token.matches_keyword(.If));
        const condition = this.parse_expression(0) orelse {
            this.append(if_token.location, "Error parsing `if` condition", .{});
            return null;
        };
        const if_branch = this.parse_statement() orelse {
            this.append(if_token.location, "Error parsing `if` branch", .{});
            return null;
        };
        const else_branch: ?*Node = if (this.lexer.accept_keyword(.Else))
            this.parse_statement() orelse {
                this.append(this.lexer.location, "Error parsing `else` branch", .{});
                return null;
            }
        else
            null;
        return this.tree.add(
            .IfStatement,
            if_token.location.merge(this.lexer.location),
            flowcontrol.IfStatement{ .condition = condition.index, .if_branch = if_branch.index, .else_branch = if (else_branch) |b| b.index else null },
        );
    }

    fn parse_import(this: *Parser) ?*Node {
        const import_token = this.lexer.lex();
        std.debug.assert(import_token.matches_keyword(.Import));
        var start: usize = 0;
        var length: usize = 0;
        while (true) {
            const ident = this.lexer.expect_identifier() catch {
                this.append(this.lexer.location, "Expected import path component", .{});
                return null;
            };
            if (start == 0) {
                start = ident.location.index;
            }
            length = ident.location.index + ident.location.length - start;
            if (!this.lexer.accept_symbol('.')) {
                break;
            }
        }
        return this.tree.add(.Import, import_token.location.merge(this.lexer.location), .{this.text_at_index(start, length)});
    }

    fn parse_include(this: *Parser) ?*Node {
        const kw = this.lexer.lex();
        this.lexer.expect_symbol('(') catch {
            this.append(kw.location, "Malformed '@include' statement: expected '('", .{});
            return null;
        };
        const file_name = this.lexer.expect(.QuotedString) catch {
            this.append(this.lexer.location, "Malformed '@include' statement: no file name", .{});
            return null;
        };
        var fname = this.text_of(file_name);
        fname = fname[1 .. fname.len - 1];
        this.lexer.expect_symbol(')') catch {
            this.append(this.lexer.location, "Malformed '@include' statement: expected ')'", .{});
            return null;
        };
        return this.tree.add(
            .Include,
            kw.location.merge(this.lexer.location),
            preprocess.Include{ .file_name = fname },
        );
    }

    fn parse_loop(this: *Parser) ?*Node {
        var label: ?[]const u8 = null;
        var location: TokenLocation = undefined;
        if (this.lexer.cursor > 1 and
            this.lexer.tokens.items[this.lexer.cursor - 1].matches_symbol(':') and
            this.lexer.tokens.items[this.lexer.cursor - 2].matches(.Identifier))
        {
            const lbl = this.lexer.tokens.items[this.lexer.cursor - 2];
            label = this.text_of(lbl);
            location = lbl.location;
        }
        const loop_token = this.lexer.lex();
        std.debug.assert(loop_token.matches_keyword(.Loop));
        if (label == null) {
            location = loop_token.location;
        }
        const stmt = this.parse_statement() orelse {
            this.append(loop_token.location, "Error parsing `loop` block", .{});
            return null;
        };
        return this.tree.add(
            .LoopStatement,
            location.merge(stmt.location),
            flowcontrol.LoopStatement{ .label = label, .statement = stmt.index },
        );
    }

    fn parse_public(this: *Parser) ?*Node {
        const t = this.lexer.peek();
        std.debug.assert(t.matches_keyword(.Public));
        this.lexer.lex();
        const decl = this.parse_module_level_statement() orelse return null;
        const name: []const u8 = blk: {
            switch (decl.node) {
                .Enum => |e| break :blk e.name,
                .FunctionDefinition => |f| break :blk f.name,
                .PublicDeclaration => {
                    this.append(decl.location, "Double public declaration", .{});
                    return null;
                },
                .Struct => |s| break :blk s.name,
                .VariableDeclaration => |v| break :blk v.name,
                else => {
                    this.append(decl.location, "Cannot declare statement of type `{s}` public", .{@tagName(decl.node)});
                    return null;
                },
            }
        };
        return this.tree.add(.Public, t.location.merge(decl.location), .{ name, decl.index });
    }

    fn parse_return_error(this: *Parser) ?*Node {
        const kw = this.lexer.lex();
        std.debug.assert(kw.matches_keyword(.Return) or kw.matches_keyword(.Error));
        const expr = this.parse_expression(0) orelse {
            this.append(kw.location, "Error parsing {s} expression", .{@tagName(kw.keyword_code())});
            return null;
        };
        if (kw.matches_keyword(.Return)) {
            return this.tree.add(.Return, kw.location.merge(expr.location), flowcontrol.Return{ .expression = expr.index });
        }
        return this.tree.add(.Error, kw.location.merge(expr.location), flowcontrol.Error{ .expression = expr.index });
    }

    fn parse_struct(this: *Parser) ?*Node {
        const struct_token = this.lexer.lex();
        std.debug.assert(struct_token.matches_keyword(.Struct));

        const name = this.lexer.expect_identifier() catch {
            this.append(this.lexer.location, "Expected struct name", .{});
            return null;
        };
        this.lexer.expect_symbol('{') catch {
            this.append(this.lexer.location, "Expected `{{` after struct name", .{});
            return null;
        };
        var fields = typespec.StructFields.init(this.allocator);
        while (!this.lexer.accept_symbol('}')) {
            const label = this.lexer.expect_identifier() catch {
                this.append(this.lexer.location, "Expected struct field name", .{});
                return null;
            };
            this.lexer.expect_symbol(':') catch {
                this.append(this.lexer.location, "Expected `:`", .{});
                return null;
            };
            const field_type = this.parse_type() orelse {
                this.append(this.lexer.location, "Expected struct member type", .{});
                return null;
            };
            fields.append(this.tree.add(
                .StructField,
                label.location.merge(this.lexer.location),
                typespec.StructField{ .label = this.text_of(label), .field_type = field_type.index },
            ).index) catch fatal.oom();
            if (!this.lexer.accept_symbol(',') and !this.lexer.matches_symbol('}')) {
                this.append(this.lexer.location, "Expected `,` or `}}`", .{});
                return null;
            }
        }
        return this.tree.add(
            .Struct,
            struct_token.location.merge(this.lexer.location),
            typespec.Struct{ .name = this.text_of(name), .fields = fields },
        );
    }

    fn parse_var(this: *Parser) ?*Node {
        const t = this.lexer.peek();
        std.debug.assert(t.matches_keyword(.Var));
        _ = this.lexer.lex();
        const decl = this.parse_var_decl() orelse return null;
        return this.tree.add(.Var, t.location.merge(decl.location), variable.Var{ .declaration = decl.index });
    }

    fn parse_var_decl(this: *Parser) ?*Node {
        const name = this.lexer.expect_identifier() catch {
            this.append(this.lexer.location, "Expected variable name", .{});
            return null;
        };
        const type_spec: ?pSyntaxNode = if (this.lexer.accept_symbol(':')) blk: {
            const typ = this.parse_type() orelse {
                this.append(this.lexer.location, "Expected variable type specification", .{});
                return null;
            };
            break :blk typ.index;
        } else null;
        const initializer: ?pSyntaxNode = if (this.lexer.accept_symbol('=')) blk: {
            const expr = this.parse_expression(0) orelse {
                this.append(this.lexer.location, "Error parsing initialization expression", .{});
                return null;
            };
            break :blk expr.index;
        } else if (type_spec == null) {
            this.append(this.lexer.location, "Expected variable initialization expression", .{});
            return null;
        } else null;
        return this.tree.add(
            .VariableDeclaration,
            name.location.merge(this.lexer.location),
            variable.VariableDeclaration{ .name = this.text_of(name), .type_spec = type_spec, .initializer = initializer },
        );
    }

    fn parse_while(this: *Parser) ?*Node {
        var label: ?[]const u8 = null;
        var location: TokenLocation = undefined;
        if (this.lexer.cursor > 1 and
            this.lexer.tokens.items[this.lexer.cursor - 1].matches_symbol(':') and
            this.lexer.tokens.items[this.lexer.cursor - 2].matches(.Identifier))
        {
            const lbl = this.lexer.tokens.items[this.lexer.cursor - 2];
            label = this.text_of(lbl);
            location = lbl.location;
        }
        const while_token = this.lexer.lex();
        std.debug.assert(while_token.matches_keyword(.While));
        if (label == null) {
            location = while_token.location;
        }
        const condition = this.parse_expression(0) orelse {
            this.append(while_token.location, "Error parsing `while` condition", .{});
            return null;
        };
        const stmt = this.parse_statement() orelse {
            this.append(while_token.location, "Error parsing `while` block", .{});
            return null;
        };
        return this.tree.add(
            .WhileStatement,
            location.merge(stmt.location),
            flowcontrol.WhileStatement{ .label = label, .condition = condition.index, .statement = stmt.index },
        );
    }

    fn parse_yield(this: *Parser) ?*Node {
        const kw = this.lexer.lex();
        std.debug.assert(kw.matches_keyword(.Yield));
        const label: ?[]const u8 = if (this.lexer.accept_symbol(':')) blk: {
            const lbl = this.lexer.expect_identifier() catch {
                this.append(this.lexer.location, "Expected label name after `:`", .{});
                return null;
            };
            break :blk this.text_of(lbl);
        } else null;
        const stmt = this.parse_statement() orelse {
            this.append(this.lexer.location, "Could not parse yield expression", .{});
            return null;
        };
        return this.tree.add(
            .Yield,
            kw.location.merge(this.lexer.location),
            flowcontrol.Yield{ .label = label, .expression = stmt.index },
        );
    }
};

test "Parser" {
    var parser = Parser.init(std.heap.c_allocator,
        \\ func foo(x: i32) i32
        \\ {
        \\   print(x)
        \\ }
        \\
    );
    _ = parser.parse();
    std.debug.print("\n\n", .{});
    for (parser.tree.nodes.items) |n| {
        n.print(std.io.getStdErr().writer());
    }
    try std.testing.expect(false);
}
