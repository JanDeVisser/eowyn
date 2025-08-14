const std = @import("std");
const expect = std.testing.expect;

pub const TokenLocation = struct {
    index: usize = 0,
    length: usize = 0,
    line: usize = 0,
    column: usize = 0,

    pub fn init() TokenLocation {
        return .{};
    }

    pub fn merge(self: *TokenLocation, other: TokenLocation) TokenLocation {
        var ret = TokenLocation.init();
        ret.index = @min(self.index, other.index);
        ret.length = @max(self.index + self.length, other.index + other.length) - ret.index;
        ret.line = @min(self.line, other.line);
        ret.column = @min(self.column, other.column);
        return ret;
    }
};

pub const TokenKind = enum {
    Unknown,
    Symbol,
    Number,
    QuotedString,
    Comment,
    Raw,
    Keyword,
    EndOfFile,
    EndOfLine,
    Identifier,
    Tab,
    Whitespace,
};

pub const QuoteType = enum(u8) {
    SingleQuote = '\'',
    DoubleQuote = '"',
    BackQuote = '`',
};

pub const CommentType = enum {
    Block,
    Line,
};

pub const NumberType = enum {
    Integer,
    Decimal,
    Hex,
    Binary,
};

pub const QuotedString = struct {
    quote_type: QuoteType,
    triple: bool,
    terminated: bool,
};

pub const CommentText = struct {
    comment_type: CommentType,
    terminated: bool,
};

pub const RawText = struct {
    marker: []const u8,
    end: ?usize,
};

pub fn KeywordMatch(comptime Keywords: type) type {
    return struct {
        const MatchType = enum {
            PrefixMatch,
            FullMatch,
        };
        keyword: Keywords,
        match_type: MatchType,
    };
}

pub fn match_enum(comptime Keywords: type, s: []const u8) ?KeywordMatch(Keywords) {
    var prefix: ?usize = null;
    var buffer: [4096]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();
    inline for (@typeInfo(Keywords).@"enum".fields) |fld| {
        const name = std.ascii.allocLowerString(allocator, fld.name) catch std.builtin.panic("OOM", null, null);
        if (std.mem.startsWith(u8, name, s)) {
            if (std.mem.eql(u8, name, s)) {
                return .{
                    .keyword = @enumFromInt(fld.value),
                    .match_type = KeywordMatch(Keywords).MatchType.FullMatch,
                };
            }
            prefix = fld.value;
        }
    }
    if (prefix != null) {
        return .{
            .keyword = @enumFromInt(prefix.?),
            .match_type = KeywordMatch(Keywords).MatchType.PrefixMatch,
        };
    }
    return null;
}

pub fn LexerTypes(comptime Keywords: type) type {
    return struct {
        pub const Token = struct {
            pub const TokenValue = union(TokenKind) {
                Unknown: void,
                Symbol: u32,
                Number: NumberType,
                QuotedString: QuotedString,
                Comment: CommentText,
                Raw: RawText,
                Keyword: Keywords,
                EndOfFile: void,
                EndOfLine: void,
                Identifier: void,
                Tab: void,
                Whitespace: void,
            };
            pub const Self = @This();

            location: TokenLocation,
            value: TokenValue,

            pub fn number(number_type: NumberType) Token {
                return .{
                    .location = TokenLocation.init(),
                    .value = TokenValue{ .Number = number_type },
                };
            }

            pub fn symbol(sym: u32) Token {
                return .{
                    .location = TokenLocation.init(),
                    .value = TokenValue{ .Symbol = sym },
                };
            }

            pub fn keyword(kw: Keywords) Token {
                return .{
                    .location = TokenLocation.init(),
                    .value = TokenValue{ .Keyword = kw },
                };
            }

            pub fn whitespace() Token {
                return .{
                    .location = TokenLocation.init(),
                    .value = TokenValue{ .Whitespace = {} },
                };
            }

            pub fn raw(marker: []const u8, end: ?usize) Token {
                return .{
                    .location = TokenLocation.init(),
                    .value = TokenValue{ .Raw = .{ .marker = marker, .end = end } },
                };
            }

            pub fn tab() Token {
                return .{
                    .location = TokenLocation.init(),
                    .value = TokenValue{ .Tab = {} },
                };
            }

            pub fn identifier() Token {
                return .{
                    .location = TokenLocation.init(),
                    .value = TokenValue{ .Identifier = {} },
                };
            }

            pub fn comment(comment_type: CommentType) Token {
                return .{
                    .location = TokenLocation.init(),
                    .value = TokenValue{ .Comment = .{ .comment_type = comment_type, .terminated = true } },
                };
            }

            pub fn unterminated_comment(comment_type: CommentType) Token {
                return .{
                    .location = TokenLocation.init(),
                    .value = TokenValue{ .Comment = .{ .comment_type = comment_type, .terminated = false } },
                };
            }

            pub fn end_of_line() Token {
                return .{
                    .location = TokenLocation.init(),
                    .value = TokenValue{ .EndOfLine = {} },
                };
            }

            pub fn end_of_file() Token {
                return .{
                    .location = TokenLocation.init(),
                    .value = TokenValue{ .EndOfFile = {} },
                };
            }

            pub fn string(quote_type: QuoteType) Token {
                return .{
                    .location = TokenLocation.init(),
                    .value = TokenValue{ .QuotedString = .{ .quote_type = quote_type, .terminated = true, .triple = false } },
                };
            }

            pub fn unterminated_string(quote_type: QuoteType) Token {
                return .{
                    .location = TokenLocation.init(),
                    .value = TokenValue{ .QuotedString = .{ .quote_type = quote_type, .terminated = false, .triple = false } },
                };
            }

            pub fn matches(this: *const Token, comptime k: TokenKind) bool {
                return switch (this.value) {
                    k => {
                        return true;
                    },
                    else => {
                        return false;
                    },
                };
            }

            pub fn matches_symbol(this: *const Token, symb: u32) bool {
                return switch (this.value) {
                    .Symbol => |sym| {
                        return sym == symb;
                    },
                    else => {
                        return false;
                    },
                };
            }

            pub fn matches_keyword(this: *const Token, kw: Self.KW) bool {
                return switch (this.value) {
                    .Keyword => |keyw| {
                        return keyw == kw;
                    },
                    else => {
                        return false;
                    },
                };
            }

            pub fn is_identifier(this: *const Token) bool {
                return this.matches(.Identifier);
            }
        };

        pub const SkipToken = struct {};

        pub const ScanResult = struct {
            pub const ScanResultType = union(enum) {
                Token: Token,
                Buffer: []const u8,
                SkipToken: SkipToken,
            };

            result: ScanResultType,
            matched: usize,

            pub fn token(t: Token, matched: usize) ScanResult {
                return ScanResult{
                    .result = .{
                        .Token = t,
                    },
                    .matched = matched,
                };
            }

            pub fn buffer(b: []const u8) ScanResult {
                return ScanResult{
                    .result = .{
                        .Buffer = b,
                    },
                    .matched = b.len,
                };
            }

            pub fn skip(s: usize) ScanResult {
                return ScanResult{
                    .result = .{
                        .SkipToken = .{},
                    },
                    .matched = s,
                };
            }
        };

        pub fn ScannerPack(comptime Scanners: []const type) type {
            return struct {
                const Self = @This();

                scanners: std.meta.Tuple(Scanners),

                pub fn init(allocator: std.mem.Allocator) Self {
                    var ret: Self = undefined;
                    inline for (Scanners, 0..) |Scanner, ix| {
                        ret.scanners[ix] = Scanner.init(allocator);
                    }
                    return ret;
                }

                pub fn deinit(this: Self) void {
                    inline for (Scanners, 0..) |Scanner, ix| {
                        if (std.meta.hasFn(Scanner, "deinit")) {
                            @call(.auto, @field(Scanner, "deinit"), .{this.scanners[ix]});
                        }
                    }
                }

                pub fn scan(this: *const Self, buffer: []const u8) ?ScanResult {
                    var ret: ?ScanResult = null;
                    inline for (this.scanners) |scanner| {
                        if (scanner.scan(buffer)) |res| {
                            ret = res;
                            break;
                        }
                    }
                    return ret;
                }
            };
        }

        pub const SlashSlash = "//";
        pub const HashmarkComments = "#";

        pub fn LineComments(comptime Marker: []const u8, comptime ignore: bool) type {
            return struct {
                const Self = @This();

                pub fn init(_: std.mem.Allocator) Self {
                    return .{};
                }

                pub fn scan(_: *const Self, buffer: []const u8) ?ScanResult {
                    var ix: usize = 0;
                    for (0..@min(Marker.len, buffer.len)) |i| {
                        ix = i;
                        if (Marker[i] != buffer[i]) {
                            return null;
                        }
                    }
                    if (ix == buffer.len and ix < Marker.len) {
                        return null;
                    }
                    for (ix..buffer.len) |i| {
                        ix = i;
                        if (buffer[i] == '\n') {
                            break;
                        }
                    }
                    if (ignore) {
                        return ScanResult.skip(ix);
                    } else {
                        return ScanResult.token(Token.comment(.Line), ix);
                    }
                }
            };
        }

        const CBlockCommentBegin = "/*";
        const CBlockCommentEnd = "*/";

        pub fn BlockComments(comptime BeginMarker: []const u8, comptime EndMarker: []const u8, comptime ignore: bool) type {
            return struct {

                // The scanner's state needs to be external from the actual
                // scanner because the comptime magic wants a const when calling
                // methods on scanners. The deinit takes a value instead of a
                // reference for similar reasons.
                const State = struct {
                    in_comment: bool,
                };
                const Self = @This();

                allocator: std.mem.Allocator,
                state: *State,

                pub fn init(allocator: std.mem.Allocator) Self {
                    const state = allocator.create(State) catch std.builtin.panic("OOM!", null, null);
                    state.in_comment = false;
                    return .{
                        .allocator = allocator,
                        .state = state,
                    };
                }

                pub fn deinit(this: Self) void {
                    this.allocator.destroy(this.state);
                }

                pub fn scan(this: *const Self, buffer: []const u8) ?ScanResult {
                    if (this.state.in_comment) {
                        return this.block_comment(buffer, 0);
                    }
                    var ix: usize = 0;
                    for (0..@min(buffer.len, BeginMarker.len)) |i| {
                        ix = i;
                        if (BeginMarker[i] != buffer[i]) {
                            return null;
                        }
                    }
                    if (ix == buffer.len and ix < BeginMarker.len) {
                        return null;
                    }
                    this.state.in_comment = true;
                    ix += 1;
                    return this.block_comment(buffer[ix..], ix);
                }

                pub fn block_comment(this: *const Self, buffer: []const u8, offset: usize) ?ScanResult {
                    for (0..buffer.len) |ix| {
                        var iix = ix;
                        for (ix..@min(buffer.len, ix + EndMarker.len)) |ii| {
                            iix = ii;
                            if (EndMarker[iix - ix] != buffer[iix]) {
                                break;
                            }
                        }
                        iix += 1;
                        if ((iix == buffer.len or buffer[iix] == '\n') and iix - ix < EndMarker.len) {
                            if (ignore) {
                                return ScanResult.skip(offset + iix);
                            } else {
                                return ScanResult.token(Token.unterminated_comment(.Block), offset + iix);
                            }
                        }
                        if (iix - ix == EndMarker.len) {
                            this.state.in_comment = false;
                            if (ignore) {
                                return ScanResult.skip(offset + iix);
                            } else {
                                return ScanResult.token(Token.comment(.Block), offset + iix);
                            }
                        }
                    }
                    unreachable;
                }
            };
        }

        pub const CStyleComments = ScannerPack(&[_]type{
            LineComments(SlashSlash, true),
            BlockComments(CBlockCommentBegin, CBlockCommentEnd, true),
        });

        pub const CStyleCommentsNoIgnore = ScannerPack(&[_]type{
            LineComments(SlashSlash, false),
            BlockComments(CBlockCommentBegin, CBlockCommentEnd, false),
        });

        pub fn RawScanner(comptime BeginMarker: []const u8, EndMarker: []const u8) type {
            return struct {
                const Self = @This();

                pub fn init(_: std.mem.Allocator) Self {
                    return .{};
                }

                pub fn scan(_: *const Self, buffer: []const u8) ?ScanResult {
                    var ix: usize = 0;
                    while (ix < @min(buffer.len, BeginMarker.len)) {
                        if (BeginMarker[ix] != buffer[ix]) {
                            return null;
                        }
                        ix += 1;
                    }
                    if (ix == buffer.len and ix < BeginMarker.len) {
                        return null;
                    }
                    while (ix < buffer.len) {
                        for (ix..@min(buffer.len, ix + EndMarker.len)) |iix| {
                            if (EndMarker[iix - ix] != buffer[iix]) {
                                break;
                            }
                            if (iix - ix + 1 == EndMarker.len) {
                                return ScanResult.token(Token.raw(BeginMarker, ix), iix + 1);
                            }
                        }
                        ix += 1;
                    }
                    return ScanResult.token(Token.raw(BeginMarker, null), ix);
                }
            };
        }

        const NumberScanner = struct {
            const Predicate = enum(usize) {
                Decimal = 0,
                Hex,
                Binary,
                const Self = @This();

                pub fn is(predicate: Self, ch: u8) bool {
                    switch (predicate) {
                        Self.Decimal => return std.ascii.isDigit(ch),
                        Self.Hex => return std.ascii.isHex(ch),
                        Self.Binary => return isBinary(ch),
                    }
                }
            };

            pub fn init(_: std.mem.Allocator) NumberScanner {
                return .{};
            }

            pub fn isBinary(ch: u8) bool {
                return ch == '0' or ch == '1';
            }

            pub fn scan(_: *const NumberScanner, buffer: []const u8) ?ScanResult {
                var number_type = NumberType.Integer;
                var ix: usize = 0;
                const cur = buffer[ix];
                if (!std.ascii.isDigit(cur)) {
                    return null;
                }

                var predicate = Predicate.Decimal;
                if (ix < buffer.len - 1 and cur == '0') {
                    if (buffer[ix + 1] == 'x' or buffer[ix + 1] == 'X') {
                        if (ix == buffer.len - 2 or !std.ascii.isHex(buffer[ix + 2])) {
                            return ScanResult.token(Token.number(NumberType.Integer), ix + 1);
                        }
                        number_type = NumberType.Hex;
                        predicate = Predicate.Hex;
                        ix = ix + 2;
                    } else if (buffer[ix + 1] == 'b' or buffer[ix + 1] == 'B') {
                        if (ix == buffer.len - 2 or (buffer[ix + 2] != '0' and buffer[ix + 2] != '1')) {
                            return ScanResult.token(Token.number(NumberType.Integer), ix + 1);
                        }
                        number_type = NumberType.Binary;
                        predicate = Predicate.Binary;
                        ix = ix + 2;
                    }
                }
                while (ix < buffer.len) {
                    const ch = buffer[ix];
                    if (!Predicate.is(predicate, ch) and ((ch != '.') or (number_type == NumberType.Decimal))) {
                        // FIXME lex '1..10' as '1', '..', '10'. It will now lex as '1.', '.', '10'
                        break;
                    }
                    if (ch == '.') {
                        if (number_type != NumberType.Integer) {
                            break;
                        }
                        number_type = NumberType.Decimal;
                    }
                    ix += 1;
                }
                return ScanResult.token(Token.number(number_type), ix);
            }
        };

        pub const DefaultQuotes = "\"'`";
        pub const SingleDoubleQuotes = "\"'";

        pub fn QuotedStringScanner(comptime Quotes: []const u8) type {
            return struct {
                const Self = @This();
                pub fn init(_: std.mem.Allocator) Self {
                    return .{};
                }

                pub fn scan(_: *const Self, buffer: []const u8) ?ScanResult {
                    var ix: usize = 0;
                    const cur = buffer[ix];
                    if (std.mem.indexOfScalar(u8, Quotes, cur)) |_| {
                        ix += 1;
                        while (ix < buffer.len and buffer[ix] != cur) {
                            ix += if (buffer[ix] == '\\') 2 else 1;
                        }
                        if (ix < buffer.len) {
                            ix += 1;
                        }
                        return ScanResult.token(Token.string(@enumFromInt(cur)), ix);
                    }
                    return null;
                }
            };
        }

        pub fn WhitespaceScanner(comptime ignore: bool) type {
            return struct {
                const Self = @This();

                pub fn init(_: std.mem.Allocator) Self {
                    return .{};
                }

                pub fn scan(_: *const Self, buffer: []const u8) ?ScanResult {
                    var ix: usize = 0;
                    const cur = buffer[ix];
                    switch (cur) {
                        '\n' => {
                            if (ignore) {
                                return ScanResult.skip(1);
                            } else {
                                return ScanResult.token(Token.end_of_line(), 1);
                            }
                        },
                        '\t' => {
                            if (ignore) {
                                return ScanResult.skip(1);
                            } else {
                                return ScanResult.token(Token.tab(), 1);
                            }
                        },
                        ' ' => {
                            while (ix < buffer.len and buffer[ix] == ' ') {
                                ix += 1;
                            }
                            if (ignore) {
                                return ScanResult.skip(ix);
                            } else {
                                return ScanResult.token(Token.whitespace(), ix);
                            }
                        },
                        else => return null,
                    }
                }
            };
        }

        pub const IdentifierScanner = struct {
            pub fn init(_: std.mem.Allocator) IdentifierScanner {
                return .{};
            }

            pub fn scan(_: *const IdentifierScanner, buffer: []const u8) ?ScanResult {
                const cur = buffer[0];
                if (std.ascii.isAlphabetic(cur) or cur == '_') {
                    var ix: usize = 1;
                    while (ix < buffer.len and (std.ascii.isAlphanumeric(buffer[ix]) or buffer[ix] == '_')) {
                        ix += 1;
                    }
                    if (Keywords.match(buffer[0..ix])) |m| {
                        if (m.match_type == KeywordMatch(Keywords).MatchType.FullMatch) {
                            return ScanResult.token(Token.keyword(m.keyword), ix);
                        }
                    }
                    return ScanResult.token(Token.identifier(), ix);
                }
                return null;
            }
        };

        pub const KeywordScanner = struct {
            pub fn init(_: std.mem.Allocator) KeywordScanner {
                return .{};
            }

            pub fn scan(_: *const KeywordScanner, buffer: []const u8) ?ScanResult {
                if (buffer.len == 0) {
                    return null;
                }
                for (1..buffer.len + 1) |ix| {
                    if (Keywords.match(buffer[0..ix])) |m| {
                        switch (m.match_type) {
                            KeywordMatch(Keywords).MatchType.PrefixMatch => continue,
                            KeywordMatch(Keywords).MatchType.FullMatch => return ScanResult.token(Token.keyword(m.keyword), ix),
                        }
                    } else {
                        break;
                    }
                }
                return null;
            }
        };

        pub const SymbolMuncher = struct {
            pub fn init(_: std.mem.Allocator) SymbolMuncher {
                return .{};
            }

            pub fn scan(_: *const SymbolMuncher, buffer: []const u8) ?ScanResult {
                if (buffer.len == 0) {
                    return null;
                }
                return ScanResult.token(Token.symbol(@as(u32, buffer[0])), 1);
            }
        };

        const CScannerPack = ScannerPack(&[_]type{
            CStyleComments,
            NumberScanner,
            QuotedStringScanner(SingleDoubleQuotes),
            WhitespaceScanner(true),
            IdentifierScanner,
            KeywordScanner,
            SymbolMuncher,
        });

        const CScannerPackNoIgnore = ScannerPack(&[_]type{
            CStyleCommentsNoIgnore,
            NumberScanner,
            QuotedStringScanner(SingleDoubleQuotes),
            WhitespaceScanner(false),
            IdentifierScanner,
            KeywordScanner,
            SymbolMuncher,
        });
    };
}

pub fn Lexer(comptime Types: type, Scanner: type) type {
    return struct {
        const Keywords = Types.Keywords;
        const Token = Types.Token;
        const ScanResult = Types.ScanResult;
        const SkipToken = Types.SkipToken;
        const Self = @This();

        const Source = struct {
            lexer: *Self,
            scanner: Scanner,
            buffer: []const u8,
            index: usize = 0,
            location: TokenLocation = .{},
            current: ?Token = null,

            pub fn init(lexer: *Self, src: []const u8) Source {
                return .{
                    .lexer = lexer,
                    .scanner = Scanner.init(lexer.allocator),
                    .buffer = src,
                };
            }

            pub fn deinit(this: *Source) void {
                this.scanner.deinit();
            }

            pub fn push_back(this: *Source, token: *const Token) void {
                this.index = token.location.index + token.location.length;
                this.location = token.location;
                this.location.index = this.index;
                if (token.matches(TokenKind.EndOfLine)) {
                    this.location.line += 1;
                    this.location.column = 0;
                } else {
                    this.location.column += token.location.length;
                }
                this.current = null;
            }

            pub fn peek(this: *Source) ScanResult {
                if (this.current != null) {
                    return ScanResult.token(this.current.?, this.current.?.location.length);
                }
                if (this.index >= this.buffer.len) {
                    return ScanResult.token(Token.end_of_file(), 1);
                }
                const res = this.scanner.scan(this.buffer[this.index..]);
                std.debug.assert(res != null);
                var ret = res.?;
                this.index += ret.matched;
                this.location.length = ret.matched;
                switch (ret.result) {
                    .Token => |*token| token.location = this.location,
                    else => {},
                }
                while (this.location.index < this.index) {
                    if (this.buffer[this.location.index] == '\n') {
                        this.location.line += 1;
                        this.location.column = 0;
                    } else {
                        this.location.column += 1;
                    }
                    this.location.index += 1;
                }
                this.location.length = 0;
                return ret;
            }

            pub fn lex(this: *Source) void {
                if (this.current != null) {
                    this.current = null;
                }
            }

            pub fn length(this: *const Source) usize {
                return this.buffer.len;
            }

            pub fn exhausted(this: *const Source) bool {
                return this.index >= this.buffer.len;
            }
        };

        current: ?Token = null,
        allocator: std.mem.Allocator,
        sources: std.ArrayList(Source),
        last_location: TokenLocation = .{},

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{
                .allocator = allocator,
                .sources = std.ArrayList(Source).init(allocator),
            };
        }

        pub fn deinit(this: *Self) void {
            for (this.sources.items) |*s| {
                s.deinit();
            }
            this.sources.deinit();
        }

        pub fn push_source(this: *Self, src: []const u8) void {
            this.sources.append(Source.init(this, src)) catch std.builtin.panic("OOM", null, null);
        }

        pub fn source(this: *const Self) *Source {
            std.debug.assert(this.sources.items.len > 0);
            return &this.sources.items[this.sources.items.len - 1];
        }

        pub fn text(this: *const Self, token: Token) []const u8 {
            return this.source().buffer[token.location.index .. token.location.index + token.location.length];
        }

        pub fn peek(this: *Self) Token {
            if (this.current != null) {
                return this.current.?;
            }
            if (this.sources.items.len == 0) {
                this.current.? = Token.end_of_file();
                return this.current.?;
            }
            while (this.current == null) {
                const res = this.source().peek();
                switch (res.result) {
                    .Token => |token| blk: {
                        if (token.matches(TokenKind.EndOfFile)) {
                            var s = this.sources.pop();
                            s.deinit();
                            if (!this.exhausted()) {
                                break :blk;
                            }
                        }
                        this.current = token;
                    },
                    .Buffer => |buf| this.push_source(buf),
                    .SkipToken => {},
                }
            }
            return this.current.?;
        }

        pub fn lex(this: *Self) Token {
            const ret = this.peek();
            if (this.sources.items.len > 0) {
                this.source().lex();
            }
            this.current = null;
            this.last_location = ret.location;
            return ret;
        }

        pub fn expect(this: *Self, kind: TokenKind) !Token {
            const ret = this.peek();
            if (!ret.matches(kind)) {
                return error.UnexpectedTokenKind;
            }
            return this.lex();
        }

        pub fn accept(this: *Self, kind: TokenKind) bool {
            const ret = this.peek();
            if (!ret.matches(kind)) {
                return false;
            }
            this.lex();
            return true;
        }

        pub fn expect_keyword(this: *Self, code: Keywords) !void {
            const ret = this.peek();
            if (!ret.matches_keyword(code)) {
                return error.UnexpectedKeywordOrTokenKind;
            }
            _ = this.lex();
        }

        pub fn accept_keyword(this: *Self, code: Keywords) bool {
            const ret = this.peek();
            if (!ret.matches_keyword(code)) {
                return false;
            }
            _ = this.lex();
            return true;
        }

        pub fn expect_symbol(this: *Self, symbol: u32) !void {
            const ret = this.peek();
            if (!ret.matches_symbol(symbol)) {
                return error.UnexpectedSymbolOrTokenKind;
            }
            _ = this.lex();
        }

        pub fn accept_symbol(this: *Self, symbol: u32) bool {
            const ret = this.peek();
            if (!ret.matches_symbol(symbol)) {
                return false;
            }
            _ = this.lex();
            return true;
        }

        pub fn expect_identifier(this: *Self) !Token {
            const ret = this.peek();
            if (!ret.is_identifier()) {
                return error.ExpectedIdentifier;
            }
            return this.lex();
        }

        pub fn accept_identifier(this: *Self) ?Token {
            const ret = this.peek();
            if (!ret.is_identifier()) {
                return null;
            }
            return this.lex();
        }

        pub fn next_matches(this: *Self, kind: TokenKind) bool {
            const n = this.peek();
            return n.matches(kind);
        }

        pub fn exhausted(this: *const Self) bool {
            return this.sources.items.len == 0;
        }

        pub fn location(this: *const Self) TokenLocation {
            std.debug.assert(!this.exhausted());
            return this.source().location();
        }
    };
}

const NoKeywords = struct {
    const Self = @This();
    pub fn match(_: []const u8) ?KeywordMatch(Self) {
        return null;
    }
};

test "Initialize stuff" {
    const lexer_types = LexerTypes(NoKeywords);

    _ = lexer_types.ScanResult.buffer("Hello");
    _ = lexer_types.CScannerPack.init(std.heap.c_allocator);
}

test "Line Comment Scanner" {
    const lexer_types = LexerTypes(NoKeywords);
    var scanner = lexer_types.LineComments("//", false).init(std.heap.c_allocator);
    const res = scanner.scan(
        \\// Well hello there
        \\foo bar
    );
    if (res) |r| {
        switch (r.result) {
            .Token => |t| {
                try expect(t.matches(TokenKind.Comment));
                try expect(t.value.Comment.comment_type == CommentType.Line);
                try expect(t.value.Comment.terminated);
                try expect(r.matched == "// Well hello there".len);
            },
            else => try expect(false),
        }
    } else {
        try expect(false);
    }
}

test "Block Comment Scanner" {
    const lexer_types = LexerTypes(NoKeywords);
    var scanner = lexer_types.BlockComments("/*", "*/", false).init(std.heap.c_allocator);
    const res = scanner.scan(
        \\/* Well hello there */ more stuff
        \\foo bar
    );
    if (res) |r| {
        switch (r.result) {
            .Token => |t| {
                try expect(t.matches(TokenKind.Comment));
                try expect(t.value.Comment.comment_type == CommentType.Block);
                try expect(t.value.Comment.terminated);
                try expect(r.matched == "/* Well hello there */".len);
            },
            else => try expect(false),
        }
    } else {
        try expect(false);
    }
}

test "Raw Scanner" {
    const lexer_types = LexerTypes(NoKeywords);
    var scanner = lexer_types.RawScanner("@begin", "@end").init(std.heap.c_allocator);
    const res = scanner.scan(
        \\@begin
        \\   There is stuff here
        \\@end
        \\foo bar
    );
    if (res) |r| {
        switch (r.result) {
            .Token => |t| {
                try expect(t.matches(TokenKind.Raw));
                try std.testing.expectEqualStrings(t.value.Raw.marker, "@begin");
                if (t.value.Raw.end) |end| {
                    try expect(end ==
                        \\@begin
                        \\   There is stuff here
                        \\

                        .len);
                } else {
                    try expect(false);
                }
                try expect(r.matched ==
                    \\@begin
                    \\   There is stuff here
                    \\@end

                    .len);
            },
            else => try expect(false),
        }
    } else {
        try expect(false);
    }
}

test "Number Scanner" {
    const lexer_types = LexerTypes(NoKeywords);
    var scanner = lexer_types.NumberScanner.init(std.heap.c_allocator);
    var ix: usize = 0;
    const numbers = "4 3.14 0xBABECAFE 0b0110";
    const lengths = .{ 1, 4, 10, 6 };
    const types = .{ NumberType.Integer, NumberType.Decimal, NumberType.Hex, NumberType.Binary };
    inline for (lengths, types) |len, typ| {
        const res = scanner.scan(numbers[ix..]);
        if (res) |r| {
            switch (r.result) {
                .Token => |t| {
                    try expect(t.matches(TokenKind.Number));
                    try expect(t.value.Number == typ);
                    try expect(r.matched == len);
                    ix += len + 1;
                },
                else => try expect(false),
            }
        } else {
            try expect(false);
        }
    }
}

test "Quoted String Scanner" {
    const lexer_types = LexerTypes(NoKeywords);
    var scanner = lexer_types.QuotedStringScanner("'\"`").init(std.heap.c_allocator);
    var ix: usize = 0;
    const strings =
        \\"Hello" 'Hello' `Hello`
    ;
    const quotes = .{ QuoteType.DoubleQuote, QuoteType.SingleQuote, QuoteType.BackQuote };
    inline for (quotes) |quote| {
        const res = scanner.scan(strings[ix..]);
        if (res) |r| {
            switch (r.result) {
                .Token => |t| {
                    try expect(t.matches(TokenKind.QuotedString));
                    try expect(t.value.QuotedString.quote_type == quote);
                    try expect(t.value.QuotedString.terminated);
                    try expect(t.value.QuotedString.triple == false);
                    try expect(r.matched == 7);
                    ix += 8;
                },
                else => try expect(false),
            }
        } else {
            try expect(false);
        }
    }
}

test "Whitespace Scanner" {
    const lexer_types = LexerTypes(NoKeywords);
    var scanner = lexer_types.WhitespaceScanner(false).init(std.heap.c_allocator);
    var ix: usize = 0;
    const ws = "    x\nx\tx";
    const kinds = .{ TokenKind.Whitespace, TokenKind.EndOfLine, TokenKind.Tab };
    inline for (kinds) |kind| {
        const res = scanner.scan(ws[ix..]);
        try expect(res != null);
        switch (res.?.result) {
            .Token => |t| {
                try expect(t.matches(kind));
                try expect(res.?.matched == if (kind == TokenKind.Whitespace) 4 else 1);
                ix += res.?.matched + 1;
            },
            else => try expect(false),
        }
    }
}

test "Identifier Scanner" {
    const lexer_types = LexerTypes(NoKeywords);
    var scanner = lexer_types.IdentifierScanner.init(std.heap.c_allocator);
    var ix: usize = 0;
    const idents = "ident ide_t ide9t iden9 _dent";
    inline for (0..5) |_| {
        const res = scanner.scan(idents[ix..]);
        try expect(res != null);
        switch (res.?.result) {
            .Token => |t| {
                try expect(t.matches(TokenKind.Identifier));
                try expect(res.?.matched == 5);
                ix += res.?.matched + 1;
            },
            else => try expect(false),
        }
    }
}

const TestKeywords = enum {
    If,
    Then,
    Else,
    While,

    const Self = @This();
    pub fn match(s: []const u8) ?KeywordMatch(Self) {
        return match_enum(Self, s);
    }
};

test "Keyword Scanner" {
    const lexer_types = LexerTypes(TestKeywords);
    var scanner = lexer_types.KeywordScanner.init(std.heap.c_allocator);
    var ix: usize = 0;
    const kws = "if while else";
    const kw_codes = .{ TestKeywords.If, TestKeywords.While, TestKeywords.Else };
    inline for (0..3, kw_codes) |_, code| {
        const res = scanner.scan(kws[ix..]);
        try expect(res != null);
        switch (res.?.result) {
            .Token => |t| {
                try expect(t.matches(TokenKind.Keyword));
                try expect(t.value.Keyword == code);
                try expect(res.?.matched == @tagName(code).len);
                ix += res.?.matched + 1;
            },
            else => try expect(false),
        }
    }
}

const test_string =
    \\ if(x == 12) {
    \\   // Success
    \\   print("Boo!");
    \\ } else {
    \\   /* Failure */
    \\   print("Error");
    \\ }
    \\
;

test "Initialize lexer" {
    const lexer_types = LexerTypes(TestKeywords);
    var lexer = Lexer(lexer_types, lexer_types.CScannerPack).init(std.heap.c_allocator);
    lexer.push_source(test_string);
    lexer.deinit();
}

test "Lex stuff" {
    const lexer_types = LexerTypes(TestKeywords);
    var lexer = Lexer(lexer_types, lexer_types.CScannerPackNoIgnore).init(std.heap.c_allocator);
    lexer.push_source(test_string);

    const expected = [_]TokenKind{
        .Whitespace,
        .Keyword,
        .Symbol,
        .Identifier,
        .Whitespace,
        .Symbol,
        .Symbol,
        .Whitespace,
        .Number,
        .Symbol,
        .Whitespace,
        .Symbol,
        .EndOfLine,
        .Whitespace,
        .Comment,
        .EndOfLine,
        .Whitespace,
        .Identifier,
        .Symbol,
        .QuotedString,
        .Symbol,
        .Symbol,
        .EndOfLine,
        .Whitespace,
        .Symbol,
        .Whitespace,
        .Keyword,
        .Whitespace,
        .Symbol,
        .EndOfLine,
        .Whitespace,
        .Comment,
        .EndOfLine,
        .Whitespace,
        .Identifier,
        .Symbol,
        .QuotedString,
        .Symbol,
        .Symbol,
        .EndOfLine,
        .Whitespace,
        .Symbol,
        .EndOfLine,
        .EndOfFile,
    };

    inline for (expected) |e| {
        const t = lexer.peek();
        try expect(t.value == e);
        // std.debug.print("{s}\n", .{@tagName(t.value)});
        // switch (t.value) {
        //     .Symbol => |s| std.debug.print("Symbol '{c}'\n", .{@as(u8, @intCast(s))}),
        //     .Identifier => std.debug.print("Identifier '{s}'\n", .{lexer.text(t)}),
        //     .Keyword => |kw| std.debug.print("Keyword '{s}'\n", .{@tagName(kw)}),
        //     .EndOfFile => std.debug.print("EOF\n", .{}),
        //     .EndOfLine => std.debug.print("EOL\n", .{}),
        //     .Tab => std.debug.print("TAB\n", .{}),
        //     .Whitespace => std.debug.print("Whitespace\n", .{}),
        //     .Comment => |c| std.debug.print("Comment {s} {s}\n", .{ @tagName(c.comment_type), lexer.text(t) }),
        //     .QuotedString => |qt| std.debug.print("QuotedString {s} {s}\n", .{ @tagName(qt.quote_type), lexer.text(t) }),
        //     .Number => |nt| std.debug.print("Number {s} {s}\n", .{ @tagName(nt), lexer.text(t) }),
        //     else => std.debug.print("?? {s}\n", .{lexer.text(t)}),
        // }
        _ = lexer.lex();
    }
    lexer.deinit();
}
