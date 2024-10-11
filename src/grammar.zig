const std = @import("std");

const lxr = @import("lexer.zig");
const hashset = @import("hashset.zig");
const resolve = @import("resolve.zig");

pub const Allocator = std.mem.Allocator;

pub fn DeepHashCtx(comptime T: type) type {
    return struct {
        pub fn hash(this: @This(), key: T) u64 {
            _ = this;
            var hasher = std.hash.Wyhash.init(0);
            return std.hash.autoHashStrat(&hasher, key, .Deep);
        }

        pub fn eql(this: @This(), a: Symbol, b: Symbol) bool {
            _ = this;
            return std.meta.eql(a, b);
        }
    };
}

pub const SymbolCtx = struct {
    pub fn hash(this: @This(), key: Symbol) u64 {
        _ = this;
        var hasher = std.hash.Wyhash.init(0);
        switch (key) {
            .Empty, .End => std.hash.autoHash(&hasher, @intFromEnum(key)),
            .Action => |ga| std.hash.autoHashStrat(&hasher, ga.full_name, .Deep),
            .Terminal => |k| std.hash.autoHashStrat(&hasher, k, .Deep),
            .NonTerminal => |nt| std.hash.autoHashStrat(&hasher, nt, .Deep),
        }
        return hasher.final();
    }

    pub fn eql(this: @This(), a: Symbol, b: Symbol) bool {
        _ = this;
        if (@intFromEnum(a) != @intFromEnum(b)) return false;
        return switch (a) {
            .Empty, .End => true,
            .Action => |ga| ga.eql(b.Action),
            .Terminal => |k| std.meta.eql(k, b.Terminal),
            .NonTerminal => |nt| std.mem.eql(u8, nt, b.NonTerminal),
        };
    }
};

pub const Set = hashset.HashSet(Symbol, SymbolCtx);
pub const Resolver = resolve.Resolver;

pub const RuleDef = 200;
pub const RuleDefStr = ":=";

pub const LibStr = "lib";
pub const PrefixStr = "prefix";
pub const StrategyStr = "strategy";
pub const LexerStr = "lexer";
pub const GrammarBuildFuncStr = "grammar_buildfunc";
pub const LexercfgBuildFuncStr = "lexercfg_buildfunc";

pub const ParsingStrategy = enum {
    TopDown,
    BottomUp,
};

pub const ValueType = enum {
    Null,
    Bool,
    Int,
    String,
    Token,
};

pub const Value = union(ValueType) {
    Null: void,
    Bool: bool,
    Int: i64,
    String: []const u8,
    Token: lxr.Token,

    pub fn deinit(this: *Value) void {
        _ = this;
    }

    pub fn decode(lexer: lxr.Lexer, s: []const u8) !Value {
        var v = s;
        const typ_str: []const u8 = if (std.mem.indexOfScalar(u8, s, ':')) |colon| blk: {
            v = s[colon + 1 ..];
            break :blk s[0..colon];
        } else "String";
        const typ: ValueType = @enumFromInt(blk: {
            inline for (@typeInfo(ValueType).@"enum".fields) |fld| {
                if (std.ascii.eqlIgnoreCase(typ_str, fld.name)) {
                    break :blk fld.value;
                }
            }
            break :blk @intFromEnum(ValueType.String);
        });
        return switch (typ) {
            .Null => .Null,
            .Bool => .{
                .Bool = std.ascii.eqlIgnoreCase("true", v),
            },
            .Int => .{
                .Int = try std.fmt.parseInt(i64, v, 0),
            },
            .String => .{
                .String = v,
            },
            .Token => .{
                .Token = try lxr.Token.decode(v, lexer),
            },
        };
    }

    pub fn format(this: Value, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        switch (this) {
            .Null => _ = try w.write("null"),
            .Bool => |b| _ = try w.write(if (b) "true" else "false"),
            .Int => |i| try w.print("{}", .{i}),
            .String => |s| _ = try w.write(s),
            .Token => |t| try w.print("{s}", .{t}),
        }
    }
};

pub const GrammarAction = struct {
    allocator: Allocator,
    resolver: Resolver,
    func: ?Action = null,
    full_name: []const u8,
    data: ?Value,

    const Action = *fn (target: *anyopaque, data: ?*Value) void;

    pub fn init(allocator: Allocator, resolver: Resolver, name: []const u8, data: ?Value) !GrammarAction {
        return .{
            .allocator = allocator,
            .resolver = resolver,
            .full_name = name,
            .data = data,
        };
    }

    pub fn action(this: *GrammarAction) !Action {
        if (this.func) |f| {
            return f;
        }
        this.func = try this.resolver.resolve(Action, this.full_name);
        return this.func;
    }

    pub fn call(this: *GrammarAction, target: *anyopaque) !void {
        const fnc = try this.action();
        try fnc(target, if (this.data) &this.data else null);
    }

    pub fn eql(this: GrammarAction, other: GrammarAction) bool {
        if (!std.mem.eql(u8, this.full_name, other.full_name)) return false;
        return std.meta.eql(this.data, other.data);
    }
};

pub const GrammarVariables = struct {
    allocator: Allocator,
    variables: std.StringHashMap(GrammarVariable),

    const GrammarVariable = struct {
        name: []const u8,
        value: Value,

        pub fn init(name: []const u8, value: Value) GrammarVariable {
            return .{
                .name = name,
                .value = value,
            };
        }

        pub fn deinit(this: *GrammarVariable) void {
            this.value.deinit();
        }

        pub fn format(this: GrammarVariable, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            try w.print("(\"{s}\", {})", .{ this.name, this.value });
        }
    };

    pub fn init(allocator: Allocator) GrammarVariables {
        return .{
            .allocator = allocator,
            .variables = std.StringHashMap(GrammarVariable).init(allocator),
        };
    }

    pub fn deinit(this: *GrammarVariables) void {
        var it = this.variables.iterator();
        for (it.next()) |e| {
            this.allocator.free(e.key_ptr.*);
            e.value_ptr.*.deinit();
        }
        this.variables.deinit();
    }

    pub fn set(this: *GrammarVariables, name: []const u8, value: Value) !void {
        const n = try this.allocator.dupe(u8, name);
        try this.variables.put(n, GrammarVariable.init(n, value));
    }

    pub fn get(this: GrammarVariables, name: []const u8) ?Value {
        if (this.variables.get(name)) |v| {
            return v.value;
        }
        return null;
    }

    pub fn has(this: GrammarVariables, name: []const u8) bool {
        return this.variables.contains(name);
    }

    pub fn iterator(this: GrammarVariables) std.StringHashMap(GrammarVariables).ValueIterator {
        return this.variables.valueIterator();
    }
};

//
// https://gist.github.com/trijuhari/f13e4ca6ef9d0557fc33d833a8151392
// https://gist.github.com/DmitrySoshnikov/29f7a9425cdab69ea68f
//
// Sequences for First Sets
//
// If X is a terminal then First(X) is just X!
// If there is a Production X → ε then add ε to first(X)
// If there is a Production X → Y1Y2..Yk then add first(Y1Y2..Yk) to first(X)
//   First(Y1Y2..Yk) is -
//     if First(Y1) doesn't contain ε
//       First(Y1)
//     else if First(Y1) does contain ε
//       First (Y1Y2..Yk) is everything in First(Y1) <except for ε >
//       as well as everything in First(Y2..Yk)
//     If First(Y1) First(Y2)..First(Yk) all contain ε
//       add ε to First(Y1Y2..Yk) as well.
//

//
// Sequences for Follow Sets
//
// First put $ (the end of input marker) in Follow(S) (S is the start symbol)
// If there is a production A → aBb, (where a can be a whole string)
//   then everything in FIRST(b) except for ε is placed in FOLLOW(B).
// If there is a production A → aB,
//   then everything in FOLLOW(A) is in FOLLOW(B)
// If there is a production A → aBb, where FIRST(b) contains ε,
//   then everything in FOLLOW(A) is in FOLLOW(B)
//

pub const GrammarError = std.mem.Allocator.Error || error{RuleNotFound};

threadlocal var firsts_tx = hashset.StringSet.init(std.heap.c_allocator);
threadlocal var follows_tx = hashset.StringSet.init(std.heap.c_allocator);

pub const Rule = struct {
    grammar: *Grammar,
    variables: GrammarVariables,
    state: u64,
    non_terminal: []const u8,
    sequences: std.ArrayList(Sequence),
    parse_table: std.HashMap(Symbol, usize, SymbolCtx, std.hash_map.default_max_load_percentage),
    firsts: Set,
    follows: Set,

    pub fn init(grammar: *Grammar, non_terminal: []const u8) Rule {
        return .{
            .grammar = grammar,
            .variables = GrammarVariables.init(grammar.allocator),
            .state = std.hash_map.hashString(non_terminal),
            .non_terminal = non_terminal,
            .sequences = std.ArrayList(Sequence).init(grammar.allocator),
            .parse_table = std.HashMap(Symbol, usize, SymbolCtx, std.hash_map.default_max_load_percentage).initContext(grammar.allocator, SymbolCtx{}),
            .firsts = Set.init(grammar.allocator),
            .follows = Set.init(grammar.allocator),
        };
    }

    pub fn deinit(this: *Rule) void {
        this.firsts.deinit();
        this.follows.deinit();
        for (this.rules) |rule| {
            rule.deinit();
        }
        this.rules.deinit();
        this.parse_table.deinit();
        this.variables.deinit();
    }

    pub fn format(this: Rule, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        try w.print("{s} :=", .{this.non_terminal});
        var first = true;
        for (this.sequences.items) |seq| {
            if (!first) {
                _ = try w.write("|");
            }
            first = false;
            try w.print("{} ", .{seq});
        }
        _ = try w.write(";");
    }

    pub fn update_firsts(this: *Rule) GrammarError!i64 {
        var count: i64 = 0;
        if (!firsts_tx.contains(this.non_terminal)) {
            // std.debug.print("\"{s}\".update_firsts\n", .{this.non_terminal});
            _ = try firsts_tx.add(this.non_terminal);
            for (this.sequences.items) |*sequence| {
                count += try sequence.build_firsts();
                count += @intCast(try this.firsts.union_with(sequence.firsts));
            }
            if (this.firsts.empty()) {
                count += @intCast(try this.firsts.add(.Empty));
            }
            // std.debug.print("\"{s}\".update_firsts => {}\n", .{ this.non_terminal, this.firsts });
            _ = firsts_tx.remove(this.non_terminal);
        }
        return count;
    }

    pub fn add_to_follows(this: *Rule, f: Set) !usize {
        // std.debug.print("\"{s}\".add_follows({})\n", .{ this.non_terminal, f });
        return try this.follows.union_with(f);
    }

    pub fn update_follows(this: *Rule) !usize {
        var count: usize = 0;
        if (!follows_tx.contains(this.non_terminal)) {
            // std.debug.print("\"{s}\".update_follows {}\n", .{ this.non_terminal, this.follows });
            _ = try follows_tx.add(this.non_terminal);
            for (this.sequences.items) |*seq| {
                count += try seq.build_follows(this.follows);
            }
            _ = follows_tx.remove(this.non_terminal);
        }
        return count;
    }

    pub fn check_LL1(this: Rule, allocator: Allocator) !bool {
        const has_empty = blk: {
            for (this.sequences.items) |seq| {
                if (seq.symbols.items.len == 0) {
                    break :blk true;
                }
            }
            break :blk false;
        };
        for (this.sequences.items, 0..) |seq, i| {
            if (try seq.check_LL1(allocator, seq.firsts, this.sequences.items[i + 1 ..], i + 1)) |j| {
                std.debug.print("LL1 check: first sets {} ({}) and {} ({}) of non-terminal '{s}' are not disjoint\n", .{ i, this.sequences.items[i].firsts, j, this.sequences.items[j].firsts, this.non_terminal });
                return false;
            }
            if (this.sequences.items.len > 1) {
                if (seq.symbols.items.len > 0) {
                    if (seq.firsts.contains(.Empty)) {
                        std.debug.print("LL1 check: first set {} of non-terminal '{s}' derives the Empty symbol\n", .{ i, this.non_terminal });
                        return false;
                    }
                } else if (has_empty) {
                    var f_i_intersect_followA = Set.init(allocator);
                    defer f_i_intersect_followA.deinit();
                    _ = try f_i_intersect_followA.union_with(seq.firsts);
                    _ = f_i_intersect_followA.intersect(this.follows);
                    if (!f_i_intersect_followA.empty()) {
                        std.debug.print("LL1 check: follow set and first set {} of non-terminal '{s}' are not disjoint\n", .{ i, this.non_terminal });
                        return false;
                    }
                }
            }
        }
        return true;
    }

    fn add_transition(this: *Rule, symbol: Symbol, ix: usize) !void {
        switch (symbol) {
            .Empty => {
                var it = this.follows.iterator();
                while (it.next()) |follow| {
                    try this.add_transition(follow, ix);
                }
            },
            else => try this.parse_table.put(symbol, ix),
        }
    }

    pub fn build_parse_table(this: *Rule) !void {
        for (this.sequences.items, 0..) |seq, ix| {
            var f_ix = Set.init(this.grammar.allocator);
            defer f_ix.deinit();
            _ = try Symbol.firsts(seq.symbols.items, this.grammar, &f_ix);
            var it = f_ix.iterator();
            while (it.next()) |symbol| {
                try this.add_transition(symbol, ix);
            }
        }
    }

    pub fn dump_parse_table(this: Rule) void {
        var it = this.parse_table.iterator();
        while (it.next()) |entry| {
            std.debug.print("{s}:{} => {s}\n", .{ this.non_terminal, entry.key_ptr.*, this.sequences.items[entry.value_ptr.*] });
        }
    }
};

pub const Sequence = struct {
    grammar: *Grammar,
    symbols: std.ArrayList(Symbol),
    firsts: Set,

    pub fn init(grammar: *Grammar) Sequence {
        return .{
            .grammar = grammar,
            .symbols = std.ArrayList(Symbol).init(grammar.allocator),
            .firsts = Set.init(grammar.allocator),
        };
    }

    pub fn deinit(this: *Sequence) void {
        this.entries.deinit();
        if (this.first != undefined) {
            this.firsts.deinit();
        }
        this.follows.deinit();
    }

    pub fn format(this: Sequence, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        if (this.symbols.items.len == 0) {
            _ = try w.write(" ε");
        } else {
            for (this.symbols.items) |entry| {
                try w.print(" {}", .{entry});
            }
        }
    }

    pub fn build_firsts(this: *Sequence) !i64 {
        return try Symbol.firsts(this.symbols.items, this.grammar, &this.firsts);
    }

    pub fn build_follows(this: *Sequence, rule_follows: Set) !usize {
        var count: usize = 0;
        for (this.symbols.items, 0..) |symbol, ix| {
            switch (symbol) {
                .NonTerminal => |nt| {
                    const non_terminal = this.grammar.rules.getPtr(nt) orelse return error.RuleNotFound;
                    // std.debug.print("\"{s}\".build_follows({})\n", .{ nt, rule_follows });
                    var f = Set.init(this.grammar.allocator);
                    defer f.deinit();
                    _ = try Symbol.firsts(this.symbols.items[ix + 1 ..], this.grammar, &f);
                    if (f.contains(.Empty)) {
                        count += try non_terminal.add_to_follows(rule_follows);
                        _ = f.remove(.Empty);
                    }
                    count += try non_terminal.add_to_follows(f);
                },
                else => {},
            }
        }
        return count;
    }

    pub fn check_LL1(this: Sequence, allocator: Allocator, f_i: Set, tail: []Sequence, j: usize) !?usize {
        if (tail.len == 0) {
            return null;
        }
        var f_j = Set.init(allocator);
        defer f_j.deinit();
        _ = try f_j.union_with(tail[0].firsts);
        _ = f_j.intersect(f_i);
        if (!f_j.empty()) {
            return j;
        }
        return this.check_LL1(allocator, f_i, tail[1..], j + 1);
    }
};

pub const Symbol = union(enum) {
    Empty: void,
    End: void,
    Action: GrammarAction,
    Terminal: lxr.Token.Kind,
    NonTerminal: []const u8,

    pub fn format(this: Symbol, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        switch (this) {
            .Empty => _ = try w.write("ε"),
            .End => _ = try w.write("☐"),
            .Action => |a| try w.print("[ {s} ]", .{a.full_name}),
            .Terminal => |k| try w.print("{s}", .{k}),
            .NonTerminal => |nt| _ = try w.write(nt),
        }
    }

    pub fn firsts(entries: []Symbol, grammar: *Grammar, f: *Set) GrammarError!i64 {
        if (entries.len == 0) {
            // std.debug.print("Symbol.firsts: entries empty\n", .{});
            return @intCast(try f.add(.Empty));
        }
        var count: i64 = 0;
        // std.debug.print("Symbol.firsts: len: {} entries[0]: {}\n", .{ entries.len, entries[0] });
        count -= @intCast(f.remove(.Empty));
        var firsts_of_entries = Set.init(grammar.allocator);
        defer firsts_of_entries.deinit();
        switch (entries[0]) {
            .End, .Empty, .Terminal => _ = try firsts_of_entries.add(entries[0]),
            .Action => {}, // FIXME
            .NonTerminal => |nt_name| {
                const rule = grammar.rules.getPtr(nt_name) orelse return error.RuleNotFound;
                count += @intCast(try rule.update_firsts());
                _ = try firsts_of_entries.union_with(rule.firsts);
            },
        }
        count += @intCast(try f.union_with(firsts_of_entries));
        // std.debug.print("Symbol.firsts: f: {}\n", .{f});
        if (firsts_of_entries.contains(.Empty)) {
            count += @intCast(try firsts(entries[1..], grammar, f));
        }
        return count;
    }
};

pub const Grammar = struct {
    allocator: Allocator,
    lexer: lxr.Config,
    resolver: Resolver,
    variables: GrammarVariables,
    rules: std.StringArrayHashMap(Rule),
    entry_point: ?[]const u8 = null,
    strategy: ParsingStrategy = .TopDown,
    build_func: ?[]const u8 = null,
    libs: std.ArrayList([]const u8),
    dryrun: bool = false,

    pub fn init(allocator: Allocator) !Grammar {
        return .{
            .allocator = allocator,
            .lexer = lxr.Config.init(allocator),
            .resolver = Resolver.init(null, null),
            .variables = GrammarVariables.init(allocator),
            .rules = std.StringArrayHashMap(Rule).init(allocator),
            .libs = std.ArrayList([]const u8).init(allocator),
        };
    }

    pub fn format(this: Grammar, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        for (this.rules.values()) |r| {
            try w.print("{}\n", .{r});
        }
    }

    pub fn configure(this: *Grammar, name: []const u8, value: []const u8) !void {
        if (std.ascii.eqlIgnoreCase(name, "prefix")) {
            this.resolver.prefix = value;
        }
        if (std.ascii.eqlIgnoreCase(name, "library")) {
            this.resolver.lib = value;
        }
        if (std.ascii.eqlIgnoreCase(name, "lexer")) {
            const scanner = if (std.mem.indexOfScalar(u8, value, ':')) |ix| std.mem.trim(u8, value[0..ix], " \t") else value;
            const scanner_config = if (std.mem.indexOfScalar(u8, value, ':')) |ix| std.mem.trim(u8, value[ix + 1 ..], " \t") else null;
            try this.lexer.configure(scanner, scanner_config);
        }
    }

    pub fn build_firsts(this: *Grammar) !void {
        firsts_tx.clear();
        while (true) {
            var count: i64 = 0;
            for (this.rules.values()) |*rule| {
                count += try rule.update_firsts();
            }
            if (count == 0) break;
        }
    }

    pub fn build_follows(this: *Grammar) !void {
        follows_tx.clear();
        if (this.entry_point) |ep| {
            if (this.rules.getPtr(ep)) |rule| {
                _ = try rule.follows.add(.End);
            }
        }
        while (true) {
            var count: usize = 0;
            for (this.rules.values()) |*rule| {
                count += try rule.update_follows();
            }
            if (count == 0) break;
        }
    }

    pub fn analyze(this: *Grammar) !bool {
        try this.build_firsts();
        try this.build_follows();
        return this.check_LL1();
    }

    pub fn check_LL1(this: Grammar) !bool {
        var ok = true;
        for (this.rules.values()) |rule| {
            ok = ok and try rule.check_LL1(this.allocator);
        }
        return ok;
    }

    pub fn build_parse_table(this: *Grammar) !bool {
        if (!try this.analyze()) return false;
        for (this.rules.values()) |*rule| {
            try rule.build_parse_table();
        }
        return true;
    }

    pub fn dump_parse_table(this: Grammar) void {
        for (this.rules.values()) |rule| {
            rule.dump_parse_table();
        }
    }
};

test "Build Grammar" {
    _ =
        \\ 
        \\program    := [ init ] statements [ done ]
        \\           ;
        \\
        \\statements := [ stmt_start ] statement [ stmt_end ] statements
        \\           |
        \\           ;
        \\
    ;
    var grammar = try Grammar.init(std.heap.c_allocator);
    var r = Rule.init(&grammar, "program");
    var seq = Sequence.init(&grammar);
    try seq.symbols.append(.{ .Action = try GrammarAction.init(grammar.allocator, grammar.resolver, "init", null) });
    try seq.symbols.append(.{ .NonTerminal = "statements" });
    try seq.symbols.append(.{ .Action = try GrammarAction.init(grammar.allocator, grammar.resolver, "done", null) });
    try r.sequences.append(seq);
    try grammar.rules.put(r.non_terminal, r);
    r = Rule.init(&grammar, "statements");
    seq = Sequence.init(&grammar);
    try seq.symbols.append(.{ .Action = try GrammarAction.init(grammar.allocator, grammar.resolver, "stmt_start", null) });
    try seq.symbols.append(.{ .NonTerminal = "statement" });
    try seq.symbols.append(.{ .Action = try GrammarAction.init(grammar.allocator, grammar.resolver, "stmt_end", null) });
    try seq.symbols.append(.{ .NonTerminal = "statements" });
    try r.sequences.append(seq);
    seq = Sequence.init(&grammar);
    try r.sequences.append(seq);
    try grammar.rules.put(r.non_terminal, r);
    std.debug.print("\n{}", .{grammar});
}

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

test "Firsts" {
    const grammar = try build_test_grammar();
    std.debug.print("\n", .{});
    for (grammar.rules.values()) |*r| {
        std.debug.print("Firsts {s}: {}\n", .{ r.non_terminal, r.firsts });
    }
}

test "Follows" {
    const grammar = try build_test_grammar();
    std.debug.print("\n", .{});
    for (grammar.rules.values()) |*r| {
        std.debug.print("Follows {s}: {}\n", .{ r.non_terminal, r.follows });
    }
}

test "Analyze" {
    const grammar = try build_test_grammar();
    try std.testing.expect(try grammar.check_LL1());
}

test "Parse Table" {
    const grammar = try build_test_grammar();
    std.debug.print("\n", .{});
    grammar.dump_parse_table();
}
