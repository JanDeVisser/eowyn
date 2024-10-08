const std = @import("std");

const lxr = @import("lexer.zig");
const hashset = @import("hashset.zig");
const resolve = @import("resolve.zig");

pub const Allocator = std.mem.Allocator;
pub const Set = hashset.HashSet(RuleEntry);
pub const Resolver = resolve.Resolver;

pub const NonTerminalDef = 200;
pub const NonTerminalDefStr = ":=";

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
        while (it.next()) |e| {
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
// Rules for First Sets
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
// Rules for Follow Sets
//
// First put $ (the end of input marker) in Follow(S) (S is the start symbol)
// If there is a production A → aBb, (where a can be a whole string)
//   then everything in FIRST(b) except for ε is placed in FOLLOW(B).
// If there is a production A → aB,
//   then everything in FOLLOW(A) is in FOLLOW(B)
// If there is a production A → aBb, where FIRST(b) contains ε,
//   then everything in FOLLOW(A) is in FOLLOW(B)
//

pub const NonTerminal = struct {
    allocator: Allocator,
    variables: GrammarVariables,
    state: u64,
    name: []const u8,
    rules: std.ArrayList(Rule),
    parse_table: std.AutoHashMap(RuleEntry, usize),
    firsts_: ?Set,
    follows_: ?Set,

    pub fn init(allocator: Allocator, name: []const u8) NonTerminal {
        return .{
            .allocator = allocator,
            .variables = GrammarVariables.init(allocator),
            .state = std.hash_map.hashString(name),
            .name = name,
            .rules = std.ArrayList(Rule).init(allocator),
            .parse_table = std.AutoHashMap(RuleEntry, usize).init(allocator),
            .firsts_ = null,
            .follows_ = null,
        };
    }

    pub fn deinit(this: *NonTerminal) void {
        this.firsts.deinit();
        this.follows.deinit();
        for (this.rules) |rule| {
            rule.deinit();
        }
        this.rules.deinit();
        this.parse_table.deinit();
        this.variables.deinit();
    }

    pub fn format(this: NonTerminal, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        try w.print("{s} :=", .{this.name});
        var first = true;
        for (this.rules.items) |rule| {
            if (!first) {
                _ = try w.write("|");
            }
            first = false;
            try w.print("{} ", .{rule});
        }
        _ = try w.write(";");
    }

    pub fn firsts(this: *NonTerminal, grammar: Grammar) !*Set {
        if (this.firsts_) |*f| {
            return f;
        }
        var f = Set.init(this.ge.grammar.allocator);
        for (this.rules.items) |rule| {
            try f.union_with(rule.firsts(grammar));
        }
        if (f.empty()) {
            try f.add(.Empty);
        }
        this.firsts_ = f;
        return this.firsts_;
    }

    pub fn follows(this: *NonTerminal, _: Grammar) !*Set {
        if (this.follows_) |*f| {
            return f;
        }
        var f = Set.init(this.allocator);
        if (std.mem.eql(u8, this.ge.grammar.entry_point, this.name)) {
            try f.add(.End);
        }
        this.follows_ = f;
        return this.follows_;
    }

    pub fn check_LL1(this: NonTerminal) bool {
        var ret = true;
        for (this.rules.items, 0..) |*r_i, i| {
            const tail = this.rules.items[i + 1 ..];
            for (tail, 0..) |*r_j, j| {
                const ok = r_i.firsts().disjoint(r_j.firsts());
                if (!ok) {
                    std.debug.print("Grammar not LL(1): non-terminal {s} - Firsts for rules {} and {} not disjoint\n", .{ this.name, i, i + j });
                    std.debug.print("FIRSTS({}): {}\n", .{ i, r_i.first });
                    std.debug.print("FIRSTS({}}): {}\n", .{ i + j, r_j.firsts });
                }
                ret = ret and ok;
                if (r_j.firsts().contains(.Empty)) {
                    ok = (try r_i.firsts()).disjoint(try r_i.follows());
                    if (!ok) {
                        std.debug.print("Grammar not LL(1): non-terminal {s} - Firsts for rule {} follows not disjoint\n", .{ this.name, i });
                    }
                    ret = ret and ok;
                    ret = ret and (try r_i.firsts()).disjoint(try this.follows());
                }
            }
        }
        return ret;
    }

    pub fn build_parse_table(this: *NonTerminal, grammar: Grammar) !void {
        this.parse_table.clearRetainingCapacity();
        for (this.rules.items) |*rule| {
            for (rule.firsts) |first| {
                try rule.add_parse_table_entry(this, first, grammar);
            }
        }
    }
};

pub const Rule = struct {
    allocator: Allocator,
    entries: std.ArrayList(RuleEntry),
    firsts_: ?Set,
    follows_: ?Set,

    pub fn init(allocator: Allocator) Rule {
        return .{
            .allocator = allocator,
            .entries = std.ArrayList(RuleEntry).init(allocator),
            .firsts_ = null,
            .follows_ = null,
        };
    }

    pub fn deinit(this: *Rule) void {
        this.entries.deinit();
        if (this.first != undefined) {
            this.firsts.deinit();
        }
        this.follows.deinit();
    }

    pub fn format(this: Rule, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        if (this.entries.items.len == 0) {
            _ = try w.write(" ε");
        } else {
            for (this.entries.items) |entry| {
                try w.print("{}", .{entry});
            }
        }
    }

    pub fn firsts(this: *Rule, grammar: Grammar) !*Set {
        //     First (Y1Y2..Yk) is everything in First(Y1) <except for ε >
        //       as well as everything in First(Y2..Yk)
        //     If First(Y1) First(Y2)..First(Yk) all contain ε
        //       add ε to First(Y1Y2..Yk) as well.

        if (this.firsts_) |*f| {
            return f;
        }

        var f = Set.init(this.allocator);
        try f.add(.Empty);
        for (this.entries.items) |e| {
            f.remove(.Empty);
            try e.firsts(grammar, f);
            if (!f.contains(.Empty)) {
                break;
            }
        }
        this.firsts_ = f;
        return this.firsts_;
    }

    pub fn follows(this: *Rule, grammar: Grammar) !void {
        _ = this;
        _ = grammar;
    }

    fn add_parse_table_entry(this: *Rule, non_terminal: NonTerminal, entry: RuleEntry, grammar: Grammar) !void {
        switch (entry) {
            .End => {
                for (non_terminal.follows(grammar)) |follow| {
                    try this.add_parse_table_entry(non_terminal, follow, grammar);
                }
            },
            else => {
                if (!non_terminal.parse_table.contains(entry)) {
                    try non_terminal.parse_table.put(entry, this.index);
                }
            },
        }
    }
};

const RuleEntry = union(enum) {
    Empty: void,
    End: void,
    Action: GrammarAction,
    Terminal: lxr.Token.Kind,
    NonTerminal: []const u8,

    pub fn format(this: RuleEntry, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        _ = try w.write(" ");
        switch (this) {
            .Empty => _ = try w.write("ε"),
            .End => _ = try w.write("☐"),
            .Action => |a| try w.print("[ {s} ]", .{a.full_name}),
            .Terminal => |k| try w.print("{s}", .{k}),
            .NonTerminal => |nt| _ = try w.write(nt),
        }
    }

    pub fn firsts(this: RuleEntry, grammar: Grammar, f: *Set) !void {
        switch (this) {
            .End => unreachable,
            .Empty, .Action => {},
            .Terminal => {
                // If X is a terminal then First(X) is just X!
                f.add(this);
            },
            .NonTerminal => |nt_name| {
                if (grammar.non_terminals.get(nt_name)) |nt| {
                    try f.addAll(try nt.firsts(grammar));
                } else {
                    return error.NonTerminalNotFound;
                }
            },
        }
    }
};

pub const Grammar = struct {
    allocator: Allocator,
    lexer: lxr.Config,
    resolver: Resolver,
    variables: GrammarVariables,
    non_terminals: std.StringArrayHashMap(NonTerminal),
    entry_point: ?[]const u8 = null,
    strategy: ParsingStrategy = .TopDown,
    prefix: ?[]const u8 = null,
    lib: ?[]const u8 = null,
    build_func: ?[]const u8 = null,
    libs: std.ArrayList([]const u8),
    dryrun: bool = false,

    pub fn init(allocator: Allocator) !Grammar {
        return .{
            .allocator = allocator,
            .variables = GrammarVariables.init(allocator),
            .non_terminals = std.StringArrayHashMap(NonTerminal).init(allocator),
            .libs = std.ArrayList([]const u8).init(allocator),
        };
    }

    pub fn format(this: Grammar, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        var it = this.non_terminals.valueIterator();
        while (it.next()) |nt| {
            try w.print("{}\n", .{nt});
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

    fn follows_reducer(this: *Grammar, non_terminal: *NonTerminal, current_sum: usize) !bool {
        var next: ?*RuleEntry = null;

        var next_firsts = std.ArrayList(usize).init(this.allocator);
        defer next_firsts.deinit();
        for (non_terminal.rules.items) |rule| {
            for (rule.entries.items, 0..) |*rule_entry, j| {
                const tail = non_terminal.rules.items[j + 1 ..];
                switch (rule_entry) {
                    .NonTerminal => |nt_name| {
                        next_firsts.clear();
                        next = null;
                        for (tail) |it| {
                            if (next == null) {
                                next = it;
                            }
                            next_firsts.union_with(it.firsts);
                            if (!next_firsts.contains(.Empty)) {
                                break;
                            }
                        }
                        const nt: NonTerminal = this.non_terminals.get(nt_name) orelse unreachable;
                        if (next == null or next_firsts.contains(.Empty)) {
                            try nt.follows.union_with(non_terminal.follows);
                        }
                        next_firsts.remove(.Empty);
                        try nt.follows.union_with(next_firsts);
                    },
                    else => {},
                }
            }
        }
        return current_sum == non_terminal.follows.size();
    }

    fn build_firsts(this: *Grammar) !void {
        var it = this.non_terminals.keyIterator();
        while (it.next()) |nt| {
            try nt.build_firsts(this);
        }
    }

    fn build_follows(this: *Grammar) !void {
        var done = false;
        while (!done) {
            var iter = this.non_terminals.keyIterator();
            while (iter.next()) |nt| {
                done = done and try this.follows_reducer(nt);
            }
        }
    }

    fn check_LL1(this: Grammar) bool {
        var it = this.non_terminals.keyIterator();
        while (it.next()) |non_terminal| {
            if (!non_terminal.check_LL1(this)) {
                return false;
            }
        }
        return true;
    }

    pub fn analyze(this: *Grammar) !bool {
        try this.build_firsts();
        try this.build_follows();
        const ll_1 = try this.check_LL1();
        if (ll_1) {
            this.build_parse_table();
        }
        return ll_1;
    }
};
