const std = @import("std");

pub fn HashSet(comptime T: type) type {
    return struct {
        map: Map,

        const Map = std.AutoHashMap(T, bool);
        const Self = @This();

        const Iterator = struct {
            key_iterator: Map.KeyIterator,

            pub fn init(map: Map) Iterator {
                return .{
                    .key_iterator = map.keyIterator(),
                };
            }

            pub fn next(this: *Iterator) ?T {
                if (this.key_iterator.next()) |key_ptr| {
                    return key_ptr.*;
                }
                return null;
            }
        };

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{
                .map = Map.init(allocator),
            };
        }

        pub fn deinit(this: *Self) void {
            this.map.deinit();
        }

        pub fn add(this: *Self, elem: T) !void {
            try this.map.put(elem, true);
        }

        pub fn addAll(this: *Self, other: Self) !void {
            var it = other.iterator();
            while (it.next()) |elem| {
                try this.add(elem);
            }
        }

        pub fn contains(this: Self, elem: T) bool {
            return this.map.contains(elem);
        }

        pub fn remove(this: *Self, elem: T) bool {
            return this.map.remove(elem, elem);
        }

        pub fn clear(this: *Self) void {
            this.map.clearRetainingCapacity();
        }

        pub fn size(this: Self) usize {
            return this.map.count();
        }

        pub fn empty(this: Self) bool {
            return this.size() == 0;
        }

        pub fn iterator(this: Self) Iterator {
            return Iterator.init(this.map);
        }

        pub fn union_with(this: *Self, other: Self) !void {
            var it = other.iterator();
            while (it.next()) |elem| {
                this.add(elem);
            }
        }

        pub fn intersect(this: *Self, other: Self) void {
            var it = this.iterator();
            while (it.next()) |elem| {
                if (!other.contains(elem)) {
                    this.remove(elem);
                }
            }
        }

        pub fn minus(this: *Self, other: Self) void {
            var it = this.iterator();
            while (it.next()) |elem| {
                if (other.contains(elem)) {
                    this.remove(elem);
                }
            }
        }

        pub fn disjoint(this: Self, other: Self) bool {
            var it = this.iterator();
            while (it.next()) |elem| {
                if (other.contains(elem)) {
                    return false;
                }
            }
            return true;
        }

        pub fn subset_of(this: Self, other: Self) bool {
            var it = this.iterator();
            while (it.next()) |elem| {
                if (!other.contains(elem)) {
                    return false;
                }
            }
            return true;
        }

        pub fn eql(this: Self, other: Self) bool {
            return this.subset_of(other) and other.subset_of(this);
        }

        pub fn format(value: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            _ = try writer.write("{");
            if (!value.empty()) {
                _ = try writer.write(" ");
                const a = value.map.allocator;
                var strings = try a.alloc([]const u8, value.size());
                errdefer a.free(strings);
                var ix: usize = 0;
                var it = value.iterator();
                while (it.next()) |i| {
                    strings[ix] = try std.fmt.allocPrint(a, "{any}", .{i});
                    ix += 1;
                }
                defer {
                    for (strings) |s| {
                        a.free(s);
                    }
                    a.free(strings);
                }
                const joined = try std.mem.join(a, ", ", strings);
                defer a.free(joined);
                _ = try writer.writeAll(joined);
                _ = try writer.write(" ");
            }
            _ = try writer.write("}");
        }
    };
}

test "Create HashSet" {
    var s = HashSet(i32).init(std.heap.c_allocator);
    try s.add(4);
    try s.add(32);
    try std.testing.expect(s.size() == 2);
}

test "Print HashSet" {
    var s = HashSet(i32).init(std.heap.c_allocator);
    try s.add(4);
    try s.add(32);
    try std.testing.expect(std.mem.eql(u8, try std.fmt.allocPrint(std.heap.c_allocator, "{any}", .{s}), "{ 4, 32 }"));
}
