const std = @import("std");

pub const StringSet = HashSet([]const u8, std.hash_map.StringContext);

pub fn HashSet(comptime T: type, comptime Context: ?type) type {
    return struct {
        map: Map,

        const Map = if (Context) |Ctx| std.HashMap(T, bool, Ctx, std.hash_map.default_max_load_percentage) else std.AutoHashMap(T, bool);
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

        pub fn add(this: *Self, elem: T) !usize {
            const old_size = this.size();
            try this.map.put(elem, true);
            return this.size() - old_size;
        }

        pub fn contains(this: Self, elem: T) bool {
            return this.map.contains(elem);
        }

        pub fn remove(this: *Self, elem: T) usize {
            return if (this.map.remove(elem)) 1 else 0;
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

        pub fn union_with(this: *Self, other: Self) !usize {
            const old_size = this.size();
            var it = other.iterator();
            while (it.next()) |elem| {
                _ = try this.add(elem);
            }
            return this.size() - old_size;
        }

        pub fn intersect(this: *Self, other: Self) usize {
            var it = this.iterator();
            var count: usize = 0;
            while (it.next()) |elem| {
                if (!other.contains(elem)) {
                    count += this.remove(elem);
                }
            }
            return count;
        }

        pub fn minus(this: *Self, other: Self) usize {
            var it = this.iterator();
            var count: usize = 0;
            while (it.next()) |elem| {
                if (other.contains(elem)) {
                    count += this.remove(elem);
                }
            }
            return count;
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

        pub fn format(this: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            _ = try writer.write("{");
            if (!this.empty()) {
                var it = this.iterator();
                var first = true;
                while (it.next()) |i| {
                    if (!first) {
                        _ = try writer.write(",");
                    }
                    first = false;
                    try writer.print(" {any}", .{i});
                }
                _ = try writer.write(" ");
            }
            _ = try writer.write("}");
        }
    };
}

test "Create HashSet" {
    var s = HashSet(i32, null).init(std.heap.c_allocator);
    _ = try s.add(4);
    _ = try s.add(32);
    try std.testing.expect(s.size() == 2);
}

test "Print HashSet" {
    var s = HashSet(i32, null).init(std.heap.c_allocator);
    _ = try s.add(4);
    _ = try s.add(32);
    const str = try std.fmt.allocPrint(std.heap.c_allocator, "{any}", .{s});
    defer std.heap.c_allocator.free(str);
    try std.testing.expectEqualStrings("{ 4, 32 }", str);
}
