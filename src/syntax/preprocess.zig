const std = @import("std");

pub const Embed = struct {
    file_name: []const u8,
};

pub const Include = struct {
    file_name: []const u8,
};

pub const Insert = struct {
    script_text: []const u8,
};
