const std = @import("std");

pub const MAX_POWER = 9_000_001;
pub const User = struct {
    power: u64,
    name: []const u8,
    manager: ?*const User = null,
};

pub const UserWithDefault = struct {
    power: u64 = 0,
    name: []const u8,
    manager: ?*const User = null,

    pub const SUPER_POWER = 9000;

    pub fn diagnose(self: UserWithDefault) void {
        if (self.power >= SUPER_POWER) {
            std.debug.print("It's over {d}\n", .{SUPER_POWER});
        }
    }

    pub fn init(name: []const u8, power: u64) UserWithDefault {
        return .{
            .name = name,
            .power = power,
        };
    }
};
