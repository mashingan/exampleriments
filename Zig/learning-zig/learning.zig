const std = @import("std");
const dprint = std.debug.print;
const user = @import("model/user.zig");
const builtin = @import("builtin");

pub fn main() anyerror!void {
    const u = user.User{
        .power = 9001,
        .name = "Goku",
    };

    const uw: user.UserWithDefault = .{
        .name = "Goku",
    };
    const sm = add(8999, 2);
    dprint("{s} power is {d}\n", .{ u.name, u.power });
    dprint("{s} with default power is {d}\n", .{ uw.name, uw.power });
    dprint("sum is {}\n", .{sm});

    // all same construct
    const a = [5]i32{ 1, 2, 3, 4, 5 };
    const _not_used: [5]i32 = .{ 1, 2, 3, 4, 5 };
    _ = _not_used;
    var b = [_]i32{ 1, 2, 3, 4, 5 };

    const c = a[1..3];
    var end: u8 = 3;
    end += 1;
    const d = a[2..end];
    dprint("{any} with type {any} and {any} with type {any}\n", .{ c, @TypeOf(c), d, @TypeOf(d) });
    const e = b[2..end];
    e[0] = 99;
    dprint("{any} with type {any} and {any} with type {any}\n", .{ b, @TypeOf(b), e, @TypeOf(e) });

    const seed = blk: {
        var seed: u64 = undefined;
        try std.posix.getrandom(std.mem.asBytes(&seed));
        break :blk seed;
    };
    dprint("The seed number is {}\n", .{seed});
    var pnrg = std.rand.DefaultPrng.init(seed);
    const rand = pnrg.random();
    const meth = if (rand.boolean()) "POST" else "GET";
    if (std.ascii.eqlIgnoreCase(meth, "post")) {
        dprint("The method is {s}\n", .{meth});
    } else if (std.ascii.eqlIgnoreCase(meth, "get")) {
        dprint("The method is {s}\n", .{meth});
    }
    dprint("Which anniversay name? {s}\n", .{anniversaryName(rand.intRangeLessThan(u16, 1, 7))});
    dprint("What arrival status? {s}\n", .{arrivalDescTime(rand.intRangeLessThan(u16, 0, 10), rand.boolean())});

    for (1..10) |
        i,
    | {
        dprint("{} ", .{i});
    }
    dprint("\n", .{});

    const _nan = Number{ .nan = {} };
    _ = _nan;

    try optionalNull();
    try memExample();
    try memExampleSingleOnly();
    try fixedBufferAlloc();
    try bufPrintBetter();
    try streamToStdOutBetter();
    try useListGeneric();
    try danglingPointers();
    try askUser();
    try writeToArray();
}

fn entryInfo(msg: []const u8) void {
    dprint("===Entry to {s}===\n", .{msg});
}

fn exitInfo(msg: []const u8) void {
    dprint("===Exit from {s}===\n", .{msg});
}

pub fn add(a: i64, b: i64) i64 {
    return a + b;
}

fn anniversaryName(years_married: u16) []const u8 {
    return switch (years_married) {
        1 => "paper",
        2 => "cotton",
        3 => "leather",
        4 => "flower",
        5 => "wood",
        6 => "sugar",
        else => "No name",
    };
}

fn arrivalDescTime(minutes: u16, isLate: bool) []const u8 {
    return switch (minutes) {
        0 => "arrived",
        1, 2 => "soon",
        3...5 => "no more than five minutes",
        else => {
            return if (!isLate) "it's will be a while" else "never";
        },
    };
}

const State = enum {
    validate,
    awaitingConfirmation,
    confirmed,
    err,

    fn isComplete(self: State) bool {
        return self == .confirmed or self == .err;
    }
};

const Number = union {
    int: i64,
    float: f64,
    nan: void,
};

const TimestampType = enum {
    unix,
    datetime,
};

// can be:
// const Timestamp = union(enum) {
// will be inferred
const Timestamp = union(TimestampType) {
    unix: i32,
    datetime: DateTime,

    const DateTime = struct {
        year: u16,
        month: u8,
        day: u8,
        hour: u8,
        minute: u8,
        second: u8,
    };

    fn seconds(self: Timestamp) u16 {
        switch (self) {
            .datetime => |dt| return dt.second,
            .unix => |ts| {
                const secondsSinceMidnight = @rem(ts, 86400);
                return @intCast(@rem(secondsSinceMidnight, 60));
            },
        }
    }
};

// see its usage in main decl
const OpenError = error{
    AccessDenied,
    NotFound,
};

pub const Save = struct {
    lives: u8,
    level: u16,

    pub fn loadLast() !?Save {
        // TODO
        return null;
    }

    pub fn blank() Save {
        return .{
            .lives = 3,
            .level = 1,
        };
    }
};

fn optionalNull() !void {
    const label = "optionalNull";
    entryInfo(label);
    defer exitInfo(label);
    const _home: ?[]const u8 = null;
    const _name: ?[]const u8 = "Leto";

    dprint("{s}\n", .{_name.?});

    if (_home) |h| {
        dprint("We got home: {s}\n", .{h});
    } else {
        dprint("No, null home\n", .{});
    }

    dprint("default home {s}\n", .{_home orelse "unknown"});
    const holmes = [_]?[]const u8{ null, "home 1", "home 2", null, "home run", "homerr", null, "last after null" };
    for (holmes) |home| {
        while (home) |h| {
            dprint("our home now {s}\n", .{h});
            break;
        }
    }

    const save = (try Save.loadLast()) orelse Save.blank();
    dprint("{any}\n", .{save});

    const leto = user.User{
        .name = "leto",
        .power = 9001,
    };
    const duncan = user.User{
        .name = "duncan",
        .power = 9000,
        .manager = &leto,
    };
    dprint("{any}\n{any}\n", .{ leto, duncan });
}

fn memExample() !void {
    const label = "memExample";
    entryInfo(label);
    defer exitInfo(label);
    var gpa = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true }){};
    const allocator = gpa.allocator();
    var arr = try allocator.alloc(usize, try getRandomCount());
    defer allocator.free(arr);
    for (0..arr.len) |i| {
        arr[i] = i;
    }
    std.debug.print("{any}\n", .{arr});
}

fn getRandomCount() !u64 {
    var seed: u64 = undefined;
    try std.posix.getrandom(std.mem.asBytes(&seed));
    var random = std.Random.DefaultPrng.init(seed);
    return random.random().uintAtMost(u8, 5) + 5;
}

fn memExampleSingleOnly() !void {
    const label = "memExampleSingleOnly";
    entryInfo(label);
    defer exitInfo(label);
    var gpa = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true }){};
    const allocator = gpa.allocator();
    const arr = try allocator.create(usize);
    defer allocator.destroy(arr);
    arr.* = 10;
    std.debug.print("{any}\n", .{arr});
}

fn fixedBufferAlloc() !void {
    const label = "fixedBufferAlloc";
    entryInfo(label);
    defer exitInfo(label);
    var buf: [150]u8 = undefined;
    var fa = std.heap.FixedBufferAllocator.init(&buf);
    defer fa.reset();
    const allocator = fa.allocator();
    const json = try std.json.stringifyAlloc(allocator, .{
        .this_is = "an anonymous struct",
        .above = true,
        .last_param = "is optional",
    }, .{ .whitespace = .indent_2 });
    defer allocator.free(json);
    std.debug.print("json: {s}\n", .{json});
}

fn bufPrintBetter() !void {
    const label = "bufPrintBetter";
    entryInfo(label);
    defer exitInfo(label);
    const name = "Leto";
    var buf: [100]u8 = undefined;
    const greeting = try std.fmt.bufPrint(&buf, "Hello {s}", .{name});
    std.debug.print("{s}\n", .{greeting});
}

fn streamToStdOutBetter() !void {
    const label = "streamToStdOutBetter";
    entryInfo(label);
    defer exitInfo(label);
    const out = std.io.getStdOut();
    try std.json.stringify(.{
        .this_is = "an anonymous struct",
        .above = true,
        .last_param = "is optional",
    }, .{ .whitespace = .indent_2 }, out.writer());
    dprint("\n", .{});
}

fn List(comptime T: type) type {
    return struct {
        pos: usize,
        items: []T,
        allocator: std.mem.Allocator,

        const Self = @This();

        fn init(allocator: std.mem.Allocator) !List(T) {
            return .{
                .pos = 0,
                .allocator = allocator,
                .items = try allocator.alloc(T, 4),
            };
        }

        fn deinit(self: Self) void {
            self.allocator.free(self.items);
        }

        fn add(self: *Self, value: T) !void {
            const pos = self.pos;
            const len = self.items.len;
            if (pos == len) {
                var larger = try self.allocator.alloc(T, len * 2);
                @memcpy(larger[0..len], self.items);
                self.allocator.free(self.items);
                self.items = larger;
            }
            self.items[pos] = value;
            self.pos += 1;
        }
    };
}

fn useListGeneric() !void {
    const label = "useListGeneric";
    entryInfo(label);
    defer exitInfo(label);
    var gpa = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true }){};
    const allocator = gpa.allocator();
    var list = try List(u32).init(allocator);
    defer list.deinit();
    for (0..10) |i| {
        try list.add(@intCast(i));
    }
    dprint("{any}\n", .{list.items[0..list.pos]});
}

fn danglingPointers() !void {
    const label = "danglingPointers";
    entryInfo(label);
    defer exitInfo(label);
    var gpa = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true }){};
    const allocator = gpa.allocator();
    var lookup = std.StringHashMap(user.User).init(allocator);
    defer lookup.deinit();

    const goku = user.User{
        .name = "Goku",
        .power = 9001,
    };
    try lookup.put("Goku", goku);
    try lookup.put("Goku2", goku);
    const entry = lookup.getPtr("Goku").?;
    dprint("Goku's power is {d}\n", .{entry.power});
    _ = lookup.remove("Goku");
    dprint("Goku's power is {d}\n", .{entry.power});
    const entry2 = lookup.get("Goku2").?;
    dprint("Goku2's power is {d}\n", .{entry2.power});
    _ = lookup.remove("Goku2");
    dprint("Goku2's power is {d}\n", .{entry2.power});
}

fn askUser() !void {
    const label = "askUser";
    entryInfo(label);
    defer exitInfo(label);
    var gpa = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true }){};
    const allocator = gpa.allocator();
    var lookup = std.StringHashMap(user.User).init(allocator);
    // defer lookup.deinit();
    defer {
        var it = lookup.keyIterator();
        while (it.next()) |key| {
            allocator.free(key.*);
        }
        lookup.deinit();
    }

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var i: i32 = 0;
    while (true) : (i += 1) {
        var buf: [30]u8 = undefined;
        try stdout.print(("Please enter a name: "), .{});
        if (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            var name = line;
            if (builtin.os.tag == .windows) {
                name = @constCast(std.mem.trimRight(u8, name, "\n"));
            }
            if (name.len == 0) {
                break;
            }
            const owned_name = try allocator.dupe(u8, name);
            try lookup.put(owned_name, .{ .power = @intCast(i), .name = name });
        }
    }
    const has_leto = lookup.contains("Leto");
    dprint("Has leto? {any}\n", .{has_leto});
}

fn writeToArray() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alctr = gpa.allocator();
    var out = std.ArrayList(u8).init(alctr);
    defer out.deinit();

    try std.json.stringify(.{
        .this_is = "an anonymous struct",
        .above = true,
        .last_param = "are options",
    }, .{ .whitespace = .indent_2 }, out.writer());
    dprint("{s}\n", .{out.items});
}

fn userFactory(data: anytype) user.User {
    const T = @TypeOf(data);
    return .{
        .power = if (@hasField(T, "power")) data.power else 0,
        .name = if (@hasField(T, "name")) data.name else "",
        .manager = if (@hasField(T, "manager")) data.manager else null,
    };
}
