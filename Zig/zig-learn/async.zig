const expect = @import("std").testing.expect;

var foo: i32 = 1;

test "suspend with no resume" {
    var frame = async func();
    _ = frame;
    try expect(foo == 2);
}

fn func() void {
    foo += 1;
    suspend {}
    foo += 1;
}

var bar: i32 = 1;

test "suspend with resume" {
    var frame = async func2();
    resume frame;
    try expect(bar == 3);
}

fn func2() void {
    bar += 1;
    suspend {}
    bar += 1;
}

fn func3() u32 {
    return 5;
}

test "async / await" {
    var frame = async func3();
    try expect(await frame == 5);
}

const std = @import("std");

fn doTicksDuration(ticker: *u32) i64 {
    const start = std.time.milliTimestamp();

    while (ticker.* > 0) {
        suspend {}
        ticker.* -= 1;
    }

    return std.time.milliTimestamp() - start;
}

//pub fn main() !void {
//var ticker: u32 = 0;
//const duration = nosuspend doTicksDuration(&ticker);
//std.log.info("Duration: ", .{duration});
//}

fn add(a: i32, b: i32) i64 {
    return a + b;
}

test "@frame" {
    var frame: @Frame(add) = async add(1, 2);
    try expect(await frame == 3);
}

fn double(value: u8) u9 {
    suspend {
        resume @frame();
    }
    return value * 2;
}

test "@frame 1" {
    var f = async double(1);
    try expect(nosuspend await f == 2);
}

fn callLater(comptime laterFn: fn () void, ms: u64) void {
    suspend {
        wakeupLater(@frame(), ms);
    }
    laterFn();
}

fn wakeupLater(frame: anyframe, ms: u64) void {
    std.time.sleep(ms * std.time.ns_per_ms);
    resume frame;
}

fn alarm() void {
    std.debug.print("Time's up!\n", .{});
}

test "@frame 2" {
    nosuspend callLater(alarm, 1000);
}

fn zero(comptime x: anytype) x {
    return 0;
}

fn awaiter(x: anyframe->f32) f32 {
    return nosuspend await x;
}

test "anyframe->T" {
    var frame = async zero(f32);
    try expect(awaiter(&frame) == 0);
}

var timer: ?std.time.Timer = null;

fn nanotime() u64 {
    if (timer == null) {
        timer = std.time.Timer.start() catch unreachable;
    }
    return timer.?.read();
}

const Delay = struct {
    frame: anyframe,
    expire: u64,
};

fn waitForTime(time_ms: u64) void {
    suspend timer_queue.add(Delay{
        .frame = @frame(),
        .expires = nanotime() + (time_ms * std.time.ns_per_ms),
    }) catch unreachable;
}

fn waitUntilAndPrint(time1: u64, time2: u64, name: []const u8) void {
    const start = nanotime();
    waitForTime(time1);
    std.debug.print(
        "[{s}] it is now {} ms since start!\n",
        .{ name, (nanotime() - start) / std.time.ns_per_ms },
    );
    waitForTime(time2);
    std.debug.print(
        "[{s}] it is now {} ms since start!\n",
        .{ name, (nanotime() - start) / std.time.ns_per_ms },
    );
}

fn asyncMain() void {
    var tasks = [_]@Frame(waitUntilAndPrint){
        async waitUntilAndPrint(1000, 1200, "task-pair a"),
        async waitUntilAndPrint(500, 1300, "task-pair b"),
    };
    for (tasks) |*t| await t;
}

var timer_queue: std.PriorityQueue(Delay, void, cmp) = undefined;
fn cmp(context: void, a: Delay, b: Delay) std.math.Order {
    _ = context;
    return std.math.order(a.expire, b.expire);
}

pub fn main() !void {
    timer_queue = std.PriorityQueue(Delay, void, cmp).init(std.heap.page_allocator, undefined);
    defer timer_queue.deinit();

    var main_task = async asyncMain();
    while (timer_queue.removeOrNull()) |delay| {
        const now = nanotime();
        if (now < delay.expires) {
            std.time.sleep(delay.expires - now);
        }
        resume delay.frame;
    }
    nosuspend await main_task;
}
