const std = @import("std");
const expect = std.testing.expect;

test "allocation" {
    const allocator = std.heap.page_allocator;
    const memory = try allocator.alloc(u8, 100);
    defer allocator.free(memory);
    try expect(memory.len == 100);
    try expect(@TypeOf(memory) == []u8);
}

test "fixed buffer allocator" {
    var buffer: [1000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    const memory = try allocator.alloc(u8, 100);
    defer allocator.free(memory);

    try expect(memory.len == 100);
    try expect(@TypeOf(memory) == []u8);
}

test "arena allocator" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    _ = try allocator.alloc(u8, 1);
    _ = try allocator.alloc(u8, 10);
    _ = try allocator.alloc(u8, 100);
}

test "allocator create/destroy" {
    const byte = try std.heap.page_allocator.create(u8);
    defer std.heap.page_allocator.destroy(byte);
    byte.* = 128;
}

test "general purpose allocator" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const leaked = gpa.deinit();
        if (leaked) expect(false) catch @panic("TEST FAIL");
    }

    const bytes = try allocator.alloc(u8, 100);
    defer allocator.free(bytes);
}

const eql = std.mem.eql;
const ArrayList = std.ArrayList;
const test_allocator = std.testing.allocator;

test "arraylist" {
    var list = ArrayList(u8).init(test_allocator);
    defer list.deinit();
    try list.append('h');
    try list.append('e');
    try list.append('l');
    try list.append('l');
    try list.append('o');
    try list.appendSlice(" world!");
    try expect(eql(u8, list.items, "hello world!"));
}

test "create file, write, seek and read" {
    const file = try std.fs.cwd().createFile(
        "junk_file.txt",
        .{ .read = true },
    );
    defer file.close();

    const bytes_written = try file.writeAll("hello file!");
    _ = bytes_written;

    var buffer: [100]u8 = undefined;
    try file.seekTo(0);
    const bytes_read = try file.readAll(&buffer);
    try expect(eql(u8, buffer[0..bytes_read], "hello file!"));
}

test "file stat" {
    const file = try std.fs.cwd().createFile(
        "junk_file2.txt",
        .{ .read = true },
    );
    defer file.close();
    const stat = try file.stat();
    try expect(stat.size == 0);
    try expect(stat.kind == .File);
    try expect(stat.ctime <= std.time.nanoTimestamp());
    try expect(stat.mtime <= std.time.nanoTimestamp());
    try expect(stat.atime <= std.time.nanoTimestamp());
}

test "make dir" {
    const dirtemp = "test-tmp";
    try std.fs.cwd().makeDir(dirtemp);
    var dir = try std.fs.cwd().openDir(
        dirtemp,
        .{},
    );
    defer {
        std.fs.cwd().deleteTree(dirtemp) catch unreachable;
    }

    for ([_]*const [1:0]u8{ "x", "y", "z" }) |fname| {
        var f = try dir.createFile(fname, .{});
        f.close();
    }
    var file_count: usize = 0;
    var iterdir = try dir.makeOpenPathIterable("", .{});
    var iter = iterdir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind == .File) file_count += 1;
    }
    iterdir.close();
    dir.close();
    try expect(file_count == 3);
}

test "io writer usage" {
    var list = ArrayList(u8).init(test_allocator);
    defer list.deinit();
    const hello = "Hello world!";
    const bytes_written = try list.writer().write(hello);
    try expect(bytes_written == hello.len);
    try expect(eql(u8, list.items, hello));
}

test "io reader usage" {
    const message = "hello file!";
    const file = try std.fs.cwd().createFile(
        "junk_file3.txt",
        .{ .read = true },
    );
    defer file.close();

    try file.writeAll(message);
    try file.seekTo(0);
    const content = try file.reader().readAllAlloc(test_allocator, message.len);
    defer test_allocator.free(content);
    try expect(eql(u8, content, message));
}

fn nextline(reader: anytype, buffer: []u8) !?[]const u8 {
    var line = (try reader.readUntilDelimiterOrEof(
        buffer,
        '\n',
    )) orelse return null;
    if (@import("builtin").os.tag == .windows) {
        return std.mem.trimRight(u8, line, "\r");
    } else {
        return line;
    }
}

// disabled to automate test
test "read until next line" {
    //const stdout = std.io.getStdOut();
    //const stdin = std.io.getStdIn();
    //try stdout.writeAll(
    //"Enter your name: ",
    //);
    //var buffer: [100]u8 = undefined;
    //const input = (try nextline(stdin.reader(), &buffer)).?;
    //try stdout.writer().print(
    //"Your name is: \"{s}\"\n",
    //.{input},
    //);
}

const MyBytelist = struct {
    data: [100]u8 = undefined,
    items: []u8 = &[_]u8{},

    const Writer = std.io.Writer(
        *MyBytelist,
        error{EndOfBuffer},
        appendWrite,
    );

    fn appendWrite(self: *MyBytelist, data: []const u8) error{EndOfBuffer}!usize {
        if (self.items.len + data.len > self.data.len) {
            return error.EndOfBuffer;
        }

        std.mem.copy(u8, self.data[self.items.len..], data);
        self.items = self.data[0 .. self.items.len + data.len];
        return data.len;
    }

    fn writer(self: *MyBytelist) Writer {
        return .{ .context = self };
    }
};

test "custom writer" {
    var bytes = MyBytelist{};
    _ = try bytes.writer().write("hello");
    _ = try bytes.writer().write(" writer!");
    try expect(eql(u8, bytes.items, "hello writer!"));
}

test "fmt" {
    const string = try std.fmt.allocPrint(
        test_allocator,
        "{d} + {d} = {d}",
        .{ 9, 10, 19 },
    );
    defer test_allocator.free(string);
    try expect(eql(u8, string, "9 + 10 = 19"));
}

test "print" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();
    try list.writer().print("{} + {} = {}", .{ 9, 10, 19 });
    try expect(eql(u8, list.items, "9 + 10 = 19"));
}

test "hello world" {
    const outfile = std.io.getStdOut();
    try outfile.writer().print("Hello, {s}\n", .{"World!"});
}

test "array printing" {
    const string = try std.fmt.allocPrint(
        test_allocator,
        "{any} + {any} = {any}",
        .{
            @as([]const u8, &[_]u8{ 1, 4 }),
            @as([]const u8, &[_]u8{ 2, 5 }),
            @as([]const u8, &[_]u8{ 3, 9 }),
        },
    );
    defer test_allocator.free(string);
    try expect(eql(u8, string, "{ 1, 4 } + { 2, 5 } = { 3, 9 }"));
}

const Person = struct {
    name: []const u8,
    birth_year: i32,
    death_year: ?i32,
    pub fn format(
        self: Person,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} ({}-", .{ self.name, self.birth_year });
        if (self.death_year) |year| {
            try writer.print("{}", .{year});
        }
        try writer.writeAll(")");
    }
};

test "custom fmt" {
    const john = Person{
        .name = "John Carmack",
        .birth_year = 1970,
        .death_year = null,
    };
    const john_string = try std.fmt.allocPrint(
        test_allocator,
        "{s}",
        .{john},
    );
    defer test_allocator.free(john_string);
    try expect(eql(u8, john_string, "John Carmack (1970-)"));

    const claude = Person{
        .name = "Claude Shannon",
        .birth_year = 1916,
        .death_year = 2001,
    };
    const claude_string = try std.fmt.allocPrint(
        test_allocator,
        "{s}",
        .{claude},
    );
    defer test_allocator.free(claude_string);
    try expect(eql(u8, claude_string, "Claude Shannon (1916-2001)"));
}

const Place = struct { lat: f32, long: f32 };

test "json parse" {
    var stream = std.json.TokenStream.init(
        \\{ "lat": 40.684540, "long": -74.401422 }
    );
    const x = try std.json.parse(Place, &stream, .{});
    try expect(x.lat == 40.684540);
    try expect(x.long == -74.401422);
}

test "json stringify" {
    const x = Place{
        .lat = 51.997664,
        .long = -0.740687,
    };
    var buf: [100]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buf);
    var string = std.ArrayList(u8).init(fba.allocator());
    try std.json.stringify(x, .{}, string.writer());
    //try expect(eql(u8, string.items,
    //\\{"lat":51.997664,"long":-0.740687}
    //));
}

test "json parse with strings" {
    var stream = std.json.TokenStream.init(
        \\{ "name": "Joe", "age": 25 }
    );
    const User = struct { name: []u8, age: u16 };
    const x = try std.json.parse(User, &stream, .{ .allocator = test_allocator });
    defer std.json.parseFree(User, x, .{ .allocator = test_allocator });
    try expect(eql(u8, x.name, "Joe"));
    try expect(x.age == 25);
}

test "random numbers" {
    var prng = std.rand.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        try std.os.getrandom(std.mem.asBytes(&seed));
        break :blk seed;
    });
    const rand = prng.random();
    const a = rand.float(f32);
    const b = rand.boolean();
    const c = rand.int(u8);
    const d = rand.intRangeAtMost(u8, 0, 255);
    if (false) _ = .{ a, b, c, d };
}

test "crypto random numbers" {
    const rand = std.crypto.random;
    const a = rand.float(f32);
    const b = rand.boolean();
    const c = rand.int(u8);
    const d = rand.intRangeAtMost(u8, 0, 255);
    if (false) _ = .{ a, b, c, d };
}

fn ticker(step: u8) void {
    while (true) {
        std.time.sleep(1 * std.time.ns_per_s);
        tick += @as(isize, step);
    }
}

var tick: isize = 0;

test "threading" {
    var thread = try std.Thread.spawn(.{}, ticker, .{@as(u8, 1)});
    _ = thread;
    try expect(tick == 0);
    std.time.sleep(3 * std.time.ns_per_s / 2);
    try expect(tick == 1);
}

test "hashing" {
    const Point = struct { x: i32, y: i32 };
    var map = std.AutoHashMap(u32, Point).init(test_allocator);
    defer map.deinit();
    try map.put(1525, .{ .x = 1, .y = -4 });
    try map.put(1550, .{ .x = 2, .y = -3 });
    try map.put(1575, .{ .x = 3, .y = -2 });
    try map.put(1600, .{ .x = 4, .y = -1 });
    try expect(map.count() == 4);

    var sum = Point{ .x = 0, .y = 0 };
    var iterator = map.iterator();
    while (iterator.next()) |entry| {
        sum.x += entry.value_ptr.x;
        sum.y += entry.value_ptr.y;
    }
    try expect(sum.x == 10);
    try expect(sum.y == -10);
}

test "fetchPut" {
    var map = std.AutoHashMap(u8, f32).init(test_allocator);
    defer map.deinit();
    try map.put(255, 10);
    const old = try map.fetchPut(255, 100);
    try expect(old.?.value == 10);
    try expect(map.get(255).? == 100);
}

test "string hashmap" {
    var map = std.StringHashMap(enum { cool, uncool }).init(test_allocator);
    defer map.deinit();
    try map.put("loris", .uncool);
    try map.put("me", .cool);
    try expect(map.get("me").? == .cool);
    try expect(map.get("loris").? == .uncool);
}

test "stack" {
    const string = "(()())";
    var stack = std.ArrayList(usize).init(test_allocator);
    defer stack.deinit();
    const Pair = struct { open: usize, close: usize };
    var pairs = std.ArrayList(Pair).init(test_allocator);
    defer pairs.deinit();

    for (string) |char, i| {
        if (char == '(') try stack.append(i);
        if (char == ')')
            try pairs.append(.{
                .open = stack.pop(),
                .close = i,
            });
    }
    for (pairs.items) |pair, i| {
        try expect(std.meta.eql(pair, switch (i) {
            0 => Pair{ .open = 1, .close = 2 },
            1 => Pair{ .open = 3, .close = 4 },
            2 => Pair{ .open = 0, .close = 5 },
            else => unreachable,
        }));
    }
}

test "sorting" {
    var data = [_]u8{ 10, 240, 0, 0, 10, 5 };
    std.sort.sort(u8, &data, {}, comptime std.sort.asc(u8));
    try expect(eql(u8, &data, &[_]u8{ 0, 0, 5, 10, 10, 240 }));
    std.sort.sort(u8, &data, {}, comptime std.sort.desc(u8));
    try expect(eql(u8, &data, &[_]u8{ 240, 10, 10, 5, 0, 0 }));
}

test "split iterator" {
    const text = "robust, optimal, reusable, maintainable, ";
    var iter = std.mem.split(u8, text, ", ");
    try expect(eql(u8, iter.next().?, "robust"));
    try expect(eql(u8, iter.next().?, "optimal"));
    try expect(eql(u8, iter.next().?, "reusable"));
    try expect(eql(u8, iter.next().?, "maintainable"));
    try expect(eql(u8, iter.next().?, ""));
    try expect(iter.next() == null);
}

const ContainIterator = struct {
    strings: []const []const u8,
    needle: []const u8,
    index: usize = 0,
    fn next(self: *ContainIterator) ?[]const u8 {
        const index = self.index;
        for (self.strings[index..]) |string| {
            self.index += 1;
            if (std.mem.indexOf(u8, string, self.needle)) |_| {
                return string;
            }
        }
        return null;
    }
};

test "custom iterator" {
    var iter = ContainIterator{
        .strings = &[_][]const u8{ "one", "two", "three" },
        .needle = "e",
    };
    try expect(eql(u8, iter.next().?, "one"));
    try expect(eql(u8, iter.next().?, "three"));
    try expect(iter.next() == null);
}

const bufprint = std.fmt.bufPrint;

test "hex" {
    var b: [8]u8 = undefined;
    _ = try bufprint(&b, "{X}", .{4294967294});
    try expect(eql(u8, &b, "FFFFFFFE"));
    _ = try bufprint(&b, "{x}", .{4294967294});
    try expect(eql(u8, &b, "fffffffe"));
    _ = try bufprint(&b, "{}", .{std.fmt.fmtSliceHexLower("Zig!")});
    try expect(eql(u8, &b, "5a696721"));
}

test "decimal float" {
    var b: [4]u8 = undefined;
    try expect(eql(u8, try bufprint(&b, "{d}", .{16.5}), "16.5"));
}

test "ascii fmt" {
    var b: [1]u8 = undefined;
    try expect(eql(u8, try bufprint(&b, "{c}", .{66}), "B"));
}

test "B Bi" {
    var b: [32]u8 = undefined;

    try expect(eql(u8, try bufprint(&b, "{}", .{std.fmt.fmtIntSizeDec(1)}), "1B"));
    try expect(eql(u8, try bufprint(&b, "{}", .{std.fmt.fmtIntSizeBin(1)}), "1B"));

    try expect(eql(u8, try bufprint(&b, "{}", .{std.fmt.fmtIntSizeDec(1024)}), "1.024kB"));
    try expect(eql(u8, try bufprint(&b, "{}", .{std.fmt.fmtIntSizeBin(1024)}), "1KiB"));

    const igb = 1024 * 1024 * 1024;
    try expect(eql(u8, try bufprint(&b, "{}", .{std.fmt.fmtIntSizeDec(igb)}), "1.073741824GB"));
    try expect(eql(u8, try bufprint(&b, "{}", .{std.fmt.fmtIntSizeBin(igb)}), "1GiB"));
}

test "bianry octal fmt" {
    var b: [8]u8 = undefined;
    try expect(eql(u8, try bufprint(&b, "{b}", .{254}), "11111110"));
    try expect(eql(u8, try bufprint(&b, "{o}", .{254}), "376"));
}

test "pointer fmt" {
    var b: [16]u8 = undefined;
    try expect(eql(u8, try bufprint(&b, "{*}", .{@intToPtr(*u8, 0xBEEFF00D)}), "u8@beeff00d"));
}

test "scientific fmt" {
    var b: [16]u8 = undefined;
    try expect(eql(u8, try bufprint(&b, "{e}", .{3.14159}), "3.14159e+00"));
}

test "string fmt" {
    var b: [6]u8 = undefined;
    const hello: [*:0]const u8 = "hello!";
    try expect(eql(u8, try bufprint(&b, "{s}", .{hello}), "hello!"));
}

test "position" {
    var b: [3]u8 = undefined;
    try expect(eql(u8, try bufprint(&b, "{0s}{0s}{1s}", .{ "a", "b" }), "aab"));
}

test "fill, alignment, width" {
    var b: [6]u8 = undefined;
    try expect(eql(u8, try bufprint(&b, "{s: <5}", .{"hi!"}), "hi!  "));
    try expect(eql(u8, try bufprint(&b, "{s:_^6}", .{"hi!"}), "_hi!__"));
    try expect(eql(u8, try bufprint(&b, "{s:!>4}", .{"hi!"}), "!hi!"));
}

test "precision" {
    var b: [4]u8 = undefined;
    try expect(eql(u8, try bufprint(&b, "{d:.2}", .{3.14159}), "3.14"));
}
