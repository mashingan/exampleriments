const std = @import("std");
const stdout = std.io.getStdOut().writer();
const print = std.io.getStdOut().writer.print;

fn generateWords(allocator: std.mem.Allocator, wlen: usize, letterlen: usize) !void {
    const randeng = std.rand.DefaultPrng.init(@intCast(u64, std.time.milliTimestamp())).random();
    var wl: usize = 0;
    var word = try allocator.alloc(u8, letterlen);
    defer allocator.free(word);
    while (wl < wlen) : (wl += 1) {
        var prevl: u8 = 0;
        var li: usize = 0;
        while (li < letterlen) {
            const letter = randeng.intRangeLessThanBiased(u7, 'a', 'z');
            if (prevl == letter) continue;
            word[li] = letter;
            prevl = letter;
            li += 1;
        }
    }
    //try stdout.print("{s}\n", .{word});
    try print("{s}\n", .{word});
}

pub fn main() anyerror!void {
    const allocator = std.testing.allocator;
    var letterlen: usize = 5;
    var wlen: usize = 5;
    const args = try std.process.argsAlloc(std.testing.allocator);
    defer std.process.argsFree(std.testing.allocator, args);
    for (args[1..]) |arg, i| {
        const count = try std.fmt.parseUnsigned(usize, arg, 0);
        if (i == 1) {
            letterlen = count;
        } else if (i == 2) {
            wlen = count;
        }
    }
    try generateWords(allocator, wlen, letterlen);
}
