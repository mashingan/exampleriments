const std = @import("std");
const stdout = std.io.getStdOut().writer();
const hash = std.crypto.hash;

pub fn main() !void {
    const args = try std.process.argsAlloc(std.testing.allocator);
    defer std.process.argsFree(std.testing.allocator, args);
    if (args.len < 2) {
        try stdout.writeAll("expected input argument\n");
        return;
    }
    const input = args[1];
    var output: [hash.Blak3.digest_length]u8 = undefined;
    hash.Black3.hash(input, &output, .{});
    try stdout.print("{s}\n", .{std.fmt.fmtSliceHexLower(&output)});
}
