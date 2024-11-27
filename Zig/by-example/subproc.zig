const std = @import("std");

pub fn main() anyerror!void {
    const args = [_][]const u8{"notepad.exe"};
    var process = std.ChildProcess.init(&args, std.testing.allocator);
    std.debug.print("Running command: {s}\n", .{args});
    try process.spawn();
    const retVal = try process.wait();
    try std.testing.expectEqual(retVal, .{ .Exited = 0 });
}
