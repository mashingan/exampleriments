const std = @import("std");
//const cwd = std.fs.cwd;
//const print = std.io.getStdOut().writer.print;
//
//pub fn main(args: [][]u8) !void {
//const dir = try cwd().openDir(if (args.len < 2) "." else args[1], .{ .iterate = true });
//var dirIterator = dir.iterate();
//
//while(try dirIterator.next()) |path| {
//try print("{s}\n", .{path.name});
//}
//}

// failed to run, shrug
const stdout = std.io.getStdOut().writer();

pub fn main() !void {
    const args = try std.process.argsAlloc(std.testing.allocator);
    defer std.process.argsFree(std.testing.allocator, args);
    const dir = try std.fs.cwd().openDir(if (args.len < 2) "." else args[1], .{ .iterate = true });
    var dirIterator = dir.iterate();
    while (try dirIterator.next()) |path| {
        try stdout.print("{s}\n", .{path.name});
    }
}
