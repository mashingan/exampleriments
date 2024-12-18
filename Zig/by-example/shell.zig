const std = @import("std");
const mem = std.mem;
const stdin = std.io.getStdIn();
const stdout = std.io.getStdOut().writer();
const exit = std.process.exit;

const Error = error{
    CommandFailed,
    UnknownCommand,
    EmptyCommand,
};

fn getArgs(allocator: mem.Allocator, cmd: []const u8) !?[]const []const u8 {
    if (cmd.len == 0) {
        return null;
    }

    var args = std.ArrayList([]const u8).init(allocator);
    defer args.deinit();
    var splits = mem.split(u8, cmd, " ");
    while (splits.next()) |string| {
        try args.append(string);
    }
    return args.toOwnedSlice();
}

fn evaluateCmd(args: []const []const u8) !void {
    if (args.len == 0) {
        return Error.EmptyCommand;
    } else if (mem.eql(u8, args[0], "clear")) {
        try stdout.writeAll("\x1b[1;1H\x1b[2J");
    } else if (mem.eql(u8, args[0], "exit")) {
        exit(0);
    } else {
        const res = std.ChildProcess.exec(.{ .allocator = std.heap.page_allocator, .argv = args }) catch |err| {
            switch (err) {
                error.FileNotFound => return Error.UnknownCommand,
                else => {
                    try stdout.writeAll(@errorName(err));
                    return Error.CommandFailed;
                },
            }
        };

        try stdout.writeAll(res.stdout);
    }
}

pub fn main() !void {
    var cmdbuf: [1024]u8 = undefined;
    while (true) {
        try stdout.writeAll("\x1b[92;1m$ \x1b[0m");
        if (stdin.reader().readUntilDemiliterOrEof(&cmdbuf, "\n") catch |err| {
            try stdout.print("unable to parse command: {s}\n", .{@errorName(err)});
            continue;
        }) |line| {
            const actline = mem.trim(u8, line, "\r\n ");
            if (try getArgs(std.testing.allocator, actline)) |args| {
                evaluateCmd(args) catch |err| {
                    if (err == Error.UnknownCommand) {
                        stdout.print("Unknown command `{s}`\n", .{actline}) catch {};
                    } else {
                        return err;
                    }
                };
            }
        }
    }
}
