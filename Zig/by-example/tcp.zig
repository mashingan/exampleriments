const std = @import("std");
const net = std.net;
const testing = std.testing;

const clientmsg = "Hello";
const servermsg = "Good bye";

const Server = struct {
    streamServer: net.StreamServer,

    pub fn init() !Server {
        const addr = net.Address.initIp4([4]u8{ 127, 0, 0, 1 }, 8080);
        var server = net.StreamServer.init(.{ .reuse_address = true });
        try server.listen(addr);
        return Server{ .streamServer = server };
    }

    pub fn deinit(self: *Server) void {
        self.streamServer.deinit();
    }

    pub fn accept(self: *Server) !void {
        const conn = try self.streamServer.accept();
        defer conn.stream.close();

        var buf: [1024]u8 = undefined;
        const msgSize = try conn.stream.read(buf[0..]);
        try testing.expectEqualSlices(u8, clientmsg, buf[0..msgSize]);
        _ = try conn.stream.write(servermsg);
    }
};

fn sendMsgToServer(addr: net.Address) !void {
    const conn = try net.tcpConnectToAddress(addr);
    defer conn.close();

    _ = try conn.write(clientmsg);
    var buf: [1024]u8 = undefined;
    const respSize = try conn.read(buf[0..]);
    try testing.expectEqualSlices(u8, servermsg, buf[0..respSize]);
}

pub fn main() !void {
    var server = try Server.init();
    defer server.deinit();
    const clientThread = try std.Thread.spawn(.{}, sendMsgToServer, .{server.streamServer.listen_address});
    defer clientThread.join();
    try server.accept();
}
