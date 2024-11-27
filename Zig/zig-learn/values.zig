const print = std.debug.print;
const std = @import("std");
const os = std.os;
const assert = std.debug.assert;

pub fn main() void {
    const onePlusOne: i32 = 1 + 1;
    print("1 + 1 = {d}\n", .{onePlusOne});

    const sevenDivThree: f32 = 7.0 / 3.0;
    print("7.0 / 3.0 = {}\n", .{sevenDivThree});

    print("{}\n{}\n{}\n", .{
        true and false,
        true or false,
        !true,
    });

    var optval: ?[]const u8 = null;
    assert(optval == null);

    print("\noptional 1\ntype: {?s}\nvalue: {?s}\n", .{
        @typeName(@TypeOf(optval)),
        optval,
    });

    optval = "hi";
    assert(optval != null);
    print("\noptional 2\ntype: {?s}\nvalue: {?s}\n", .{
        @typeName(@TypeOf(optval)),
        optval,
    });

    var numErr: anyerror!i32 = error.ArgnotFound;
    print("\nerror union 1\ntype: {!s}\nvalue: {!d}\n", .{
        @typeName(@TypeOf(numErr)),
        numErr,
    });
    numErr = 1234;
    print("\nerror union 2\ntype: {!s}\nvalue: {!d}\n", .{
        @typeName(@TypeOf(numErr)),
        numErr,
    });
}
