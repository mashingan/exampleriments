const print = @import("std").debug.print;

pub fn main() void {
    // this is comment
    // no multiline comments
    
    print("Hello {s}", .{"comments"}); // side comment
}
