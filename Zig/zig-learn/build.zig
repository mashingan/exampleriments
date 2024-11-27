const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();
    const exe = b.addExecutable("init-exe", "ch3-build/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const runcmd = exe.run();
    runcmd.step.dependOn(b.getInstallStep());
    const runstep = b.step("run", "Run the app");
    runstep.dependOn(&runcmd.step);
}
