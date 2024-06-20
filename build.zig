const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "sock",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    var extra_volk_flags = std.ArrayList([]const u8).init(b.allocator);
    defer extra_volk_flags.deinit();

    if (target.result.os.tag == .windows) {
        exe.linkSystemLibrary("Kernel32");
        exe.linkSystemLibrary("User32");
        exe.addWin32ResourceFile(.{
            .file = b.path("resources.rc"),
        });
        extra_volk_flags.append("-DVK_USE_PLATFORM_WIN32_KHR") catch @panic("OOM");
    }

    // vulkan
    const vk_dep = b.dependency("vulkan", .{});
    exe.addIncludePath(vk_dep.path("include"));
    const volk_dep = b.dependency("volk", .{});
    exe.addIncludePath(volk_dep.path(""));
    exe.addCSourceFile(.{
        .file = volk_dep.path("volk.c"),
        .flags = extra_volk_flags.items,
    });
    exe.linkLibC();

    // mach_dxc
    const mach_dxc_dep = b.dependency("machdxcompiler", .{
        .target = target,
        .optimize = optimize,
        .shared = true,
        .spirv = true,
        .from_source = true,
    });
    const shader_compiler = b.addExecutable(.{
        .name = "shc",
        .root_source_file = b.path("util/shader_compiler.zig"),
        .target = target,
        .optimize = optimize,
    });
    shader_compiler.linkLibC();
    shader_compiler.root_module.addImport("mach-dxcompiler", mach_dxc_dep.module("mach-dxcompiler"));
    b.installArtifact(shader_compiler);
    b.installArtifact(artifactLinkage(mach_dxc_dep, "machdxcompiler", .dynamic));

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_exe_unit_tests.step);
}

fn artifactLinkage(d: *std.Build.Dependency, name: []const u8, linkage: std.builtin.LinkMode) *std.Build.Step.Compile {
    var found: ?*std.Build.Step.Compile = null;
    for (d.builder.install_tls.step.dependencies.items) |dep_step| {
        const inst = dep_step.cast(std.Build.Step.InstallArtifact) orelse continue;
        if (std.mem.eql(u8, inst.artifact.name, name) and inst.artifact.linkage == linkage) {
            if (found != null) std.debug.panic("artifact name '{s}' is ambiguous", .{name});
            found = inst.artifact;
        }
    }
    return found orelse {
        for (d.builder.install_tls.step.dependencies.items) |dep_step| {
            const inst = dep_step.cast(std.Build.Step.InstallArtifact) orelse continue;
            if (inst.artifact.linkage != linkage) continue;
            std.log.info("available artifact: '{s}'", .{inst.artifact.name});
        }
        std.debug.panic("unable to find artifact '{s}'", .{name});
    };
}
