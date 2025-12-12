const std = @import("std");
const Asteroids = @import("Asteroids");
const rl = @import("raylib");

const Vector2 = rl.Vector2;
const THICKNESS = 2.0;

const State = struct {
    shipPos: Vector2,
};
var state: State = .{};

fn drawLines(origin: Vector2, scale: f32, points: []const Vector2) void {
    const Transformer = struct {
        origin: Vector2,
        scale: f32,

        fn apply(self: @This(), p: Vector2) Vector2 {
            return p.scale(self.scale).add(self.origin);
        }
    };

    const t = Transformer{ .scale = scale, .origin = origin };

    for (0..points.len) |i| {
        rl.drawLineEx(
            t.apply(points[i]),
            t.apply(points[(i + 1) % points.len]),
            THICKNESS,
            .white,
        );
    }
}

pub fn main() !void {
    const background_color: rl.Color = rl.Color.black;
    const window_title: *const [9:0]u8 = "Asteroids";

    std.debug.print("Hello, World", .{});
    rl.initWindow(640 * 2, 480 * 2, window_title);
    rl.setTargetFPS(60);
    defer rl.closeWindow();

    while (!rl.windowShouldClose()) {
        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(background_color);

        drawLines(
            Vector2.init(40, 40),
            32.0,
            &.{
                Vector2.init(-0.4, -0.5),
                Vector2.init(0.0, 0.5),
                Vector2.init(0.4, -0.5),
                Vector2.init(0.3, -0.4),
                Vector2.init(-0.3, -0.4),
            },
        );
    }
}
