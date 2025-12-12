const std = @import("std");
const Asteroids = @import("Asteroids");
const rl = @import("raylib");
const BoundedArray = @import("bounded_array").BoundedArray;

const math = std.math;
const rand = std.Random;

const Vector2 = rl.Vector2;

const THICKNESS = 2.0;
const SCALE = 38.0;
const SIZE = Vector2.init(640 * 2, 480 * 2);

const Ship = struct {
    pos: Vector2,
    vel: Vector2,
    dead: bool = false,
    rot: f32,
    deathTime: f32 = 0.0,

    fn isDead(self: Ship) bool {
        return self.deathTime != 0.0;
    }
};

const AsteroidSize = enum {
    BIG,
    MEDIUM,
    SMALL,

    fn size(self: @This()) f32 {
        return switch (self) {
            .BIG => SCALE * 3.0,
            .MEDIUM => SCALE * 1.4,
            .SMALL => SCALE * 0.8,
        };
    }
    fn velScale(self: @This()) f32 {
        return switch (self) {
            .BIG => 0.75,
            .MEDIUM => 1.0,
            .SMALL => 1.6,
        };
    }
};

const Asteroid = struct {
    pos: Vector2,
    vel: Vector2,
    size: AsteroidSize,
    seed: u64,
};

const ParticleType = enum {
    LINE,
    DOT,
};

const Particle = struct {
    pos: Vector2,
    vel: Vector2,
    ttl: f32,

    values: union(ParticleType) {
        LINE: struct {
            rot: f32,
            length: f32,
        },
        DOT: struct {
            radius: f32,
        },
    },
};

const Projectile = struct {
    pos: Vector2,
    vel: Vector2,
    ttl: f32,
};

const State = struct {
    now: f32 = 0.0,
    delta: f32 = 0.0,
    ship: Ship,
    asteroids: std.ArrayList(Asteroid),
    particles: std.ArrayList(Particle),
    projectiles: std.ArrayList(Projectile),
    rand: std.Random,
    alloc: std.mem.Allocator,
};
var state: State = undefined;

fn drawLines(origin: Vector2, scale: f32, rot: f32, points: []const Vector2) void {
    const Transformer = struct {
        origin: Vector2,
        scale: f32,
        rot: f32,

        fn apply(self: @This(), p: Vector2) Vector2 {
            return p.rotate(self.rot).scale(self.scale).add(self.origin);
        }
    };

    const t = Transformer{ .scale = scale, .origin = origin, .rot = rot };

    for (0..points.len) |i| {
        rl.drawLineEx(
            t.apply(points[i]),
            t.apply(points[(i + 1) % points.len]),
            THICKNESS,
            .white,
        );
    }
}

fn drawAsteroid(pos: Vector2, size: AsteroidSize, seed: u64) !void {
    var prng = rand.Xoshiro256.init(seed);
    var random = prng.random();
    var points = try BoundedArray(Vector2, 16).init(0);
    const n = random.intRangeLessThan(i32, 8, 15);
    for (0..@intCast(n)) |i| {
        var radius = 0.3 + (0.2 * random.float(f32));
        if (random.float(f32) < 0.2) {
            radius -= 0.2;
        }
        const angle: f32 = (@as(f32, @floatFromInt(i)) * (math.tau / @as(f32, @floatFromInt(n)))) + (math.pi * 0.125) * random.float(f32);
        try points.append(
            Vector2.init(math.cos(angle), math.sin(angle)).scale(radius),
        );
    }

    drawLines(
        pos,
        size.size(),
        0.0,
        points.slice(),
    );
}

fn spawnDestructionParticles(pos: Vector2) !void {
    for (0..5) |_| {
        const angle = math.tau * state.rand.float(f32);
        try state.particles.append(
            state.alloc,
            .{
                .pos = pos.add(Vector2.init(
                    state.rand.float(f32) * 3,
                    state.rand.float(f32) * 3,
                )),
                .vel = Vector2.init(
                    math.cos(angle),
                    math.sin(angle),
                ).scale(2.0 * state.rand.float(f32)),
                .ttl = 3.0 + state.rand.float(f32),
                .values = .{
                    .LINE = .{
                        .rot = math.tau * state.rand.float(f32),
                        .length = SCALE * (0.6 + (0.4 * state.rand.float(f32))),
                    },
                },
            },
        );
    }
}

fn update() !void {
    // Rotations / second
    const ROT_SPEED = 1.4;
    const SHIP_SPEED = 24;

    const BULLET_SPEED = 600.0;
    const BULLET_TTL = 1.5;

    if (rl.isKeyPressed(.space) and !state.ship.isDead()) {
        const angle = state.ship.rot + (math.pi * 0.5);
        const dir = Vector2.init(math.cos(angle), math.sin(angle));

        try state.projectiles.append(state.alloc, .{
            .pos = state.ship.pos.add(dir.scale(SCALE * 0.7)),
            .vel = dir.scale(BULLET_SPEED),
            .ttl = BULLET_TTL,
        });
    }

    if (rl.isKeyDown(.a)) {
        state.ship.rot -= state.delta * math.tau * ROT_SPEED;
    }

    if (rl.isKeyDown(.d)) {
        state.ship.rot += state.delta * math.tau * ROT_SPEED;
    }

    const dirAngle = state.ship.rot + (math.pi * 0.5);
    const shipDir = Vector2.init(math.cos(dirAngle), math.sin(dirAngle));

    if (rl.isKeyDown(.w)) {
        state.ship.vel = state.ship.vel.add(shipDir.scale(state.delta * SHIP_SPEED));
    }

    const DRAG = 0.03;

    state.ship.vel = state.ship.vel.scale(1.0 - DRAG);
    state.ship.pos = state.ship.pos.add(state.ship.vel);

    state.ship.pos = Vector2.init(
        @mod(state.ship.pos.x, SIZE.x),
        @mod(state.ship.pos.y, SIZE.y),
    );

    for (state.asteroids.items) |*a| {
        a.pos = a.pos.add(a.vel);
        a.pos = Vector2.init(
            @mod(a.pos.x, SIZE.x),
            @mod(a.pos.y, SIZE.y),
        );
        // Check for ship v. asteroid collision
        if (!state.ship.isDead() and a.pos.distance(state.ship.pos) < a.size.size() * 0.6) {
            state.ship.deathTime = state.now;

            try spawnDestructionParticles(state.ship.pos);
        }
    }

    var i: usize = 0;
    while (i < state.particles.items.len) {
        var p = state.particles.items[i];

        p.pos = p.pos.add(p.vel);
        p.pos = Vector2.init(
            @mod(p.pos.x, SIZE.x),
            @mod(p.pos.y, SIZE.y),
        );

        p.ttl -= state.delta;

        if (p.ttl > 0) {
            state.particles.items[i] = p;
            i += 1;
        } else {
            _ = state.particles.swapRemove(i);
        }
    }

    // Update bullets
    i = 0;
    while (i < state.projectiles.items.len) {
        var b = state.projectiles.items[i];

        b.pos = b.pos.add(b.vel.scale(state.delta));

        // Destroy if outside screen bounds
        if (b.pos.x < 0 or b.pos.x > SIZE.x or
            b.pos.y < 0 or b.pos.y > SIZE.y)
        {
            _ = state.projectiles.swapRemove(i);
            continue;
        }

        // Lifetime expiration
        b.ttl -= state.delta;
        if (b.ttl <= 0) {
            _ = state.projectiles.swapRemove(i);
            continue;
        }

        // Check asteroid collision
        var hit = false;
        for (state.asteroids.items, 0..) |a, ai| {
            if (b.pos.distance(a.pos) < a.size.size() * 0.6) {
                hit = true;

                _ = state.projectiles.swapRemove(i);

                try splitAsteroid(a.size, a.pos);

                for (0..5) |_| {
                    const angle = math.tau * state.rand.float(f32);
                    try state.particles.append(
                        state.alloc,
                        .{
                            .pos = a.pos.add(Vector2.init(
                                state.rand.float(f32) * 3,
                                state.rand.float(f32) * 3,
                            )),
                            .vel = Vector2.init(
                                math.cos(angle),
                                math.sin(angle),
                            ).scale(2.0 * state.rand.float(f32)),
                            .ttl = 1.0 + state.rand.float(f32),
                            .values = .{
                                .DOT = .{
                                    .radius = 1 + state.rand.float(f32),
                                },
                            },
                        },
                    );
                }

                _ = state.asteroids.swapRemove(ai);

                break;
            }
        }

        if (!hit) {
            state.projectiles.items[i] = b;
            i += 1;
        }
    }

    if (state.ship.isDead() and (state.now - state.ship.deathTime) > 3.5) {
        try resetStage();
    }
}

fn render() !void {
    if (!state.ship.isDead()) {
        drawLines(
            state.ship.pos,
            SCALE,
            state.ship.rot,
            &.{
                Vector2.init(-0.4, -0.5),
                Vector2.init(0.0, 0.5),
                Vector2.init(0.4, -0.5),
                Vector2.init(0.3, -0.4),
                Vector2.init(-0.3, -0.4),
            },
        );
        if (rl.isKeyDown(.w) and @mod(@as(i32, @intFromFloat(state.now * 20)), 2) == 0) {
            drawLines(
                state.ship.pos,
                SCALE,
                state.ship.rot,
                &.{
                    Vector2.init(-0.3, -0.4),
                    Vector2.init(0.0, -1.0),
                    Vector2.init(0.3, -0.4),
                },
            );
        }
    }

    for (state.asteroids.items) |a| {
        try drawAsteroid(a.pos, a.size, a.seed);
    }

    for (state.projectiles.items) |b| {
        rl.drawCircleV(b.pos, 2.0, .white);
    }

    for (state.particles.items) |p| {
        switch (p.values) {
            .LINE => |line| {
                drawLines(p.pos, line.length, line.rot, &.{ Vector2.init(-0.5, 0), Vector2.init(0.5, 0) });
            },
            .DOT => |dot| {
                rl.drawCircleV(p.pos, dot.radius, .white);
            },
        }
    }
}

fn splitAsteroid(size: AsteroidSize, pos: Vector2) !void {
    if (size == .SMALL) {
        return;
    }
    const s = switch (size) {
        .BIG => AsteroidSize.MEDIUM,
        .MEDIUM => AsteroidSize.SMALL,
        .SMALL => AsteroidSize.SMALL,
    };
    for (0..2) |_| {
        const angle = math.tau * state.rand.float(f32);
        try state.asteroids.append(state.alloc, .{
            .pos = pos,
            .vel = Vector2.init(
                math.cos(angle),
                math.sin(angle),
            ).scale(
                size.velScale() * 3.0 * state.rand.float(f32),
            ),
            .size = s,
            .seed = state.rand.int(u64),
        });
    }
}

fn resetStage() !void {
    state.ship.deathTime = 0.0;
    state.ship = .{
        .pos = SIZE.scale(0.5),
        .vel = Vector2.init(0, 0),
        .rot = 0.0,
    };
    //TODO: Clear asteroids on player lose
    // Is this the best way to do this?
    state.asteroids.clearRetainingCapacity();

    for (0..20) |_| {
        const angle = math.tau * state.rand.float(f32);
        const size = state.rand.enumValue(AsteroidSize);
        try state.asteroids.append(state.alloc, .{
            .pos = Vector2.init(
                state.rand.float(f32) * SIZE.x,
                state.rand.float(f32) * SIZE.y,
            ),
            .vel = Vector2.init(
                math.cos(angle),
                math.sin(angle),
            ).scale(
                size.velScale() * 3.0 * state.rand.float(f32),
            ),
            .size = size,
            .seed = state.rand.int(u64),
        });
    }
}

pub fn main() !void {
    const background_color: rl.Color = rl.Color.black;
    const window_title: *const [9:0]u8 = "Asteroids";

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    rl.initWindow(SIZE.x, SIZE.y, window_title);
    rl.setTargetFPS(60);
    defer rl.closeWindow();

    var prng = rand.Xoshiro256.init(@bitCast(std.time.timestamp()));

    state = .{
        .ship = .{
            .pos = SIZE.scale(0.5),
            .vel = Vector2.init(0, 0),
            .rot = 0.0,
        },
        .asteroids = try std.ArrayList(Asteroid).initCapacity(allocator, 0),
        .particles = try std.ArrayList(Particle).initCapacity(allocator, 0),
        .projectiles = try std.ArrayList(Projectile).initCapacity(allocator, 0),
        .rand = prng.random(),
        .alloc = allocator,
    };
    defer state.asteroids.deinit(allocator);
    defer state.particles.deinit(allocator);
    defer state.projectiles.deinit(allocator);

    try resetStage();

    while (!rl.windowShouldClose()) {
        state.delta = rl.getFrameTime();
        state.now += state.delta;

        try update();

        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(background_color);

        try render();
    }
}
