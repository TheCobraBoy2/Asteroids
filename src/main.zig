const std = @import("std");
const Asteroids = @import("Asteroids");
const rl = @import("raylib");
const BoundedArray = @import("bounded_array").BoundedArray;

const math = std.math;
const rand = std.Random;
const fmt = std.fmt;

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
    lives: u8 = 3,
    invulTime: f32 = 0.0,
    invisTime: f32 = 0.0,

    fn isDead(self: Ship) bool {
        return self.deathTime != 0.0;
    }
    fn isInvul(self: Ship) bool {
        return self.invulTime != 0.0;
    }
    fn cantDraw(self: Ship) bool {
        return self.invisTime != 0.0;
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

    fn onDeath(self: Asteroid) !void {
        try spawnAsteroidDestructionParticles(self.pos);
        try splitAsteroid(self.size, self.pos);

        switch (self.size) {
            .BIG => rl.playSound(state.sfx.large),
            .MEDIUM => rl.playSound(state.sfx.med),
            .SMALL => rl.playSound(state.sfx.small),
        }

        state.score += switch (self.size) {
            .BIG => 500,
            .MEDIUM => 100,
            .SMALL => 50,
        };
    }
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

const Sfx = struct {
    fire: rl.Sound,
    thrust: rl.Sound,
    beat1: rl.Sound,
    beat2: rl.Sound,
    small: rl.Sound,
    med: rl.Sound,
    large: rl.Sound,
    extraLife: rl.Sound,
};

const State = struct {
    now: f32 = 0.0,
    delta: f32 = 0.0,
    score: u32 = 0,
    music_timer: f32 = 0.0,
    ship: Ship,
    sfx: Sfx,
    asteroids: std.ArrayList(Asteroid),
    particles: std.ArrayList(Particle),
    projectiles: std.ArrayList(Projectile),
    rand: std.Random,
    alloc: std.mem.Allocator,
    next_beat: i32 = 1,
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

fn spawnShipDestructionParticles(pos: Vector2) !void {
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

fn spawnAsteroidDestructionParticles(pos: Vector2) !void {
    for (0..10) |_| {
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
                .ttl = 1.0 + state.rand.float(f32),
                .values = .{
                    .DOT = .{
                        .radius = 1 + (1.3 * state.rand.float(f32)),
                    },
                },
            },
        );
    }
}

fn update() !void {
    // Rotations / second
    const ROT_SPEED = 1;
    const SHIP_SPEED = 24;

    const BULLET_SPEED = 600.0;
    const BULLET_TTL = 1.5;

    if (!state.ship.isDead() and !state.ship.cantDraw()) {
        if (rl.isKeyPressed(.space) and !state.ship.isDead()) {
            const angle = state.ship.rot + (math.pi * 0.5);
            const dir = Vector2.init(math.cos(angle), math.sin(angle));
            rl.playSound(state.sfx.fire);

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
            const s = try rl.loadSound("resources/thrust.wav");
            rl.playSound(s);
        }
    }

    const DRAG = 0.03;

    state.ship.vel = state.ship.vel.scale(1.0 - DRAG);
    state.ship.pos = state.ship.pos.add(state.ship.vel);

    state.ship.pos = Vector2.init(
        @mod(state.ship.pos.x, SIZE.x),
        @mod(state.ship.pos.y, SIZE.y),
    );

    if (state.asteroids.items.len <= 0) {
        try spawnWave(5);
        state.ship.lives += 1;
        rl.playSound(state.sfx.extraLife);
    }

    for (state.asteroids.items) |*a| {
        a.pos = a.pos.add(a.vel);
        a.pos = Vector2.init(
            @mod(a.pos.x, SIZE.x),
            @mod(a.pos.y, SIZE.y),
        );
        // Check for ship v. asteroid collision
        if (!state.ship.isDead() and a.pos.distance(state.ship.pos) < a.size.size() * 0.65 and !state.ship.isInvul()) {
            try spawnShipDestructionParticles(state.ship.pos);
            state.ship.pos = SIZE.scale(0.5);
            state.ship.lives -= 1;
            state.ship.invulTime = state.now;
            state.ship.invisTime = state.now;
            state.ship.vel = Vector2.init(0.0, 0.0);
            state.ship.rot = 0.0;
            if (state.ship.lives <= 0) {
                state.ship.deathTime = state.now;
            }
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
            if (b.pos.distance(a.pos) < a.size.size() * 0.65) {
                hit = true;

                _ = state.projectiles.swapRemove(i);

                try a.onDeath();

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
    if (state.ship.isInvul() and (state.now - state.ship.invulTime) > 2.5) {
        state.ship.invulTime = 0.0;
    }
    if (state.ship.cantDraw() and (state.now - state.ship.invisTime) > 1.0) {
        state.ship.invisTime = 0.0;
    }
}

fn drawShip(pos: Vector2, rot: ?f32) void {
    const final_rot = rot orelse math.pi;
    drawLines(
        pos,
        SCALE,
        final_rot,
        &.{
            Vector2.init(-0.4, -0.5),
            Vector2.init(0.0, 0.5),
            Vector2.init(0.4, -0.5),
            Vector2.init(0.2, -0.35),
            Vector2.init(-0.2, -0.35),
        },
    );
}
fn render() !void {
    if (!state.ship.isDead() and !state.ship.cantDraw()) {
        drawShip(state.ship.pos, state.ship.rot);
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
    for (0..state.ship.lives) |l| {
        drawShip(Vector2.init(25 + (@as(f32, @floatFromInt(@as(u32, @intCast(l)))) * 40), 75), null);
    }

    var buf: [32]u8 = undefined;
    const slice = try fmt.bufPrint(buf[0..], "{:0>4}", .{state.score});
    buf[slice.len] = 0;
    const str: [:0]const u8 = @ptrCast(buf[0 .. slice.len + 1]);
    rl.drawText(str, 10, 10, 40, .white);

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

fn spawnWave(count: usize) !void {
    for (0..count) |_| {
        const angle = math.tau * state.rand.float(f32);
        const size = AsteroidSize.BIG;
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
            .size = AsteroidSize.BIG,
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
    state.score = 0;
    state.asteroids.clearRetainingCapacity();
    state.projectiles.clearAndFree(state.alloc);
    state.particles.clearAndFree(state.alloc);

    try spawnWave(5);
}

fn min(a: f32, b: f32) f32 {
    return if (a < b) a else b;
}

fn updateMusic() !void {
    const BASE_INTERVAL: f32 = 0.65;
    const MAX_ASTEROIDS: f32 = 20.0;

    const asteroid_count: f32 = @floatFromInt(state.asteroids.items.len);
    const interval = BASE_INTERVAL * (1.0 - min(asteroid_count / MAX_ASTEROIDS, 1.0) * 0.5);

    state.music_timer += state.delta;

    if (state.music_timer >= interval) {
        const beat_to_play = switch (state.next_beat) {
            1 => state.sfx.beat1,
            2 => state.sfx.beat2,
            else => unreachable,
        };

        rl.playSound(beat_to_play);

        state.next_beat = if (state.next_beat == 1) 2 else 1;
        state.music_timer = 0.0;
    }
}

pub fn main() !void {
    const background_color: rl.Color = rl.Color.black;
    const window_title: *const [9:0]u8 = "Asteroids";

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    rl.initWindow(SIZE.x, SIZE.y, window_title);
    rl.initAudioDevice();
    rl.setTargetFPS(60);
    defer rl.closeWindow();

    var prng = rand.Xoshiro256.init(@bitCast(std.time.timestamp()));

    const fire_sound = try rl.loadSound("resources/fire.wav");
    const thrust_sound = try rl.loadSound("resources/thrust.wav");
    const extra_sound = try rl.loadSound("resources/extraShip.wav");
    const beat1 = try rl.loadSound("resources/beat1.wav");
    const beat2 = try rl.loadSound("resources/beat2.wav");
    const bangSmall = try rl.loadSound("resources/bangSmall.wav");
    const bangMed = try rl.loadSound("resources/bangMedium.wav");
    const bangLarge = try rl.loadSound("resources/bangLarge.wav");

    state = .{
        .ship = .{
            .pos = SIZE.scale(0.5),
            .vel = Vector2.init(0, 0),
            .rot = 0.0,
        },
        .sfx = .{
            .fire = fire_sound,
            .thrust = thrust_sound,
            .extraLife = extra_sound,
            .beat1 = beat1,
            .beat2 = beat2,
            .small = bangSmall,
            .med = bangMed,
            .large = bangLarge,
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

        try updateMusic();

        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(background_color);

        try render();
    }
}
