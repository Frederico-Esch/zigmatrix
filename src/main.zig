const std = @import("std");
const Random = std.rand.DefaultPrng;
const verify = @import("comptime_verifications.zig");

// Planos
// TODO finish the inverse matrix algorithm -> adjunt matrix | covariant matrix | ~determinant~
//

pub fn Matrix(comptime _rows: u64, comptime _cols: u64, comptime T: type) type {
    return __Matrix(_rows, _cols, T, false);
}

pub fn Tmatrix(comptime _rows: u64, comptime _cols: u64, comptime T: type) type {
    return __Matrix(_rows, _cols, T, true);
}

fn __Matrix(comptime _rows: u64, comptime _cols: u64, comptime T: type, comptime is_transposed: bool) type {

    const M = struct {

        const Self = @This();

        comptime rows: u64 = _rows,
        comptime cols: u64 = _cols,
        data: []T = undefined,
        allocator: ?std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) anyerror!Self {
            const result =  .{
                .rows = _rows,
                .cols = _cols,
                .allocator = allocator,
                .data = try allocator.alloc(T, _rows*_cols)
            };
            @memset(result.data, 0);
            return result;
        }

        pub fn deinit(self: Self) void {
            if (self.allocator) |allocator| {
                if (@sizeOf(T) > 0) {
                    allocator.free(self.data.ptr[0..self.cols*self.rows]);
                }
            }
        }

        pub fn from(allocator: std.mem.Allocator, data: [_rows][_cols]T) anyerror!Self {
            const result = Self {
                .allocator = allocator,
                .data = try allocator.alloc(T, _rows*_cols)
            };
            for (0.._rows) |x| {
                for (0.._cols) |y| {
                    result.at(x, y).* = data[x][y];
                }
            }
            return result;
        }

        pub fn at(self: Self, x: usize, y: usize) *T {
            // [A01, A02, A03, A11, A12, A13]
            //[ A01 A02 A03 ]
            //[ A11 A12 A13 ]
            // x -> row, y -> col
            switch (is_transposed) {
                true => {
                    return &self.data[self.rows*y + x];
                },
                false => {
                    return &self.data[self.cols*x + y];
                }
            }
        }

        pub fn fill(self: Self, val: T) void {
            //var i: usize = 0;
            //while (i < self.cols) : (i += 1) {
            //    var j: usize = 0;
            //    while (j < self.rows) : (j += 1) {
            //        self.at(i, j).* = val;
            //    }
            //}
            for (self.data) |*elem| {
                elem.* = val;
            }
        }

        pub fn random(self: Self, min: T, max: T, _random: std.rand.Random ) verify.verifyTypeRandom(T) {

            //var i: usize = 0;
            //while (i < self.rows) : (i += 1) {
            //    var j: usize = 0;
            //    while (j < self.cols) : (j += 1) {
            //        const val = self.at(i, j);
            //        switch (@typeInfo(T)) {
            //            .Float => {
            //                val.* = _random.float(T) * (max-min) + min;
            //            },
            //            .Int => {
            //                val.* = @mod(_random.int(T), max-min) + min;
            //            },
            //            else => {}
            //        }
            //    }
            //}

            for (self.data) |*elem| {
                elem.* = switch (@typeInfo(T)) {
                    .Int => _random.intRangeAtMost(T, min, max),
                    .Float => _random.float(T) * (max-min) + min,
                    else => {}
                };
            }
        }

        pub fn add(self: Self, b: anytype) verify.verifyTypeNoReturn(@TypeOf(self), @TypeOf(b), "add") {

            //while ( i < self.rows*self.cols) : (i += 1) {
            for (0..self.rows) |x| {
                for (0..self.cols) |y| {
                    self.at(x, y).* += b.at(x, y).*;
                }
            }
        }

        pub fn sub(self: Self, b:anytype) verify.verifyTypeNoReturn(@TypeOf(self), @TypeOf(b), "sub") {

            for (0..self.rows) |x| {
                for (0..self.cols) |y| {
                    self.at(x, y).* -= b.at(x, y).*;
                }
            }
        }

        pub fn mul(self: Self, b:anytype, mul_allocator: std.mem.Allocator) !verify.verifyTypeMultiplication(@TypeOf(self), @TypeOf(b)) {
            var result = try Matrix(self.rows, b.cols, T).init(mul_allocator);

            //var i: usize = 0;
            //var j: usize = 0;
            //var k: usize = 0;
            //while (i < self.rows) : (i += 1) {
            //    j = 0;
            //    while (j < b.cols) : (j += 1) {
            //        k = 0;
            //        while ( k < self.cols) : (k += 1) {
            //            result.at(i, j).* += (self.at(i, k).*) * (b.at(k, j).*);
            //        }
            //    }
            //}

            result.fill(0);
            for (0..self.rows) |i| {
                for (0..b.cols) |j| {
                    for (0..self.cols) |k| {
                        result.at(i, j).* += (self.at(i, k).*) * (b.at(k, j).*);
                    }
                }
            }

            return result;
        }

        pub fn scalarMul(self: Self, b:anytype) verify.verifyTypeScalarMultiplication(Self, @TypeOf(b)) {

            //var i: usize = 0;
            //var j: usize = 0;
            //while (i < self.rows) : (i += 1) {
            //    j = 0;
            //    while (j < self.cols) : (j+= 1) {
            //        self.at(i, j).* *= b;
            //    }
            //}

            for (self.data) |*elem| {
                elem.* *= b;
            }

        }

        pub fn transpose(self: Self) value: {
            if (self.rows != self.cols) {
                @compileError("transposed should be used instead to create a new matrix, the inplace function only works for square matrices");
            }
            break :value void;
        } {
            //var i: usize = 0;
            //var j: usize = 0;
            //while (i < self.rows) : (i += 1) {
            //    while (j < self.cols) : (j += 1) {
            for (0..self.rows) |i| {
                for (0..self.cols) |j| {
                    const temp = self.at(i, j).*;
                    self.at(i, j).* = self.at(j, i).*;
                    self.at(j, i).* = temp;
                }
            }
        }

        pub fn transposed(self: Self) Tmatrix(self.cols, self.rows, T) {
            const result: Tmatrix(self.cols, self.rows, T) = .{
                .rows = self.cols,
                .cols = self.rows,
                .allocator = null,
                .data = self.data,
            };

            return result;
        }

        pub fn det(self: Self) value: {

            if (self.rows != self.cols) {
                @compileError("Only square matrices have a determinant");
            }

            break :value T;
        } {

            var result: T = 0;

            for (0..self.cols) |diag| {
                var positive_part: T = 1;
                var negative_part: T = 1;

                for (0..self.rows) |row| {
                    const col = @mod(diag+row, self.cols);
                    positive_part *= self.at(row, col).*;
                }

                for (0..self.rows) |_j| {
                    const row = self.cols - _j - 1;
                    const col = @mod(_j+diag, self.cols);
                    negative_part *= self.at(row, col).*;
                }
                result += positive_part - negative_part;
            }

            return result;

        }

        pub fn print(self: Self) void {
            std.debug.print("\n", .{});
            //var i:usize = 0;
            //while (i < self.rows) : (i += 1) {
            for (0..self.rows) |i| {
                //var j: usize = 0;
                //while (j < self.cols) : (j += 1) {
                for (0..self.cols) |j| {
                    std.debug.print("{} ", .{self.at(i, j).*});
                }
                std.debug.print("\n", .{});
            }
        }

        pub fn equals(self: Self, other: anytype) verify.verifyTypeAndReturn(@TypeOf(self), @TypeOf(other), "equals", bool) {
            for (0..self.rows) |x| {
                for (0..self.cols) |y| {
                    if (self.at(x, y).* != other.at(x, y).*) return false;
                }
            }
            return true;
        }
    };

    return M;
}

pub fn main() !void {

    const a = try Matrix(3, 3, i32).from(std.heap.page_allocator, .{
        .{2, 3, 4},
        .{3, 2, 4},
        .{1, 2, 1}
    });

    std.debug.print("{}\n", .{a.det()});

}

test "Sum" {
    var arena_1 = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_1.deinit();
    const allocator_a = arena_1.allocator();

    var buffer: [100]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator_b = fba.allocator();

    var rand = Random.init(@intCast(std.time.timestamp()));
    const random = rand.random();

    const a = try Matrix(3, 3, i32).init(allocator_a);
    const b = try Matrix(3, 3, i32).init(allocator_b);

    a.random(0, 10, random);
    b.random(5, 15, random);

    var c = try Matrix(3, 3, i32).from(allocator_a, .{
        .{ a.at(0, 0).* + b.at(0, 0).*, a.at(0, 1).* + b.at(0, 1).*, a.at(0, 2).* + b.at(0, 2).* },
        .{ a.at(1, 0).* + b.at(1, 0).*, a.at(1, 1).* + b.at(1, 1).*, a.at(1, 2).* + b.at(1, 2).* },
        .{ a.at(2, 0).* + b.at(2, 0).*, a.at(2, 1).* + b.at(2, 1).*, a.at(2, 2).* + b.at(2, 2).* },
    });

    a.add(b);
    try std.testing.expect(a.equals(c));
}

test "Sub" {
    var arena_1 = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_1.deinit();
    const allocator_a = arena_1.allocator();

    var buffer: [100]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator_b = fba.allocator();

    var rand = Random.init(@intCast(std.time.timestamp()));
    const random = rand.random();

    const a = try Matrix(3, 3, i32).init(allocator_a);
    const b = try Matrix(3, 3, i32).init(allocator_b);

    a.random(0, 10, random);
    b.random(5, 15, random);

    const c = try Matrix(3, 3, i32).from(allocator_a, . {
        .{ b.at(0, 0).* - a.at(0, 0).*, b.at(0, 1).* - a.at(0, 1).*, b.at(0, 2).* - a.at(0, 2).* },
        .{ b.at(1, 0).* - a.at(1, 0).*, b.at(1, 1).* - a.at(1, 1).*, b.at(1, 2).* - a.at(1, 2).* },
        .{ b.at(2, 0).* - a.at(2, 0).*, b.at(2, 1).* - a.at(2, 1).*, b.at(2, 2).* - a.at(2, 2).* },
    });
    //std.debug.print("\n B - C = D", .{});
    b.sub(a);
    //b.print();

    try std.testing.expect(b.equals(c));
}

test "Mul" {
    var arena_1 = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_1.deinit();
    const allocator_a = arena_1.allocator();

    var buffer: [100]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator_b = fba.allocator();

    const a = try Matrix(3, 3, i32).from(allocator_a, .{
        .{ 1, 2, 1 },
        .{ 2, 3, 2 },
        .{ 1, 2, 1 },
    });
    const b = try Matrix(3, 3, i32).from(allocator_b, .{
        .{ 3, 2, 1 },
        .{ 2, 3, 2 },
        .{ 1, 2, 3 },
    });

    const c = try a.mul(b, allocator_a);
    //b.print();
    //a.print();
    //c.print();

    const d = try Matrix(3, 3, i32).from(allocator_a, .{
        .{8, 10, 8},
        .{14, 17, 14},
        .{8, 10, 8}
    });
    try std.testing.expect(c.equals(d));
}

test "Det" {
    var buffer: [100]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator_a = fba.allocator();

    var rand = Random.init(@intCast(std.time.timestamp()));
    const random = rand.random();

    const a = try Matrix(3, 3, i32).init(allocator_a);

    a.random(0, 10, random);

    //std.debug.print("det(A): {}\n", .{a.det()});

    //[A00 A01 A02]
    //[A10 A11 A12]
    //[A20 A21 A22]
    const det = (a.at(0, 0).*)*(a.at(1, 1).* * a.at(2, 2).* - a.at(2, 1).* * a.at(1, 2).*)
              - (a.at(0, 1).*)*(a.at(1, 0).* * a.at(2, 2).* - a.at(2, 0).* * a.at(1, 2).*)
              + (a.at(0, 2).*)*(a.at(1, 0).* * a.at(2, 1).* - a.at(2, 0).* * a.at(1, 1).*);
    std.testing.expect(a.det() == det) catch {
        std.debug.print("{} {}\n", .{a.det(), det});
        try std.testing.expect(false);
    };
}

test "Transpose" {
    var buffer: [4*9*4]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator_b = fba.allocator();

    var rand = Random.init(@intCast(std.time.timestamp()));
    const random = rand.random();

    const a = try Matrix(3, 3, i32).init(allocator_b);
    const b = try Matrix(3, 3, i32).init(allocator_b);
    const c = try Matrix(3, 3, i32).init(allocator_b);

    a.random(0, 10, random);
    b.random(5, 15, random);
    c.random(10, 25, random);

    //a.print();
    const d = a.transposed();

    const e = try Matrix(3, 3, i32).from(allocator_b, .{
        .{ a.at(0, 0).*, a.at(1, 0).*, a.at(2, 0).* },
        .{ a.at(0, 1).*, a.at(1, 1).*, a.at(2, 1).* },
        .{ a.at(0, 2).*, a.at(1, 2).*, a.at(2, 2).* },
    });

    try std.testing.expect(d.equals(e));

    d.add(b);
    e.add(b);
    try std.testing.expect(d.equals(e));

    d.sub(c);
    e.sub(c);
    try std.testing.expect(d.equals(e));
}

test "Life Times" {
    //var buffer: [4*9*5]u8 = undefined;
    //var fba = std.heap.FixedBufferAllocator.init(&buffer);
    //const allocator = fba.allocator();

    //var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    //defer arena.deinit();
    //const allocator = arena.allocator();

    const allocator = std.testing.allocator;

    var rand = Random.init(@intCast(std.time.timestamp()));
    const random = rand.random();

    const a = try Matrix(3, 4, i32).init(allocator);

    a.random(0, 10, random);
    const b = a.transposed();
    a.deinit();
    _ = b;
}

test "Scalar mul" {
    var arena_1 = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_1.deinit();
    const allocator = arena_1.allocator();

    var rand = Random.init(@intCast(std.time.timestamp()));
    const random = rand.random();

    const a = try Matrix(2, 2, f32).init(allocator);
    a.random(0, 100, random);

    const scalar: f32  = 2;
    const b = try Matrix(2, 2, f32).from(allocator, .{
        .{ a.at(0, 0).* * scalar, a.at(0, 1).* * scalar },
        .{ a.at(1, 0).* * scalar, a.at(1, 1).* * scalar }
    });

    a.scalarMul(scalar);

    try std.testing.expect(a.equals(b));
}

test "Random Types" {
    var buffer: [100]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    var rand = Random.init(@intCast(std.time.timestamp()));
    const random = rand.random();

    const a = try Matrix(3, 3, u8).init(allocator);
    a.random(0, 255, random);
}
