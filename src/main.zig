const std = @import("std");
const Random = std.rand.DefaultPrng;

// Planos
// smul -> scalar multiplication DONE
// TODO finish the inverse matrix algorithm
//

fn verifyType (comptime typeA: type, comptime typeB: type, comptime fn_name: []const u8) type {
    const fields_b = @typeInfo(typeB).Struct.fields;
    const expected_names = [_][] const u8{ "rows", "cols", "data" };
    const expected_comptime = [_] bool { true, true, false };

    var i = 0;
    while (i < expected_names.len) : (i += 1) {
        if (!std.mem.eql(u8, fields_b[i].name, expected_names[i])) {
            @compileError("Field " ++ fields_b[i].name ++ " should be " ++ expected_names[i] ++ " in the argument of " ++ fn_name);
        }

        if (!fields_b[i].is_comptime == expected_comptime[i]) {
            @compileError("Field " ++ fields_b[i].name ++ " is computed at the wrong time in the argument of " ++ fn_name);
        }
    }

    //simple comparisson
    if (typeA != typeB) {
        @compileError("Matrices don't match in " ++ fn_name);
    }

    return void;
}

fn verifyTypeMultiplication(comptime typeA: type, comptime typeB: type) type {

    const T = @typeInfo(@typeInfo(typeA).Struct.fields[2].type).Pointer.child;
    if (@typeInfo(typeA).Struct.fields[2].type != @typeInfo(typeB).Struct.fields[2].type) {
        @compileError("The type of numbers in the two matrices should be the same");
    }

    {
        const colsf = @typeInfo(typeA).Struct.fields[1];
        const rowsf = @typeInfo(typeB).Struct.fields[0];
        if (colsf.default_value) | dcols | {
            const dcols_aligned = @alignCast(colsf.alignment, dcols);
            if (rowsf.default_value) |drows| {
                const drows_aligned = @alignCast(rowsf.alignment, drows);
                if(@ptrCast(*const rowsf.type, drows_aligned).* != @ptrCast(*const colsf.type, dcols_aligned).*) {
                    @compileError("Cols of the first matrix should be the same as the Rows of the second");
                }
            }
        }
    }

    const colsf = @typeInfo(typeB).Struct.fields[1];
    const rowsf = @typeInfo(typeA).Struct.fields[0];
    if (colsf.default_value) | dcols | {
        const dcols_aligned = @alignCast(colsf.alignment, dcols);
        if (rowsf.default_value) |drows| {
            const drows_aligned = @alignCast(rowsf.alignment, drows);
            const rows = @ptrCast(*const rowsf.type, drows_aligned).*;
            const cols = @ptrCast(*const colsf.type, dcols_aligned).*;

            return Matrix(rows, cols, T);
        }
    }

    @compileError("Error in the matrices");
}

fn verifyScalarMultiplication(comptime matType: type, comptime scaType: type) type {

    const T = @typeInfo(@typeInfo(matType).Struct.fields[2].type).Pointer.child;

    if ( T == scaType) {
        return void;
    }

    @compileError("Error in one or more parameter types");
}

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
            var i: usize = 0;

            while (i < self.cols) : (i += 1) {
                var j: usize = 0;
                while (j < self.rows) : (j += 1) {
                    self.at(i, j).* = val;
                }
            }
        }

        pub fn random(self: Self, min: T, max: T, _random: std.rand.Random ) void {
            comptime switch (@typeInfo(T)) {
                .Float => {},
                .Int => {},
                else => {
                    @compileError("Type not supported");
                }
            };

            var i: usize = 0;
            while (i < self.rows) : (i += 1) {
                var j: usize = 0;
                while (j < self.cols) : (j += 1) {
                    const val = self.at(i, j);
                    switch (@typeInfo(T)) {
                        .Float => {
                            val.* = _random.float(T) * (max-min) + min;
                        },
                        .Int => {
                            val.* = @mod(_random.int(T), max);
                            if (val.* <= 0) {
                                val.* += min;
                            }
                        },
                        else => {}
                    }
                }
            }
        }

        pub fn add(self: Self, b: anytype) verifyType(@TypeOf(self), @TypeOf(b), "add") {
            var i: usize = 0;
            while ( i < self.rows*self.cols) : (i += 1) {
                self.data[i] += b.data[i];
            }
        }

        pub fn sub(self: Self, b:anytype) verifyType(@TypeOf(self), @TypeOf(b), "sub") {
            var i: usize = 0;
            while(i < self.rows*self.cols) : (i += 1) {
                self.data[i] -= b.data[i];
            }
        }

        pub fn mul(self: Self, b:anytype, mul_allocator: std.mem.Allocator) !verifyTypeMultiplication(@TypeOf(self), @TypeOf(b)) {
            var result = try Matrix(self.rows, b.cols, T).init(mul_allocator);

            var i: usize = 0;
            var j: usize = 0;
            var k: usize = 0;

            while (i < self.rows) : (i += 1) {
                j = 0;
                while (j < b.cols) : (j += 1) {
                    k = 0;
                    while ( k < self.cols) : (k += 1) {
                        result.at(i, j).* += (self.at(i, k).*) * (b.at(k, j).*);
                    }
                }
            }

            return result;
        }

        pub fn scalarMul(self: Self, b:anytype) verifyScalarMultiplication(Self, @TypeOf(b)) {

            var i: usize = 0;
            var j: usize = 0;

            while (i < self.rows) : (i += 1) {
                j = 0;
                while (j < self.cols) : (j+= 1) {
                    self.at(i, j).* *= b;
                }
            }

        }

        pub fn transpose(self: Self) value: {
            if (self.rows != self.cols) {
                @compileError("transposed should be used instead to create a new matrix, the inplace function only works for square matrices");
            }
            break :value void;
        } {
            var i: usize = 0;
            var j: usize = 0;
            while (i < self.rows) : (i += 1) {
                while (j < self.cols) : (j += 1) {
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
            var i: isize = 0;

            while ( i < self.rows ) : (i += 1) {
                var j: isize = 0;
                var positive_part: T = 1;
                var negative_part: T = 1;

                while (j < self.cols) : (j += 1) {
                    positive_part *= self.at(@intCast(usize, j), @intCast(usize, @mod(i+j, self.cols))).*;
                }

                j -= 1;
                while (j >= 0) : (j -= 1) {
                    negative_part *= self.at(@intCast(usize, @intCast(isize, self.rows) - j - 1), @intCast(usize, @mod(j+i, self.cols))).*;
                }
                result += positive_part - negative_part;
            }

            return result;

        }

        pub fn print(self: Self) void {
            var i:usize = 0;
            std.debug.print("\n", .{});
            while (i < self.rows) : (i += 1) {
                var j: usize = 0;
                while (j < self.cols) : (j += 1) {
                    std.debug.print("{} ", .{self.at(i, j).*});
                }
                std.debug.print("\n", .{});
            }
        }

    };

    return M;
}



pub fn main() !void {
    var arena_1 = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_1.deinit();
    const allocator_a = arena_1.allocator();

    var buffer: [100]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator_b = fba.allocator();


    const a = try Matrix(3, 3, i32).init(allocator_a);
    const b = try Matrix(3, 3, i32).init(allocator_b);

    var rand = Random.init(@intCast(u64, std.time.timestamp()));
    const random = rand.random();

    a.random(0, 10, random);
    b.random(5, 15, random);

    //b.print();

    //std.debug.print("\n A + B = C", .{});
    //a.add(b);
    //a.print();

    //std.debug.print("\n B - C = D", .{});
    //b.sub(a);
    //b.print();

    const c = try b.mul(a, allocator_a);
    b.print();
    a.print();
    c.print();

    std.debug.print("det(A): {}, det(B):{}, det(C):{}\n", .{a.det(), b.det(), c.det()});

    c.transpose();
    c.print();

    const d = try Matrix(3, 4, i32).init(allocator_a);
    d.random(0, 10, random);
    d.print();
    const e = d.transposed();
    e.print();
    e.deinit();


    const f = try Matrix(2, 2, f32).init(allocator_a);
    f.random(0, 100, random);
    f.print();
    const scalar: f32  = 2;
    f.scalarMul(scalar);
    f.print();
}
