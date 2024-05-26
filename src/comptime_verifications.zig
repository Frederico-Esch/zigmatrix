const std = @import("std");
const zigMatrix = @import("main.zig");

fn isNumeric(comptime T: type) bool {
    return comptime switch (@typeInfo(T)) {
        .Int => true,
        .Float => true,
        else => false
    };
}

pub fn verifyTypeRandom(comptime T: type) type {
    if (!isNumeric(T)) @compileError("Only numeric types are accepted");
    return void;
}

fn verifyType(comptime typeA: type, comptime typeB: type, comptime fn_name: []const u8 ) bool {
    const fields_b = @typeInfo(typeB).Struct.fields;
    const expected_names = [_][] const u8{ "rows", "cols", "data" };
    const expected_comptime = [_] bool { true, true, false };

    //var i = 0;
    //while (i < expected_names.len) : (i += 1) {
    for (0..expected_names.len) |i| {
        if (!std.mem.eql(u8, fields_b[i].name, expected_names[i])) {
            @compileError("Wrong structure in the argument of " ++ fn_name ++ ": Expected Matrix type");
            //@compileError("Field " ++ fields_b[i].name ++ " should be " ++ expected_names[i] ++ " in the argument of " ++ fn_name);
        }

        if (!fields_b[i].is_comptime == expected_comptime[i]) {
            @compileError("Wrong structure in the argument of " ++ fn_name ++ ": Expected Matrix type");
            //@compileError("Field " ++ fields_b[i].name ++ " is computed at the wrong time in the argument of " ++ fn_name);
        }
    }

    //simple comparison
    if (typeA == typeB) {
        return true;
    }

    //complex comparison
    const typeMatA = @typeInfo(@typeInfo(typeA).Struct.fields[2].type).Pointer.child;
    const typeMatB = @typeInfo(@typeInfo(typeB).Struct.fields[2].type).Pointer.child;
    if (!isNumeric(typeMatA) or !isNumeric(typeMatB)) {
        @compileError("Only numeric types are accepted");
    }

    if (@typeInfo(typeA).Struct.fields[2].type != @typeInfo(typeB).Struct.fields[2].type) {
        @compileError("The type of numbers in the two matrices should be the same");
    }

    const rowsf_a = @typeInfo(typeA).Struct.fields[0];
    const rowsf_b = @typeInfo(typeB).Struct.fields[0];
    if (rowsf_a.default_value) |drows_a| {
        const drows_a_aligned: *const align(rowsf_a.alignment) anyopaque = @alignCast(drows_a);
        if (rowsf_b.default_value) |drows_b| {
            const drows_b_aligned: *const align(rowsf_b.alignment) anyopaque = @alignCast(drows_b);

            const rows_a: *const rowsf_a.type = @ptrCast(drows_a_aligned);
            const rows_b: *const rowsf_b.type = @ptrCast(drows_b_aligned);
            if (rows_a.* != rows_b.*) {
                @compileError("Rows of matrices don't match in " + fn_name);
            }
        }
    }

    const colsf_a = @typeInfo(typeA).Struct.fields[1];
    const colsf_b = @typeInfo(typeB).Struct.fields[1];
    if (colsf_a.default_value) |dcols_a| {
        const dcols_a_aligned: *const align(colsf_a.alignment) anyopaque = @alignCast(dcols_a);
        if (colsf_b.default_value) |dcols_b| {
            const dcols_b_aligned: *const align(colsf_b.alignment) anyopaque = @alignCast(dcols_b);

            const cols_a: *const colsf_a.type = @ptrCast(dcols_a_aligned);
            const cols_b: *const colsf_b.type = @ptrCast(dcols_b_aligned);
            if (cols_a.* != cols_b.*) {
                @compileError("Cols of matrices don't match in " + fn_name);
            }
        }
    }

    return true;
}

pub fn verifyTypeAndReturn(comptime typeA: type, comptime typeB: type, comptime fn_name: []const u8, comptime returnType: type) type {
    if (!verifyType(typeA, typeB, fn_name)) @compileError("Something unexpected happened");
    return returnType;
}

pub fn verifyTypeNoReturn (comptime typeA: type, comptime typeB: type, comptime fn_name: []const u8) type {
    return verifyTypeAndReturn(typeA, typeB, fn_name, void);
}

pub fn verifyTypeMultiplication(comptime typeA: type, comptime typeB: type) type {
    if (!verifyType(typeA, typeB, "Multiplication")) @compileError("Something unexpected happened");

    const T = @typeInfo(@typeInfo(typeA).Struct.fields[2].type).Pointer.child;
    if (@typeInfo(typeA).Struct.fields[2].type != @typeInfo(typeB).Struct.fields[2].type) {
        @compileError("The type of numbers in the two matrices should be the same");
    }

    {
        const colsf = @typeInfo(typeA).Struct.fields[1];
        const rowsf = @typeInfo(typeB).Struct.fields[0];
        if (colsf.default_value) | dcols | {
            const dcols_aligned: *const align(colsf.alignment) anyopaque = @alignCast(dcols); //colsf.alignment
            if (rowsf.default_value) |drows| {
                const drows_aligned: *const align(rowsf.alignment) anyopaque = @alignCast(drows); //rowsf.alignment

                const drows_aligned_cast: *const rowsf.type = @ptrCast(drows_aligned);
                const dcols_aligned_cast: *const colsf.type = @ptrCast(dcols_aligned);
                if(drows_aligned_cast.* != dcols_aligned_cast.*) {
                    @compileError("Cols of the first matrix should be the same as the Rows of the second");
                }
            }
        }
    }

    const colsf = @typeInfo(typeB).Struct.fields[1];
    const rowsf = @typeInfo(typeA).Struct.fields[0];
    if (colsf.default_value) | dcols | {
        const dcols_aligned: *const align(colsf.alignment) anyopaque = @alignCast(dcols); //colsf.alignment,
        if (rowsf.default_value) |drows| {
            const drows_aligned: *const align(rowsf.alignment) anyopaque = @alignCast(drows); //rowsf.alignment,
            const rows_ptr:*const rowsf.type = @ptrCast(drows_aligned);
            const cols_ptr:*const colsf.type = @ptrCast(dcols_aligned);

            const rows = rows_ptr.*;
            const cols = cols_ptr.*;

            return zigMatrix.Matrix(rows, cols, T);
        }
    }

    @compileError("Error in the matrices");
}

pub fn verifyTypeScalarMultiplication(comptime matType: type, comptime scaType: type) type {
    const T = @typeInfo(@typeInfo(matType).Struct.fields[2].type).Pointer.child;

    if ( T == scaType) {
        return void;
    }

    @compileError("Error in one or more parameter types");
}
