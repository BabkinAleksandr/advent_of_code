const std = @import("std");

const MAX_ITERATIONS_COUNT = 100_000;
const CORRECT_ANSWER = 54697;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    var buffer: [1024]u8 = undefined;
    var sum: i32 = 0;
    while (try stdin.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        sum += try getCalibrationValue(line);
    }

    std.debug.assert(sum == CORRECT_ANSWER);
    try stdout.print("\n/2023/d1/P1 result: {d}\n", .{sum});
}

fn getCalibrationValue(s: []const u8) !i32 {
    var start: usize = 0;
    var end: usize = s.len - 1;
    var d1: i32 = -1;
    var d2: i32 = -1;

    var counter: i32 = 0; // guarding from ininite loop
    while (start <= end and counter <= MAX_ITERATIONS_COUNT) {
        if (std.ascii.isDigit(s[start])) {
            if (d1 == -1) {
                d1 = s[start] - '0';
            }
        }
        if (std.ascii.isDigit(s[end])) {
            if (d2 == -1) {
                d2 = s[end] - '0';
            }
        }

        if (d1 == -1) {
            start += 1;
        }
        if (d2 == -1) {
            end -= 1;
        }

        if (d1 != -1 and d2 != -1) {
            break;
        }

        counter += 1;
    }

    if (counter >= MAX_ITERATIONS_COUNT) {
        return error.IterationsCountExhausted;
    }

    if (d1 == -1 or d2 == -1) {
        return error.ParseError;
    }

    return d1 * 10 + d2;
}
