const std = @import("std");

const MAX_ITERATIONS_COUNT = 100_000;
const CORRECT_ANSWER = 54885;

const Substitute = struct { source: []const u8, target: []const u8 };
const SUBSTITUTES = [_]Substitute{
    Substitute{ .source = "one", .target = "o1e" },
    Substitute{ .source = "two", .target = "t2o" },
    Substitute{ .source = "three", .target = "th3ee" },
    Substitute{ .source = "four", .target = "f4ur" },
    Substitute{ .source = "five", .target = "f5ve" },
    Substitute{ .source = "six", .target = "s6x" },
    Substitute{ .source = "seven", .target = "se7en" },
    Substitute{ .source = "eight", .target = "ei8ht" },
    Substitute{ .source = "nine", .target = "n9ne" },
};

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    var buffer: [1024]u8 = undefined;
    var sum: i32 = 0;
    while (try stdin.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        const withReplacements = try replaceAll(SUBSTITUTES.len, &SUBSTITUTES, line);
        sum += try getCalibrationValue(withReplacements);
    }

    std.debug.assert(sum == CORRECT_ANSWER);
    try stdout.print("\n/2023/d1/P2 result: {d}\n", .{sum});
}

/// Applies all provided substitutes
fn replaceAll(comptime N: usize, replacements: *const [N]Substitute, line: []const u8) ![]const u8 {
    const buffer = try std.heap.page_allocator.dupe(u8, line);

    for (replacements) |*sub| {
        const replaced = std.mem.replace(u8, buffer, sub.source, sub.target, buffer);
        _ = replaced;
    }
    return buffer;
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
