const std = @import("std");
const movstd = @import("./mov.zig");
const Mov = movstd.Mov;

pub fn main() !void {
    var fixed_allocator_buffer: [1000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&fixed_allocator_buffer);
    const instructions = std.ArrayList(Instruction).init(std.heap.page_allocator);
    defer instructions.deinit();

    var args = try std.process.argsWithAllocator(fba.allocator());
    defer args.deinit();

    const command_name = args.next().?;
    const filename = args.next() orelse {
        std.debug.print("Usage: {s} [filename]\n", .{command_name});
        std.os.exit(1);
    };

    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var buffered_reader = std.io.bufferedReader(file.reader());
    var in_stream = buffered_reader.reader();
    var buf: [1024]u8 = undefined;

    var parser = Parser{
        .instructions = instructions,
    };

    while (try in_stream.read(&buf) != 0) {
        _ = try parser.parse(&buf);
    }

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (parser.instructions.items) |instruction| {
        const str = try instruction.toStr(allocator);
        std.debug.print("{s}\n", .{str});
    }
}
const NotImplemented = struct {
    fn toStr() []u8 {
        return "Not Implemented";
    }
};
const InstructionEnum = enum { mov, add, sub, mul, cmp, je, jmp, hlt, not_implemented };
const Instruction = union(InstructionEnum) {
    mov: Mov,
    add: void,
    sub: void,
    mul: void,
    cmp: void,
    je: void,
    jmp: void,
    hlt: void,
    not_implemented: NotImplemented,

    fn toStr(self: *const Instruction, allocator: std.mem.Allocator) ![]u8 {
        switch (self.*) {
            InstructionEnum.mov => |v| return v.toStr(allocator),
            else => unreachable,
        }
    }
};

const Parser = struct {
    instructions: std.ArrayList(Instruction),
    cursor: u8 = 0,

    fn parse(self: *Parser, buffer: []u8) !usize {
        var i: u8 = 0;
        while (buffer[i] != 0b10101010) {
            const bytes_to_skip = try self.decode(buffer, i);
            i = i + bytes_to_skip;
        }

        return 0;
    }

    fn decode100(self: *Parser, buffer: []u8, i: usize) !u8 {
        const ops = (buffer[i] >> 2) & 0b111;

        switch (ops) {
            0b010 => {
                const mov = Mov.parseRegToReg(buffer[i .. i + 4]);
                try self.instructions.append(Instruction{ .mov = mov });
                switch (mov.mode.?) {
                    .eight_bit => return 3,
                    .sixteen_bit => return 4,
                    else => return 2,
                }
            },
            else => {
                std.debug.print("Missing ops {b} {b}\n", .{ ops, buffer[i] });
                return 1;
            },
        }
    }

    fn decode110(self: *Parser, buffer: []u8, i: usize) !u8 {
        const ops = (buffer[i] >> 2) & 0b111;

        switch (ops) {
            0b001 => {
                const mov = Mov.parseImmediateToRegMem(buffer[i .. i + 6]);
                try self.instructions.append(Instruction{ .mov = mov });
                switch (mov.word) {
                    0 => return 5,
                    1 => return 6,
                }
            },
            else => {
                std.debug.print("Missing ops {b} {b}\n", .{ ops, buffer[i] });
                return 1;
            },
        }
    }

    fn decode101(self: *Parser, buffer: []u8, i: usize) !u8 {
        const ops = (buffer[i] >> 4) & 0b1;

        switch (ops) {
            0b1 => {
                const mov = Mov.parseImmediateToReg(buffer[i .. i + 3]);
                try self.instructions.append(Instruction{ .mov = mov });

                switch (mov.word) {
                    0 => return 2,
                    1 => return 3,
                }
            },
            else => {
                std.debug.print("Missing ops {b} {b}\n", .{ ops, buffer[i] });
                return 1;
            },
        }
    }

    // TODO inline functions
    fn decode(self: *Parser, buffer: []u8, i: usize) !u8 {
        const ops = (buffer[i] >> 5);

        switch (ops) {
            0b100 => {
                return self.decode100(buffer, i);
            },
            0b110 => {
                return self.decode110(buffer, i);
            },
            0b101 => {
                return self.decode101(buffer, i);
            },
            else => {
                std.debug.print("Missing ops {b} {b}\n", .{ ops, buffer[i] });
                return 1;
            },
        }
    }
};

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
