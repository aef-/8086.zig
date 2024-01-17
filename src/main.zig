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
    cursor: usize = 0,

    fn parse(self: *Parser, buffer: []u8) !usize {
        var i: usize = 0;
        while (buffer[i] != 0b10101010) {
            const bytes_to_skip = try self.decode(buffer, i);
            i = i + bytes_to_skip;
        }

        return 0;
    }
    // TODO inline functions
    fn decode(self: *Parser, buffer: []u8, i: usize) !usize {
        const optional_mov, const bytes_to_skip = Mov.parseFromBuffer(buffer, i);

        if (optional_mov) |mov| {
            try self.instructions.append(Instruction{ .mov = mov });
            return bytes_to_skip;
        }

        std.debug.print("Missing ops {b} \n", .{buffer[i]});
        return 1;
    }
};

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
