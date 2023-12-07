const std = @import("std");

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

const MemMode = enum { none, eight_bit, sixteen_bit, register };
const Mov = struct {
    dst: RegMemName,
    src: RegMemName,
    word: u1, // 0 byte operation, 1 word operation
    direction: u1, // 0 to reg field, 1 from reg field
    mode: MemMode,

    fn init(d: u1, w: u1, mod: u2, reg: u3, regmem: u3) Mov {
        var src: RegMemName = undefined;
        var dst: RegMemName = undefined;
        switch (d) {
            0 => {
                src = getRegister(w, reg);
                dst = getRegister(w, regmem);
            },
            1 => {
                src = getRegister(w, regmem);
                dst = getRegister(w, reg);
            },
        }
        return Mov{
            .src = src,
            .dst = dst,
            .word = w,
            .direction = d,
            .mode = @as(MemMode, @enumFromInt(mod)),
        };
    }

    fn getRegister(w: u1, reg: u3) RegMemName {
        switch (w) {
            0 => return @as(RegMemName, @enumFromInt(reg)),
            1 => {
                const l = @typeInfo(RegMemName).Enum.fields.len / 2;
                return @as(RegMemName, @enumFromInt(reg + l));
            },
        }
    }

    fn toStr(self: *const Mov, allocator: std.mem.Allocator) ![]u8 {
        return try std.fmt.allocPrint(allocator, "mov {s}, {s}", .{ @tagName(self.dst), @tagName(self.src) });
    }
};

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

const RegMemName = enum { al, cl, dl, bl, ah, ch, dh, bh, ax, cx, dx, bx, sp, bp, si, di };

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

    fn decode(self: *Parser, buffer: []u8, i: usize) !u8 {
        const ops = (buffer[i] >> 2);
        switch (ops) {
            0b100010 => {
                const d: u1 = @intCast((buffer[i] >> 1) & 1);
                const w: u1 = @intCast(buffer[i] & 1);
                const mod: u2 = @intCast(buffer[i + 1] >> 6);
                const reg: u3 = @intCast((buffer[i + 1] >> 3) & 0b111);
                const regmem: u3 = @intCast(buffer[i + 1] & 0b111);
                try self.instructions.append(Instruction{ .mov = Mov.init(d, w, mod, reg, regmem) });
                return 2;
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
