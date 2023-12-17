const std = @import("std");

const RegMemName = enum { al, cl, dl, bl, ah, ch, dh, bh, ax, cx, dx, bx, sp, bp, si, di };
const MovType = enum { regMemToReg, immediateRegMem, immediateToMem };
pub const Mov = union(MovType) {
    regMemToReg: RegMemToRegMov,
    immediateRegMem: ImmediateRegMemMov,
    immediateToMem: ImmediateToRegMov,

    pub fn parseFromBuffer(buffer: []u8) Mov {
        const ops = buffer[i] & 0b100111;

        switch (ops) {
            0b100010 => {
                const mov = Mov.parseRegToReg(buffer[i .. i + 4]);
                try self.instructions.append(Instruction{ .mov = mov });
                switch (mov.mode.?) {
                    .eight_bit => return 3,
                    .sixteen_bit => return 4,
                    else => return 2,
                }
            },
            0b110001 => {
                const mov = Mov.parseImmediateToRegMem(buffer[i .. i + 6]);
                try self.instructions.append(Instruction{ .mov = mov });
                switch (mov.word) {
                    0 => return 5,
                    1 => return 6,
                }
            },
            0b1011 => {
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
};

const MemMode = enum { none, eight_bit, sixteen_bit, register };
const ImmediateRegMemMov = struct {
    dst: RegMemName,
    src: RegMemName,
    word: u1, // 0 byte operation, 1 word operation
    direction: u1, // 0 to reg field, 1 from reg field
    mode: ?MemMode = null,
    displacement_lo: ?u8 = null,
    displacement_hi: ?u8 = null,
    data1: u1,
    data2: ?u1 = null,

    fn parseFromBuffer(buffer: []u8) ImmediateRegMemMov {
        const w: u1 = @intCast(buffer[0] & 1);
        const mod: u2 = @intCast(buffer[1] >> 6);
        const regmem: u3 = @intCast(buffer[1] & 0b111);
        const mode = @as(MemMode, @enumFromInt(mod));
        const displacement_lo: u8, const displacement_hi: u8 = ImmediateRegMemMov.getDisplacement(buffer, mode);
        var data1: ?u8 = null;
        var data2: ?u8 = null;

        switch (w) {
            0 => {
                data1 = buffer[5];
            },
            1 => {
                data1 = buffer[5];
                data2 = buffer[6];
            },
        }

        return ImmediateRegMemMov{
            .src = RegMemName.al,
            .dst = @as(RegMemName, @enumFromInt(regmem)),
            .word = w,
            .direction = 1,
            .mode = mode,
            .displacement_lo = displacement_lo,
            .displacement_hi = displacement_hi,
            .data1 = data1,
            .data2 = data2,
        };
    }
};

const ImmediateToRegMov = struct {
    fn parseFromBuffer(buffer: []u8) ImmediateToRegMov {
        const w: u1 = @intCast(buffer[0] >> 3 & 1);
        const regmem: u3 = @intCast(buffer[0] & 0b111);
        var data1: ?u8 = null;
        var data2: ?u8 = null;

        switch (w) {
            0 => {
                data1 = buffer[1];
            },
            1 => {
                data1 = buffer[1];
                data2 = buffer[2];
            },
        }

        return ImmediateToRegMov{
            .src = RegMemName.al,
            .dst = @as(RegMemName, @enumFromInt(regmem)),
            .word = w,
            .direction = 1,
            .data1 = data1,
            .data2 = data2,
        };
    }
};

const RegMemToRegMov = struct {
    dst: RegMemName,
    src: RegMemName,
    word: u1, // 0 byte operation, 1 word operation
    direction: u1, // 0 to reg field, 1 from reg field
    mode: ?MemMode = null,
    displacement_lo: ?u8 = null,
    displacement_hi: ?u8 = null,

    fn parseFromBuffer(buffer: []u8) RegMemToRegMov {
        const d: u1 = @intCast((buffer[0] >> 1) & 1);
        const w: u1 = @intCast(buffer[0] & 1);
        const mod: u2 = @intCast(buffer[1] >> 6);
        const reg: u3 = @intCast((buffer[1] >> 3) & 0b111);
        const regmem: u3 = @intCast(buffer[1] & 0b111);

        const mode = @as(MemMode, @enumFromInt(mod));
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

        const displacement_lo: u8, const displacement_hi: u8 = RegMemToRegMov.getDisplacement(buffer, mode);

        return RegMemToRegMov{ .src = src, .dst = dst, .word = w, .direction = d, .mode = mode, .displacement_lo = displacement_lo, .displacement_hi = displacement_hi };
    }

    fn getDisplacement(buffer: []u8, mode: MemMode) std.meta.Tuple(&.{ u8, u8 }) {
        var displacement_lo: u8 = undefined;
        var displacement_hi: u8 = undefined;

        switch (mode) {
            .none => {},
            .eight_bit => {
                displacement_lo = buffer[2];
            },
            .sixteen_bit => {
                displacement_lo = buffer[2];
                displacement_hi = buffer[3];
            },
            .register => {},
        }

        return .{ displacement_lo, displacement_hi };
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

    fn toStr(self: *const RegMemToRegMov, allocator: std.mem.Allocator) ![]u8 {
        return try std.fmt.allocPrint(allocator, "mov {s}, {s}", .{ @tagName(self.dst), @tagName(self.src) });
    }
};
