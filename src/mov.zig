const std = @import("std");

const RegMemName = enum { al, cl, dl, bl, ah, ch, dh, bh, ax, cx, dx, bx, sp, bp, si, di };
const SegRegCode = enum { es, cs, ss, ds };
const MovType = enum { regMemToReg, immediateRegMem, immediateToMem, memToAcc, accToMem, regMemToSegReg, segRegToRegMem };

pub const Mov = union(MovType) {
    regMemToReg: RegMemToRegMov,
    immediateRegMem: ImmediateRegMemMov,
    immediateToMem: ImmediateToRegMov,
    memToAcc: MemToAcc,
    accToMem: AccToMem,
    regMemToSegReg: RegMemToSegReg,
    segRegToRegMem: SegRegToRegMem,

    pub fn toStr(self: *const Mov, allocator: std.mem.Allocator) ![]u8 {
        switch (self.*) {
            .regMemToReg => return self.regMemToReg.toStr(allocator),
            .immediateRegMem => return self.immediateRegMem.toStr(allocator),
            .immediateToMem => return self.immediateToMem.toStr(allocator),
            .memToAcc => return self.memToAcc.toStr(allocator),
            .accToMem => return self.accToMem.toStr(allocator),
            .regMemToSegReg => return self.regMemToSegReg.toStr(allocator),
            .segRegToRegMem => return self.segRegToRegMem.toStr(allocator),
        }
    }

    pub fn parseFromBuffer(buffer: []u8, i: usize) std.meta.Tuple(&.{ ?Mov, usize }) {
        const ops7 = buffer[i] >> 1;
        const ops6 = ops7 >> 1;
        const ops4 = ops6 >> 2;

        if (buffer[i] == 0b10001110) {
            const mov = ImmediateRegMemMov.parseFromBuffer(buffer[i .. i + 6]);
            var bytes_used: usize = 0;
            switch (mov.word) {
                0 => bytes_used = 5,
                1 => bytes_used = 6,
            }
            return .{ Mov{ .immediateRegMem = mov }, bytes_used };
        } else if (ops7 == 0b1100011) {
            const mov = ImmediateRegMemMov.parseFromBuffer(buffer[i .. i + 6]);
            var bytes_used: usize = 0;
            switch (mov.word) {
                0 => bytes_used = 5,
                1 => bytes_used = 6,
            }
            return .{ Mov{ .immediateRegMem = mov }, bytes_used };
        } else if (ops7 == 0b1010000) {
            const mov = MemToAcc.parseFromBuffer(buffer[i .. i + 3]);
            const bytes_used: usize = 3;

            return .{ Mov{ .memToAcc = mov }, bytes_used };
        } else if (ops7 == 0b1010001) {
            const mov = AccToMem.parseFromBuffer(buffer[i .. i + 3]);
            const bytes_used: usize = 3;

            return .{ Mov{ .accToMem = mov }, bytes_used };
        } else if (ops6 == 0b100010) {
            const mov = RegMemToRegMov.parseFromBuffer(buffer[i .. i + 4]);
            var bytes_used: usize = 0;
            switch (mov.mode.?) {
                .eight_bit => bytes_used = 3,
                .sixteen_bit => bytes_used = 4,
                else => bytes_used = 2,
            }
            return .{ Mov{ .regMemToReg = mov }, bytes_used };
        } else if (ops4 == 0b1011) {
            // TODO for this and above need to handle when sub parses fail to find anything...
            const mov = ImmediateToRegMov.parseFromBuffer(buffer[i .. i + 3]);

            var bytes_used: usize = 0;
            switch (mov.word) {
                0 => bytes_used = 2,
                1 => bytes_used = 3,
            }
            return .{ Mov{ .immediateToMem = mov }, bytes_used };
        } else {
            return .{ null, 0 };
        }
    }
};

const MemMode = enum { none, eight_bit, sixteen_bit, register };
const ImmediateRegMemMov = struct {
    dst: RegMemName,
    src: RegMemName,
    word: u1, // 0 byte operation, 1 word operation
    mode: ?MemMode = null,
    displacement_lo: ?u8 = null,
    displacement_hi: ?u8 = null,
    data1: u8,
    data2: ?u8 = null,

    fn parseFromBuffer(buffer: []u8) ImmediateRegMemMov {
        const w: u1 = @intCast(buffer[0] & 1);
        const mod: u2 = @intCast(buffer[1] >> 6);
        const regmem: u3 = @intCast(buffer[1] & 0b111);
        const mode = @as(MemMode, @enumFromInt(mod));
        const displacement_lo: u8, const displacement_hi: u8 = getDisplacement(buffer, mode);
        const data1: u8 = buffer[5];
        var data2: ?u8 = null;

        if (w == 1) {
            data2 = buffer[6];
        }

        return ImmediateRegMemMov{
            .src = RegMemName.al,
            .dst = @as(RegMemName, @enumFromInt(regmem)),
            .word = w,
            .mode = mode,
            .displacement_lo = displacement_lo,
            .displacement_hi = displacement_hi,
            .data1 = data1,
            .data2 = data2,
        };
    }

    pub fn toStr(self: *const ImmediateRegMemMov, allocator: std.mem.Allocator) ![]u8 {
        return try std.fmt.allocPrint(allocator, "1mov {s}, {s}", .{ @tagName(self.dst), @tagName(self.src) });
    }
};

const ImmediateToRegMov = struct {
    dst: RegMemName,
    mode: ?MemMode = null,
    data1: u8,
    data2: ?u8 = null,
    word: u1,

    fn parseFromBuffer(buffer: []u8) ImmediateToRegMov {
        const w: u1 = @intCast((buffer[0] >> 3) & 1);
        const data1: u8 = @intCast(buffer[1]);
        var data2: ?u8 = null;

        const reg: u3 = @intCast(buffer[0] & 0b111);
        const dst: RegMemName = getRegister(w, reg);

        if (w == 1) {
            data2 = @intCast(buffer[2]);
        }

        return ImmediateToRegMov{ .dst = dst, .data1 = data1, .data2 = data2, .word = w };
    }
    pub fn toStr(self: *const ImmediateToRegMov, allocator: std.mem.Allocator) ![]u8 {
        if (self.data2) |data2| {
            const d: u16 = @intCast(data2);
            const data: i16 = @bitCast((d << 8) | self.data1);
            return try std.fmt.allocPrint(allocator, "2mov {s}, {d}", .{ @tagName(self.dst), data });
        } else {
            const data: i8 = @bitCast(self.data1);
            return try std.fmt.allocPrint(allocator, "2mov {s}, {d}", .{ @tagName(self.dst), data });
        }
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

        const displacement_lo: u8, const displacement_hi: u8 = getDisplacement(buffer, mode);

        return RegMemToRegMov{ .src = src, .dst = dst, .word = w, .direction = d, .mode = mode, .displacement_lo = displacement_lo, .displacement_hi = displacement_hi };
    }

    pub fn toStr(self: *const RegMemToRegMov, allocator: std.mem.Allocator) ![]u8 {
        return try std.fmt.allocPrint(allocator, "3mov {s}, {s}", .{ @tagName(self.dst), @tagName(self.src) });
    }
};

const MemToAcc = struct {
    word: u1,
    src: u16,
    dst: RegMemName,

    fn parseFromBuffer(buffer: []u8) MemToAcc {
        const w: u1 = @intCast(buffer[0] >> 7 & 1);
        const reg = 0b000;
        const dst = getRegister(w, reg);
        const addr_lo: u8 = buffer[1];
        const addr_hi: u16 = @intCast(buffer[2]);

        const addr: u16 = @bitCast((addr_hi << 8) | addr_lo);

        return MemToAcc{
            .dst = dst,
            .src = addr,
            .word = w,
        };
    }
    pub fn toStr(self: *const MemToAcc, allocator: std.mem.Allocator) ![]u8 {
        return try std.fmt.allocPrint(allocator, "mov {s}, [{d}]", .{ @tagName(self.dst), self.src });
    }
};

const AccToMem = struct {
    word: u1,
    dst: RegMemName,
    src: u16,

    fn parseFromBuffer(buffer: []u8) AccToMem {
        const w: u1 = @intCast(buffer[0] >> 7 & 1);
        const reg = 0b000;
        const dst = getRegister(w, reg);
        const addr_lo: u8 = buffer[1];
        const addr_hi: u16 = @intCast(buffer[2]);

        const addr: u16 = @bitCast((addr_hi << 8) | addr_lo);

        return AccToMem{
            .src = addr,
            .dst = dst,
            .word = w,
        };
    }
    pub fn toStr(self: *const AccToMem, allocator: std.mem.Allocator) ![]u8 {
        return try std.fmt.allocPrint(allocator, "mov [{d}], {s}", .{ self.src, @tagName(self.dst) });
    }
};

const RegMemToSegReg = struct {
    dst: RegMemName,
    src: RegMemName,
    word: u1, // 0 byte operation, 1 word operation
    mode: ?MemMode = null,
    segreg: SegRegCode,
    displacement_lo: ?u8 = null,
    displacement_hi: ?u8 = null,

    fn parseFromBuffer(buffer: []u8) ImmediateRegMemMov {
        const w: u1 = @intCast(buffer[0] & 1);
        const segreg: u2 = @intCast((buffer[1] >> 2) & 0b11);
        const segreg_code: SegRegCode = @as(SegRegCode, @enumFromInt(segreg));
        const mod: u2 = @intCast(buffer[1] >> 6);
        const regmem: u3 = @intCast(buffer[1] & 0b111);
        const mode = @as(MemMode, @enumFromInt(mod));
        const displacement_lo: u8, const displacement_hi: u8 = getDisplacement(buffer, mode);

        return RegMemToSegReg{
            .src = RegMemName.al,
            .dst = @as(RegMemName, @enumFromInt(regmem)),
            .segrerg = segreg_code,
            .word = w,
            .mode = mode,
            .displacement_lo = displacement_lo,
            .displacement_hi = displacement_hi,
        };
    }
    pub fn toStr(self: *const RegMemToSegReg, allocator: std.mem.Allocator) ![]u8 {
        return try std.fmt.allocPrint(allocator, "mov {s}, {s}", .{ @tagName(self.dst), @tagName(self.src) });
    }
};

const SegRegToRegMem = struct {
    dst: RegMemName,
    src: RegMemName,
    word: u1, // 0 byte operation, 1 word operation
    mode: ?MemMode = null,
    segreg: SegRegCode,
    displacement_lo: ?u8 = null,
    displacement_hi: ?u8 = null,

    fn parseFromBuffer(buffer: []u8) ImmediateRegMemMov {
        const w: u1 = @intCast(buffer[0] & 1);
        const segreg: u2 = @intCast((buffer[1] >> 2) & 0b11);
        const segreg_code: SegRegCode = @as(SegRegCode, @enumFromInt(segreg));
        const mod: u2 = @intCast(buffer[1] >> 6);
        const regmem: u3 = @intCast(buffer[1] & 0b111);
        const mode = @as(MemMode, @enumFromInt(mod));
        const displacement_lo: u8, const displacement_hi: u8 = getDisplacement(buffer, mode);

        return SegRegToRegMem{
            .src = RegMemName.al,
            .dst = @as(RegMemName, @enumFromInt(regmem)),
            .segreg = segreg_code,
            .word = w,
            .mode = mode,
            .displacement_lo = displacement_lo,
            .displacement_hi = displacement_hi,
        };
    }
    pub fn toStr(self: *const SegRegToRegMem, allocator: std.mem.Allocator) ![]u8 {
        return try std.fmt.allocPrint(allocator, "mov {s}, {s}", .{ @tagName(self.dst), @tagName(self.src) });
    }
};

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
