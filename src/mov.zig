const std = @import("std");

const RegName = enum { al, cl, dl, bl, ah, ch, dh, bh, ax, cx, dx, bx, sp, bp, si, di };
const MemName1 = enum { bx, bx1, bp, bp1, si, di, bp2, bx2 };
const MemName2 = enum { si, di };
const SegRegCode = enum { es, cs, ss, ds };
const MovType = enum { regMemToReg, immediateRegMem, immediateToMem, memToAcc, accToMem, regMemToSegReg, segRegToRegMem };
const MemMode = enum { no_displacement, eight_bit, sixteen_bit, register };

// Adapted from https://github.com/cryptocode/bithacks/blob/main/bithacks.zig
pub fn signExtendFixed(comptime target: type, val: anytype) target {
    const T = @TypeOf(val);
    const SignedType = std.meta.Int(.signed, @typeInfo(T).Int.bits);
    return @as(SignedType, @bitCast(val));
}

const Address = union(MemMode) {
    no_displacement: NoDisplacement,
    eight_bit: Displacement,
    sixteen_bit: Displacement,
    register: RegName,

    pub fn bytesUsed(self: *const Address) usize {
        switch (self.*) {
            .no_displacement => {
                if (self.no_displacement.direct_address) |_| {
                    return 2;
                }
                return 0;
            },
            .eight_bit => return 1,
            .sixteen_bit => return 2,
            .register => return 0,
        }
    }

    pub fn toStr(self: *const Address, buffer: []u8) ![]u8 {
        var b = std.io.fixedBufferStream(buffer);

        switch (self.*) {
            .no_displacement => {
                if (self.no_displacement.addr1) |addr| {
                    try b.writer().print("[{s}", .{@tagName(addr)[0..2]});
                }
                if (self.no_displacement.addr2) |addr| {
                    try b.writer().print(" + {s}", .{@tagName(addr)});
                }
                if (self.no_displacement.direct_address) |addr| {
                    try b.writer().print("[{d}", .{addr});
                }
                try b.writer().print("]", .{});
            },
            .eight_bit => {
                try b.writer().print("[{s}", .{@tagName(self.eight_bit.addr1)[0..2]});
                if (self.eight_bit.addr2) |addr2| {
                    try b.writer().print(" + {s}", .{@tagName(addr2)});
                }
                if (self.eight_bit.displacement < 0) {
                    try b.writer().print(" - {d}]", .{self.eight_bit.displacement * -1});
                } else if (self.eight_bit.displacement > 0) {
                    try b.writer().print(" + {d}]", .{self.eight_bit.displacement});
                } else {
                    try b.writer().print("]", .{});
                }
            },
            .sixteen_bit => {
                try b.writer().print("[{s}", .{@tagName(self.sixteen_bit.addr1)[0..2]});
                if (self.sixteen_bit.addr2) |addr2| {
                    try b.writer().print(" + {s}", .{@tagName(addr2)});
                }
                if (self.sixteen_bit.displacement < 0) {
                    try b.writer().print(" - {d}]", .{self.sixteen_bit.displacement * -1});
                } else if (self.sixteen_bit.displacement > 0) {
                    try b.writer().print(" + {d}]", .{self.sixteen_bit.displacement});
                } else {
                    try b.writer().print("]", .{});
                }
            },
            .register => {
                try b.writer().print("{s}", .{@tagName(self.register)});
            },
        }
        return b.getWritten();
    }
};
// TODO this is a bad type, it should be a union of MemNames or Direct Address
const NoDisplacement = struct { addr1: ?MemName1, addr2: ?MemName2, direct_address: ?i16 };
const Displacement = struct { addr1: MemName1, addr2: ?MemName2, displacement: i16 };

pub const Mov = union(MovType) {
    regMemToReg: RegMemToReg,
    immediateRegMem: ImmediateRegMem,
    immediateToMem: ImmediateToReg,
    memToAcc: MemToAcc,
    accToMem: AccToMem,
    regMemToSegReg: RegMemToSegReg,
    segRegToRegMem: SegRegToRegMem,

    pub fn bytesUsed(self: *const Mov) usize {
        switch (self.*) {
            .regMemToReg => |m| {
                const bytes_used = 2;
                var address: Address = m.src;
                if (m.src == MemMode.register) {
                    address = m.dst;
                }
                return bytes_used + address.bytesUsed();
            },
            .immediateRegMem => |m| {
                var bytes_used: usize = 0;
                switch (m.word) {
                    0 => bytes_used = 3,
                    1 => bytes_used = 4,
                }

                _ = switch (m.dst) {
                    .register => 0,
                    .eight_bit => bytes_used += 1,
                    .sixteen_bit => bytes_used += 2,
                    .no_displacement => 0,
                };
                return bytes_used;
            },
            .immediateToMem => |m| {
                var bytes_used: usize = 0;
                switch (m.word) {
                    0 => bytes_used = 2,
                    1 => bytes_used = 3,
                }

                return bytes_used + m.dst.bytesUsed();
            },
            .memToAcc => return 3,
            .accToMem => return 3,
            .regMemToSegReg => return 4,
            .segRegToRegMem => return 4,
        }
    }

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

    pub fn parseFromBuffer(buffer: []u8, i: usize) ?Mov {
        const ops7 = buffer[i] >> 1;
        const ops6 = ops7 >> 1;
        const ops4 = ops6 >> 2;

        if (buffer[i] == 0b10001110) {
            const mov = RegMemToSegReg.parseFromBuffer(buffer[i .. i + 4]);
            return Mov{ .regMemToSegReg = mov };
        } else if (buffer[i] == 0b10001100) {
            const mov = SegRegToRegMem.parseFromBuffer(buffer[i .. i + 4]);
            return Mov{ .segRegToRegMem = mov };
        } else if (ops7 == 0b1100011) {
            const mov = ImmediateRegMem.parseFromBuffer(buffer[i .. i + 7]);
            return Mov{ .immediateRegMem = mov };
        } else if (ops7 == 0b1010000) {
            const mov = MemToAcc.parseFromBuffer(buffer[i .. i + 3]);
            return Mov{ .memToAcc = mov };
        } else if (ops7 == 0b1010001) {
            const mov = AccToMem.parseFromBuffer(buffer[i .. i + 3]);
            return Mov{ .accToMem = mov };
        } else if (ops6 == 0b100010) {
            const mov = RegMemToReg.parseFromBuffer(buffer[i .. i + 4]);
            return Mov{ .regMemToReg = mov };
        } else if (ops4 == 0b1011) {
            // TODO for this and above need to handle when sub parses fail to find anything...
            const mov = ImmediateToReg.parseFromBuffer(buffer[i .. i + 3]);
            return Mov{ .immediateToMem = mov };
        } else {
            return null;
        }
    }
};

const ImmediateRegMem = struct {
    dst: Address,
    src: i16,
    word: u1,

    fn parseFromBuffer(buffer: []u8) ImmediateRegMem {
        const w: u1 = @intCast(buffer[0] & 1);
        const mod: u2 = @intCast(buffer[1] >> 6);
        const regmem: u3 = @intCast(buffer[1] & 0b111);
        const mode = @as(MemMode, @enumFromInt(mod));
        const dst = getAddress(buffer, mode, w, regmem);
        const data1: u8 = buffer[5];
        var data2: u8 = 0;

        if (w == 1) {
            data2 = buffer[6];
        }

        const d: u16 = @intCast(data2);
        const data: i16 = @bitCast((d << 8) | data1);

        return ImmediateRegMem{ .src = data, .dst = dst, .word = w };
    }

    pub fn toStr(self: *const ImmediateRegMem, allocator: std.mem.Allocator) ![]u8 {
        var buffer: [32]u8 = undefined;
        return try std.fmt.allocPrint(allocator, "mov {s}, {d}", .{ try self.dst.toStr(&buffer), self.src });
    }
};

const ImmediateToReg = struct {
    dst: Address,
    mode: ?MemMode = null,
    word: u1,
    data1: u8,
    data2: ?u8 = null,

    fn parseFromBuffer(buffer: []u8) ImmediateToReg {
        const w: u1 = @intCast((buffer[0] >> 3) & 1);
        const data1: u8 = @intCast(buffer[1]);
        var data2: ?u8 = null;

        const reg: u3 = @intCast(buffer[0] & 0b111);
        const dst: Address = getAddress(buffer, MemMode.register, w, reg);

        if (w == 1) {
            data2 = @intCast(buffer[2]);
        }

        return ImmediateToReg{ .dst = dst, .data1 = data1, .data2 = data2, .word = w };
    }
    pub fn toStr(self: *const ImmediateToReg, allocator: std.mem.Allocator) ![]u8 {
        if (self.data2) |data2| {
            const d: u16 = @intCast(data2);
            const data: i16 = @bitCast((d << 8) | self.data1);
            var buffer: [32]u8 = undefined;
            return try std.fmt.allocPrint(allocator, "mov {s}, {d}", .{ try self.dst.toStr(&buffer), data });
        } else {
            const data: i8 = @bitCast(self.data1);
            var buffer: [32]u8 = undefined;
            return try std.fmt.allocPrint(allocator, "mov {s}, {d}", .{ try self.dst.toStr(&buffer), data });
        }
    }
};

const RegMemToReg = struct {
    dst: Address,
    src: Address,
    mode: ?MemMode,
    word: u1, // 0 byte operation, 1 word operation
    direction: u1, // 0 to reg field, 1 from reg field

    fn parseFromBuffer(buffer: []u8) RegMemToReg {
        const d: u1 = @intCast((buffer[0] >> 1) & 1);
        const w: u1 = @intCast(buffer[0] & 1);
        const mod: u2 = @intCast(buffer[1] >> 6);
        const reg: u3 = @intCast((buffer[1] >> 3) & 0b111);
        const regmem: u3 = @intCast(buffer[1] & 0b111);

        const mode = @as(MemMode, @enumFromInt(mod));
        var src: Address = undefined;
        var dst: Address = undefined;

        switch (d) {
            0 => {
                src = getAddress(buffer, MemMode.register, w, reg);
                dst = getAddress(buffer, mode, w, regmem);
            },
            1 => {
                src = getAddress(buffer, mode, w, regmem);
                dst = getAddress(buffer, MemMode.register, w, reg);
            },
        }

        return RegMemToReg{ .src = src, .dst = dst, .word = w, .direction = d, .mode = mode };
    }

    pub fn toStr(self: *const RegMemToReg, allocator: std.mem.Allocator) ![]u8 {
        var buffer: [32]u8 = undefined;
        var buffer1: [32]u8 = undefined;
        return try std.fmt.allocPrint(allocator, "mov {s}, {s}", .{ try self.dst.toStr(&buffer), try self.src.toStr(&buffer1) });
    }
};

const MemToAcc = struct {
    word: u1,
    dst: Address,
    src: u16,

    fn parseFromBuffer(buffer: []u8) MemToAcc {
        const w: u1 = @intCast(buffer[0] >> 7 & 1);
        const reg = 0b000;
        const dst = getAddress(buffer, MemMode.register, w, reg);
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
        var buffer: [32]u8 = undefined;
        return try std.fmt.allocPrint(allocator, "mov {s}, [{d}]", .{ try self.dst.toStr(&buffer), self.src });
    }
};

const AccToMem = struct {
    word: u1,
    dst: u16,
    src: Address,

    fn parseFromBuffer(buffer: []u8) AccToMem {
        const w: u1 = @intCast(buffer[0] >> 7 & 1);
        const reg = 0b000;
        const src = getAddress(buffer, MemMode.register, w, reg);
        const addr_lo: u8 = buffer[1];
        const addr_hi: u16 = @intCast(buffer[2]);

        const addr: u16 = @bitCast((addr_hi << 8) | addr_lo);

        return AccToMem{
            .src = src,
            .dst = addr,
            .word = w,
        };
    }
    pub fn toStr(self: *const AccToMem, allocator: std.mem.Allocator) ![]u8 {
        var buffer: [32]u8 = undefined;
        return try std.fmt.allocPrint(allocator, "mov [{d}], {s}", .{
            self.dst,
            try self.src.toStr(&buffer),
        });
    }
};

const RegMemToSegReg = struct {
    dst: RegName,
    src: RegName,
    word: u1, // 0 byte operation, 1 word operation
    mode: ?MemMode = null,
    segreg: SegRegCode,

    fn parseFromBuffer(buffer: []u8) RegMemToSegReg {
        const w: u1 = @intCast(buffer[0] & 1);
        const segreg: u2 = @intCast((buffer[1] >> 2) & 0b11);
        const segreg_code: SegRegCode = @as(SegRegCode, @enumFromInt(segreg));
        const mod: u2 = @intCast(buffer[1] >> 6);
        const regmem: u3 = @intCast(buffer[1] & 0b111);
        const mode = @as(MemMode, @enumFromInt(mod));

        return RegMemToSegReg{
            .src = RegName.al,
            .dst = @as(RegName, @enumFromInt(regmem)),
            .segreg = segreg_code,
            .word = w,
            .mode = mode,
        };
    }
    pub fn toStr(self: *const RegMemToSegReg, allocator: std.mem.Allocator) ![]u8 {
        return try std.fmt.allocPrint(allocator, "mov {s}, {s}", .{ @tagName(self.dst), @tagName(self.src) });
    }
};

const SegRegToRegMem = struct {
    dst: RegName,
    src: RegName,
    word: u1, // 0 byte operation, 1 word operation
    mode: ?MemMode = null,
    segreg: SegRegCode,

    fn parseFromBuffer(buffer: []u8) SegRegToRegMem {
        const w: u1 = @intCast(buffer[0] & 1);
        const segreg: u2 = @intCast((buffer[1] >> 2) & 0b11);
        const segreg_code: SegRegCode = @as(SegRegCode, @enumFromInt(segreg));
        const mod: u2 = @intCast(buffer[1] >> 6);
        const regmem: u3 = @intCast(buffer[1] & 0b111);
        const mode = @as(MemMode, @enumFromInt(mod));

        return SegRegToRegMem{ .src = RegName.al, .dst = @as(RegName, @enumFromInt(regmem)), .segreg = segreg_code, .word = w, .mode = mode };
    }
    pub fn toStr(self: *const SegRegToRegMem, allocator: std.mem.Allocator) ![]u8 {
        return try std.fmt.allocPrint(allocator, "mov {s}, {s}", .{ @tagName(self.dst), @tagName(self.src) });
    }
};

fn getDisplacement(buffer: []u8, mode: MemMode) i16 {
    var displacement_lo: u8 = undefined;
    var displacement_hi: u8 = undefined;
    var displacement: i16 = undefined;

    switch (mode) {
        .no_displacement => unreachable,
        .eight_bit => {
            displacement_hi = 0;
            displacement_lo = buffer[2];
            displacement = signExtendFixed(i16, displacement_lo);
        },
        .sixteen_bit => {
            displacement_lo = buffer[2];
            displacement_hi = buffer[3];
            const d: u16 = @intCast(displacement_hi);
            displacement = @bitCast((d << 8) | displacement_lo);
        },
        .register => unreachable,
    }

    return displacement;
}

fn getAddress(buffer: []u8, mode: MemMode, w: u1, regmem: u3) Address {
    switch (mode) {
        .register => {
            switch (w) {
                0 => return Address{ .register = @as(RegName, @enumFromInt(regmem)) },
                1 => {
                    const l = @typeInfo(RegName).Enum.fields.len / 2;
                    return Address{ .register = @as(RegName, @enumFromInt(regmem + l)) };
                },
            }
        },
        .eight_bit => {
            const disp: i16 = getDisplacement(buffer, mode);
            var addr2: ?MemName2 = null;
            if (regmem < 0b100) {
                addr2 = @as(MemName2, @enumFromInt(regmem));
            }
            return Address{ .eight_bit = Displacement{ .addr1 = @as(MemName1, @enumFromInt(regmem)), .addr2 = addr2, .displacement = disp } };
        },
        .sixteen_bit => {
            const disp: i16 = getDisplacement(buffer, mode);
            var addr2: ?MemName2 = null;
            if (regmem < 0b100) {
                addr2 = @as(MemName2, @enumFromInt(regmem));
            }

            return Address{ .sixteen_bit = Displacement{ .addr1 = @as(MemName1, @enumFromInt(regmem)), .addr2 = addr2, .displacement = disp } };
        },
        .no_displacement => {
            var addr1: ?MemName1 = null;
            var addr2: ?MemName2 = null;
            var direct_address: ?i16 = null;
            if (regmem != 0b110) {
                addr1 = @as(MemName1, @enumFromInt(regmem));
                if (regmem < 0b100) {
                    addr2 = @as(MemName2, @enumFromInt(regmem));
                }
            } else {
                direct_address = getDisplacement(buffer, MemMode.sixteen_bit);
            }

            return Address{ .no_displacement = NoDisplacement{ .addr1 = addr1, .addr2 = addr2, .direct_address = direct_address } };
        },
    }
}
