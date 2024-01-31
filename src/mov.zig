const std = @import("std");

const RegName = enum { al, cl, dl, bl, ah, ch, dh, bh, ax, cx, dx, bx, sp, bp, si, di };
const MemName1 = enum { bx, bp, si, di };
const MemName2 = enum { si, di };
const SegRegCode = enum { es, cs, ss, ds };
const MovType = enum { regMemToReg, immediateRegMem, immediateToMem, memToAcc, accToMem, regMemToSegReg, segRegToRegMem };
const MemMode = enum { no_displacement, eight_bit, sixteen_bit, register };

const Address = union(MemMode) {
    no_displacement: NoDisplacement,
    eight_bit: Displacement,
    sixteen_bit: Displacement,
    register: RegName,

    pub fn toStr(self: *const Address, buffer: []u8) ![]u8 {
        var b = std.io.fixedBufferStream(buffer);

        switch (self.*) {
            .no_displacement => {
                try b.writer().print("[{s} + {s}]", .{ @tagName(self.no_displacement.addr1.?), @tagName(self.no_displacement.addr2.?) });
                //return try std.fmt.allocPrint(allocator, "no displacement", .{});
            },
            .eight_bit => {
                try b.writer().print("[{s}]", .{@tagName(self.eight_bit.addr1)});
                //return try std.fmt.allocPrint(allocator, "mov eightbit", .{});
            },
            .sixteen_bit => {
                try b.writer().print("3({s})", .{@tagName(self.sixteen_bit.addr1)});
                if (self.sixteen_bit.addr2) |addr2| {
                    try b.writer().print("+ ({s})", .{@tagName(addr2)});
                }
                //const str3 = try std.fmt.allocPrint(allocator, "+ ({d})", .{self.sixteen_bit.displacement});
                //t = t.append(str3);
                //
            },
            .register => {
                try b.writer().print("{s}", .{@tagName(self.register)});
            },
        }
        return b.getWritten();
    }
};
const NoDisplacement = struct { addr1: ?MemName1, addr2: ?MemName2, direct_address: i16 };
const Displacement = struct { addr1: MemName1, addr2: ?MemName2, displacement: i16 };

pub const Mov = union(MovType) {
    regMemToReg: RegMemToReg,
    immediateRegMem: ImmediateRegMem,
    immediateToMem: ImmediateToReg,
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
            const mov = ImmediateRegMem.parseFromBuffer(buffer[i .. i + 6]);
            var bytes_used: usize = 0;
            switch (mov.word) {
                0 => bytes_used = 5,
                1 => bytes_used = 6,
            }
            return .{ Mov{ .immediateRegMem = mov }, bytes_used };
        } else if (ops7 == 0b1100011) {
            const mov = ImmediateRegMem.parseFromBuffer(buffer[i .. i + 6]);
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
            const mov = RegMemToReg.parseFromBuffer(buffer[i .. i + 4]);
            var bytes_used: usize = 0;
            switch (mov.mode.?) {
                .eight_bit => bytes_used = 3,
                .sixteen_bit => bytes_used = 4,
                else => bytes_used = 2,
            }
            return .{ Mov{ .regMemToReg = mov }, bytes_used };
        } else if (ops4 == 0b1011) {
            // TODO for this and above need to handle when sub parses fail to find anything...
            const mov = ImmediateToReg.parseFromBuffer(buffer[i .. i + 3]);

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

const ImmediateRegMem = struct {
    dst: Address,
    src: RegName,
    word: u1, // 0 byte operation, 1 word operation
    mode: ?MemMode = null,
    data1: u8,
    data2: ?u8 = null,

    fn parseFromBuffer(buffer: []u8) ImmediateRegMem {
        const w: u1 = @intCast(buffer[0] & 1);
        const mod: u2 = @intCast(buffer[1] >> 6);
        const regmem: u3 = @intCast(buffer[1] & 0b111);
        const mode = @as(MemMode, @enumFromInt(mod));
        const dst = getAddress(buffer, mode, w, regmem);
        const data1: u8 = buffer[5];
        var data2: ?u8 = null;

        if (w == 1) {
            data2 = buffer[6];
        }

        return ImmediateRegMem{
            .src = RegName.al,
            .dst = dst,
            .word = w,
            .data1 = data1,
            .data2 = data2,
        };
    }

    pub fn toStr(self: *const ImmediateRegMem, allocator: std.mem.Allocator) ![]u8 {
        var buffer: [12]u8 = undefined;
        return try std.fmt.allocPrint(allocator, "mov {s}, {s}", .{ try self.dst.toStr(&buffer), @tagName(self.src) });
    }
};

const ImmediateToReg = struct {
    dst: Address,
    mode: ?MemMode = null,
    data1: u8,
    data2: ?u8 = null,
    word: u1,

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
            var buffer: [12]u8 = undefined;
            return try std.fmt.allocPrint(allocator, "2mov {s}, {d}", .{ try self.dst.toStr(&buffer), data });
        } else {
            const data: i8 = @bitCast(self.data1);
            var buffer: [12]u8 = undefined;
            return try std.fmt.allocPrint(allocator, "2mov {s}, {d}", .{ try self.dst.toStr(&buffer), data });
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
        std.debug.print("Mod {b}\n", .{(buffer[1] >> 6)});

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
        var buffer: [12]u8 = undefined;
        var buffer1: [12]u8 = undefined;
        return try std.fmt.allocPrint(allocator, "3mov {s}, {s}", .{ (try self.dst.toStr(&buffer)), try self.src.toStr(&buffer1) });
    }
};

const MemToAcc = struct {
    word: u1,
    src: u16,
    dst: Address,

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
        var buffer: [12]u8 = undefined;
        return try std.fmt.allocPrint(allocator, "mov {s}, [{d}]", .{ try self.dst.toStr(&buffer), self.src });
    }
};

const AccToMem = struct {
    word: u1,
    dst: Address,
    src: u16,

    fn parseFromBuffer(buffer: []u8) AccToMem {
        const w: u1 = @intCast(buffer[0] >> 7 & 1);
        const reg = 0b000;
        const dst = getAddress(buffer, MemMode.register, w, reg);
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
        var buffer: [12]u8 = undefined;
        return try std.fmt.allocPrint(allocator, "mov [{s}], {d}", .{ try self.dst.toStr(&buffer), self.src });
    }
};

const RegMemToSegReg = struct {
    dst: RegName,
    src: RegName,
    word: u1, // 0 byte operation, 1 word operation
    mode: ?MemMode = null,
    segreg: SegRegCode,
    displacement_lo: ?u8 = null,
    displacement_hi: ?u8 = null,

    fn parseFromBuffer(buffer: []u8) ImmediateRegMem {
        const w: u1 = @intCast(buffer[0] & 1);
        const segreg: u2 = @intCast((buffer[1] >> 2) & 0b11);
        const segreg_code: SegRegCode = @as(SegRegCode, @enumFromInt(segreg));
        const mod: u2 = @intCast(buffer[1] >> 6);
        const regmem: u3 = @intCast(buffer[1] & 0b111);
        const mode = @as(MemMode, @enumFromInt(mod));
        const displacement_lo: u8, const displacement_hi: u8 = getDisplacement(buffer, mode);

        return RegMemToSegReg{
            .src = RegName.al,
            .dst = @as(RegName, @enumFromInt(regmem)),
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
    dst: RegName,
    src: RegName,
    word: u1, // 0 byte operation, 1 word operation
    mode: ?MemMode = null,
    segreg: SegRegCode,
    displacement_lo: ?u8 = null,
    displacement_hi: ?u8 = null,

    fn parseFromBuffer(buffer: []u8) ImmediateRegMem {
        const w: u1 = @intCast(buffer[0] & 1);
        const segreg: u2 = @intCast((buffer[1] >> 2) & 0b11);
        const segreg_code: SegRegCode = @as(SegRegCode, @enumFromInt(segreg));
        const mod: u2 = @intCast(buffer[1] >> 6);
        const regmem: u3 = @intCast(buffer[1] & 0b111);
        const mode = @as(MemMode, @enumFromInt(mod));
        const displacement_lo: u8, const displacement_hi: u8 = getDisplacement(buffer, mode);

        return SegRegToRegMem{
            .src = RegName.al,
            .dst = @as(RegName, @enumFromInt(regmem)),
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

fn getDisplacement(buffer: []u8, mode: MemMode) i16 {
    var displacement_lo: u8 = undefined;
    var displacement_hi: u8 = undefined;

    switch (mode) {
        .no_displacement => unreachable,
        .eight_bit => {
            displacement_hi = 0;
            displacement_lo = buffer[2];
        },
        .sixteen_bit => {
            displacement_lo = buffer[2];
            displacement_hi = buffer[3];
        },
        .register => unreachable,
    }

    const d: u16 = @intCast(displacement_hi);
    const displacement: i16 = @bitCast((d << 8) | displacement_lo);
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
            return Address{ .eight_bit = Displacement{ .addr1 = @as(MemName1, @enumFromInt(regmem)), .addr2 = @as(MemName2, @enumFromInt(regmem)), .displacement = disp } };
        },
        .sixteen_bit => {
            const disp: i16 = getDisplacement(buffer, mode);
            return Address{ .sixteen_bit = Displacement{ .addr1 = @as(MemName1, @enumFromInt(regmem)), .addr2 = @as(MemName2, @enumFromInt(regmem)), .displacement = disp } };
        },
        .no_displacement => {
            return Address{ .no_displacement = NoDisplacement{ .addr1 = @as(MemName1, @enumFromInt(regmem)), .addr2 = @as(MemName2, @enumFromInt(regmem)), .direct_address = 1 } };
        },
    }
}
