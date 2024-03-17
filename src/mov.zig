const std = @import("std");
const addrstd = @import("./address.zig");
const Address = addrstd.Address;
const MemMode = addrstd.MemMode;
const RegName = addrstd.RegName;
const SegRegCode = addrstd.SegRegCode;
const MemName1 = addrstd.MemName1;
const MemName2 = addrstd.MemName2;
const NoDisplacement = addrstd.NoDisplacement;
const Displacement = addrstd.Displacement;

const MovType = enum { regMemToReg, immediateRegMem, immediateToMem, memToAcc, accToMem, regMemToSegReg, segRegToRegMem };

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
            const mov = ImmediateRegMem.parseFromBuffer(buffer[i .. i + 6]);
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
        const dst = Address.init(buffer, mode, w, regmem);
        var dataIndex: usize = 0;

        switch (mode) {
            .no_displacement => {
                dataIndex = 2;
            },
            .eight_bit => {
                dataIndex = 3;
            },
            .sixteen_bit => {
                dataIndex = 4;
            },
            .register => {
                dataIndex = 2;
            },
        }

        const data1: u8 = buffer[dataIndex];
        var data2: u8 = 0;
        if (w == 1) {
            data2 = buffer[dataIndex + 1];
        }

        const d: u16 = @intCast(data2);
        const data: i16 = @bitCast((d << 8) | data1);

        return ImmediateRegMem{ .src = data, .dst = dst, .word = w };
    }

    pub fn toStr(self: *const ImmediateRegMem, allocator: std.mem.Allocator) ![]u8 {
        var buffer: [32]u8 = undefined;
        if (self.word == 1) {
            return try std.fmt.allocPrint(allocator, "mov {s}, word {d}", .{ try self.dst.toStr(&buffer), self.src });
        } else {
            return try std.fmt.allocPrint(allocator, "mov {s}, byte {d}", .{ try self.dst.toStr(&buffer), self.src });
        }
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
        const dst: Address = Address.init(buffer, MemMode.register, w, reg);

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
                src = Address.init(buffer, MemMode.register, w, reg);
                dst = Address.init(buffer, mode, w, regmem);
            },
            1 => {
                src = Address.init(buffer, mode, w, regmem);
                dst = Address.init(buffer, MemMode.register, w, reg);
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
        const dst = Address.init(buffer, MemMode.register, w, reg);
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
        const src = Address.init(buffer, MemMode.register, w, reg);
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
