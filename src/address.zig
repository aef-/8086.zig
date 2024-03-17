const std = @import("std");

pub const MemMode = enum { no_displacement, eight_bit, sixteen_bit, register };

pub const RegName = enum { al, cl, dl, bl, ah, ch, dh, bh, ax, cx, dx, bx, sp, bp, si, di };
pub const MemName1 = enum { bx, bx1, bp, bp1, si, di, bp2, bx2 };
pub const MemName2 = enum { si, di };
pub const SegRegCode = enum { es, cs, ss, ds };

// TODO this is a bad type, it should be a union of MemNames or Direct Address
pub const NoDisplacement = struct { addr1: ?MemName1, addr2: ?MemName2, direct_address: ?i16 };
pub const Displacement = struct { addr1: MemName1, addr2: ?MemName2, displacement: i16 };

pub const Address = union(MemMode) {
    no_displacement: NoDisplacement,
    eight_bit: Displacement,
    sixteen_bit: Displacement,
    register: RegName,

    pub fn init(buffer: []u8, mode: MemMode, w: u1, regmem: u3) Address {
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

// Adapted from https://github.com/cryptocode/bithacks/blob/main/bithacks.zig
fn signExtendFixed(comptime target: type, val: anytype) target {
    const T = @TypeOf(val);
    const SignedType = std.meta.Int(.signed, @typeInfo(T).Int.bits);
    return @as(SignedType, @bitCast(val));
}
