const std = @import("std");
const mem = std.mem;
const debug = std.debug;
const testing = std.testing;

const meta = std.meta;

pub const TraitFn = fn (type) callconv(.Inline) bool;

pub fn multiTrait(comptime traits: anytype) TraitFn {
    const Closure = struct {
        pub inline fn trait(comptime T: type) bool {
            inline for (traits) |t|
                if (!t(T)) return false;
            return true;
        }
    };
    return Closure.trait;
}

test "multiTrait" {
    const Vector2 = struct {
        const MyType = @This();

        x: u8,
        y: u8,

        pub fn add(self: MyType, other: MyType) MyType {
            return MyType{
                .x = self.x + other.x,
                .y = self.y + other.y,
            };
        }
    };

    const isVector = multiTrait(.{
        hasFn("add"),
        hasField("x"),
        hasField("y"),
    });
    try testing.expect(isVector(Vector2));
    try testing.expect(!isVector(u8));
}

pub fn hasFn(comptime name: []const u8) TraitFn {
    const Closure = struct {
        pub inline fn trait(comptime T: type) bool {
            if (!isContainer(T)) return false;
            if (!@hasDecl(T, name)) return false;
            const DeclType = @TypeOf(@field(T, name));
            return @typeInfo(DeclType) == .Fn;
        }
    };
    return Closure.trait;
}

test "hasFn" {
    const TestStruct = struct {
        pub fn useless() void {}
    };

    try testing.expect(hasFn("useless")(TestStruct));
    try testing.expect(!hasFn("append")(TestStruct));
    try testing.expect(!hasFn("useless")(u8));
}

pub fn hasField(comptime name: []const u8) TraitFn {
    const Closure = struct {
        pub inline fn trait(comptime T: type) bool {
            const fields = switch (@typeInfo(T)) {
                .Struct => |s| s.fields,
                .Union => |u| u.fields,
                .Enum => |e| e.fields,
                else => return false,
            };

            inline for (fields) |field| {
                if (mem.eql(u8, field.name, name)) return true;
            }

            return false;
        }
    };
    return Closure.trait;
}

test "hasField" {
    const TestStruct = struct {
        value: u32,
    };

    try testing.expect(hasField("value")(TestStruct));
    try testing.expect(!hasField("value")(*TestStruct));
    try testing.expect(!hasField("x")(TestStruct));
    try testing.expect(!hasField("x")(**TestStruct));
    try testing.expect(!hasField("value")(u8));
}

pub fn is(comptime id: std.builtin.TypeId) TraitFn {
    const Closure = struct {
        pub inline fn trait(comptime T: type) bool {
            return id == @typeInfo(T);
        }
    };
    return Closure.trait;
}

test "is" {
    try testing.expect(is(.Int)(u8));
    try testing.expect(!is(.Int)(f32));
    try testing.expect(is(.Pointer)(*u8));
    try testing.expect(is(.Void)(void));
    try testing.expect(!is(.Optional)(anyerror));
}

pub fn isPtrTo(comptime id: std.builtin.TypeId) TraitFn {
    const Closure = struct {
        pub inline fn trait(comptime T: type) bool {
            if (!isSingleItemPtr(T)) return false;
            return id == @typeInfo(meta.Child(T));
        }
    };
    return Closure.trait;
}

test "isPtrTo" {
    try testing.expect(!isPtrTo(.Struct)(struct {}));
    try testing.expect(isPtrTo(.Struct)(*struct {}));
    try testing.expect(!isPtrTo(.Struct)(**struct {}));
}

pub fn isSliceOf(comptime id: std.builtin.TypeId) TraitFn {
    const Closure = struct {
        pub inline fn trait(comptime T: type) bool {
            if (!isSlice(T)) return false;
            return id == @typeInfo(meta.Child(T));
        }
    };
    return Closure.trait;
}

test "isSliceOf" {
    try testing.expect(!isSliceOf(.Struct)(struct {}));
    try testing.expect(isSliceOf(.Struct)([]struct {}));
    try testing.expect(!isSliceOf(.Struct)([][]struct {}));
}

///////////Strait trait Fns

//@TODO:
// Somewhat limited since we can't apply this logic to normal variables, fields, or
//  Fns yet. Should be isExternType?
pub inline fn isExtern(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .Struct => |s| s.layout == .Extern,
        .Union => |u| u.layout == .Extern,
        else => false,
    };
}

test "isExtern" {
    const TestExStruct = extern struct {};
    const TestStruct = struct {};

    try testing.expect(isExtern(TestExStruct));
    try testing.expect(!isExtern(TestStruct));
    try testing.expect(!isExtern(u8));
}

pub inline fn isPacked(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .Struct => |s| s.layout == .Packed,
        .Union => |u| u.layout == .Packed,
        else => false,
    };
}

test "isPacked" {
    const TestPStruct = packed struct {};
    const TestStruct = struct {};

    try testing.expect(isPacked(TestPStruct));
    try testing.expect(!isPacked(TestStruct));
    try testing.expect(!isPacked(u8));
}

pub inline fn isUnsignedInt(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .Int => |i| i.signedness == .unsigned,
        else => false,
    };
}

test "isUnsignedInt" {
    try testing.expect(isUnsignedInt(u32) == true);
    try testing.expect(isUnsignedInt(comptime_int) == false);
    try testing.expect(isUnsignedInt(i64) == false);
    try testing.expect(isUnsignedInt(f64) == false);
}

pub inline fn isSignedInt(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .ComptimeInt => true,
        .Int => |i| i.signedness == .signed,
        else => false,
    };
}

test "isSignedInt" {
    try testing.expect(isSignedInt(u32) == false);
    try testing.expect(isSignedInt(comptime_int) == true);
    try testing.expect(isSignedInt(i64) == true);
    try testing.expect(isSignedInt(f64) == false);
}

pub inline fn isSingleItemPtr(comptime T: type) bool {
    if (is(.Pointer)(T)) {
        return @typeInfo(T).Pointer.size == .One;
    }
    return false;
}

test "isSingleItemPtr" {
    const array = [_]u8{0} ** 10;
    try testing.expect(isSingleItemPtr(@TypeOf(&array[0])));
    try testing.expect(!isSingleItemPtr(@TypeOf(array)));
    var runtime_zero: usize = 0;
    _ = &runtime_zero;
    try testing.expect(!isSingleItemPtr(@TypeOf(array[runtime_zero..1])));
}

pub inline fn isManyItemPtr(comptime T: type) bool {
    if (is(.Pointer)(T)) {
        return @typeInfo(T).Pointer.size == .Many;
    }
    return false;
}

test "isManyItemPtr" {
    const array = [_]u8{0} ** 10;
    const mip = @as([*]const u8, @ptrCast(&array[0]));
    try testing.expect(isManyItemPtr(@TypeOf(mip)));
    try testing.expect(!isManyItemPtr(@TypeOf(array)));
    try testing.expect(!isManyItemPtr(@TypeOf(array[0..1])));
}

pub inline fn isSlice(comptime T: type) bool {
    if (is(.Pointer)(T)) {
        return @typeInfo(T).Pointer.size == .Slice;
    }
    return false;
}

test "isSlice" {
    const array = [_]u8{0} ** 10;
    var runtime_zero: usize = 0;
    _ = &runtime_zero;
    try testing.expect(isSlice(@TypeOf(array[runtime_zero..])));
    try testing.expect(!isSlice(@TypeOf(array)));
    try testing.expect(!isSlice(@TypeOf(&array[0])));
}

pub inline fn isIndexable(comptime T: type) bool {
    if (is(.Pointer)(T)) {
        if (@typeInfo(T).Pointer.size == .One) {
            return (is(.Array)(meta.Child(T)));
        }
        return true;
    }
    return is(.Array)(T) or is(.Vector)(T) or isTuple(T);
}

test "isIndexable" {
    const array = [_]u8{0} ** 10;
    const slice = @as([]const u8, &array);
    const vector: @Vector(2, u32) = [_]u32{0} ** 2;
    const tuple = .{ 1, 2, 3 };

    try testing.expect(isIndexable(@TypeOf(array)));
    try testing.expect(isIndexable(@TypeOf(&array)));
    try testing.expect(isIndexable(@TypeOf(slice)));
    try testing.expect(!isIndexable(meta.Child(@TypeOf(slice))));
    try testing.expect(isIndexable(@TypeOf(vector)));
    try testing.expect(isIndexable(@TypeOf(tuple)));
}

pub inline fn isNumber(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .Int, .Float, .ComptimeInt, .ComptimeFloat => true,
        else => false,
    };
}

test "isNumber" {
    const NotANumber = struct {
        number: u8,
    };

    try testing.expect(isNumber(u32));
    try testing.expect(isNumber(f32));
    try testing.expect(isNumber(u64));
    try testing.expect(isNumber(@TypeOf(102)));
    try testing.expect(isNumber(@TypeOf(102.123)));
    try testing.expect(!isNumber([]u8));
    try testing.expect(!isNumber(NotANumber));
}

pub inline fn isIntegral(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .Int, .ComptimeInt => true,
        else => false,
    };
}

test "isIntegral" {
    try testing.expect(isIntegral(u32));
    try testing.expect(!isIntegral(f32));
    try testing.expect(isIntegral(@TypeOf(102)));
    try testing.expect(!isIntegral(@TypeOf(102.123)));
    try testing.expect(!isIntegral(*u8));
    try testing.expect(!isIntegral([]u8));
}

pub inline fn isFloat(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .Float, .ComptimeFloat => true,
        else => false,
    };
}

test "isFloat" {
    try testing.expect(!isFloat(u32));
    try testing.expect(isFloat(f32));
    try testing.expect(!isFloat(@TypeOf(102)));
    try testing.expect(isFloat(@TypeOf(102.123)));
    try testing.expect(!isFloat(*f64));
    try testing.expect(!isFloat([]f32));
}

pub inline fn isConstPtr(comptime T: type) bool {
    if (!is(.Pointer)(T)) return false;
    return @typeInfo(T).Pointer.is_const;
}

test "isConstPtr" {
    var t: u8 = 0;
    t = t;
    const c: u8 = 0;
    try testing.expect(isConstPtr(*const @TypeOf(t)));
    try testing.expect(isConstPtr(@TypeOf(&c)));
    try testing.expect(!isConstPtr(*@TypeOf(t)));
    try testing.expect(!isConstPtr(@TypeOf(6)));
}

pub inline fn isVolatilePtr(comptime T: type) bool {
    if (!is(.Pointer)(T)) return false;
    return @typeInfo(T).Pointer.is_volatile;
}

test "isVolatilePtr" {
    const x: u8 = 0;
    try testing.expect(isVolatilePtr(*volatile @TypeOf(x)));
    try testing.expect(isVolatilePtr(*const volatile ?fn (@TypeOf(x)) void));
    try testing.expect(!isVolatilePtr(@TypeOf(&x)));
}

pub inline fn isAllowzeroPtr(comptime T: type) bool {
    if (!is(.Pointer)(T)) return false;
    return @typeInfo(T).Pointer.is_allowzero;
}

test "isAllowzeroPtr" {
    try testing.expect(isAllowzeroPtr(*allowzero const u8));
    try testing.expect(!isAllowzeroPtr(@TypeOf(42)));
}

pub inline fn isContainer(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .Struct, .Union, .Enum, .Opaque => true,
        else => false,
    };
}

test "isContainer" {
    const TestStruct = struct {};
    const TestUnion = union {
        a: void,
    };
    const TestEnum = enum {
        A,
        B,
    };
    const TestOpaque = opaque {};

    try testing.expect(isContainer(TestStruct));
    try testing.expect(isContainer(TestUnion));
    try testing.expect(isContainer(TestEnum));
    try testing.expect(isContainer(TestOpaque));
    try testing.expect(!isContainer(u8));
}

pub inline fn isTuple(comptime T: type) bool {
    return is(.Struct)(T) and @typeInfo(T).Struct.is_tuple;
}

test "isTuple" {
    const t1 = struct {};
    const t2 = .{ .a = 0 };
    const t3 = .{ 1, 2, 3 };
    try testing.expect(!isTuple(t1));
    try testing.expect(!isTuple(@TypeOf(t2)));
    try testing.expect(isTuple(@TypeOf(t3)));
}

/// Returns true if the passed type will coerce to []const u8.
/// Any of the following are considered strings:
/// ```
/// []const u8, [:S]const u8, *const [N]u8, *const [N:S]u8,
/// []u8, [:S]u8, *[:S]u8, *[N:S]u8.
/// ```
/// These types are not considered strings:
/// ```
/// u8, [N]u8, [*]const u8, [*:0]const u8,
/// [*]const [N]u8, []const u16, []const i8,
/// *const u8, ?[]const u8, ?*const [N]u8.
/// ```
pub inline fn isZigString(comptime T: type) bool {
    return blk: {
        // Only pointer types can be strings, no optionals
        const info = @typeInfo(T);
        if (info != .Pointer) break :blk false;

        const ptr = &info.Pointer;
        // Check for CV qualifiers that would prevent coerction to []const u8
        if (ptr.is_volatile or ptr.is_allowzero) break :blk false;

        // If it's already a slice, simple check.
        if (ptr.size == .Slice) {
            break :blk ptr.child == u8;
        }

        // Otherwise check if it's an array type that coerces to slice.
        if (ptr.size == .One) {
            const child = @typeInfo(ptr.child);
            if (child == .Array) {
                const arr = &child.Array;
                break :blk arr.child == u8;
            }
        }

        break :blk false;
    };
}

test "isZigString" {
    try testing.expect(isZigString([]const u8));
    try testing.expect(isZigString([]u8));
    try testing.expect(isZigString([:0]const u8));
    try testing.expect(isZigString([:0]u8));
    try testing.expect(isZigString([:5]const u8));
    try testing.expect(isZigString([:5]u8));
    try testing.expect(isZigString(*const [0]u8));
    try testing.expect(isZigString(*[0]u8));
    try testing.expect(isZigString(*const [0:0]u8));
    try testing.expect(isZigString(*[0:0]u8));
    try testing.expect(isZigString(*const [0:5]u8));
    try testing.expect(isZigString(*[0:5]u8));
    try testing.expect(isZigString(*const [10]u8));
    try testing.expect(isZigString(*[10]u8));
    try testing.expect(isZigString(*const [10:0]u8));
    try testing.expect(isZigString(*[10:0]u8));
    try testing.expect(isZigString(*const [10:5]u8));
    try testing.expect(isZigString(*[10:5]u8));

    try testing.expect(!isZigString(u8));
    try testing.expect(!isZigString([4]u8));
    try testing.expect(!isZigString([4:0]u8));
    try testing.expect(!isZigString([*]const u8));
    try testing.expect(!isZigString([*]const [4]u8));
    try testing.expect(!isZigString([*c]const u8));
    try testing.expect(!isZigString([*c]const [4]u8));
    try testing.expect(!isZigString([*:0]const u8));
    try testing.expect(!isZigString([*:0]const u8));
    try testing.expect(!isZigString(*[]const u8));
    try testing.expect(!isZigString(?[]const u8));
    try testing.expect(!isZigString(?*const [4]u8));
    try testing.expect(!isZigString([]allowzero u8));
    try testing.expect(!isZigString([]volatile u8));
    try testing.expect(!isZigString(*allowzero [4]u8));
    try testing.expect(!isZigString(*volatile [4]u8));
}

pub inline fn hasDecls(comptime T: type, comptime names: anytype) bool {
    inline for (names) |name| {
        if (!@hasDecl(T, name))
            return false;
    }
    return true;
}

test "hasDecls" {
    const TestStruct1 = struct {};
    const TestStruct2 = struct {
        pub var a: u32 = undefined;
        pub var b: u32 = undefined;
        c: bool,
        pub fn useless() void {}
    };

    const tuple = .{ "a", "b", "c" };

    try testing.expect(!hasDecls(TestStruct1, .{"a"}));
    try testing.expect(hasDecls(TestStruct2, .{ "a", "b" }));
    try testing.expect(hasDecls(TestStruct2, .{ "a", "b", "useless" }));
    try testing.expect(!hasDecls(TestStruct2, .{ "a", "b", "c" }));
    try testing.expect(!hasDecls(TestStruct2, tuple));
}

pub inline fn hasFields(comptime T: type, comptime names: anytype) bool {
    inline for (names) |name| {
        if (!@hasField(T, name))
            return false;
    }
    return true;
}

test "hasFields" {
    const TestStruct1 = struct {};
    const TestStruct2 = struct {
        a: u32,
        b: u32,
        c: bool,
        pub fn useless() void {}
    };

    const tuple = .{ "a", "b", "c" };

    try testing.expect(!hasFields(TestStruct1, .{"a"}));
    try testing.expect(hasFields(TestStruct2, .{ "a", "b" }));
    try testing.expect(hasFields(TestStruct2, .{ "a", "b", "c" }));
    try testing.expect(hasFields(TestStruct2, tuple));
    try testing.expect(!hasFields(TestStruct2, .{ "a", "b", "useless" }));
}

pub inline fn hasFunctions(comptime T: type, comptime names: anytype) bool {
    inline for (names) |name| {
        if (!hasFn(name)(T))
            return false;
    }
    return true;
}

test "hasFunctions" {
    const TestStruct1 = struct {};
    const TestStruct2 = struct {
        pub fn a() void {}
        fn b() void {}
    };

    const tuple = .{ "a", "b", "c" };

    try testing.expect(!hasFunctions(TestStruct1, .{"a"}));
    try testing.expect(hasFunctions(TestStruct2, .{ "a", "b" }));
    try testing.expect(!hasFunctions(TestStruct2, .{ "a", "b", "c" }));
    try testing.expect(!hasFunctions(TestStruct2, tuple));
}

/// True if every value of the type `T` has a unique bit pattern representing it.
/// In other words, `T` has no unused bits and no padding.
pub inline fn hasUniqueRepresentation(comptime T: type) bool {
    switch (@typeInfo(T)) {
        else => return false, // TODO can we know if it's true for some of these types ?

        .AnyFrame,
        .Enum,
        .ErrorSet,
        .Fn,
        => return true,

        .Bool => return false,

        .Int => |info| return @sizeOf(T) * 8 == info.bits,

        .Pointer => |info| return info.size != .Slice,

        .Array => |info| return hasUniqueRepresentation(info.child),

        .Struct => |info| {
            var sum_size = @as(usize, 0);

            inline for (info.fields) |field| {
                const FieldType = field.type;
                if (!hasUniqueRepresentation(FieldType)) return false;
                sum_size += @sizeOf(FieldType);
            }

            return @sizeOf(T) == sum_size;
        },

        .Vector => |info| return hasUniqueRepresentation(info.child) and
            @sizeOf(T) == @sizeOf(info.child) * info.len,
    }
}

test "hasUniqueRepresentation" {
    const TestStruct1 = struct {
        a: u32,
        b: u32,
    };

    try testing.expect(hasUniqueRepresentation(TestStruct1));

    const TestStruct2 = struct {
        a: u32,
        b: u16,
    };

    try testing.expect(!hasUniqueRepresentation(TestStruct2));

    const TestStruct3 = struct {
        a: u32,
        b: u32,
    };

    try testing.expect(hasUniqueRepresentation(TestStruct3));

    const TestStruct4 = struct { a: []const u8 };

    try testing.expect(!hasUniqueRepresentation(TestStruct4));

    const TestStruct5 = struct { a: TestStruct4 };

    try testing.expect(!hasUniqueRepresentation(TestStruct5));

    const TestUnion1 = packed union {
        a: u32,
        b: u16,
    };

    try testing.expect(!hasUniqueRepresentation(TestUnion1));

    const TestUnion2 = extern union {
        a: u32,
        b: u16,
    };

    try testing.expect(!hasUniqueRepresentation(TestUnion2));

    const TestUnion3 = union {
        a: u32,
        b: u16,
    };

    try testing.expect(!hasUniqueRepresentation(TestUnion3));

    const TestUnion4 = union(enum) {
        a: u32,
        b: u16,
    };

    try testing.expect(!hasUniqueRepresentation(TestUnion4));

    inline for ([_]type{ i0, u8, i16, u32, i64 }) |T| {
        try testing.expect(hasUniqueRepresentation(T));
    }
    inline for ([_]type{ i1, u9, i17, u33, i24 }) |T| {
        try testing.expect(!hasUniqueRepresentation(T));
    }

    try testing.expect(!hasUniqueRepresentation([]u8));
    try testing.expect(!hasUniqueRepresentation([]const u8));

    try testing.expect(hasUniqueRepresentation(@Vector(4, u16)));
}

/// True if type can be used only in comppile-time
pub inline fn isComptimeOnly(comptime T: type) bool {
    return @typeInfo(@TypeOf(.{_indicateComptime(T)})).Struct.fields[0].is_comptime;
}

fn _indicateComptime(comptime T: type) T {
    return undefined;
}

test "isComptimeOnly" {
    const ComptimeStringMap = std.comptime_string_map.ComptimeStringMap;
    const TestEnum = enum {
        A,
        B,
        C,
        D,
        E,
    };
    const map = ComptimeStringMap(TestEnum, .{
        .{ "these", .D },
        .{ "have", .A },
        .{ "nothing", .B },
        .{ "incommon", .C },
        .{ "samelen", .E },
    });
    const ComptimeType = struct {
        field: type,
    };
    const NonComptimeType = struct {
        field: u8,
    };

    const test_comptime_types = [_]type{ comptime_int, comptime_float, u0, i0, map, fn (comptime_int) void, ComptimeType };
    const test_runtime_types = [_]type{ u1, i1, u8, i8, u16, i16, u29, u32, i32, u64, i64, NonComptimeType, *const fn (u32) callconv(.C) u32 };
    inline for (test_comptime_types) |T|
        try testing.expect(isComptimeOnly(T));
    inline for (test_runtime_types) |T|
        try testing.expect(!isComptimeOnly(T));
}

pub inline fn isExhaustiveEnum(comptime T: type) bool {
    if (!is(.Enum)(T)) return false;
    return @typeInfo(T).Enum.is_exhaustive;
}

test "isExhaustiveEnum" {
    const NonExhaustive = enum(u8) {
        a,
        b,
        c,
        d,
        _,
    };
    const Exhaustive = enum(u64) {
        a,
        b,
        c,
        d,
    };

    try testing.expect(!isExhaustiveEnum(NonExhaustive));
    try testing.expect(isExhaustiveEnum(Exhaustive));
    try testing.expect(!isExhaustiveEnum(u8));
}
