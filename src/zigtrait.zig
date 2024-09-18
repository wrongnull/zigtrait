const std = @import("std");
const mem = std.mem;
const debug = std.debug;
const testing = std.testing;

const StructField = std.builtin.Type.StructField;
const Child = std.meta.Child;

pub const TraitFn = fn (type) callconv(.Inline) bool;

pub const PredicatePolicy = enum {
    all,
    any,
};

fn getStructFields(comptime T: type) []const StructField {
    return @typeInfo(T).@"struct".fields;
}

pub fn multiTrait(comptime policy: PredicatePolicy, comptime traits: anytype) TraitFn {
    checkTraitsTuple(@TypeOf(traits));
    const Closure = struct {
        pub inline fn trait(comptime T: type) bool {
            // TODO: Decide if this call is necessary here
            //@setEvalBranchQuota(traits.len + 2);
            comptime var accumulator = 0;
            inline for (traits) |t|
                accumulator += @intCast(@intFromBool(comptime t(T)));
            return switch (policy) {
                .all => accumulator == traits.len,
                .any => accumulator > 0,
            };
        }
    };
    return Closure.trait;
}

inline fn checkTraitsTuple(comptime T: type) void {
    if (!isTuple(T))
        @compileError("multiTrait: expected tuple, found " ++ @typeName(T));
    const fields = getStructFields(T);
    inline for (fields) |field|
        if (field.type != TraitFn)
            @compileError("multiTrait: expected tuple of " ++ @typeName(TraitFn) ++ " found " ++ @typeName(T));
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

    const isVector = multiTrait(.all, .{
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
            return is(.@"fn")(@TypeOf(@field(T, name)));
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
                inline .@"struct", .@"union", .@"enum" => |c| c.fields,
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
    try testing.expect(is(.int)(u8));
    try testing.expect(!is(.int)(f32));
    try testing.expect(is(.pointer)(*u8));
    try testing.expect(is(.void)(void));
    try testing.expect(!is(.optional)(anyerror));
}

pub fn isPtrTo(comptime id: std.builtin.TypeId) TraitFn {
    const Closure = struct {
        pub inline fn trait(comptime T: type) bool {
            return ptrOfSize(.One)(T) and is(id)(Child(T));
        }
    };
    return Closure.trait;
}

test "isPtrTo" {
    try testing.expect(!isPtrTo(.@"struct")(struct {}));
    try testing.expect(isPtrTo(.@"struct")(*struct {}));
    try testing.expect(!isPtrTo(.@"struct")(**struct {}));
}

pub fn isSliceOf(comptime id: std.builtin.TypeId) TraitFn {
    const Closure = struct {
        pub inline fn trait(comptime T: type) bool {
            return ptrOfSize(.Slice)(T) and is(id)(Child(T));
        }
    };
    return Closure.trait;
}

test "isSliceOf" {
    try testing.expect(!isSliceOf(.@"struct")(struct {}));
    try testing.expect(isSliceOf(.@"struct")([]struct {}));
    try testing.expect(!isSliceOf(.@"struct")([][]struct {}));
}

pub fn containerOfLayout(comptime container_layuout: std.builtin.Type.ContainerLayout) TraitFn {
    const Closure = struct {
        pub inline fn trait(comptime T: type) bool {
            return switch (@typeInfo(T)) {
                inline .@"struct", .@"union" => |container| container.layout == container_layuout,
                else => false,
            };
        }
    };
    return Closure.trait;
}

test "conainerOfLayout" {
    const isExtern = containerOfLayout(.@"extern");
    const isPacked = containerOfLayout(.@"packed");

    const TestExStruct = extern struct {};
    const TestPStruct = packed struct {};
    const TestStruct = struct {};

    try testing.expect(isExtern(TestExStruct));
    try testing.expect(!isExtern(TestStruct));
    try testing.expect(!isExtern(u8));

    try testing.expect(isPacked(TestPStruct));
    try testing.expect(!isPacked(TestStruct));
    try testing.expect(!isPacked(u8));
}

pub fn intOfSignedness(comptime int_signedness: std.builtin.Signedness) TraitFn {
    const Closure = struct {
        pub inline fn trait(comptime T: type) bool {
            return switch (@typeInfo(T)) {
                .int => |i| i.signedness == int_signedness,
                .comptime_int => int_signedness == .signed,
                else => false,
            };
        }
    };
    return Closure.trait;
}

test "intOfSignedness" {
    const isUnsignedInt = intOfSignedness(.unsigned);
    const isSignedInt = intOfSignedness(.signed);

    try testing.expect(isUnsignedInt(u32) == true);
    try testing.expect(isUnsignedInt(comptime_int) == false);
    try testing.expect(isUnsignedInt(i64) == false);
    try testing.expect(isUnsignedInt(f64) == false);

    try testing.expect(isSignedInt(u32) == false);
    try testing.expect(isSignedInt(comptime_int) == true);
    try testing.expect(isSignedInt(i64) == true);
    try testing.expect(isSignedInt(f64) == false);
}

pub fn ptrOfSize(comptime ptr_size: std.builtin.Type.Pointer.Size) TraitFn {
    const Closure = struct {
        pub inline fn trait(comptime T: type) bool {
            return is(.pointer)(T) and @typeInfo(T).pointer.size == ptr_size;
        }
    };
    return Closure.trait;
}

test "isSingleItemPtr" {
    const isSingleItemPtr = ptrOfSize(.One);
    const isManyItemPtr = ptrOfSize(.Many);
    const isSlice = ptrOfSize(.Slice);

    const array = [_]u8{0} ** 10;
    var runtime_zero: usize = 0;
    _ = &runtime_zero;

    try testing.expect(isSingleItemPtr(@TypeOf(&array[0])));
    try testing.expect(!isSingleItemPtr(@TypeOf(array)));
    try testing.expect(!isSingleItemPtr(@TypeOf(array[runtime_zero..1])));

    const mip = @as([*]const u8, @ptrCast(&array[0]));
    try testing.expect(isManyItemPtr(@TypeOf(mip)));
    try testing.expect(!isManyItemPtr(@TypeOf(array)));
    try testing.expect(!isManyItemPtr(@TypeOf(array[0..1])));

    try testing.expect(isSlice(@TypeOf(array[runtime_zero..])));
    try testing.expect(!isSlice(@TypeOf(array)));
    try testing.expect(!isSlice(@TypeOf(&array[0])));
}

pub inline fn isIndexable(comptime T: type) bool {
    if (is(.pointer)(T)) {
        if (ptrOfSize(.One)(T))
            return is(.array)(Child(T));
        return true;
    }
    return multiTrait(.any, .{
        is(.array),
        is(.vector),
        isTuple,
    })(T);
}

test "isIndexable" {
    const array = [_]u8{0} ** 10;
    const slice = @as([]const u8, &array);
    const vector: @Vector(2, u32) = [_]u32{0} ** 2;
    const tuple = .{ 1, 2, 3 };

    try testing.expect(isIndexable(@TypeOf(array)));
    try testing.expect(isIndexable(@TypeOf(&array)));
    try testing.expect(isIndexable(@TypeOf(slice)));
    try testing.expect(!isIndexable(Child(@TypeOf(slice))));
    try testing.expect(isIndexable(@TypeOf(vector)));
    try testing.expect(isIndexable(@TypeOf(tuple)));
}

pub inline fn isNumber(comptime T: type) bool {
    return multiTrait(.any, .{
        is(.int),
        is(.comptime_int),
        is(.float),
        is(.comptime_float),
    })(T);
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
    return multiTrait(.any, .{
        is(.int),
        is(.comptime_int),
    })(T);
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
    return multiTrait(.any, .{
        is(.float),
        is(.comptime_float),
    })(T);
}

test "isFloat" {
    try testing.expect(!isFloat(u32));
    try testing.expect(isFloat(f32));
    try testing.expect(!isFloat(@TypeOf(102)));
    try testing.expect(isFloat(@TypeOf(102.123)));
    try testing.expect(!isFloat(*f64));
    try testing.expect(!isFloat([]f32));
}

pub const PointerQualifiers = enum {
    @"const",
    @"allowzero",
    @"volatile",
};

pub fn ptrQualifiedWith(comptime qualifier: PointerQualifiers) TraitFn {
    const Closure = struct {
        pub inline fn trait(comptime T: type) bool {
            return is(.pointer)(T) and @field(@typeInfo(T).pointer, "is_" ++ @tagName(qualifier));
        }
    };
    return Closure.trait;
}

test "ptrQualifiedWith" {
    const isConstPtr = ptrQualifiedWith(.@"const");
    const isAllowzeroPtr = ptrQualifiedWith(.@"allowzero");
    const isVolatilePtr = ptrQualifiedWith(.@"volatile");

    var t: u8 = 0;
    t = t;
    const c: u8 = 0;

    try testing.expect(isConstPtr(@TypeOf(&c)));
    try testing.expect(isConstPtr(*const @TypeOf(t)));
    try testing.expect(isConstPtr(@TypeOf(&c)));
    try testing.expect(!isConstPtr(*@TypeOf(t)));
    try testing.expect(!isConstPtr(@TypeOf(6)));

    try testing.expect(isVolatilePtr(*volatile @TypeOf(t)));
    try testing.expect(isVolatilePtr(*const volatile ?fn (@TypeOf(t)) void));
    try testing.expect(!isVolatilePtr(@TypeOf(&t)));

    try testing.expect(isAllowzeroPtr(*allowzero const u8));
    try testing.expect(!isAllowzeroPtr(@TypeOf(42)));
}

pub inline fn isContainer(comptime T: type) bool {
    return multiTrait(.any, .{
        is(.@"struct"),
        is(.@"union"),
        is(.@"enum"),
        is(.@"opaque"),
    })(T);
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
    return is(.@"struct")(T) and @typeInfo(T).@"struct".is_tuple;
}

test "isTuple" {
    const t1 = struct {};
    const t2 = .{ .a = 0 };
    const t3 = .{ 1, 2, 3 };
    try testing.expect(!isTuple(t1));
    try testing.expect(!isTuple(@TypeOf(t2)));
    try testing.expect(isTuple(@TypeOf(t3)));
}

/// True if every value of the type `T` has a unique bit pattern representing it.
/// In other words, `T` has no unused bits and no padding.
pub inline fn hasUniqueRepresentation(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        else => false, // TODO can we know if it's true for some of these types ?

        .@"anyframe",
        .@"enum",
        .error_set,
        .@"fn",
        => true,

        .bool => false,

        .int => @sizeOf(T) * @bitSizeOf(c_char) == @bitSizeOf(T),

        .pointer => !ptrOfSize(.Slice)(T),

        .array => hasUniqueRepresentation(Child(T)),

        .@"struct" => |info| blk: {
            var sum_size: usize = 0;

            inline for (info.fields) |field| {
                const FieldType = field.type;
                if (!hasUniqueRepresentation(FieldType)) return false;
                sum_size += @sizeOf(FieldType);
            }

            break :blk @sizeOf(T) == sum_size;
        },

        .vector => |info| hasUniqueRepresentation(info.child) and
            @sizeOf(T) == @sizeOf(info.child) * info.len,
    };
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

/// True if value of such type can be used only in comppile-time
pub inline fn isComptimeOnly(comptime T: type) bool {
    return @typeInfo(@TypeOf(.{_indicateComptime(T)})).@"struct".fields[0].is_comptime;
}

fn _indicateComptime(comptime T: type) T {
    return undefined;
}

test "isComptimeOnly" {
    const ComptimeType = struct {
        field: type,
    };
    const NonComptimeType = struct {
        field: u8,
    };

    const test_comptime_types = [_]type{ comptime_int, comptime_float, u0, i0, fn (comptime_int) void, ComptimeType };
    const test_runtime_types = [_]type{ u1, i1, u8, i8, u16, i16, u29, u32, i32, u64, i64, NonComptimeType, *const fn (u32) callconv(.C) u32 };
    inline for (test_comptime_types) |T|
        try testing.expect(isComptimeOnly(T));
    inline for (test_runtime_types) |T|
        try testing.expect(!isComptimeOnly(T));
}

pub inline fn isExhaustiveEnum(comptime T: type) bool {
    return is(.@"enum")(T) and @typeInfo(T).@"enum".is_exhaustive;
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

pub const FunctionProperties = enum {
    generic,
    var_args,
};

pub fn functionIs(comptime property: FunctionProperties) TraitFn {
    const Closure = struct {
        pub inline fn trait(comptime T: type) bool {
            if (ptrOfSize(.One)(T) and is(.@"fn")(Child(T)))
                return trait(Child(T));
            return is(.@"fn")(T) and @field(@typeInfo(T).@"fn", "is_" ++ @tagName(property));
        }
    };
    return Closure.trait;
}

test "isGeneicFunction" {
    const isGenericFunction = functionIs(.generic);

    const generic_function_types = [_]type{
        fn (anytype) void,
        // wait for async implemetation in the self-hosted compiler
        //fn (anyframe) u8,
        fn () type,
    };
    const non_generic_function_types = [_]type{
        fn (u8) callconv(.Naked) void,
        fn () void,
        fn (u29) u111,
        fn (c_int) c_longdouble,
    };
    inline for (generic_function_types) |T| {
        try testing.expect(isGenericFunction(T));
        try testing.expect(isGenericFunction(*const T));
    }
    inline for (non_generic_function_types) |T| {
        try testing.expect(!isGenericFunction(T));
        try testing.expect(!isGenericFunction(*const T));
    }
}

pub fn tupleOfLength(comptime length: usize) TraitFn {
    const Closure = struct {
        pub inline fn trait(comptime T: type) bool {
            return isTuple(T) and @typeInfo(T).@"struct".fields.len == length;
        }
    };
    return Closure.trait;
}

test "tupleOfLength" {
    try testing.expect(tupleOfLength(0)(@TypeOf(.{})));
    try testing.expect(tupleOfLength(3)(@TypeOf(.{ 1, 2, 3 })));
    try testing.expect(!tupleOfLength(42)(*const @TypeOf(.{})));
    try testing.expect(!tupleOfLength(0)(u8));
}

pub fn structOfBackingInt(comptime IntTy: type) TraitFn {
    const Closure = struct {
        pub inline fn trait(comptime T: type) bool {
            return is(.@"struct")(T) and containerOfLayout(.@"packed")(T) and @typeInfo(T).@"struct".backing_integer.? == IntTy;
        }
    };
    return Closure.trait;
}

test "structOfBackingInt" {
    const S = packed struct(u8) {
        dummy: u8,
    };
    const V = packed struct {};
    const O = u8;
    const f = structOfBackingInt(u8);
    try testing.expect(f(S));
    try testing.expect(!f(V));
    try testing.expect(!f(O));
}

pub inline fn isSentinelTerminated(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        inline .array, .pointer => |ty| ty.sentinel != null,
        else => false,
    };
}

test "isSentinelTerminated" {
    try testing.expect(isSentinelTerminated([:0]const u8));
    try testing.expect(isSentinelTerminated([1:0]u8));
    try testing.expect(!isSentinelTerminated(anyerror!u8));
}
