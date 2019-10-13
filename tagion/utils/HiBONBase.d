module tagion.utils.HiBONBase;


import tagion.Types;
import tagion.Base : isOneOf;
import tagion.TagionExceptions : Check, TagionException;

import std.format;
import std.meta : AliasSeq; //, Filter;
import std.traits : isBasicType, isSomeString, isIntegral, isNumeric, isType, EnumMembers, Unqual, getUDAs, hasUDA;

import std.system : Endian;
import bin = std.bitmanip;

alias binread(T, R) = bin.read!(T, Endian.littleEndian, R);

void binwrite(T, R, I)(R range, const T value, I index) {
    bin.write!(T, Endian.littleEndian, R)(range, value, index);
}
//alias binwrite(T, R) = bin.write!(T, Endian.littleEndian, R);

/**
 * Exception type used by tagion.utils.BSON module
 */
@safe
class HiBONException : TagionException {
    this(string msg, string file = __FILE__, size_t line = __LINE__ ) {
        super( msg, file, line );
    }
}

alias check=Check!HiBONException;


enum Type : ubyte {
//     MIN             = -1,       /// Special type which compares lower than all other possible BSON element values
    NONE            = 0x00,  /// End Of Document
        FLOAT64         = 0x01,  /// Floating point
        STRING          = 0x02,  /// UTF8 STRING
        DOCUMENT        = 0x03,  /// Embedded document (Both Object and Documents)
        BOOLEAN         = 0x08,  /// Boolean - true or false
        UTC             = 0x09,  /// UTC datetime
        INT32           = 0x10,  /// 32-bit integer
        INT64           = 0x12,  /// 64-bit integer,
        //       FLOAT128        = 0x13, /// Decimal 128bits
        //       BIGINT          = 0x1B,  /// Signed Bigint

        UINT32          = 0x20,  // 32 bit unsigend integer
        FLOAT32         = 0x21,  // 32 bit Float
        UINT64          = 0x22,  // 64 bit unsigned integer
//        HASHDOC         = 0x23,  // Hash point to documement
//        UBIGINT         = 0x2B,  /// Unsigned Bigint
//        TRUNC           = 0x3f,  // Mask for basic values


        DEFINED_NATIVE  = 0x40,
        NATIVE_DOCUMENT = DEFINED_NATIVE | 0x3e, // This type is only used as an internal represention (Document type)

        DEFINED_ARRAY   = 0x80,  // Indicated an Intrinsic array types
        BINARY          = DEFINED_ARRAY | 0x05, // Binary data
        INT32_ARRAY     = DEFINED_ARRAY | INT32,
        INT64_ARRAY     = DEFINED_ARRAY | INT64,
        FLOAT64_ARRAY   = DEFINED_ARRAY | FLOAT64,
        BOOLEAN_ARRAY   = DEFINED_ARRAY | BOOLEAN,
        UINT32_ARRAY    = DEFINED_ARRAY | UINT32,
        UINT64_ARRAY    = DEFINED_ARRAY | UINT64,
        FLOAT32_ARRAY   = DEFINED_ARRAY | FLOAT32,
        //     FLOAT128_ARRAY   = DEFINED_ARRAY | FLOAT128,

        /// Native types is only used inside the BSON object
        NATIVE_HIBON_ARRAY    = DEFINED_ARRAY | DEFINED_NATIVE | DOCUMENT, // Represetents (HISON[]) is convert to an ARRAY of DOCUMENT's
        NATIVE_DOCUMENT_ARRAY = DEFINED_ARRAY | DEFINED_NATIVE | NATIVE_DOCUMENT, // Represetents (Document[]) is convert to an ARRAY of DOCUMENT's
        NATIVE_STRING_ARRAY   = DEFINED_ARRAY | DEFINED_NATIVE | STRING, // Represetents (string[]) is convert to an ARRAY of string's
        }


@safe
bool isNative(Type type) pure nothrow {
    with(Type) {
        return ((type & DEFINED_NATIVE) !is 0);
    }
}

@safe
bool isArray(Type type) pure nothrow {
    with(Type) {
        return ((type & DEFINED_ARRAY) !is 0) && (type !is DEFINED_ARRAY) && (type !is NONE);
    }
}

@safe
bool isHiBONType(Type type) pure nothrow {
    with(Type) {
        switch(type) {
            static foreach(E; EnumMembers!Type) {
            case E:
                pragma(msg, "isHiBONType E=", E, " : ", isNative(E));
                static if (isNative(E) || (E is NONE) || (E is DEFINED_ARRAY)) {
                    return false;
                }
                else {
                    return true;
                }
            }
        default:
            // empty
        }
    }
    return false;
}

/*
static unittest {
    with(Type) {
        assert(isHiBON(
}
*/

//@safe class HiBON;
//@safe struct Document;

enum isBasicValueType(T) = isBasicType!T || is(T : decimal_t);

@safe
union ValueT(bool NATIVE=false, HiBON,  Document) {
    @Type(Type.FLOAT32)   float     float32;
    @Type(Type.FLOAT64)   double    float64;
    // @Type(Type.FLOAT128)  decimal_t float128;
    @Type(Type.STRING)    string    text;
    @Type(Type.BOOLEAN)   bool      boolean;
    //  @Type(Type.LIST)
    static if ( !is(HiBON == void ) ) {
        @Type(Type.DOCUMENT)  HiBON      document;
    }
    // static if ( !is(HiList == void ) ) {
    //     @Type(Type.LIST)  HiList    list;
    // }
    @Type(Type.UTC)       ulong     date;
    @Type(Type.INT32)     int       int32;
    @Type(Type.INT64)     long      int64;
    @Type(Type.UINT32)    uint      uint32;
    @Type(Type.UINT64)    ulong     uint64;
    static if ( !is(Document == void) ) {
        @Type(Type.NATIVE_DOCUMENT) Document    native_document;
    }
    @Type(Type.BINARY)         immutable(ubyte)[]   binary;
    @Type(Type.BOOLEAN_ARRAY)  immutable(bool)[]    boolean_array;
    @Type(Type.INT32_ARRAY)    immutable(int)[]     int32_array;
    @Type(Type.UINT32_ARRAY)   immutable(uint)[]    uint32_array;
    @Type(Type.INT64_ARRAY)    immutable(long)[]    int64_array;
    @Type(Type.UINT64_ARRAY)   immutable(ulong)[]   uint64_array;
    @Type(Type.FLOAT32_ARRAY)  immutable(float)[]   float32_array;
    @Type(Type.FLOAT64_ARRAY)  immutable(double)[]  float64_array;
    // @Type(Type.FLOAT128_ARRAY) immutable(decimal_t)[] float128_array;
    static if ( NATIVE ) {
        @Type(Type.NATIVE_HIBON_ARRAY)    HiBON[]     native_hibon_array;
        @Type(Type.NATIVE_DOCUMENT_ARRAY) Document[]  native_document_array;
        @Type(Type.NATIVE_STRING_ARRAY) string[]    native_string_array;
        //  @Type(Type.NONE) alias NativeValueDataTypes = AliasSeq!(HiBON, HiBON[], Document[]);

    }
    // else {
        alias NativeValueDataTypes = AliasSeq!();
    // }
    protected template GetFunctions(string text, bool first, TList...) {
        static if ( TList.length is 0 ) {
            enum GetFunctions=text~"else {\n    static assert(0, format(\"Not support illegal %s \", type )); \n}";
        }
        else {
            enum name=TList[0];
            enum member_code="alias member=ValueT."~name~";";
            mixin(member_code);
            static if (  __traits(compiles, typeof(member)) && hasUDA!(member, Type) ) {
                enum MemberType=getUDAs!(member, Type)[0];
                alias MemberT=typeof(member);
                static if ( (MemberType is Type.NONE) || ( !NATIVE && isOneOf!(MemberT, NativeValueDataTypes)) ) {
                    enum code="";
                }
                else {
                    enum code = format("%sstatic if ( type is Type.%s ) {\n    return %s;\n}\n",
                        (first)?"":"else ", MemberType, name);
                }
                enum GetFunctions=GetFunctions!(text~code, false, TList[1..$]);
            }
            else {
                enum GetFunctions=GetFunctions!(text, false, TList[1..$]);
            }
        }

    }

    @trusted
    auto get(Type type)() pure const {
        enum code=GetFunctions!("", true, __traits(allMembers, ValueT));
//        pragma(msg, code);
        mixin(code);
        assert(0);
    }


    protected template GetType(T, TList...) {
        static if (TList.length is 0) {
            enum GetType = Type.NONE;
        }
        else {
            enum name = TList[0];
            enum member_code = "alias member=ValueT."~name~";";
            mixin(member_code);
            static if ( __traits(compiles, typeof(member)) && hasUDA!(member, Type) ) {
                enum MemberType=getUDAs!(member, Type)[0];
                alias MemberT=typeof(member);
                static if ( is(T == MemberT) ) {
                    enum GetType = MemberType;
                }
                else {
                    enum GetType = GetType!(T, TList[1..$]);
                }
            }
            else {
                enum GetType = GetType!(T, TList[1..$]);
            }
        }
    }

    enum asType(T) = GetType!(T, __traits(allMembers, ValueT));
    enum hasType(T) = asType!T !is Type.NONE;

    version(none)
    static unittest {
        static assert(hasType!int);
    }

    @trusted
    this(T)(T x) if (isOneOf!(Unqual!T, typeof(this.tupleof)) ) {
        alias MutableT = Unqual!T;
        static foreach(m; __traits(allMembers, ValueT) ) {
            static if ( is(typeof(__traits(getMember, this, m)) == MutableT ) ){
            enum code=format("alias member=ValueT.%s;", m);
            mixin(code);
            static if ( hasUDA!(member, Type ) ) {
                alias MemberT   = typeof(member);
                static if ( is(T == MemberT) ) {
                    __traits(getMember, this, m) = x;
                    return;
                }
            }
        }
        }
        assert (0, format("%s is not supported", T.stringof ) );
    }

    @trusted
    void opAssign(T)(T x) if (isOneOf!(T, typeof(this.tupleof))) {
        static foreach(m; __traits(allMembers, ValueT) ) {
            static if ( is(typeof(__traits(getMember, this, m)) == T ) ){
                enum code="alias member=ValueT."~m~";";
                mixin(code);
                enum MemberType=getUDAs!(member, Type)[0];
                static assert ( MemberType !is Type.NONE, format("%s is not supported", T ) );
                static if ( is(T == struct) && !__traits(compiles, __traits(getMember, this, m) = x) ) {
//                    pragma(msg, "T-> ", T, " : ", typeof(__traits(getMember, this, m)));
                    x.copy(&__traits(getMember, this, m));
//                    __traits(getMember, this, m) = T(x);
                }
                else {
                    __traits(getMember, this, m) = x;
                }
            }
        }
    }

    alias TypeT(Type aType) = typeof(get!aType());

    uint size(Type E)() const pure nothrow {
        pragma(msg, "Size ", E, " : ", isHiBONType(E));
        static if (isHiBONType(E)) {
            alias T = TypeT!E;
            static if ( isBasicValueType!T ) {
                return T.sizeof;
            }
            else static if ( is(T: U[], U) && isBasicValueType!U ) {
                return cast(uint)(get!(E).length * U.sizeof);
            }
            else {
                static assert(0, format("Type %s of %s is not defined", E, T.stringof));
            }
        }
        else {
            static assert(0, format("Illegal type %s", E));
        }
    }

};


unittest {
    import std.stdio;
    ValueT!(false, void, void) value;
    value.int32=10;
    auto x=value.get!(Type.INT32);
    value.float32=13.45;
    auto y=value.get!(Type.FLOAT32);

    value=1;
    value=1.3;

//    char a='x';
    static assert(!__traits(compiles, value='x'));
    //value=a;
}


@safe bool is_index(string a, out uint result) pure {
    import std.conv : to;
    enum MAX_UINT_SIZE=to!string(uint.max).length;
    if ( a.length <= MAX_UINT_SIZE ) {
        if ( a[0] == '0' ) {
            return false;
        }
        foreach(c; a[1..$]) {
            if ( (c < '0') || (c > '9') ) {
                return false;
            }
        }
        immutable number=a.to!ulong;
        if ( number <= uint.max ) {
            result = cast(uint)number;
            return true;
        }
    }
    return false;
}

@safe bool less_than(string a, string b) pure
    in {
        assert(a.length > 0);
        assert(b.length > 0);
    }
body {
    uint a_index;
    uint b_index;
    if ( is_index(a, a_index) && is_index(b, b_index) ) {
        return a_index < b_index;
    }
    return a < b;
}

@safe bool is_key_valid(string a) pure nothrow {
    enum : char {
        SPACE = 0x20,
            DEL = 0x7F,
            DOUBLE_QUOTE = 34,
            QUOTE = 39,
            BACK_QUOTE = 0x60
            }
    if ( a.length > 0 ) {
        foreach(c; a) {
            // Chars between SPACE and DEL is valid
            // except for " ' ` is not valid
            if ( (c <= SPACE) || (c >= DEL) ||
                ( c == DOUBLE_QUOTE ) || ( c == QUOTE ) ||
                ( c == BACK_QUOTE ) ) {
                return false;
            }
        }
        return true;
    }
    return false;
}

unittest {
    assert(less_than("abe", "bob"));
    assert(less_than("0", "abe"));
    assert(less_than("0", "1"));
    assert(!less_than("00", "0"));
}
