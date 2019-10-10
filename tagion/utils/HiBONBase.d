module tagion.utils.HiBONBase;


import tagion.Types;
import tagion.Base : isOneOf;
import tagion.TagionExceptions : Check, TagionException;

import std.format;
import std.meta : AliasSeq; //, Filter;
import std.traits : isBasicType, isSomeString, isIntegral, isNumeric, isType, getUDAs, hasUDA;

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
        LIST            = 0x04,  /// Array List
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
//        TRUNC           = 0x3f,  // Trunc value for the native type

        NATIVE_DOCUMENT = 0x40 | DOCUMENT, // This type is only used as an internal represention (Document type)

        DEFINED_ARRAY   = 0x80,  // Indicated an Intrinsic array types
        BINARY          = DEFINED_ARRAY | 0x05, // Binary data
        INT32_ARRAY     = DEFINED_ARRAY | INT32,
        INT64_ARRAY     = DEFINED_ARRAY | INT64,
        FLOAT64_ARRAY   = DEFINED_ARRAY | FLOAT64,
        BOOLEAN_ARRAY   = DEFINED_ARRAY | BOOLEAN,
        UINT32_ARRAY    = DEFINED_ARRAY | UINT32,
        UINT64_ARRAY    = DEFINED_ARRAY | UINT64,
        FLOAT32_ARRAY    = DEFINED_ARRAY | FLOAT32,
        //     FLOAT128_ARRAY   = DEFINED_ARRAY | FLOAT128,

        /// Native types is only used inside the BSON object
        NATIVE_HIBON_ARRAY    = DEFINED_ARRAY | DOCUMENT, // Represetents (HISON[]) is convert to an ARRAY of DOCUMENT's
        NATIVE_DOCUMENT_ARRAY = DEFINED_ARRAY | NATIVE_DOCUMENT, // Represetents (Document[]) is convert to an ARRAY of DOCUMENT's
        NATIVE_STRING_ARRAY   = DEFINED_ARRAY | STRING, // Represetents (string[]) is convert to an ARRAY of string's
        }


@safe class HiBON;
//@safe struct Document;

enum isBasicValueType(T) = isBasicType!T || is(T : decimal_t);

@safe
union ValueT(bool NATIVE=false, HiBON,  HiList, Document) {
    @Type(Type.FLOAT32)   float     float32;
    @Type(Type.FLOAT64)   double    float64;
    // @Type(Type.FLOAT128)  decimal_t float128;
    @Type(Type.STRING)    string    text;
    @Type(Type.BOOLEAN)   bool      boolean;
    //  @Type(Type.LIST)
    static if ( !is(HiBON == void ) ) {
        @Type(Type.DOCUMENT)  HiBON      document;
    }
    static if ( !is(HiList == void ) ) {
        @Type(Type.LIST)  HiList    list;
    }
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
            // pragma(msg, member_code, __traits(compiles, typeof(member)));

            static if (  __traits(compiles, typeof(member)) && hasUDA!(member, Type) ) {

            //alias member = mixin("Value."~name);
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
                    pragma(msg, ":", MemberType.stringof, " ", MemberT, " ", typeof(MemberType));
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

    this(T)(T x) {
        pragma(msg, typeof(this.tupleof));
        pragma(msg, "*** ", T, " * ", isOneOf!(T, typeof(this.tupleof)));
//        static assert(isOneOf!(T,
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
                    pragma(msg, "T-> ", T, " : ", typeof(__traits(getMember, this, m)));
                    x.copy(&__traits(getMember, this, m));
//                    __traits(getMember, this, m) = T(x);
                }
                else {
                    __traits(getMember, this, m) = x;
                }
            }
        }
    }

    alias type(Type aType) = typeof(get!aType());

    uint size(Type E)() const pure nothrow {
        alias T = type!E;
//        static assert(!isOneOf(T, NativeType), format("Type of %s is not defined", T.stringof));
        // static if ( isValueBasicType!T ) {
        //     return T.sizeof;
        // }
        // else
        // static if ( aType is Type.UTC ) {
        //     return T.sizeof;
        // }
        // else
        pragma(msg, format("Type %s %s is %s", T.stringof, E, isBasicValueType!(T).stringof));
//        pragma(msg, format("Type %s %s is", T.stringof, aType, isBasicType!T));
//        pragma(msg, format("Type %s %s is %s", T, aType, isBasicType!T));
//        pragma(msg, format("Type %s", T.stringof));
        static if ( isBasicValueType!T ) {
            return T.sizeof;
        }
        // else static if ( isSomeString!T ) {
        //     return text.length;
        // }
        else static if ( is(T: U[], U) && isBasicValueType!U ) {
            return cast(uint)(get!(E).length * U.sizeof);
        }
        else {
            static assert(0, format("Type %s of %s is not defined", E, T.stringof));
        }
    }

};


unittest {
    import std.stdio;
    ValueT!(false, void, void, void) value;
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

template ValueSeqBase(T, Members...) {
    static if ( Members.length == 0 ) {
        alias ValueSeqBase=AliasSeq!();
    }
    else {
        enum name=Members[0];
        enum code="alias member=T."~name~";";
        mixin(code);
        alias MemberT=typeof(member);
        enum MemberUDA=getUDAs!(member, Type)[0];
        static if ( Type.NONE == MemberUDA ) {
            alias MemberSeq=AliasSeq!();
        }
        else {
            alias MemberSeq=AliasSeq!(MemberT, MemberUDA, name);
        }
        alias ValueSeqBase=AliasSeq!(MemberSeq, ValueSeqBase!(T, Members[1..$]));
    }
}

//alias ValueSeq(T, H) = ValueSeq!(T, __traits(allMembers, T));
//alias ValueSeq(V) = ValueSeqBase!(V, __traits(allMembers, V));

template ValueTypeBase(Type type, Seq...) {
//    static assert(Seg.length == 0, format("Type %s not supported", type));
    static if ( Seq.length == 0 ) {
        alias ValueTypeBase = void;
    }
    else static if ( type == Seq[1] ) {
//        pragma(msg, ":: ", Seq[1], type);
        alias ValueTypeBase = Seq[0];
    }
    else {
        alias ValueTypeBase = ValueTypeBase!(type, Seq[3..$]);
    }
}

alias ValueType(Type type, VSeq...) = ValueTypeBase!(type, VSeq);

template ValueSeqFilterBase(alias pred, Seq...) {
    static if (Seq.length == 0) {
        alias ValueSeqFilterBase=AliasSeq!();
    }
    else {
        static if (__traits(compiles, pred!(Seq[0..3])) ) {
            static if (pred!(Seq[0..3])) {
                alias taken=AliasSeq!(Seq[0..3]);
            }
            else {
                alias taken=AliasSeq!();
            }
        }
        else {
            static if (pred!(Seq[0])) {
                alias taken=AliasSeq!(Seq[0..3]);
            }
            else {
                alias taken=AliasSeq!();
            }
        }
        alias ValueSeqFilterBase=AliasSeq!(taken, ValueSeqFilterBase!(pred, Seq[3..$]));
    }
}

// HBSON DType sequency Filter
alias ValueSeqFilter(VSeq, alias pred)=ValueSeqFilterBase!(pred, VSeq);

enum CheckValueBasicType(TList...) = isBasicType!(TList[0]) && (TList[1] !is Type.UTC);

alias ValueSeqBasicTypes(VSeq)=ValueSeqFilter!(VSeq, CheckValueBasicType);

template isValueBinaryType(T) {
    static if ( is(T:immutable(decimal_t)[]) ) {
        enum isValueBinaryType = true;
    }
    else static if ( is(T: U[], U) && !isSomeString!T ) {
        enum isValueBinaryType = isBasicType!U;
    }
    else {
        enum isValueBinaryType = false;
    }
}

alias ValueSeqBinaryTypes(V)=ValueSeqFilter!(V, isValueBinaryType);

enum CheckValueIntegralType(TList...) = isIntegral!(TList[0]) && (TList[1] !is Type.UTC);

alias ValueSeqIntegralTypes(V) = ValueSeqFilter!(V, CheckValueIntegralType);

enum CheckValueNumericType(TList...) = isNumeric!(TList[0]) && (TList[1] !is Type.UTC);

alias ValueSeqNumericTypes(V) = ValueSeqFilter!(V, CheckValueNumericType);

template DTypes(Seq...) {
    static if ( Seq.length == 0 ) {
        alias ValueDTypes = AliasSeq!();
    }
    else {
        alias ValueDTypes = AliasSeq!(T[0], ValueDTypes!(Seq[3..$]));
    }
}

template HiBONTypes(Seq...) {
    static if ( Seq.length == 0 ) {
        enum HiBONTypes = AliasSeq!();
    }
    else {
        pragma(msg, Seq[1]);
        enum HiBONTypes = AliasSeq!(Seq[1], HiBONTypes!(Seq[3..$]));
    }
}


//alias TypeEnum(T, VSeg...) = VSeq[staticIndexOf!(T, VSeq)+1];

//enum  TypeName(T, VSeq...) = VSeq[staticIndexOf!(T, VSeq)+2];

//alias NativeValueDataTypes = AliasSeq!(HiBON, HiBON[], Document[]);

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
