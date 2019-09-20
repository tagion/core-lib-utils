import tagion.utils.HiBONBase;

import tagion.Types;
import std.meta : AliasSeq; //, Filter;
import std.traits : isBasicType, isSomeString, isIntegral, isNumeric, getUDAs;

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
        FLOAT128        = 0x13, /// Decimal 128bits
        BITINT          = 0x1B,  /// Signed Bigint

        UINT32          = 0x20,  // 32 bit unsigend integer
        FLOAT32         = 0x21,  // 32 bit Float
        UINT64          = 0x22,  // 64 bit unsigned integer
//        HASHDOC         = 0x23,  // Hash point to documement
        UBITINT         = 0x2B,  /// Unsigned Bigint
        TRUNC           = 0x3f,  // Trunc value for the native type

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
        FLOAT128_ARRAY   = DEFINED_ARRAY | FLOAT128,

        /// Native types is only used inside the BSON object
        NATIVE_HIBON_ARRAY    = DEFINED_ARRAY | DOCUMENT, // Represetents (HISON[]) is convert to an ARRAY of DOCUMENT's
        NATIVE_DOCUMENT_ARRAY = DEFINED_ARRAY | NATIVE_DOCUMENT, // Represetents (Document[]) is convert to an ARRAY of DOCUMENT's
        NATIVE_STRING_ARRAY   = DEFINED_ARRAY | STRING, // Represetents (string[]) is convert to an ARRAY of string's
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
        alias MemberSeq=AliasSeq!(MemberT, MemberUDA, name);
        alias ValueSeqBase=AliasSeq!(MemberSeq, ValueSeqBase!(T, Members[1..$]));
    }
}

//alias ValueSeq(T, H) = ValueSeq!(T, __traits(allMembers, T));
alias ValueSeq = ValueSeqBase!(Value, __traits(allMembers, Value));

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

alias ValueType(Type type) = ValueTypeBase!(type, ValueSeq);

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
alias ValueSeqFilter(alias pred)=ValueSeqFilterBase!(pred, ValueSeq);

enum isValueBasicType(TList...) = isBasicType!(TList[0]) && (TList[1] !is Type.UTC);

alias ValueSeqBasicTypes=ValueSeqFilter!(isValueBasicType);

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

alias ValueSeqBinaryTypes=ValueSeqFilter!(isValueBinaryType);

enum isValueIntegralType(TList...) = isIntegral!(TList[0]) && (TList[1] !is Type.UTC);

alias ValueSeqIntegralTypes = ValueSeqFilter!(isValueIntegralType);

enum isValueNumericType(TList...) = isNumeric!(TList[0]) && (TList[1] !is Type.UTC);

alias ValueSeqNumericTypes = ValueSeqFilter!(isValueNumericType);

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


alias TypeEnum(T) = Test[staticIndexOf!(T, ValueSeq)+1];

enum  TypeName(T) = Test[staticIndexOf!(T, ValueSeq)+2];
