module tagion.utils.HiBON2JSON;

import std.conv : to;
import std.format;
import std.traits : EnumMembers;
import std.range.primitives : isInputRange;

import tagion.utils.HiBONBase : Type, isNative, isHiBONType, HiBONException;
import tagion.utils.Document : Document;
import tagion.utils.JSONOutStream;
import tagion.utils.JSONInStream : JSONType;

import tagion.TagionExceptions : Check;

/**
 * Exception type used by tagion.utils.BSON module
 */
@safe
class HiBON2JSONException : HiBONException {
    this(string msg, string file = __FILE__, size_t line = __LINE__ ) {
        super( msg, file, line );
    }
}

alias check=Check!HiBON2JSONException;

enum NotSupported = "none";

protected Type[string] generateLabelMap(const(string[Type]) typemap) {
    Type[string] result;
    foreach(e, label; typemap) {
        if (label != NotSupported) {
            result[label]=e;
        }
    }
    return result;
}

enum typeMap=[
        Type.NONE     : NotSupported,
        Type.FLOAT32  : "f32",
        Type.FLOAT64  : "f64",
        Type.STRING   : "text",
        Type.DOCUMENT : "{}",
        Type.BOOLEAN  : "bool",
        Type.UTC      : "utc",
        Type.INT32    : "i32",
        Type.INT64    : "i64",
        Type.UINT32   : "u32",
        Type.UINT64   : "u64",
        Type.DEFINED_NATIVE : NotSupported,
        Type.BINARY         : "bin",
        Type.INT32_ARRAY    : "i32[]",
        Type.INT64_ARRAY    : "i64[]",
        Type.FLOAT64_ARRAY  : "f64[]",
        Type.FLOAT32_ARRAY  : "f32[]",
        Type.BOOLEAN_ARRAY  : "bool[]",
        Type.UINT32_ARRAY   : "u32[]",
        Type.UINT64_ARRAY   : "u64[]",
        Type.DEFINED_ARRAY         : NotSupported,
        Type.NATIVE_DOCUMENT       : NotSupported ,
        Type.NATIVE_HIBON_ARRAY    : NotSupported,
        Type.NATIVE_DOCUMENT_ARRAY : NotSupported ,
        Type.NATIVE_STRING_ARRAY   : NotSupported
        ];

unittest {
    static foreach(E; EnumMembers!Type) {
         assert(E in typeMap, format("TypeMap %s is not defined", E));
    }
}
//    generateTypeMap;
enum labelMap=generateLabelMap(typeMap);

@safe
struct JSONElement {
    immutable(string) key;
    const JSONType jtype;

    union JValue {
        string text;
        double number;
        bool   boolean;
    }
    immutable JValue value;

    this(T)(string key, T x) {
        this.key=key;
        static if (is(T == string)) {
            jtype=JSONType.STRING;
            this.value.text=x;
        }
        else static if (is(T : double)) {
            jtype=JSONType.NUMBER;
            this.value.number=x;
        }
        else static if (is(T == bool)) {
            jtype=JSONType.BOOLEAN;
            this.value.boolean=x;
        }
        else {
            static assert(0, format("Type %s not supported by JSON", JType.stringof));
        }
    }

    @trusted
    const(T) get(T)() const {
        enum EJType=asJType!T;
        check(jtype is EJType, format("Mismatched type %s JSON type expected is %s", T.stringof, jtype));
        static if (EJType is JSONType.STRING) {
            return value.text;
        }
        else static if (EJType is JSONType.NUMBER) {
            return value.number;
        }
        else static if (EJType is JSONType.BOOLEAN) {
            return value.boolean;
        }
        assert(0, format("Bad type %s", T.stringof));
    }

    const(JSONType) type() const pure nothrow {
        return jtype;
    }

    JSONElement range()
        in {
            assert((jtype is JSONType.OBJECT) || (jtype is JSONType.ARRAY), "Range is only defined for JSON ARRAY or OBJECT");
        }
    do {
        assert(0, "Not implemented");
    }
}

@safe
struct JSONRangeE(Type E) if ((E !is Type.DOCUMENT) && !isNative(E)) {
//    static assert(!isNative(E), format("Type %s not supported for %s", E, JSONRange.stringof));
    alias ElementT=Document.Value.TypeT!E;
    const Document.Element element;
    enum embeddedArray = isArray(E) && (E !is Type.BINARY);
    immutable string EOL;
    immutable string indent;
    protected uint state;
    enum TYPE_FLAG="$type";
    this(const(Document.Element) element, string indent="  ", string EOL="\n") {
        this.element=element;
        this.indent=indent;
        this.EOL=EOL;
    }

    const(JSONElement) front() const pure nothrow {
        if ( state is 0 ) {
            return JSONElement(TYPE, labelMap[element.type]);
        }
        else {
            static if (embeddedArray) {
                immutable index=state-1;

            }
            else if ( state is 1 ) {
                enum EJSON=asJType!ElementT;
                return JSONElement(VALUE, convert(element.get!ElementT));
            }
        }
    }

    static auto convert(T)(T x) pure {
        alias UnqualT=Unqual!T;
        static if (is(UnqualT == bool)) {
            return x;
        }
        else static if(is(UnqualT == string)) {
            return x;
        }
        else static if(is(UnqualT == ulong) || is(UnqualT == ulong)) {
            return format("0x%X", x);
        }
        else static if(is(UnqualT == long)) {
            return format("0x%X", x);
        }
        else static if(is(T : immutable(ubyte)[])) {
            return format("0x%s", x.toHex);
        }
        else static if(is(T : double)) {
            return cast(double)x;
        }
        else {
            static assert(0, format("Unsuported type %s", T.stringof));
        }
    }

    bool empty() const pure nothrow {
        static if (embeddedArray) {
            return state > 2+element.get!ElementT.length;
        }
        else {
            return state > 1;
        }
    }

    void popFront() {
        state++;
    }

    /*
    JSONElement range() {
        assert(0, "Not implemented");
    }
    */
}

struct JSONRangeE(Type E) if (E is Type.DOCUMENT) {
    private Document.Range doc_range;
    immutable string EOL;
    immutable string indent;
    this(const Document doc) {
        doc_range=doc[];
    }

    bool empty() const pure nothrow {
        return doc_range.empty;
    }

    JSONElement front() const pure nothrow {
        const e=doc_range.front;
        with(Type) {
            switch(e.type) {
                static foreach(MemberE; EnumMembers!Type) {
                case MemberE:
                    static if(isHiBONType(E)) {
                        static if (E is Type.DOCUMENT) {
                            assert(0, format("%s is not implemented yet"));
                        }
                        else {
                            return JSONElement(e.key, e.by!MemberE);
                        }
                    }
                    goto default;
                }
            default:
                // empty
            }
        }
        assert(0, format("Invalid HiBON type %s", e.type));
    }

    void popFront() {
        doc_range.popFront;
    }

    JSONElement range() {
        assert(0, "Not implemented");
    }
}

auto JSONRange(T)(T x) {
    alias E=Document.Value.asType!T;
    pragma(msg, E.stringof, " : ", T, " : ", Document);
    static if(is(T : const Document)) {
        return JSONRangeE!E(x);
    }
    /*
    else {
        return JSONRangeE!E(x[]);
    }
    */
}

unittest {
    import tagion.utils.HiBON : HiBON;

    auto hibon=new HiBON;
    hibon["x"]=int(-42);

    immutable data=hibon.serialize;
    const doc=Document(data);

    auto range=JSONRange(doc);

    pragma(msg, "isInputRange ", isInputRange!(JSONRangeE!(Type.DOCUMENT)));
//    JSONOutStream(range);
}
