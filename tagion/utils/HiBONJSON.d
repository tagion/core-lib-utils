module tagion.utils.HiBONJSON;

import std.conv : to;
import std.format;

import tagion.utils.HiBONBase : Type
import tagion.utils.Document : Document;


enum NotSupported = "none";
protected string[Type] generateTypeMap() {
    with(Type) {
        string[Type] map=[
        NONE : "none" NotSupported ,
        FLOAT64 : "f64",
        STRING  : "text",
        DOCUMENT : "{}",
        BOOLEAN  : "bool",
        UTC      : "utc",
        INT32    : "i32",
        INT64    : "i64",
        UINT32   : "u32",
        UINT64   : "u64",
        DEFINED_NATIVE : "none",
        BINARY         : "bin",
        INT32_ARRAY    : "i32[]",
        INT64_ARRAY    : "i64[]",
        FLOAT64_ARRAY  : "f64[]",
        FLOAT32_ARRAY  : "f32[]",
        BOOLEAN_ARRAY  : "bool[]",
        UINT32_ARRAY   : "u32[]",
        UINT64_ARRAY   : "u64[]",
        NATIVE_HIBON_ARRAY : NotSupported,
        NATIVE_DOCUMENT_ARRAY : NotSupported ,
        NATIVE_STRING_ARRAY : NotSupported
        ];

        foreach(E; EnumMembers!Type) {
            assert(E in map, format("TypeMap %s is not defined"));
        }
    }
    return map;
}

protected Type[string] generateLabelTypeMap(const(string[Type]) typemap) {
    Type[string] result;
    foreach(e, label; typemap) {
        if (label != NotSupported) {
            result[label]=e;
        }
    }
    return result;
}

enum typeMap=generateTypeMap;
enum labelMap=generateLabelMap(typeMap);

@safe
struct JSONRange(Type E) {
    static assert(!isNative(E), format("Type %s not supported for %s", E, JSONRange.stringof));
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

    struct JSONElement {
        immutable(string) key;
        immutable JSONType jtype;

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

        const(T) get(T)() const pure {
            enum EJSON=asJType!T;
            check(jstype is EJSON, format("Mismatched type %s JSON type expected is %s", T.stringof, jstype));
            switch(EJSON) {
            case JSONType.STRING:
                return value.text;
                break;
            case JSONType.NUMBER:
                return value.number;
                break;
            case JSONType.BOOLENA:
                return value.boolean;
                break;
            default:
                // empty
            }
            assert(0, format("Bad type %s", T.string));
        }

        JSONType type() const pure nothrow {
            return value.jtype;
        }

        auto range()
            in {
                assert((jtype is JSONType.OBJECT) || (jtype is JSONType.ARRAY), "Range is only defined for JSON ARRAY or OBJECT");
            }
        do {
            return 0;
        }
    }
}

unittest {
}
