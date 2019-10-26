module tagion.utils.HiBON2JSON;

import std.stdio;

import std.json;
import std.conv : to;
import std.format;
import std.traits : EnumMembers, Unqual, ReturnType, ForeachType;
import std.range.primitives : isInputRange;

import tagion.utils.HiBONBase : Type, isNative, isArray, isHiBONType, HiBONException;
import tagion.utils.Document : Document;
import tagion.utils.JSONOutStream;
import tagion.utils.JSONInStream : JSONType;

import tagion.TagionExceptions : Check;
import tagion.utils.Miscellaneous : toHex=toHexString;

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
class JSONElement {
    immutable(string) key;
    immutable JSONType jtype;
    this(string key, JSONType jtype) {
        this.key=key;
        this.jtype=jtype;
    }

    // @trusted const(T) get(T)() pure const {
    //     assert(0, "Not implemented");
    // }

    // @trusted const(T==string) get(T)() pure const {
    //     assert(0, "Not implemented");
    // }

    // @trusted const(T==bool) get(T)() pure const {
    //     assert(0, "Not implemented");
    // }

    @trusted const(T) get(T)() pure const{
//        return cast(T)null;
        assert(0, "Not implemented");
    }

    static unittest {
        pragma(msg, "ReturnType=", ReturnType!(typeof(JSONElement.get!bool)));
//        static assert(is(ReturnType!(typeof(JSONELement.get!bool)) == bool));
    }
    // static this() {
    //     a
    //     // auto dummy=
    //     // get!double;
    //     // get!bool;
    //     // get!string;
    // }
//    assert(0, "Not implemented");

    @property bool empty() const pure nothrow {
        assert(0, "Not implemented");
    }

    @property void popFront() {
        assert(0, "Not implemented");
    }

    @property const(JSONElement) front() const {
        assert(0, "Not implemented");
    }

    version(none)
    JSONElement opSlice() {
        assert(0, "Not implemented");
    }
}


enum {
    TYPE="$T",
    VALUE="$V",
}

// JSONValue toJSON(T)(const T e) if(!is(T==Document) && is(T:U[])) {
//     alias E=Document.Value.asType!T;
//     JSONValue result;
//     foreach(v; e[]) {
//         result.
//     }

//     with(Type) {
//     CaseType:
//         switch(E) {

//         }
//     }
// }

JSONValue toJSON(const Document doc) {
    JSONValue result;
    immutable isarray=doc.isArray;
    foreach(e; doc[]) {
        with(Type) {
        CaseType:
            switch(e.type) {
                static foreach(E; EnumMembers!Type) {
                    static if (isHiBONType(E)) {
                    case E:
                        JSONValue doc_element;
                        doc_element[TYPE]=JSONValue(typeMap[E]);
                        static if (E is Type.DOCUMENT) {
                            const sub_doc=e.by!E;
                            doc_element[VALUE]=toJSON(sub_doc);
                        }
                        else static if (E is Type.UTC) {
                            assert(0, format("%s is not implemented yet", E));
                        }
                        else static if (isArray(E) && (E !is Type.BINARY)) {
                            alias T=Document.Value.TypeT!E;
                            alias U=ForeachType!T;
                            alias JSType=JSONTypeT!U;
                            scope JSType[] array;

// //                            static if (is(T:

//                             static if (is(T:U[],U)) {
                                // alias JTypeT=ReturnType!(toJSONType(e.by!E);
                                // T array;
                            foreach(a; e.by!E) {
                                array~=toJSONType(a);
                            }
                            doc_element[VALUE]=array;
                            // }
                            //assert(0, format("%s is not implemented yet", E));
                        }
                        else {
                            doc_element[VALUE]=toJSONType(e.by!E);
                        }
                        if ( isarray ) {
                            result.array~=doc_element;
                        }
                        else {
                            result[e.key]=doc_element;
                        }
                        break CaseType;
                    }
                }
            default:
                .check(0, format("HiBON type %s notsupported and can not be converted to JSON", e.type));
            }
        }
    }
    return result;
}

// void toJSON(scope const HiBON hibon, ref File fout=stdout) {
//     scope doc=Document(hibon.serialize, fout);
//     toJSON(hibon, fout);
// }

// void toJSON(scope const Document doc, ref File fout=stdout) {

// }
version(none)
@safe struct JSONElement {
    const Document.Element element;
    this(const Element element) {
        this.element=element;
    }

    immutable EOL="<";
    immutable indent="**";

    union JValue {
        string text;
        double number;
        bool   boolean;
    }
    immutable JValue value;

    @trusted
    this(T)(string key, T x) {
        static if (is(T == string)) {
            enum _jtype=JSONType.STRING;
            this.value.text=x;
        }
        else static if (is(T : double)) {
            enum _jtype=JSONType.NUMBER;
            this.value.number=x;
        }
        else static if (is(T == bool)) {
            enum _jtype=JSONType.BOOLEAN;
            this.value.boolean=x;
        }
        else {
            enum _jtype=JSONType.NULL;
            static assert(0, format("Type %s not supported by JSON", T.stringof));
        }
        super(key, jtype);
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

    /*
    override JSONElement opSlice()
        in {
            assert((jtype is JSONType.OBJECT) || (jtype is JSONType.ARRAY), "Range is only defined for JSON ARRAY or OBJECT");
        }
    do {
        assert(0, "Not implemented");
    }
    */
}


version(none)
@safe
struct JSONDocElement {
    mixin BasicElement;
    struct KeyValue {
        JSONElement key;
        JSONElement value;
    }
    union Element {
        Document.Range doc_range;
    }
    protected Element element;

    enum TYPE="$T";
    enum VALUE="$V";
    enum ElementType {
        DOC, ELEMENT, BASIC
    }
    immutable ElementType elementType;

//    const(JSONElement)[] elements;
//    immutable JSONType type;
//    immutable(string) key;
    static string EOL="<>";
    static string indent="";
    this(string key, const Document doc) {
        doc_range=doc[];
        elementType=ElementType.DOC;
        jtype=JSONType.OBJECT;
        //  super(key, JSONType.OBJECT);
    }

    this(string key, scope ref ) {
        this.elements=elements;
        super(key, JSONType.OBJECT);
    }

    @property bool empty() const pure nothrow {
        return elements.length is 0;
    }

    @property void popFront() {
        elements=elements[1..$];
    }

    // @property override const(JSONElement) front() const pure {
    //     return elements[0];
    // }

    JSONDocElement front() const {

        scope e=doc_range.front;

//        JSONDocElement[] elements;
        with(Type) {
            switch(e.type) {
                static foreach(MemberE; EnumMembers!Type) {
                case MemberE:
                    static if(isHiBONType(MemberE)) {
                        elements~=JSONElement(TYPE, typeMap[e.type]);
                        static if (MemberE is Type.DOCUMENT) {
                            pragma(msg, "e.by!MemberE ", typeof(e.by!MemberE));
                            elements~=new JSONObjectElement(e.key, e.by!MemberE);
                            //assert(0, format("%s is not implemented yet", MemberE));
                        }
                        else static if (MemberE is Type.UTC) {
                            assert(0, format("%s is not implemented yet", MemberE));
                        }
                        else static if (isArray(MemberE)) {
                            assert(0, format("%s is not implemented yet", MemberE));
                        }
                        else {
                            elements~=JSONElement(VALUE, toJSONType(e.by!MemberE));
                        }
                        return new JSONDocElement(e.key, elements);
                    }
                    goto default;
                }
            default:
                // empty
            }
        }
        assert(0, format("Invalid HiBON type %s", e.type));
    }

    JSONDocElement opSlice()  {
        //    pragma(msg, "JSONElement ", isInputRange!(JSONElement[]));
        return this;
    }
}

version(none)
@safe
class JSONObjectElement : JSONElement { //E(Type E) if (E is Type.DOCUMENT) {
    mixin BasicElement;
    private Document.Range doc_range;
    enum TYPE="$T";
    enum VALUE="$V";
    this(string key, const Document doc) {
        doc_range=doc[];
        //  super(key, JSONType.OBJECT);
    }

    override bool empty() const pure nothrow {
        return doc_range.empty;
    }

    override JSONDocElement front() const {
        const e=doc_range.front;

//        JSONDocElement[] elements;
        with(Type) {
            switch(e.type) {
                static foreach(MemberE; EnumMembers!Type) {
                case MemberE:
                    static if(isHiBONType(MemberE)) {
                        elements~=JSONElement(TYPE, typeMap[e.type]);
                        static if (MemberE is Type.DOCUMENT) {
                            pragma(msg, "e.by!MemberE ", typeof(e.by!MemberE));
                            elements~=new JSONObjectElement(e.key, e.by!MemberE);
                            //assert(0, format("%s is not implemented yet", MemberE));
                        }
                        else static if (MemberE is Type.UTC) {
                            assert(0, format("%s is not implemented yet", MemberE));
                        }
                        else static if (isArray(MemberE)) {
                            assert(0, format("%s is not implemented yet", MemberE));
                        }
                        else {
                            elements~=JSONElement(VALUE, toJSONType(e.by!MemberE));
                        }
                        return new JSONDocElement(e.key, elements);
                    }
                    goto default;
                }
            default:
                // empty
            }
        }
        assert(0, format("Invalid HiBON type %s", e.type));
    }

    override void popFront() {
        doc_range.popFront;
    }

    JSONObjectElement opSlice() {
        return this;
//        assert(0, "Not implemented");
    }

    // JSONElement range() {
    //     assert(0, "Not implemented");
    // }
}



template JSONTypeT(T) {
    alias UnqualT=Unqual!T;
    static if (is(UnqualT == bool)) {
        alias JSONTypeT=bool;
    }
    else static if (is(UnqualT == string)) {
        alias JSONTypeT=string;
    }
    else static if(is(UnqualT == ulong) || is(UnqualT == ulong)) {
        alias JSONTypeT=string;
    }
    else static if(is(UnqualT == long)) {
        alias JSONTypeT=string;
    }
    else static if(is(T : immutable(ubyte)[])) {
        alias JSONTypeT=string;
    }
    else static if(is(UnqualT  : double)) {
        alias JSONTypeT=double;
    }
    else {
        static assert(0, format("Unsuported type %s", T.stringof));
    }
}

@safe
auto toJSONType(T)(T x) pure {
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
    else static if(is(UnqualT  : double)) {
        return cast(double)x;
    }
    else {
        static assert(0, format("Unsuported type %s", T.stringof));
    }
}


// auto JSONRange(T)(T x) {
//     alias E=Document.Value.asType!T;
//     pragma(msg, E.stringof, " : ", T, " : ", Document);
//     static if(is(T : const Document)) {
//         return JSONRangeE!E(x);
//     }
//     /*
//     else {
//         return JSONRangeE!E(x[]);
//     }
//     */
// }

unittest {
    import tagion.utils.HiBON : HiBON;

    auto hibon=new HiBON;
    hibon["x"]=int(-42);
    hibon["txt"]="some text";

    auto sub_hibon = new HiBON;
    sub_hibon["bool"]=true;
    sub_hibon["txt2"]="some other text";
    immutable arr=[10, 20, 30];
    immutable arr2=cast(long[])[-10, 20e6, 30];
    sub_hibon["arr"]=arr;
    sub_hibon["arr2"]=arr2;

    hibon["obj"]=sub_hibon;
    immutable data=hibon.serialize;
    const doc=Document(data);

//    auto range=JSONRange(doc);
    // auto a=new JSONElement("x", JSONType.NULL);
    // alias ElementT=ForeachType!(JSONElement);
    // version(none) {
    auto json=doc.toJSON;//new JSONObjectElement("", doc);//, "", "^^");
    // pragma(msg, "range ", typeof(range));
    // alias ElementT=ForeachType!(JSONElement);
    // foreach(e; range) {
    //     writefln("e=%s", e);
    //     foreach(s; e) {
    //         writefln("\ts=%s", s);
    //     }
    // }
//    pragma(msg, "isInputRange ", isInputRange!(JSONRangeE!(Type.DOCUMENT)));
    // auto base=JSONOutStream(range);

    //   base(stdout);
//    auto fout=File("/tmp/dump.json", "w");
//    JSONOutStream(range)(stdout);
//    }

    writefln("%s", json.toPrettyString);
}
