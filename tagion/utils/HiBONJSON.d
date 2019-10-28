module tagion.utils.HiBONJSON;

import std.stdio;

import std.json;
import std.conv : to;
import std.format;
import std.traits : EnumMembers, Unqual, ReturnType, ForeachType;
import std.range.primitives : isInputRange;

import tagion.utils.HiBONBase : Type, isNative, isArray, isHiBONType, HiBONException;
import tagion.utils.HiBON : HiBON;
import tagion.utils.Document : Document;
// import tagion.utils.JSONOutStream;
// import tagion.utils.JSONInStream : JSONType;

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


enum {
    TYPE=0,
    VALUE=1,
}

JSONValue toJSON(Document doc, bool hashsafe=true) {
    if (hashsafe) {
        return toJSONT!true(doc);
    }
    else {
        return toJSONT!false(doc);
    }
}

//@safe
struct toJSONT(bool HASHSAFE) {
    static JSONValue opCall(const Document doc) {
        JSONValue result;
        immutable isarray=doc.isArray;
//        writefln("HASHSAFE=%s",HASHSAFE);
        foreach(e; doc[]) {
            with(Type) {
            CaseType:
                switch(e.type) {
                    static foreach(E; EnumMembers!Type) {
                        static if (isHiBONType(E)) {
                        case E:
                            static if (E is Type.DOCUMENT) {
                                const sub_doc=e.by!E;
                                auto doc_element=toJSONT(sub_doc);
                                if ( isarray ) {
                                    result.array~=doc_element;
                                }
                                else {
                                    result[e.key]=doc_element;
                                }
                            }
                            else {
                                auto doc_element=new JSONValue[2];
                                doc_element[TYPE]=JSONValue(typeMap[E]);
                                static if (E is Type.UTC) {
                                    assert(0, format("%s is not implemented yet", E));
                                }
                                else static if (isArray(E) && (E !is Type.BINARY)) {
                                    alias T=Document.Value.TypeT!E;
                                    alias U=ForeachType!T;
                                    alias JSType=JSONTypeT!U;
                                    scope JSType[] array;
                                    foreach(a; e.by!E) {
                                        array~=toJSONType(a);
                                    }
                                    doc_element[VALUE]=array;
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


    template JSONTypeT(T) {
        alias UnqualT=Unqual!T;
        static if (is(UnqualT == bool)) {
            alias JSONTypeT=bool;
        }
        else static if (is(UnqualT == string)) {
            alias JSONTypeT=string;
        }
        else static if(is(UnqualT == ulong) || is(UnqualT == long)) {
            alias JSONTypeT=string;
        }
        else static if(is(UnqualT == uint) || is(UnqualT == int)) {
            alias JSONTypeT=UnqualT;
        }
        else static if(is(T : immutable(ubyte)[])) {
            alias JSONTypeT=string;
        }
        else static if(is(UnqualT  : double)) {
            static if (HASHSAFE) {
                alias JSONTypeT=string;
            }
            else {
                alias JSONTypeT=double;
            }
        }
        else {
            static assert(0, format("Unsuported type %s", T.stringof));
        }
    }

    static auto toJSONType(T)(T x) {
        alias UnqualT=Unqual!T;
        static if (is(UnqualT == bool)) {
            return x;
        }
        else static if(is(UnqualT == string)) {
            return x;
        }
        else static if(is(UnqualT == ulong) || is(UnqualT == long)) {
            return format("%d", x);
        }
        else static if(is(UnqualT == uint) || is(UnqualT == int)) {
            return x;
        }
        else static if(is(T : immutable(ubyte)[])) {
            return format("0x%s", x.toHex);
        }
        else static if(is(UnqualT  : double)) {
            static if (HASHSAFE) {
                return format("%a", x);
            }
            else {
                return cast(double)x;
            }
        }
        else {
            static assert(0, format("Unsuported type %s", T.stringof));
        }
    }

}

HiBON toHiBON(scope const JSONValue json) {
    static const(T) get(T)(scope JSONValue jvalue) {
        alias UnqualT=Unqual!T;
        writefln("Inside get=%s type=%s" ,jvalue, jvalue.type);
        static if (is(UnqualT==bool)) {
            return jvalue.boolean;
        }
        else static if(is(UnqualT==uint)) {
            return jvalue.uinteger.to!uint;
        }
        else static if(is(UnqualT==int)) {
            return jvalue.integer.to!int;
        }
        else static if(is(UnqualT==long) || is(UnqualTT==ulong)) {
            return jvalue.str.to!UnqualT;
        }
        else static if(is(UnqualT==string)) {
            return jvalue.str;
        }
        else static if(is(T==immutable(ubyte)[])) {
            return decode(jvalue.str);
        }
        else static if(is(T:const(double))) {
            if (jvalue.type is JSONType.float_) {
                return jvalue.floating.to!UnqualT;
            }
            else {
                return jvalue.str.to!UnqualT;
            }
        }
        else static if(is(T:U[],U)) {
            scope array=new U[jvalue.array.length];
            foreach(i, ref a; jvalue) {
                array[i]=a.get!U;
            }
            return array.idup;
        }
        else {
            static assert(0, format("Type %s is not supported", T.stringof));
        }
        assert(0);
    }

    //static HiBON Obj(scope JSONValue json);


    static HiBON Obj(scope JSONValue json) {
    static bool set(ref HiBON sub_result, string key, scope JSONValue jvalue) {
        immutable label=jvalue.array[TYPE].str;
        .check((label in labelMap) !is null, "HiBON type name '%s' is not valid", label);
        immutable type=labelMap[label];
        writefln("type=%s", type);
        scope(failure) {
            writefln("Faild at key %s jstype=%s %s", key, jvalue.type, jvalue);
        }

        with(Type) {
            final switch(type) {
                static foreach(E; EnumMembers!Type) {
                case E:
                    static if (isHiBONType(E)) {
                        alias T=HiBON.Value.TypeT!E;
                        scope value=jvalue.array[VALUE];

                        static if(E is DOCUMENT) {
                            return false;
//                            .check(0, format("Type abel '%s' should not be used explicit", label));
//                            pragma(msg, "Obj ", typeof(value),  " ", ReturnType!(Obj));
//                            sub_result[key]=Obj(value);
                        }
                        else {
                            static if(E is UTC) {
                                assert(0, format("Type %s is supported yet", E));
                            }
                            else static if (isArray(E)) {
                                .check(value.type is JSONType.array, format("JSON array expected for %s for member %s", E, key));
                                alias U=Unqual!(ForeachType!T);
                                scope array=new U[value.array.length];
                                pragma(msg, "value=", typeof(value));
                                foreach(size_t i, ref e; value) {
                                    array[i]=get!U(e);
                                }
                                sub_result[key]=array.idup;

                            }
                            else {
                                writefln("Before get =%s", value);
                                sub_result[key]=get!T(value);
                            }
                            return true;
                        }
                    }
                    else {
                        assert(0, format("Unsupported type %s for member %s", E, key));
                    }
                }
            }
        }
        assert(0);
    }
        HiBON result=new HiBON;
        // static foreach(E; EnumMembers!JSONType) {
        //     writefln("case %s:\nbreak;", E);
        // }
        foreach(string key, ref jvalue;json) {
            with(JSONType) {
                final switch(jvalue.type) {
                case null_:
                    .check(0, "HiBON does not support null");
                    break;
                case string:
                    result[key]=jvalue.str;
                    break;
                case integer:
                    result[key]=jvalue.integer;
                    break;
                case uinteger:
                    result[key]=jvalue.uinteger;
                    break;
                case float_:
                    result[key]=jvalue.floating;
                    break;
                case array:
                    if (!set(result, key, jvalue)) {
                        result[key]=Obj(jvalue);
                    }
                    break;
                case object:
                    result[key]=Obj(jvalue);
                    break;
                case true_:
                    result[key]=true;
                    break;
                case false_:
                    result[key]=false;
                    break;
                }
            }
            writefln("%s: <%s>", key,  jvalue.type);
        }
        return result;
    }
    return Obj(json);
}

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
    immutable arr3=cast(float[])[-10.2, 20.3e6, 0.0333];
    sub_hibon["arr"]=arr;
    sub_hibon["arr2"]=arr2;
    sub_hibon["arr3"]=arr3;

    hibon["obj"]=sub_hibon;
    immutable data=hibon.serialize;
    const doc=Document(data);

//    auto range=JSONRange(doc);
    // auto a=new JSONElement("x", JSONType.NULL);
    // alias ElementT=ForeachType!(JSONElement);
    // version(none) {
//    writefln("Before toJSON");
    auto json=doc.toJSON(true);//new JSONObjectElement("", doc);//, "", "^^");
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

    //writefln("%s", typeof(json));

    writefln("%s", json.toPrettyString);
    string str=json.toString;
    auto parse=str.parseJSON;
    auto h=parse.toHiBON;

    const parse_doc=Document(h.serialize);
    writefln("After %s", parse_doc.toJSON(true).toPrettyString);
//    pragma(msg, typeof(parse.toHiBON));
}
