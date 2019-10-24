module tagion.utils.JSONOutStream;

import std.stdio : File, stdout;

import tagion.utils.JSONInStream : JSONType;
import std.traits : ForeachType, hasMember;

template isJSONElement(T) {
    static if (!is(typeof(T.type) : JSONType)) {
        enum isJSONElement=false;
    }
    static if (!is(typeof(T.key) : string)) {
        enum isJSONElement=false;
    }
    static if (!is(typeof(T.get!string) : string)) {
        enum isJSONElement=false;
    }
    static if (!is(typeof(T.get!double) : const double)) {
        enum isJSONElement=false;
    }
    static if (!is(typeof(T.get!bool) : const bool)) {
        enum isJSONElement=false;
    }
    enum isJSONElement=true;
}

enum {
    OBJECT_BRACKETS="{}",
    ARRAY_BRACKETS ="[]",
}

template asJType(T) {
    static if (is(T:const string)) {
        enum asJType=JSONType.STRING;
    }
    else static if (is(T: const bool)) {
        enum asJType=JSONType.BOOLEAN;
    }
    else static if (is(T: const double)) {
        enum asJType=JSONType.NUMBER;
    }
    else {
        static assert(0, format("Type %s is uknowed to JSON", T.stringof));
    }
}

template JTypeT(JSONType jtype) {
    static if (jtype is JSONType.STRING) {
        alias JTypeT=string;
    }
    else static if (jtype is JSONType.NUMBER) {
        alias JTypeT=double;
    }
    else static if (jtype is JSONType.BOOLEAN) {
        alias JTypeT=double;
    }
    else {
        static assert(0, format("JType %s is not supported", jtype));
    }
}

void JSONOutStream(Range) (
    Range range, ref File fout=stdout, immutable(bool) OBJ=true, immutable(string) indent="", immutable(string) EOL="\n") if (isInputRange!Range) {
    pragma(msg, "Add JSONElement check");
    if (OBJ) {
        fout.writef("%s{%s", indent, EOL);
        scope(exit) {
            fout.writef("%s}%s", indent, EOL);
        }
    }
    else {
        fout.writef("%s]%s", indent, EOL);
        scope(exit) {
            fout.writef("%s]%s", indent, EOL);
        }
    }
    bool first=true;
    alias ElementT=ForeachType!Range;
    foreach(e; range) {
        if ( !first ) {
            fout.writef(",%s", range.EOL);
        }
        first=false;
        if (OBJ) {
            fout.writef("%s\"%s\" : ", indent, e.key);
        }
        else if (range.EOL) {
            fout.write(indent);
        }

        with(JSONType) {
            final switch(e.type) {
            case OBJECT:
                static if (hasMember!(ElementT, "range")) {
                    JSONOutStream(e.range, fout, true, indent~range.indent, range.EOL);
                }
                else {
                    assert(0, "Object expect an JSON Range");
                }
                break;
            case ARRAY:
                static if (hasMember!(ElementT, "range")) {
                    JSONOutStream(e.range, fout, false, indent~range.indent, range.EOL);
                }
                else {
                    assert(0, "Object expect an JSON Range");
                }
                break;
            case NULL:
                fout.write("null");
                break;
            case STRING:
                fout.writef("\"%s\"", e.get!string);
                break;
            case NUMBER:
                fout.writef("%d", e.get!double);
                break;
            case BOOLEAN:
                fout.writef("%s", e.get!bool);
                break;
            }
        }
    }
}
