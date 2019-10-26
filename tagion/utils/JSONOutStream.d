module tagion.utils.JSONOutStream;

import std.stdio : File, stdout;

import tagion.utils.JSONInStream : JSONType;
import std.traits : ForeachType, hasMember;
import std.range.primitives : isInputRange;
import std.conv : to;
import std.format;

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


static JSONOutStreamT!R JSONOutStream(R)(R range) {
    // JSONOutStreamT!R stream;
    // stream.range=range;
    return JSONOutStreamT!R(range);
    // this(Range range) {
    //     this.range=range;
//        this.fout=stdout;
}

struct JSONOutStreamT(Range) if (isInputRange!Range) {
    protected Range range;
    this(Range range) {
        this.range=range;
    }

    void opCall(File fout=stdout) {
        output(range, fout); //, EOL, indent);
    }


    protected mixin template Format(string name, string default_format="") {
        static if (hasMember!(Range, name)) {
            enum code=format("immutable %s=range.%s;", name, name);
        }
        else {
            enum code=format("enum %s=\"%s\";", name, default_format);
        }
        mixin(code);
    }
    protected mixin template Flag(string name, bool default_flag=false) {
        static if (hasMember!(Range, name)) {
            enum code=format("immutable %s=range.%s;", name, name);
        }
        else {
            enum code=format("enum %s=%s;", name, default_flag);
        }
        mixin(code);
    }
    protected static
    void output(Range)(Range range, ref File fout) { //, const(string) EOL, const(string) INDENT="..") {
        void inner(Range)(Range range, const(string) current_indent="") {
//            mixin Format!("EOL");
//            mixin Format!("EOL", r"\n");
//            mixin Format!("BEGIN", r"{<BEGIN>\n");
            mixin Format!("BEGIN", r"{\n");
//            mixin Format!("END", r"}<END>");
            mixin Format!("END", r"}");
            mixin Format!("ENDBEND", r"\n");

//            mixin Format!("SPACER", r"<:>");
            mixin Format!("SPACER", r" : ");
//            mixin Format!("BETWEEN", r"<,>");
            mixin Format!("BETWEEN", r",\n");
            mixin Format!("INDENT", r"..");
            mixin Flag!("ARRAY");
            mixin Flag!("NOINDENT");
            pragma(msg, "Range ", Range);
            alias ElementT=ForeachType!Range;
            static if (NOINDENT) {
                enum base_indent="";
            }
            else {
                immutable base_indent=current_indent;
            }

            fout.write(BEGIN);
            scope(exit) {
                if(ENDBEND) {
                    fout.write(ENDBEND, current_indent, END);
                }
                else {
                    fout.write(END);
                }
            }
            bool first=true;
            foreach(e; range) {
                if ( !first ) {
                    fout.write(BETWEEN);
                }
                first=false;
                if(ARRAY) {
                    fout.write(base_indent);
                }
                else {
                    fout.writef("%s\"%s\"%s", base_indent, e.key, SPACER);
                }

                with(JSONType) {
                    switch(e.jtype) {
                        static if (__traits(compiles, e[])) {
                        case OBJECT, ARRAY:
                            inner(e[], current_indent~INDENT);
                            break;
                        }
                        else static if (hasMember!(ElementT, "get")) {
                        case NULL:
                            fout.write("null");
                        break;
                        case STRING:
                            fout.writef("\"%s\"", e.get!string);
                            break;
                        case NUMBER:
                            fout.write(e.get!double.to!string);
                            break;
                    case BOOLEAN:
                        fout.writef("%s", e.get!bool);
                        break;
                    }
                    default:
                        import std.traits : ReturnType;
                        fout.writefln("e=%s %s %s %s", e, __traits(compiles, e[]), hasMember!(typeof(e), "opSlice"));
                        // fout.writefln("ReturnType=%s", ReturnType!(e[]).stringof);
                        assert(0, format("Bad JSON %s for this element", e.jtype));
                    }
                }
            }
        }
        inner(range);
    }
}
