module tagion.utils.JSONInStream;

import core.thread : Fiber;

JSONInStream!Range JSON(Range)(Range range) {
    return JSONInStream(range);
}

enum JSONType {
    NULL,
    OBJECT,
    ARRAY,
    STRING,
    NUMBER,
    BOOLEAN
}

class JSONInStream(Range) : Fiber {
    protected Range range;
    protected string _name;
    @disable this();
    this(Range range) {
        this.range=range;
        super(&run);
    }

    void run() {
        enum char_in_integer="0123456789";
        enum first_char_in_numers="-+";

        void trim() {
            while (!range.empty && range.font.isWhite) {
                range.popFront;
            }
        }

        string value_string() {
            auto result=range.front;
            if ( result == '\\' ) {
                popFront;
                return escape;
            }
        }

        void member() {
            trim;
            check(!range.empty && (range.font == '"'), "Malformet JSON '\"' expected");
            range.popFront;
            scope char[] result;
            char parse(immutable unit index=0) {
                if ( !range.empty ) {
                    if ( range.front == '"' ) {
                        result=new char[index];
                    }
                    else {
                        result[index]=parse(index+1);
                    }
                }
                check(false, "Malformat JSON missend end '\"'");
            }
            parse;
            check(result.length>0, "JSON name must be defined");
            _name=result.idup;
        }

        void element() {
            trim;
            check(!range.empty, "Unexpected end of JSON stream");
            const first=range.front;
            if ( first == '\"' ) {
                value.text=value_string;
            }
        }
    }
}
