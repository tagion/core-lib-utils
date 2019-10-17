/**
 * HBSON Document
 *
 */
module tagion.utils.Document;

import std.format;
import std.meta : AliasSeq, Filter;
import std.traits : isBasicType, isSomeString, isIntegral, isNumeric, getUDAs, EnumMembers, Unqual;
import std.conv : to, emplace;
import std.algorithm.iteration : map;

import tagion.Types : decimal_t;
import tagion.Base : isOneOf;
import tagion.utils.HiBONBase;

static assert(uint.sizeof == 4);

@safe struct Document {
    protected alias Value=ValueT!(false, void, Document);
    immutable(ubyte[]) data;

    @disable this();

    this(immutable ubyte[] data) nothrow {
        this.data = data;
    }

    this(const Document document) nothrow {
        this.data = document.data;
    }

    @trusted
    void copy(ref const Document doc) {
        emplace(&this, doc);
    }

    @property nothrow pure const {
        @safe bool empty() {
            return data.length < 5;
        }


        @trusted uint size() {
            return *cast(uint*)(data[0..uint.sizeof].ptr);
        }
    }

    @trusted
    @property uint length() const {
        uint counter;
        foreach(e; this[]) {
            counter++;
        }
        return counter;
    }

    alias ErrorCallback =void function(ref scope const(Element));

    Element.ErrorCode valid(ErrorCallback error_callback =null) const {
        foreach(ref e; this[]) {
            Element.ErrorCode error_code;
            if ( e.type is Type.DOCUMENT ) {
                error_code = e.get!(Document).valid(error_callback);
            }
            else {
                error_code = e.valid;
            }
            if ( error_code !is Element.ErrorCode.NONE ) {
                if ( error_callback ) {
                    error_callback(e);
                }
                return error_code;
            }
        }
        return Element.ErrorCode.NONE;
    }

    void toJSON(T, string INDENT="  ", string EOL="\n", string SPACE=" ")(T stream, string indent=null) {
        enum BETWEEN=","~EOL;
        bool first=true;
        void print(string key, string element_type, string element_text) {
            stream.writef("%s%s%s:%s{%s", indent, key, SPACE, SPACE, EOL);
            stream.writef("%s%s$type%s:%s%s%s", indent, INDENT, SPACE, SPACE, element_type, BETWEEN);
            string text;
            e.as(text);
            stream.writef("%s%s$type%s:%s%s%s", indent, INDENT, SPACE, SPACE, element_text, EOL);
            stream.writef("%s}", indent);
        }
        foreach(ref e; this[]) {
            if ( !first ) {
                stream.writef("%s", BETWEEN);
            }
            first = false;
            if (e.type is Type.DOCUMENT) {
                e.get!(Document).toJSON(stream);
            }
            else {
            CaseType:
                switch(e.type) {
                    static foreach(E; EnumMembers!Type) {
                        static if (isHiBONType!E) {
                        case E:
                            string text;
                            e.as(text);
                            alias BaseT = Value.TypeT!E;
                            static if ( isNumeric!BaseT || is(Unqual!BaseT == bool) || isArray(E) ) {
                                print(e.key, e.type.to!string, text);
                            }
                            else {
                                print(e.key, e.type.to!string, '"'~text~'"');
                            }
                            break CaseType;
                        }
                    }
                default:
                    print(e.key, cast(ubyte)(e.type).to!string, format("\"Bad data type\""));
                }
            }
        }
    }

    version(none)
    string toText(string INDENT="  ", string EOL="\n")() const {
        enum BETWEEN=","~EOL;
        string object_toText(Document doc, const Type type, immutable(string) indent=null) @safe {
            string buf;
            bool any=false;
            immutable bool array=(type == Type.ARRAY);
            buf ~=indent;
            buf =(array)?"[":"{";
            string lines(Range)(Range range, immutable(string) indent, immutable(string) separator=EOL) @safe {
                if ( !range.empty) {
                    const e=range.front;
                    range.popFront;
                    if ( e.isDocument ) {
                        return format("%s%s%s : %s", separator, indent, e.key, object_toText(e.get!Document, e.type, indent))~
                            lines(range, indent, BETWEEN);

                    }
                    else {
                        return format("%s%s%s : (%s)%s", separator, indent, e.key, e.typeString, e.toInfo) ~
                            lines(range, indent, BETWEEN);
                    }
                }
                return "\n";
            }
            buf~=lines(doc[], indent~INDENT);
            buf ~=indent;
            buf ~= (array)?"]":"}";
            return buf;
        }
        return object_toText(this, Type.DOCUMENT);
    }

    @safe
    struct Range {
        immutable(ubyte[]) data;
    protected:
        size_t            _index;
        Element           _element;
    public:
        this(immutable(ubyte[]) data) {
            this.data = data;

            if (data.length == 0) {
                _index = 0;
            }
            else {
                _index = uint.sizeof;
                popFront();
            }
        }

        this(const Document doc) {
            this(doc.data);
        }

        @property pure nothrow const {
            bool empty() {
                return _index >= data.length;
            }


            /**
             * InputRange primitive operation that returns the currently iterated element.
             */
            const(Element) front() {
                return _element;
            }
        }


        /**
         * InputRange primitive operation that advances the range to its next element.
         */
        @trusted
        void popFront() {
            emplace!Element(&_element, data[_index..$]);
            _index += _element.size;
        }
    }


    Range opSlice() const {
        return Range(data);
    }

    auto keys() const {
        return map!"a.key"(Range(data));
    }

    // Throws an std.conv.ConvException if the keys can not be convert to an uint
    auto indices() const {
        return map!"a.key.to!uint"(Range(data));
    }

    bool hasElement(in string key) const {
        return !opIn_r(key).isEod();
    }

    bool hasElement(Index)(in Index index) const if (isIntegral!Index) {
        return hasElement(index.to!string);
    }

    const(Element) opIn_r(in string key) const {
        foreach (ref element; this[]) {
            if (element.key == key) {
                return element;
            }
        }
        return Element();
    }

    const(Element) opIndex(in string key) const {
        auto result=key in this;
        .check(!result.isEod, format("Member named '%s' not found", key));
        return result;
    }

    const(Element) opIndex(Index)(in Index index) const if (isIntegral!Index) {
        return opIndex(index.to!string);
    }


    alias serialize=data;

    static void build(T)(ref ubyte[] buffer, Type type, string key, T x, ref size_t index) {
        buffer.binwrite(type, &index);
        buffer.binwrite(cast(ubyte)(key.length), &index);
        buffer.array_write(key, index);
        static if ( is(T: U[], U) ) {
            immutable size=cast(uint)(x.length*U.sizeof);
            buffer.binwrite(size, &index);
            buffer.array_write(x, index);
        }
        else static if (is(T : const Document)) {
            buffer.array_write(x.data, index);
        }
        else {
            buffer.binwrite(x, &index);
        }
    }

    unittest {
        import std.typecons : Tuple, isTuple;
        auto buffer=new ubyte[0x200];

        size_t make(R)(ref ubyte[] buffer, R range, size_t count=size_t.max) if (isTuple!R) {
            size_t index;
            buffer.binwrite(uint.init, &index);
            foreach(i, t; range) {
                if ( i is count ) {
                    break;
                }
                enum name = range.fieldNames[i];
//                writefln("name=%s", name);
                alias U = range.Types[i];
                enum  E = Value.asType!U;
                build(buffer, E, name, t, index);
            }
            buffer.binwrite(Type.NONE, &index);
            uint size;
            size = cast(uint)(index - uint.sizeof);
            buffer.binwrite(size, 0);
            return index;
        }

        { // Test of null document
            const doc = Document(null);
            assert(doc.length is 0);
            assert(doc[].empty);
        }

        { // Test of empty Document
            size_t index;
            buffer.binwrite(uint.init, &index);
            buffer.binwrite(Type.NONE, &index);
            buffer.binwrite(uint(1), 0);
            immutable data=buffer[0..index].idup;
            const doc = Document(data);
            assert(doc.length is 0);
            assert(doc[].empty);

        }

        alias Tabel = Tuple!(
            float,  Type.FLOAT32.stringof,
            double, Type.FLOAT64.stringof,
            bool,   Type.BOOLEAN.stringof,
            int,    Type.INT32.stringof,
            long,   Type.INT64.stringof,
            uint,   Type.UINT32.stringof,
            ulong,  Type.UINT64.stringof,
//                utc_t,  Type.UTC.stringof
            );

        Tabel test_tabel;
        test_tabel.FLOAT32 = 1.23;
        test_tabel.FLOAT64 = 1.23e200;
        test_tabel.INT32   = -42;
        test_tabel.INT64   = -0x0123_3456_789A_BCDF;
        test_tabel.UINT32   = 42;
        test_tabel.UINT64   = 0x0123_3456_789A_BCDF;
        test_tabel.BOOLEAN  = true;

        alias TabelArray = Tuple!(
            immutable(ubyte)[],  Type.BINARY.stringof,
            immutable(float)[],  Type.FLOAT32_ARRAY.stringof,
            immutable(double)[], Type.FLOAT64_ARRAY.stringof,
            immutable(int)[],    Type.INT32_ARRAY.stringof,
            immutable(long)[],   Type.INT64_ARRAY.stringof,
            immutable(uint)[],   Type.UINT32_ARRAY.stringof,
            immutable(ulong)[],  Type.UINT64_ARRAY.stringof,
            immutable(bool)[],   Type.BOOLEAN_ARRAY.stringof,
            string,              Type.STRING.stringof

            );
        TabelArray test_tabel_array;
        test_tabel_array.BINARY        = [1, 2, 3];
        test_tabel_array.FLOAT32_ARRAY = [-1.23, 3, 20e30];
        test_tabel_array.FLOAT64_ARRAY = [10.3e200, -1e-201];
        test_tabel_array.INT32_ARRAY   = [-11, -22, 33, 44];
        test_tabel_array.INT64_ARRAY   = [0x17, 0xffff_aaaa, -1, 42];
        test_tabel_array.UINT32_ARRAY  = [11, 22, 33, 44];
        test_tabel_array.UINT64_ARRAY  = [0x17, 0xffff_aaaa, 1, 42];
        test_tabel_array.BOOLEAN_ARRAY = [true, false];
        test_tabel_array.STRING        = "Text";

        { // Document with simple types
            //test_tabel.UTC      = 1234;

            size_t index;

            { // Document with a single value
                index = make(buffer, test_tabel, 1);
                immutable data = buffer[0..index].idup;
                const doc=Document(data);
                assert(doc.length is 1);
                assert(doc[Type.FLOAT32.stringof].get!float == test_tabel[0]);
            }

            { // Document including basic types
                index = make(buffer, test_tabel);
                immutable data = buffer[0..index].idup;
                const doc=Document(data);

                auto keys=doc.keys;
                foreach(i, t; test_tabel) {
                    enum name = test_tabel.fieldNames[i];
                    alias U = test_tabel.Types[i];
                    enum  E = Value.asType!U;
                    assert(doc.hasElement(name));
                    const e = doc[name];
                    assert(e.get!U == test_tabel[i]);
                    assert(keys.front == name);
                    keys.popFront;

                    auto e_in = name in doc;
                    assert(e.get!U == test_tabel[i]);

                    assert(e.type is E);
                    assert(e.isType!U);
                }
            }
//            Type
            { // Document which includes basic arrays and string
                index = make(buffer, test_tabel_array);
                immutable data = buffer[0..index].idup;
                const doc=Document(data);
                foreach(i, t; test_tabel_array) {
                    enum name = test_tabel_array.fieldNames[i];
                    alias U   = test_tabel_array.Types[i];
                    const v = doc[name].get!U;
                    assert(v.length is test_tabel_array[i].length);
                    assert(v == test_tabel_array[i]);

                }
            }

            { // Document which includes sub-documents
                auto buffer_subdoc=new ubyte[0x200];
                index = make(buffer_subdoc, test_tabel);
                immutable data_sub_doc = buffer_subdoc[0..index].idup;
                const sub_doc=Document(data_sub_doc);

                index = 0;
                uint size;
                buffer.binwrite(uint.init, &index);
                enum doc_name="doc";

                build(buffer, Type.INT32, Type.INT32.stringof, int(42), index);
                build(buffer, Type.DOCUMENT, doc_name, sub_doc, index);
                build(buffer, Type.STRING, Type.STRING.stringof, "Text", index);

                buffer.binwrite(Type.NONE, &index);

                size = cast(uint)(index - uint.sizeof);
                buffer.binwrite(size, 0);

                immutable data = buffer[0..index].idup;
                const doc=Document(data);

                { // Check int32 in doc
                    const int32_e = doc[Type.INT32.stringof];
                    assert(int32_e.type is Type.INT32);
                    assert(int32_e.get!int is int(42));
                    assert(int32_e.by!(Type.INT32) is int(42));
                }

                { // Check string in doc )
                    const string_e = doc[Type.STRING.stringof];
                    assert(string_e.type is Type.STRING);
                    const text = string_e.get!string;
                    assert(text.length is "Text".length);
                    assert(text == "Text");
                    assert(text == string_e.by!(Type.STRING));
                }

                { // Check the sub/under document
                    const under_e = doc[doc_name];
                    assert(under_e.key == doc_name);
                    assert(under_e.type == Type.DOCUMENT);
                    assert(under_e.size == data_sub_doc.length + Type.sizeof + ubyte.sizeof + doc_name.length);

                    const under_doc = doc[doc_name].get!Document;
                    assert(under_doc.data.length == data_sub_doc.length);

                    auto keys=under_doc.keys;
                    foreach(i, t; test_tabel) {
                        enum name = test_tabel.fieldNames[i];
                        alias U = test_tabel.Types[i];
                        enum  E = Value.asType!U;
                        assert(under_doc.hasElement(name));
                        const e = under_doc[name];
                        assert(e.get!U == test_tabel[i]);
                        assert(keys.front == name);
                        keys.popFront;

                        auto e_in = name in doc;
                        assert(e.get!U == test_tabel[i]);
                    }
                }
            }

        }
    }

/**
 * HiBON element representation
 */
    @safe struct Element {
        /*
         * -----
         * //data image:
         * +-------------------------------------------+
         * | [Type] | [len] | [key] | [val | unused... |
         * +-------------------------------------------+
         *          ^ type offset(1)
         *                  ^ len offset(2)
         *                          ^ keySize + 2
         *                                 ^ size
         *                                             ^ data.length
         *
         */
        immutable(ubyte[]) data;
    public:
        this(immutable(ubyte[]) data) {
            // In this time, Element does not parse a binary data.
            // This is lazy initialization for some efficient.
            this.data = data;
        }

        enum KEY_POS = Type.sizeof + keyLen.sizeof;

        @property const {
            bool isType(T)() {
                enum E = Value.asType!T;
                return (E !is Type.NONE) && (type is E);
            }

            @trusted const(Value*) value() {
                with(Type)
                TypeCase:
                switch(type) {
                    static foreach(E; EnumMembers!Type) {
                        static if (isArray(E) || (E is STRING) || (E is DOCUMENT) ) {
                        case E:
                            static if (E is Type.DOCUMENT) {
                                immutable byte_size = *cast(uint*)(data[valuePos..valuePos+uint.sizeof].ptr);
                                return new Value(Document(data[valuePos..valuePos+uint.sizeof+byte_size]));
                            }
                            else static if (isArray(E) || (E is Type.STRING)) {
                                alias T = Value.TypeT!E;
                                static if ( is(T: U[], U) ) {
                                    immutable birary_array_pos = valuePos+uint.sizeof;
                                    immutable byte_size = *cast(uint*)(data[valuePos..birary_array_pos].ptr);
                                    immutable len = byte_size / U.sizeof;
                                    return new Value((cast(immutable(U)*)(data[birary_array_pos..$].ptr))[0..len]);
//                                }
                                }
                            }
                            break TypeCase;
                        }
                    }
                default:
                    if (isHiBONType(type)) {
                        return cast(Value*)(data[valuePos..$].ptr);
                    }
                }
                    .check(0, format("Invalid type %s", type));

                assert(0);
            }

            auto by(Type E)() {
                .check(type is E, format("Type expected type is %s but the actual type is %s", E, type));
                .check(E !is Type.NONE, format("Type is not supported %s the actual type is %s", E, type));
                return value.get!E;

            }

            T get(T)() {
                enum E = Value.asType!T;
                return by!E;
            }

            /**
               Tryes to convert the value to the type T.
               Returns true if the function succeeds
            */
            bool as(T)(ref T result) {
                switch(type) {
                    static foreach(E; EnumMembers!Type) {
                        static if (isHiBONType(E)) {
                        case E:
                            alias BaseT = Value.TypeT!E;
                            static if (isImplicitlyConvertible!(BaseT, T)) {
                                result=value.get!BaseT;
                                return true;
                            }
                            else static if (__traits(compiles, value.get!(BaseT).to!T)) {
                                result = value.get!(BaseT).to!T;
                            }
                        }
                    }
                }
                return false;
            }
        }

        @property const pure nothrow {
            bool isEod() {
                return data.length == 0;
            }

            Type type() {
                if (isEod) {
                    return Type.NONE;
                }
                return cast(Type)(data[0]);
            }

            ubyte keyLen() {
                return cast(Type)(data[Type.sizeof]);
            }

            string key() {
                return cast(string)data[KEY_POS..valuePos];
            }

            /*
              bool isIndex() {
              return len is 0;
              }
            */

/*
  uint index() {
  .check(isIndex, "This a key not an index");
  return
  }
*/

            uint valuePos() {
                return KEY_POS+keyLen;
            }

            @trusted
                size_t size() {
                with(Type) {
                TypeCase:

                    switch(type) {
                        static foreach(E; EnumMembers!Type) {
                        case E:
                            static if (isHiBONType(E)) {
                                alias T = Value.TypeT!E;
                                static if ( isArray(E) || (E is STRING) || (E is DOCUMENT) ) {
                                    static if (isNative(E)) {
                                        return 0;
                                    }
                                    else {
                                        immutable binary_array_pos = valuePos+uint.sizeof;
                                        immutable byte_size = *cast(uint*)(data[valuePos..binary_array_pos].ptr);
                                        return binary_array_pos + byte_size;
                                    }
                                }
                                else {
                                    return valuePos + T.sizeof;
                                }
                            }
                            else static if ( E is Type.NONE ) {
                                return Type.sizeof;
                            }
                            break TypeCase;
                        }
                    default:
                        // empty
                    }
                }
                assert(0, format("Bad type %s", type));
            }

            enum ErrorCode {
                NONE,           // No errors
                DOCUMENT_TYPE,  // Warning document type
                TOO_SMALL,      // Data stream is too small to contain valid data
                ILLEGAL_TYPE,   // Use of internal types is illegal
                INVALID_TYPE,   // Type is not defined
                OVERFLOW,       // The specifed data does not fit into the data stream
                ARRAY_SIZE_BAD // The binary-array size in bytes is not a multipla of element size in the array
            }

            /**
               Check if the element is valid
            */
            @trusted
                ErrorCode valid() {
                        enum MIN_ELEMENT_SIZE = Type.sizeof + ubyte.sizeof + char.sizeof + uint.sizeof;

                with(ErrorCode) {
                    if ( type is Type.DOCUMENT ) {
                        return DOCUMENT_TYPE;
                    }
                    if ( data.length < MIN_ELEMENT_SIZE ) {
                        return TOO_SMALL;
                    }
                TypeCase:
                    switch(type) {
                        static foreach(E; EnumMembers!Type) {
                        case E:
                            static if ( (isNative(E) || (E is Type.DEFINED_ARRAY) ) ) {
                                return ILLEGAL_TYPE;
                            }
                            break TypeCase;
                        }
                    default:
                        return INVALID_TYPE;
                    }
                    if ( size < data.length ) {
                        return OVERFLOW;
                    }
                    if ( isArray(type) ) {
                        immutable binary_array_pos = valuePos+uint.sizeof;
                        immutable byte_size = *cast(uint*)(data[valuePos..binary_array_pos].ptr);
                        switch(type) {
                            static foreach(E; EnumMembers!Type) {
                                static if ( isArray(E) && !isNative(E) ) {
                                case E:
                                    alias T = Value.TypeT!E;
                                    static if ( is(T : U[], U) ) {
                                        if ( byte_size % U.sizeof !is 0 ) {
                                            return ARRAY_SIZE_BAD;
                                        }
                                    }
                                }
                            }
                        default:
                            // empty
                        }
                    }
                    return NONE;
                }
            }

            /**
               Check if the type match That template.
               That template must have one parameter T as followes
               alias That(T) = ...;
            */
            bool isThat(alias That)() {
                switch(type) {
                    static foreach(E; EnumMembers!Type) {
                        static if (isHiBONType!E) {
                        case E:
                            alias T = Value.TypeT!E;
                            return That!T;
                        }
                    }
                default:
                    // empty
                }
                return false;
            }
        }


        // bool isType(T)() pure const nothrow {
        //     enum expectedType=Value.asType!T;
        //     if ( expectedType is Type.NONE ) {
        //         return false;
        //     }
        //     return (type is expectedType);
        // }

        /*
          T get(T)() const if (is(T==string)) {
          return cast(string)value[4..$];
          }
        */
        version(none) {
            alias BasicTypes=Filter!(isBasicType, ValueSeqBasicTypes);

            T get(T)() const if (isOneOf!(T, BasicTypes )) {
                check_type(TypeEnum!(T));
                return fromValue!T;
            }

            enum isBasicArrayType(T) = is(T:U[],U) && isBasicType!U;
            alias BasicArrayType      = Filter!(isBasicArrayType, DTypes);

            T get(T)() const if (isOneOf!(T, BasicArrayType)) {

                immutable buf=binary_buffer;
                .check(buf.length % U.sizeof == 0, format("The size of binary subtype '%s' should be a mutiple of %d but is %d", subtype, U.sizeof, buf.length));

            }
        }
        version(none) {

            T[] toArray(T)() const {
                .check(isArray, format("ARRAY type expected not %s", typeString));
                auto doc=get!Document;
                auto last_index=doc.indices.maxElement;
                auto array=new T[last_index+1];
                uint previous_index;
                foreach(e; doc[]) {
                    immutable current_index=e.index;
                    .check((previous_index is 0) || (previous_index < current_index), format("Index of an Array should be ordred @index %d next %d", previous_index, current_index));

                    array[current_index]=e.get!T;
                    previous_index=current_index;
                }
                return array;
            }


        }


        @safe
        bool opEquals(ref const Element other) const {
            immutable s = size;
            if (s != other.size) {
                return false;
            }
            return data[0..s] == other.data[0..s];
        }

    }

}












version(none)
unittest { // toArray
    auto strings=["Hej", "med", "Dig"];
    auto bson=new HBSON;

    bson[strings.stringof]=strings;

    auto doc=Document(bson.serialize);
    auto same=doc[strings.stringof].toArray!string;

    assert(same == strings);
}


version(none)
unittest { // BSON with const member
    alias GBSON=BSON!true;
    auto bson1=new GBSON;
    auto bson2=new GBSON;
    bson1["hugh"]="Some data";
    bson1["age"]=42;
    bson1["height"]=155.7;

    bson2["obj"]=bson1;
    immutable bson1_data=bson1.serialize;
    immutable bson2_data=bson2.serialize;

    auto doc1=Document(bson1_data);
    auto doc2=Document(bson2_data);

    assert(bson1_data.length == doc1.data.length);
    assert(bson1_data == doc1.data);

    assert(bson2_data.length == doc2.data.length);
    assert(bson2_data == doc2.data);

    void doc_bson_const(GBSON bson, const(GBSON) b) {

        bson["obj"]=b;
    }

    auto bson2c=new GBSON;
    doc_bson_const(bson2c, bson1);

    immutable bson2c_data=bson2c.serialize;
    auto doc2c=Document(bson2c_data);
    assert(bson2c_data == doc2c.data);
    assert(doc2c.data == doc2.data);

}

version(none)
unittest { // Test of Native Document type
    // The native document type is only used as an internal representation of the Document
    auto bson1=new HBSON;
    auto bson2=new HBSON;
    auto doc_bson=new HBSON;
    doc_bson["int"]=10;
    doc_bson["bool"]=true;
    bson1["obj"]=doc_bson;

    // Test of using native Documnet as a object member
    auto doc=Document(doc_bson.serialize);
    bson2["obj"]=doc;
    auto data1=bson1.serialize;
    // writefln("%s:%d", data1, data1.length);
    auto data2=bson2.serialize;
    // writefln("%s:%d", data2, data2.length);
    assert(data1.length == data2.length);
    assert(data1 == data2);
}
