/**
 * HBSON Document
 *
 */
module tagion.utils.Document;

//import core.stdc.string;  // Some operations in Phobos not safe, pure and nothrow, e.g. cmp

/*
  import std.algorithm;
  import std.conv;
  import std.exception;  // assumeUnique
  import std.datetime;   // Date, DateTime
  import std.typecons;   // Tuple
  import std.format;
  import std.traits : isSomeString, isIntegral, isArray;
  import std.algorithm.searching : maxElement;
//import std.array : Appender;
private import std.bitmanip;
*/
import std.format;
import std.meta : AliasSeq, Filter;
import std.traits : isBasicType, isSomeString, isIntegral, isNumeric, getUDAs, EnumMembers;

import tagion.Types : decimal_t;
import tagion.Base : isOneOf;
import tagion.utils.HiBONBase;


//import std.stdio;

import tagion.utils.Miscellaneous : toHexString;
//import tagion.TagionExceptions : Check, TagionException;

//public alias HBSON=BSON!(true,true);

static assert(uint.sizeof == 4);



@safe struct Document {
    protected alias Value=ValueT!(false, void, Document);
    // alias ValueSeq = .ValueSeq!(Value);
    // alias ValueType(Type type) = .ValueType!(type, ValueSeq);
    // pragma(msg, ValueSeq);
    //alias ValueSeqBasicTypes = ValueSeqBasicTypes!(ValueSeq);
    //pragma(msg, ValueSeqBasicTypes);
    // pragma(msg, ValueSeqBinaryTypes);
    // pragma(msg, ValueSeq);
    // pragma(msg, ValueSeqIntegralTypes);
    // pragma(msg, ValueSeqNumericTypes);
    // enum table=HiBONTypes!(ValueSeqNumericTypes);
    // pragma(msg, table.stringof);
    immutable(ubyte[]) data;

    this(immutable ubyte[] data) nothrow {
        this.data = data;
    }

    this(const Document document) nothrow {
        this.data = document.data;
    }

    @trusted
    void copy(Document* doc_ptr)
        in {
            assert(doc_ptr !is null);
        }
    do {
//         extern(C) void* memcpy (
//   scope return void* s1,
//   scope const(void*) s2,
//   ulong n
// ) pure nothrow @nogc;
        import core.stdc.string : memcpy;
        memcpy(doc_ptr, &this, this.sizeof);
    }
    /*
    Document idup() const nothrow {
        return Document(data.idup);
    }
    */

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
        foreach(i; Range(data)) {
            counter++;
        }
        return counter;
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

    version(none)
    unittest { // isInOrder
        void build(B)(B bson) {
            auto obj=new B;
            obj["a"]=7;
            obj["b"]=8;
            bson["banana"]=1;
            bson["orange"]=obj;
            bson["apple"]=3;
        }
        //auto hbson=new HBSON;
        auto bson=new BSON!(false, false);
        build(bson);

        assert(!Document(bson.serialize).isInOrder);
        auto hbson=new HBSON;
        build(hbson);
        // writefln("hbson is inOrder=%s", Document(hbson.serialize).isInOrder);
        assert(Document(hbson.serialize).isInOrder);

    }

    struct Range {
        immutable(ubyte[]) data;
    protected:
        size_t            _index;
        Element           _element;


    public:
        @safe
        this(immutable(ubyte[]) data) {
            this.data = data;

            if (data.length == 0) {
                _index = 0;
            }
            else {
                _index = 4;
                popFront();
            }
        }

        this(const Document doc) {
            this(doc.data);
        }

        @property @safe pure nothrow const {
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
            import std.conv;

            emplace!Element(&_element, data[_index..$]);
            _index += _element.size;
        }
    }


    Range opSlice() const {
        return Range(data);
    }

    version(none) {
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
            foreach (ref element; Range(data)) {
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

        string toString() const {
            if (empty) {
                return "{}";
            }
            return "";
        }
    }


version(none) {
    unittest {
        // {foo: "bar", bool: true, num: 10}
        immutable ubyte[] data = [0x22, 0x00, 0x00, 0x00, 0x02, 0x66, 0x6f, 0x6f, 0x00, 0x04, 0x00, 0x00, 0x00, 0x62, 0x61, 0x72, 0x00,
            0x08, 0x62, 0x6f, 0x6f, 0x6c, 0x00, 0x01, 0x10, 0x6e, 0x75, 0x6d, 0x00, 0x0a, 0x00, 0x00, 0x00, 0x00];
        auto doc = Document(data);

        { // hasElement
            assert(doc.hasElement("bool"));
            assert(doc.hasElement("foo"));
            assert(doc.hasElement("num"));
            assert(!doc.hasElement("missing"));
        }
        { // opSlice
            auto range = doc[];
            assert(count(range) == 3);
        }
        { // keys
            assert(equal(doc.keys, ["foo", "bool", "num"]));
        }
        { // opIndex([])
            auto strElem = doc["foo"];
            assert(strElem.str == "bar");

            auto numElem = doc["num"];
            assert(numElem.get!int == 10);
            assert(numElem.get!(const(int)) == 10);
            assert(numElem.get!(immutable(int)) == 10);
            // assert(numElem.get!uint == 10);
            // assert(numElem.get!(const(uint)) == 10);
            // assert(numElem.get!(immutable(uint)) == 10);

            auto boolElem = doc["bool"];
            assert(boolElem.get!bool);

            // Typedef check
            alias NewInt=Typedef!int;
            assert(numElem.get!NewInt == 10);

        }
    }
}

/**
 * BSON element representation
 */
@safe
struct Element {
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
     * For a list time the len is zero as follows
     * +-------------------------------------------+
     * | [Type] | [len] | [index] | [val | unused... |
     * +---------------------------------------------+
     *          ^ type offset(1)
     *                  ^ len offset(2)
     *                            ^ keySize + 2
     *                                    ^ size
     *                                               ^ data.length
     *
     * -----
     */
    immutable uint index; // This only used to list elements
    immutable(ubyte[]) _data;
    enum MIN_ELEMENT_SIZE = Type.sizeof + ubyte.sizeof + char.sizeof + uint.sizeof;
    // size_t size() const pure nothrow {
    //     return 0;
    // }
public:
    this(immutable(ubyte[]) data) {
        // In this time, Element does not parse a binary data.
        // This is lazy initialization for some efficient.
        _data = data;
    }

    enum KEY_POS = Type.sizeof + keyLen.sizeof;

    @property const {
        string key() {
            //    .check(!isIndex, "This an index not a key");
            return cast(string)(_data[KEY_POS..valuePos]);
        }

        bool isType(T)() {
            enum E = ValueT.asType!T;
            return type is E;
        }

        @trusted
        const(Value*) value() {
            if ( isArray(type) ) {
                switch(type) {
                    static foreach(E; EnumMembers!Type) {
                        static if (!isNative(E) && isArray(E)) {
                        case E:
                            pragma(msg, "E=",E, " isArray(E)=", isArray(E));
                            alias T = Value.TypeT!E;
                            static if ( is(T: U[], U) ) {
                            immutable birary_array_pos = valuePos+uint.sizeof;
                            immutable byte_size = *cast(uint*)(_data[valuePos..birary_array_pos].ptr);
                            immutable len = byte_size / U.sizeof;
//                            Value* result;
//                            static if (isArray(E)) {
                            return new Value((cast(immutable(U)*)(_data[birary_array_pos..$].ptr))[0..len]);
                            // }
                            // else {
                            //     result = (cast(T*)(_data[birary_array_pos..$].ptr))[0..len];
                            // }

                            }
                            goto default;
                        }
                    }
                default:
                    .check(0, format("Invalid type %s", type));
                }
            }
            else {
                return cast(Value*)(_data[valuePos..$].ptr);
            }
            assert(0);
        }

        /*
        const(T) get(T)() {
            enum E = ValueT.asType!T;
            .check(type is E, format("Type expected type is %s but the actual type is %s", E, type));
            return
        }
        */
    }

    @property @safe const pure nothrow {
        bool isEod() {
            return _data.length == 0;
        }

        Type type() {
            if (isEod) {
                return Type.NONE;
            }
            return cast(Type)(_data[0]);
        }

        ubyte keyLen() {
            return cast(Type)(_data[Type.sizeof]);
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
            with(Type) switch(type) {
                static foreach(E; EnumMembers!Type) {
                case E:
                    static if (E is DOCUMENT) {
                        immutable document_pos = valuePos+uint.sizeof;
                        immutable byte_size = *cast(uint*)(_data[valuePos..document_pos].ptr);
                        return byte_size;
                    }
                    else static if (isHiBONType(E)) {
                        alias T = Value.TypeT!E;
                        static if ( isArray(E) ) {
                            static if (isNative(E)) {
                                return 0;
                            }
                            else {
                                immutable binary_array_pos = valuePos+uint.sizeof;
                                immutable byte_size = *cast(uint*)(_data[valuePos..binary_array_pos].ptr);
                                return binary_array_pos + byte_size;
                            }
                        }
                        else {
                            return valuePos + T.sizeof;
                        }
                    }
                    goto default;
                }
                default:
                    // empty
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
            with(ErrorCode) {
                if ( type is Type.DOCUMENT ) {
                    return DOCUMENT_TYPE;
                }
                if ( _data.length < MIN_ELEMENT_SIZE ) {
                    return TOO_SMALL;
                }
            TypeCase:
                switch(type) {
                    static foreach(E; EnumMembers!Type) {
                    case E:
                        static if ( (isNative(E) || (E is Type.TRUNC) || (E is Type.DEFINED_ARRAY) ) ) {
                            return ILLEGAL_TYPE;
                        }
                        break TypeCase;
                    }
                default:
                    return INVALID_TYPE;
                }
                if ( size < _data.length ) {
                    return OVERFLOW;
                }
                if ( isArray(type) ) {
                    immutable binary_array_pos = valuePos+uint.sizeof;
                    immutable byte_size = *cast(uint*)(_data[valuePos..binary_array_pos].ptr);
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

        version(none) {

            bool isNumber() {

                switch (type) {
                case Type.INT32, Type.INT64, Type.DOUBLE, Type.UINT32, Type.UINT64:
                    return true;
                default:
                    return false;
                }
            }


            bool isSimple() {
                switch (type) {
                case Type.INT32, Type.INT64, Type.DOUBLE, Type.UINT32, Type.UINT64, Type.STRING, Type.BOOLEAN, Type.DATE, Type.OID:
                    return true;
                default:
                    return false;
                }
            }

            bool isTrue() {
                switch (type) {
                case Type.INT32:
                    return _int32() != 0;
                case Type.INT64:
                    return _int64() != 0L;
                case Type.UINT32:
                    return _uint32() != 0;
                case Type.UINT64:
                    return _uint64() != 0L;
                case Type.DOUBLE:
                    return _double() != 0.0;
                case Type.BOOLEAN:
                    return _boolean();
                case Type.NONE, Type.NULL, Type.UNDEFINED:
                    return false;
                default:
                    return true;
                }
            }


            bool isDocument() const pure nothrow {
                switch (type) {
                case Type.DOCUMENT, Type.ARRAY:
                    return true;
                default:
                    return false;
                }
            }

            bool isArray()  {
                switch (type) {
                case Type.ARRAY:
                    return true;
                default:
                    return false;
                }
            }

            bool isBinary() {
                return type == Type.BINARY;
            }

            /*
              BinarySubType subtype() {
              if ( (type == Type.BINARY) && (4<value.length) ) {
              return cast(BinarySubType)value[4];
              }
              else {
              return BinarySubType.not_defined;
              }
              }
            */
        }
    }

    version(none)
    @property @safe const pure nothrow {
        Type type() {
            if (isEod) {
                return Type.NONE;
            }
            return cast(Type)_data[0];
        }

    }

    version(none) {
        @property byte canonicalType() const {
            Type t = type;

            with(Type) final switch (t) {
                case MIN, MAX, TRUNC:
                    return t;
                case NONE, UNDEFINED:
                    return 0;
                case HASHDOC:
                    assert(0, "Hashdoc not implemented yet");
                    break;
                case NULL:
                    return 5;
                case DOUBLE, INT32, INT64:
                    return 10;
                case STRING, SYMBOL:
                    return 15;
                case DOCUMENT:
                    return 20;
                case ARRAY:
                    return 25;
                case BINARY:
                    return 30;
                case OID:
                    return 35;
                case BOOLEAN:
                    return 40;
                case DATE, TIMESTAMP:
                    return 45;
                case REGEX:
                    return 50;
                case DBPOINTER:
                    return 55;
                case JS_CODE:
                    return 60;
                case JS_CODE_W_SCOPE:
                    return 65;
                case FLOAT, UINT32, UINT64:
                    return 70;
                case NATIVE_DOCUMENT, NATIVE_ARRAY, NATIVE_BSON_ARRAY, NATIVE_STRING_ARRAY:
                    .check(0, format("Invalid type %s",t));
                }
            .check(0, format("Type code 0x%02x not supported", cast(ubyte)t));
            assert(0);
        }
    }

    @property const pure nothrow {

        string key() @trusted {
            if (isEod) {
                return null;
            }
            immutable k = cast(string)_data[1..$];
            // immutable strsize=strlen(k.ptr);
            // immutable len=(strsize<k.length)?strsize:k.length;
            immutable len=_data[0];
            return k[0..len];
        }

        // size_t keySize() {
        //     return key.length;
        // }

    }
/*
        uint index() const {
            uint result;
            check(is_index(key, result), format("Key is '%s' which is not a valid index number", key));
            return result;
        }
*/
    version(none) {
        string typeString() pure const  {
            if ( type is Type.BINARY ) {
                return subtype.to!string;
            }
            else {
                return type.to!string;
            }
        }

    }

    @property @safe const {
        immutable(ubyte[]) value() {
            if (isEod) {
                return null;
            }
            return _data[1 + key.length..size];
        }

/+
            size_t valueSize() {
                return value.length;
            }
            +/
    }

    //Binary buffer
    version(none)
    @trusted protected immutable(ubyte[]) binary_buffer() const {
        immutable v=value;
        immutable len=*cast(uint*)(v.ptr);
        return v[uint.sizeof..len+uint.sizeof];
    }

    @trusted
    protected T fromValue(T)() const pure nothrow {
        return *cast(T*)(value.ptr);
    }

    version(none)
    @property
    size_t size() const {
        size_t s;
        with(Type) final switch (type) {
                foreach (E; EnumMembers!Type) {
                case E:
                    alias T=ValueType!E;
                    static if ( isOneOf!(T, Value.NativeValueDataTypes) ) {
                        .check(0, format("Illigal HiBSON type %s", E));
                    }
                    else static if ( is(T:U[], U) ) {
                        s=bodySize+uint.sizeof;
                    }
                    else {
                        s=T.sizeof;
                    }
                    break;
                }
            }
        return 1 + key.length + s;
    }
    //alias size length;


        bool isType(T)() pure const nothrow {
            enum expectedType=TypeEnum!T;
            if ( expectedType == Type.NONE ) {
                return false;
            }
            return (_type == expectedType);
        }

        T get(T)() const if (is(T==string)) {
            return cast(string)value[4..$];
        }

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

        version(none) {

        }
        /**
         * Returns an DOCUMENT document.
         */
        Document get(T)() inout if (is(TypedefType!T == Document)) {
            check(Type.DOCUMENT);
            return Document(value);
        }



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

        @trusted T get(T)() inout if (isSubType!(TypedefType!T)) {
            alias BaseT=TypedefType!T;
            static if ( is(BaseT : immutable(U[]), U) ) {
                static if ( is(BaseT : immutable(ubyte[]) ) ) {
                    return binary_buffer;
                }
                else if ( (type == Type.BINARY ) && ( subtype == getSubtype!BaseT ) )  {
                    auto buf=binary_buffer;
                    .check(buf.length % U.sizeof == 0, format("The size of binary subtype '%s' should be a mutiple of %d but is %d", subtype, U.sizeof, buf.length));
                    return cast(BaseT)(buf.ptr[0..buf.length]);
                }
            }
            else {
                static assert(0, "Only immutable type is supported not "~T.stringof);
            }
            throw new BSONException(format("Invalide type expected '%s' but the type used is '%s'", subtype, T.stringof));
            assert(0, "Should never go here! Unsupported type "~T.stringof);
        }

        version(none)
            @trusted
            T get(T)() inout if ( is(TypedefType!T : immutable(ubyte)[]) ) {
            if ( type == Type.BINARY)  {
                return binary_buffer;
            }
            throw new BSONException(format("Invalide type expected '%s' but the type used is '%s'", to!string(subtype), T.stringof));
            assert(0, "Should never go here! Unsupported type "~T.stringof);
        }


    }

    @property @trusted const pure nothrow {
        int as(T)() if (is(T == int)) {
            switch (type) {
            case Type.INT32:
                return _int32();
            case Type.UINT32:
                return cast(int)_uint32();
            case Type.INT64:
                return cast(int)_int64();
            case Type.DOUBLE:
                return cast(int)_double();
            case Type.FLOAT:
                return cast(int)_float();
            default:
                return 0;
            }
        }

        int as(T)() if (is(T == uint)) {
            switch (type) {
            case Type.INT32:
                return cast(uint)_int32();
            case Type.UINT32:
                return _uint32();
            case Type.INT64:
                return cast(uint)_int64();
            case Type.DOUBLE:
                return cast(uint)_double();
            case Type.FLOAT:
                return cast(uint)_float();
            default:
                return 0;
            }
        }

        long as(T)() if (is(T == long)) {
            switch (type) {
            case Type.INT32:
                return _int32();
            case Type.UINT32:
                return _uint32();
            case Type.INT64:
                return _int64();
            case Type.UINT64:
                return cast(long)_uint64();
            case Type.DOUBLE:
                return cast(long)_double();
            case Type.FLOAT:
                return cast(long)_float();
            default:
                return 0;
            }
        }


        ulong as(T)() if (is(T == ulong)) {
            switch (type) {
            case Type.INT32:
                return _int32();
            case Type.UINT32:
                return _uint32();
            case Type.INT64:
                return cast(ulong)_int64();
            case Type.UINT64:
                return _uint64();
            case Type.DOUBLE:
                return cast(ulong)_double();
            case Type.FLOAT:
                return cast(ulong)_float();
            default:
                return 0;
            }
        }

        double as(T)() if (is(T == double)) {
            switch (type) {
            case Type.INT32:
                return cast(double)_int32();
            case Type.UINT32:
                return cast(double)_uint32();
            case Type.INT64:
                return cast(double)_int64();
            case Type.UINT64:
                return cast(double)_uint64();
            case Type.DOUBLE:
                return _double();
            case Type.FLOAT:
                return cast(double)_float();
            default:
                return 0;
            }
        }

        float as(T)() if (is(T == float))
        {
            switch (type) {
            case Type.INT32:
                return cast(float)_int32();
            case Type.UINT32:
                return cast(float)_uint32();
            case Type.INT64:
                return cast(float)_int64();
            case Type.UINT64:
                return cast(float)_uint64();
            case Type.DOUBLE:
                return cast(float)_double();
            case Type.FLOAT:
                return _float();
            default:
                return 0;
            }
        }
    }


    // TODO: Add more BSON specified type accessors, e.g.  BINARY
        version(none)

    @property @trusted const
    {
        string str()
        {
            return cast(string)value[4..$ - 1];
        }
        alias str dbPointer;


        // Date date()
        // {
        //     return cast(Date)SysTime(_int64());
        // }


        immutable(ubyte[]) binData()
        {
            return value[5..$];
        }
    }


    @safe
    bool opEquals(ref const Element other) const {
        immutable s = size;
        if (s != other.size) {
            return false;
        }
        return _data[0..s] == other._data[0..s];
    }

    version(none) {
    @safe
        int opCmp(ref const Element other) const {
        int typeDiff = canonicalType - other.canonicalType;
        if (typeDiff < 0) {
            return -1;
        }
        else if (typeDiff > 0) {
            return 1;
        }
        return compareValue(this, other);
    }


    @safe
        string toString() const {
        return toInfo(true, true);
    }

    @trusted
        string toInfo(bool includeKey = false, bool full = false) const {
        string result;
        if (!isEod && includeKey) {
            result = key ~ " : ";
        }

        with(Type) final switch (type) {
            case MIN:
                result ~= "MinKey";
                break;
            case MAX:
                result ~= "MaxKey";
                break;
            case TRUNC:
                result ~= "Trunc";
                break;
            case NONE:
                result ~= "End of Document";
                break;
            case UNDEFINED:
                result ~= "UNDEFINED";
                break;
            case NULL:
                result ~= "null";
                break;
            case BOOLEAN:
                result ~= to!string(_boolean());
                break;
            case INT32:
                result ~= to!string(_int32());
                break;
            case UINT32:
                result ~= to!string(_uint32());
                break;
            case INT64:
                result ~= to!string(_int64());
                break;
            case UINT64:
                result ~= to!string(_uint64());
                break;
            case DOUBLE:
                result ~= to!string(_double());
                break;
            case FLOAT:
                result ~= to!string(_float());
                break;
            case DATE:
                result ~= "new Date(" ~ date.toString() ~ ")";
                break;
            case TIMESTAMP:
                result ~= "Timestamp " ~ timestamp.toString();
                break;
            case OID:
                auto oid = get!ObjectId;
                result ~= "ObjectId(" ~ oid.toString() ~ ")";
                break;
            case DOCUMENT:
                //result ~= DOCUMENT.toFormatString(false, full);
                break;
            case ARRAY:
                //result ~= DOCUMENT.toFormatString(true, full);
                break;
            case HASHDOC:
                assert(0, "Hashdoc not implemented yet");
                break;

            case JS_CODE_W_SCOPE:
                result ~= "codeWScope(" ~ codeWScope ~ ")";
                // TODO: Add codeWScopeObject
                break;
            case STRING, SYMBOL, JS_CODE:
                // TODO: Support ... representation with bool = true
                result ~= '"' ~ str ~ '"';
                break;
            case BINARY:
                enum max_display_size=80;
                if ( binary_buffer.length > max_display_size ) {
                    result ~= binary_buffer[0..max_display_size/2].toHexString~
                        "..."~
                        binary_buffer[max_display_size/2+1..$].toHexString;
                }
                else {
                    result ~= binary_buffer.toHexString;
                }
                break;
            case DBPOINTER:
                result ~= "DBRef(" ~ str ~ ")";
                break;
            case REGEX:
                immutable re = regex;
                result ~= "/" ~ re.field[0] ~ "/" ~ re.field[1];
                break;
            case NATIVE_DOCUMENT:
                result ~= "NativeDoc("~_data.length.to!string~")";
                break;
            case NATIVE_STRING_ARRAY:
                assert(0, "Not implemented");
            case NATIVE_ARRAY:
                assert(0, "Not implemented");
            case NATIVE_BSON_ARRAY:
                assert(0, "Not implemented");
            }

        return result;
    }

    unittest {
        // { "obj" : { "x" : 10 } }
        immutable(ubyte)[] data = [
            0x16,  0x00,  0x00,  0x00,
            0x03,  0x6F,  0x62,  0x6A,  0x00,  0x0C,  0x00,  0x00,  0x00,  0x10,  0x78,  0x00,  0x0A,  0x00,  0x00,  0x00,  0x00,  0x00,
            ];
        auto doc = Document(data);
        { // hasElement
            assert(doc.hasElement("obj"));
            assert(doc["obj"].isDocument);
        }
        {
            auto objElem = doc["obj"];
            auto subobj=doc["obj"].get!Document;
            assert(subobj["x"].get!int == 10);
        }
    }
    }

private:
    void check_type(Type t) const {
        if (t != type) {
            .check( isEod, format("Field not found: expected type = %s ", t));
            .check(0, format("Wrong type for field: [%s].type != %s  expected %s",
                    key, t, type) );
        }
    }

    version(none) {

    void check(BinarySubType t) const /* pure */ {
        if (t != subtype) {
            string typeName = to!string(t); // why is to! not pure?
            string message;
            if (isEod) {
                message = "Field not found: expected subtype = " ~ typeName;
            }
            else {
                message = "Wrong subtype for field: " ~ key ~ " != " ~ typeName ~ " expected " ~ to!string(type) ;
            }
            throw new BSONException(message);
        }
    }

    @trusted const pure nothrow {
        bool _boolean() {
            return value[0] == 0 ? false : true;
        }


        int _int32() {
            return *cast(int*)(value.ptr);
        }

        uint _uint32() {
            return *cast(uint*)(value.ptr);
        }


        long _int64() {
            return *cast(long*)(value.ptr);
        }

        ulong _uint64() {
            return *cast(ulong*)(value.ptr);
        }


        double _double() {
            return *cast(double*)(value.ptr);
        }

        float _float() {
            return *cast(float*)(value.ptr);
        }
    }
    }
    @property const pure nothrow {
        // @safe size_t rawKeySize() {
        //     return key.length + 1;  // including null character termination
        // }

        @trusted uint bodySize() {
            return *cast(uint*)(_data[1 + key.length..$].ptr);
        }
    }

}
}



version(none)
unittest {
    struct ETest {
        ubyte[] data;
        Type    type;
        string  key;
        ubyte[] value;
        bool    isTrue;
        bool    isNumber;
        bool    isSimple;
    }

    Element test(ref const ETest set, string msg) {
        auto amsg = "Assertion failure(" ~ msg ~ " type unittest)";
        auto elem = Element(set.data.idup);

        assert(elem.type      == set.type,         amsg);
        assert(elem.key       == set.key,          amsg);
        assert(elem.keySize   == set.key.length,   amsg);
        assert(elem.value     == set.value,        amsg);
        assert(elem.valueSize == set.value.length, amsg);
        assert(elem.isTrue    == set.isTrue,       amsg);
        assert(elem.isNumber  == set.isNumber,     amsg);
        assert(elem.isSimple  == set.isSimple,     amsg);

        return elem;
    }

    { // EOD element
        ubyte[] data = [];
        ETest   set  = ETest(data, Type.NONE, null, null, false, false, false);

        assert(test(set, "EOD").isEod);
    }
    { // {"hello": "world"} elemement
        ubyte[] data = [0x02, 0x68, 0x65, 0x6C, 0x6C, 0x6F, 0x00, 0x06, 0x00, 0x00, 0x00, 0x77, 0x6F, 0x72, 0x6C, 0x64, 0x00, 0x00, 0x1f];
        auto    set  = ETest(data, Type.STRING, "hello", data[7..$ - 2], true, false, true);
        auto    elem = test(set, "UTF8 STRING");

        assert(elem.str  == "world");
        assert(elem.size == data.length - 2);  // not including extra space
    }

    immutable size_t keyOffset = 3;

    { // {"k": false} elemement
        ubyte[] data = [0x08, 0x6b, 0x00, 0x00];
        ETest   set  = ETest(data, Type.BOOLEAN, "k", data[keyOffset..$], false, false, true);

        assert(!test(set, "Boolean false").get!bool);
    }
    { // {"k": true} elemement
        ubyte[] data = [0x08, 0x6b, 0x00, 0x01];
        ETest   set  = ETest(data, Type.BOOLEAN, "k", data[keyOffset..$], true, false, true);

        assert(test(set, "Boolean true").get!bool);
    }
    { // {"k": int.max} elemement
        { // true
            ubyte[] data = [0x10, 0x6b, 0x00, 0xff, 0xff, 0xff, 0x7f];
            ETest   set  = ETest(data, Type.INT32, "k", data[keyOffset..$], true, true, true);

            assert(test(set, "32bit integer").get!int == int.max);
        }
        { // false
            ubyte[] data = [0x10, 0x6b, 0x00, 0x00, 0x00, 0x00, 0x00];
            ETest   set  = ETest(data, Type.INT32, "k", data[keyOffset..$], false, true, true);

            assert(test(set, "32bit integer").get!int == 0);
        }
    }
    { // {"k": long.min} elemement
        { // true
            ubyte[] data = [0x12, 0x6b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80];
            ETest   set  = ETest(data, Type.INT64, "k", data[keyOffset..$], true, true, true);

            assert(test(set, "64bit integer").get!long == long.min);
        }
        { // false
            ubyte[] data = [0x12, 0x6b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
            ETest   set  = ETest(data, Type.INT64, "k", data[keyOffset..$], false, true, true);

            assert(test(set, "64bit integer").get!long == 0);
        }
    }
    { // {"k": 10000.0} elemement
        { // true
            ubyte[] data = [0x01, 0x6b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0xc3, 0x40];
            ETest   set  = ETest(data, Type.DOUBLE, "k", data[keyOffset..$], true, true, true);

            assert(test(set, "Floating point").get!double == 10000.0f);
        }
        { // false
            ubyte[] data = [0x01, 0x6b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
            ETest   set  = ETest(data, Type.DOUBLE, "k", data[keyOffset..$], false, true, true);

            assert(test(set, "Floating point").get!double == 0.0f);
        }
    }
    { // {"k": Date or DateTime(2011/09/26...)} elemement
        immutable time = 1316968892700L;
        {
            ubyte[] data = [0x09, 0x6b, 0x00, 0x1c, 0x89, 0x76, 0xa1, 0x32, 0x01, 0x00, 0x00];
            ETest   set  = ETest(data, Type.DATE, "k", data[keyOffset..$], true, false, true);

            assert(test(set, "Date").get!Date == cast(Date)SysTime(time));
        }
        {
            ubyte[] data = [0x11, 0x6b, 0x00, 0x1c, 0x89, 0x76, 0xa1, 0x32, 0x01, 0x00, 0x00];
            ETest   set  = ETest(data, Type.TIMESTAMP, "k", data[keyOffset..$], true, false, false);

            assert(test(set, "Timestamp").get!DateTime == cast(DateTime)SysTime(time));
        }
    }
    { // {"k": ObjectId(...)} elemement
        ubyte[]  data = [0x07, 0x6b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff];
        ETest    set  = ETest(data, Type.OID, "k", data[keyOffset..$], true, false, true);

        auto check = ObjectId(long.min, uint.max);
        assert((test(set, "ObjectId").get!ObjectId) == check );
    }
    { // No content elemements, null, MinKey, MaxKey
        foreach (i, type; [Type.NULL, Type.MIN, Type.MAX]) {
            ubyte[] data = [type, 0x6b, 0x00];
            ETest   set  = ETest(data, type, "k", data[keyOffset..$], i > 0);

            test(set, to!string(type));
        }
    }

    // TODO: Add other type tests
}


version(none)
@trusted
int wellOrderedCompare(ref const Element lhs, ref const Element rhs, bool considerKey = true)
{
    int r = lhs.canonicalType - rhs.canonicalType;
    if (r != 0 && (!lhs.isNumber() || !rhs.isNumber()))
        return r;

    if (considerKey) {
        r = strcmp(lhs.key.ptr, rhs.key.ptr);
        if (r != 0)
            return r;
    }

    return compareValue(lhs, rhs);
}


version(none)
@trusted
int compareValue(ref const Element lhs, ref const Element rhs) {
    with(Type) final switch (lhs.type) {
        case MIN, MAX, TRUNC, NONE, UNDEFINED,  NULL:
            auto r = lhs.canonicalType - rhs.canonicalType;
            if (r < 0)
                return -1;
            return r == 0 ? 0 : 1;
        case DOUBLE:
        Ldouble:
            import std.math;

            double l = lhs.as!double;
            double r = rhs.as!double;

            if (l < r)
                return -1;
            if (l == r)
                return 0;
            if (isNaN(l))
                return isNaN(r) ? 0 : -1;
            return 1;
        case FLOAT:
            if (rhs.type == FLOAT) {
                immutable l = lhs.as!float;
                immutable r = rhs.as!float;

                if (l < r)
                    return -1;
                return l == r ? 0 : 1;
            }
            goto Ldouble;
        case INT32:
            if (rhs.type == INT32) {
                immutable l = lhs.as!int;
                immutable r = rhs.as!int;

                if (l < r)
                    return -1;
                return l == r ? 0 : 1;
            }
            goto Ldouble;
        case UINT32:
            if (rhs.type == UINT32) {
                immutable l = lhs.as!int;
                immutable r = rhs.as!int;

                if (l < r)
                    return -1;
                return l == r ? 0 : 1;
            }
            goto Ldouble;
        case INT64:
            if (rhs.type == INT64) {
                immutable l = lhs.as!long;
                immutable r = rhs.as!long;

                if (l < r)
                    return -1;
                return l == r ? 0 : 1;
            }
            goto Ldouble;
        case UINT64:
            if (rhs.type == UINT64) {
                immutable l = lhs.as!ulong;
                immutable r = rhs.as!ulong;

                if (l < r)
                    return -1;
                return l == r ? 0 : 1;
            }
            goto Ldouble;
        case STRING, SYMBOL, JS_CODE:
            import std.algorithm;

            immutable ls = lhs.bodySize;
            immutable rs = rhs.bodySize;
            immutable r  = memcmp(lhs.str.ptr, rhs.str.ptr, min(ls, rs));

            if (r != 0) {
                return r;
            }
            else if (ls < rs) {
                return -1;
            }
            return ls == rs ? 0 : 1;
        case DOCUMENT,  ARRAY:
            // TODO
            return 0;
        case HASHDOC:
            assert(0, "Hashdoc not implemented yet");
            break;
        case BINARY:
            immutable ls = lhs.bodySize;
            immutable rs = rhs.bodySize;

            if ((ls - rs) != 0)
                return ls - rs < 0 ? -1 : 1;
            return memcmp(lhs.value[4..$].ptr, rhs.value[4..$].ptr, ls + 1);  // +1 for subtype
        case OID:
            return memcmp(lhs.value.ptr, rhs.value.ptr, 12);
        case BOOLEAN:
            return lhs.value[0] - rhs.value[0];
        case DATE, TIMESTAMP:
            // TODO: Fix for correct comparison
            // Following comparison avoids non-pure function call.
            immutable l = lhs._int64();
            immutable r = rhs._int64();

            if (l < r)
                return -1;
            return l == r ? 0 : 1;
        case REGEX:
            immutable re1 = lhs.regex;
            immutable re2 = rhs.regex;

            immutable r = strcmp(re1.field[0].ptr, re2.field[0].ptr);
            if (r != 0)
                return r;
            return strcmp(re1.field[1].ptr, re2.field[1].ptr);
        case DBPOINTER:
            immutable ls = lhs.valueSize;
            immutable rs = rhs.valueSize;

            if ((ls - rs) != 0)
                return ls - rs < 0 ? -1 : 1;
            return memcmp(lhs.str.ptr, rhs.str.ptr, ls);
        case JS_CODE_W_SCOPE:
            auto r = lhs.canonicalType - rhs.canonicalType;
            if (r != 0)
                return r;
            r = strcmp(lhs.codeWScope.ptr, rhs.codeWScope.ptr);
            if (r != 0)
                return r;
            r = strcmp(lhs.codeWScopeData.ptr, rhs.codeWScopeData.ptr);
            if (r != 0)
                return r;
            return 0;
        case NATIVE_DOCUMENT, NATIVE_ARRAY, NATIVE_BSON_ARRAY, NATIVE_STRING_ARRAY:
            assert(0, "A native document can not be compared");
        }
}

version(none)
unittest
{
    auto oidElem   = Element(cast(immutable(ubyte[]))[0x07, 0x6b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff]);
    auto strElem   = Element(cast(immutable(ubyte[]))[0x02, 0x6b, 0x00, 0x06, 0x00, 0x00, 0x00, 0x77, 0x6F, 0x72, 0x6C, 0x64, 0x00]);  // world
    auto intElem   = Element(cast(immutable(ubyte[]))[0x10, 0x6b, 0x00, 0xff, 0xff, 0xff, 0x7f]);  // int.max
    auto longElem  = Element(cast(immutable(ubyte[]))[0x12, 0x6b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);  // 0
    auto trueElem  = Element(cast(immutable(ubyte[]))[0x08, 0x6b, 0x00, 0x01]);
    auto dateElem  = Element(cast(immutable(ubyte[]))[0x09, 0x6b, 0x00, 0x1c, 0x89, 0x76, 0xa1, 0x32, 0x01, 0x00, 0x00]);
    auto someElems = [longElem, strElem, oidElem, trueElem, dateElem];  // canonicalType order

    { // MinKey
        auto minKeyElem = Element(cast(immutable(ubyte[]))[Type.MIN, 0x6b, 0x00]);
        auto expect_1 = Element(cast(immutable(ubyte[]))[Type.MIN, 0x6b, 0x00]);
        assert(minKeyElem == expect_1);
        foreach (ref elem; someElems)
            assert(minKeyElem < elem);

        auto expect_2= Element(cast(immutable(ubyte[]))[Type.MIN, 0x6b, 0x00]);
        assert(!(minKeyElem < expect_2));

        auto expect_3= Element(cast(immutable(ubyte[]))[Type.MIN, 0x6a, 0x00]);
        assert(!(minKeyElem < expect_3));  // not consider key
        auto expect_4= Element(cast(immutable(ubyte[]))[Type.MIN, 0x6c, 0x00]);
        assert(wellOrderedCompare(minKeyElem, expect_4) < 0);
        auto expect_5=Element(cast(immutable(ubyte[]))[Type.MIN, 0x6c, 0x00]);
        assert(wellOrderedCompare(minKeyElem, expect_5, false) == 0);
    }
    { // str
        foreach (ref elem; someElems[0..1])
            assert(strElem > elem);
        foreach (ref elem; someElems[2..$])
            assert(strElem < elem);

        auto strElem2 = Element(cast(immutable(ubyte[]))[0x02, 0x6b, 0x00, 0x05, 0x00, 0x00, 0x00, 0x62, 0x73, 0x6f, 0x6e, 0x00]);  // bson
        auto strElem3 = Element(cast(immutable(ubyte[]))[0x02, 0x6c, 0x00, 0x05, 0x00, 0x00, 0x00, 0x62, 0x73, 0x6f, 0x6e, 0x00]);  // bson

        assert(strElem > strElem2);
        assert(strElem > strElem3);
        assert(wellOrderedCompare(strElem, strElem3) < 0);
        assert(wellOrderedCompare(strElem, strElem3, false) > 0);
    }
    { // int
        foreach (ref elem; someElems[1..$])
            assert(intElem < elem);

        auto intElem2 = Element(cast(immutable(ubyte[]))[0x10, 0x6c, 0x00, 0x00, 0x00, 0x00, 0x00]);  // 0

        assert(intElem > intElem2);
        assert(intElem > longElem);
        assert(wellOrderedCompare(intElem, intElem2) < 0);
    }
    { // long
        foreach (ref elem; someElems[1..$])
            assert(longElem < elem);

        auto longElem2 = Element(cast(immutable(ubyte[]))[0x12, 0x6a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80]);  // long.min

        assert(intElem  > longElem2);
        assert(longElem > longElem2);
        assert(wellOrderedCompare(longElem, longElem2) > 0);
    }
    { // boolean
        foreach (ref elem; someElems[0..2])
            assert(trueElem > elem);
        foreach (ref elem; someElems[4..$])
            assert(trueElem < elem);

        auto falseElem = Element(cast(immutable(ubyte[]))[0x08, 0x6c, 0x00, 0x00]);

        assert(falseElem < trueElem);
        assert(wellOrderedCompare(falseElem, trueElem) > 0);
        assert(wellOrderedCompare(falseElem, trueElem, false) < 0);
    }
    { // MaxKey
        auto maxKeyElem = Element(cast(immutable(ubyte[]))[Type.MAX, 0x6b, 0x00]);
        auto expect_1=Element(cast(immutable(ubyte[]))[Type.MAX, 0x6b, 0x00]);
        assert(maxKeyElem == expect_1);

        foreach (ref elem; someElems)
            assert(maxKeyElem > elem);

        auto expect_2=Element(cast(immutable(ubyte[]))[Type.MAX, 0x6b, 0x00]);
        assert(!(maxKeyElem < expect_2));

        auto expect_3=Element(cast(immutable(ubyte[]))[Type.MAX, 0x6a, 0x00]);
        assert(!(maxKeyElem < expect_3));  // not consider key

        auto expect_4=Element(cast(immutable(ubyte[]))[Type.MAX, 0x6c, 0x00]);
        assert(wellOrderedCompare(maxKeyElem, expect_4) < 0);
        auto expect_5=Element(cast(immutable(ubyte[]))[Type.MAX, 0x6c, 0x00]);
        assert(wellOrderedCompare(maxKeyElem, expect_5, false) == 0);
    }

    // TODO: Add other type tests
}


/**
 * The BSON ObjectId Datatype
 *
 * See_Also:
 *  $(LINK2 http://www.mongodb.org/display/DOCS/Object+IDs, Object IDs)
 */
struct ObjectId {
private:
    // ObjectId is 12 bytes
    union
    {
        ubyte[12] data;

        struct
        {
            long a;
            uint b;
        }

        struct
        {
            ubyte[4] time;
            ubyte[3] machine;
            ushort   pid;
            ubyte[3] inc;
        }
    }


    // ourMachine shoulde be immutable
    // immutable static ubyte[3] ourMachine;
    // See: http://dusers.dip.jp/modules/forum/index.php?topic_id=104#post_id399
    __gshared static ubyte[3] ourMachine;


    version(none)
    @trusted
    shared static this() {
        // import std.md5;  // TODO: Will be replaced with std.digest
        import std.digest.md;
        import std.socket;

        ubyte[16] digest;

        digest=md5Of(Socket.hostName());
        //sum(digest, Socket.hostName());
        ourMachine[] = digest[0..3];
    }

    version(none)
    unittest {
        ObjectId oid;
        oid.initialize();

        assert(oid.machine == ourMachine);
    }


public:
    @property
    static uint machineID() nothrow
        {
            static union MachineToID
            {
                ubyte[4] machine;
                uint     id;
            }

            MachineToID temp;
            temp.machine[0..3] = ourMachine;
            return temp.id;
        }


    @safe pure nothrow
        {
            this(in ubyte[] bytes)
                in
                {
                    assert(bytes.length == 12, "The length of bytes must be 12");
                }
            body
            {
                data[] = bytes;
            }


            this(long a, uint b)
            {
                this.a = a;
                this.b = b;
            }


            this(in string hex)
                in
                {
                    assert(hex.length == 24, "The length of hex string must be 24");
                }
            body
            {
                data[] = fromHex(hex);
            }
        }

    version(none)
    @trusted
    void initialize()
        {
            import std.process;

            { // time
                uint   t = cast(uint)Clock.currTime().toUnixTime();
                ubyte* p = cast(ubyte*)&t;
                time[0]  = p[3];
                time[1]  = p[2];
                time[2]  = p[1];
                time[3]  = p[0];
            }

            // machine
            machine = ourMachine;

            // pid(or thread id)
            static if (__VERSION__ >= 70) {
                pid = cast(ushort)thisProcessID();
            }
            else {
                pid = cast(ushort)getpid();
            }

            { // inc
                //See: http://d.puremagic.com/issues/show_bug.cgi?id = 6670
                //import core.atomic;
                /* shared */ __gshared static uint counter;
                //atomicOp!"+="(counter, 1u);
                uint   i = counter++;
                ubyte* p = cast(ubyte*)&i;
                inc[0]   = p[2];
                inc[1]   = p[1];
                inc[2]   = p[0];
            }
        }


    @safe
    bool opEquals(ref const ObjectId other) const pure nothrow {
        return data == other.data;
    }

    version(none)
    @safe
    string toString() const pure nothrow {
        return data.toHex();
    }

    @safe
    immutable(ubyte)[12] id() const pure nothrow {
        return data;
    }
}


version(none)
unittest
{
    { // ==
        string hex = "ffffffffffffff7fffffffff";

        auto oid1 = ObjectId(long.max, uint.max);
        auto oid2 = ObjectId(hex);
        assert(oid1 == oid2);
        assert(oid1.toString() == hex);
        assert(oid2.toString() == hex);

        ObjectId oid;
        oid.initialize();
        assert(oid.machineID > 0);
    }
    { // !=
        auto oid1 = ObjectId(long.max, uint.max);
        auto oid2 = ObjectId(long.max,  int.max);

        assert(oid1 != oid2);
    }
    { // internal data
        ObjectId oid = ObjectId("000102030405060708090a0b");

        assert(oid.data == [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b]);
    }
}


// private:

// Phobos does not have 0-filled hex conversion functions?


version(none)
@trusted
string toHex(in ubyte[] nums) pure nothrow {
    immutable static lowerHexDigits = "0123456789abcdef";

    char[] result = new char[](nums.length * 2);
    foreach (i, num; nums) {
        immutable index = i * 2;
        result[index]     = lowerHexDigits[(num & 0xf0) >> 4];
        result[index + 1] = lowerHexDigits[num & 0x0f];
    }

    return assumeUnique(result);
}


@safe
ubyte[] fromHex(in string hex) pure nothrow {
    static ubyte toNum(in char c) pure nothrow
    {
        if ('0' <= c && c <= '9')
            return cast(ubyte)(c - '0');
        if ('a' <= c && c <= 'f')
            return cast(ubyte)(c - 'a' + 10);
        assert(false, "Out of hex: " ~ c);
    }

    ubyte[] result = new ubyte[](hex.length / 2);

    foreach (i, ref num; result) {
        immutable index = i * 2;
        num = cast(ubyte)((toNum(hex[index]) << 4) | toNum(hex[index + 1]));
    }

    return result;
}


bool isSubType(T)() {
    return (is(T:const(bool)[]))||
        (is(T:const(ubyte)[])) ||
        (is(T:const(int)[])) ||
        (is(T:const(uint)[])) ||
        (is(T:const(long)[])) ||
        (is(T:const(ulong)[])) ||
        (is(T:const(double)[])) ||
        (is(T:const(float)[])) ||
        (is(T:const(ubyte)[]));
}

static
BinarySubType getSubtype(T)() {
    with(BinarySubType) {
        static if (is(T:const(bool)[])) {
            return BOOLEAN_array;
        }
        else static if (is(T:const(int)[])) {
            return INT32_array;
        }
        else static if (is(T:const(uint)[])) {
            return UINT32_array;
        }
        else static if (is(T:const(long)[])) {
            return INT64_array;
        }
        else static if (is(T:const(ulong)[])) {
            return UINT64_array;
        }
        else static if (is(T:const(double)[])) {
            return DOUBLE_array;
        }
        else static if (is(T:const(float)[])) {
            return FLOAT_array;
        }
        else static if (is(T:const(ubyte)[])) {
            return GENERIC;
        }
        else  {
            static assert(0, "Unsupport type "~T.stringof);
        }
    }
}

version(none)
unittest
{
    static struct Test {
        ubyte[] source;
        string  answer;
    }

    Test[] tests = [
        Test([0x00], "00"), Test([0xff, 0xff], "ffff"),
        Test([0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde], "123456789abcde")
        ];

    foreach (ref test; tests)
        assert(test.source.toHex() == test.answer);
    foreach (ref test; tests)
        assert(fromHex(test.answer) == test.source);
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
