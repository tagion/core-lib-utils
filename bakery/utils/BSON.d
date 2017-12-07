// Written in the D programming language.

/**
 * BSON spec implementation
 *
 * See_Also:
 *  $(LINK2 http://bsonspec.org/, BSON - Binary JSON)
 *
 * Copyright: Copyright Masahiro Nakagawa 2011-.
 * License:   <a href="http://www.apache.org/licenses/">Apache LICENSE Version 2.0</a>.
 * Authors:   Masahiro Nakagawa
 * Modified   Carsten Bleser Rasmussen
 *
 *            Copyright Masahiro Nakagawa 2011-.
 *    Distributed under the Apache LICENSE Version 2.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *            http://www.apache.org/licenses/)
 */
module bakery.utils.BSON;

import core.stdc.string;  // Some operations in Phobos not safe, pure and nothrow, e.g. cmp

import std.algorithm;
import std.conv;
import std.exception;  // assumeUnique
import std.datetime;   // Date, DateTime
import std.typecons;   // Tuple
private import std.bitmanip;

//import std.stdio;
//private import proton.core.Misc;
import tango.text.convert.Format;
private import tango.core.Traits : isStringType;
static import tango.text.convert.Integer;

import tango.io.Stdout;

enum Type : byte {
        MIN             = -1,    /// Special type which compares lower than all other possible BSON element values
        NONE            = 0x00,  /// End Of Document
        DOUBLE          = 0x01,  /// Floating point
        STRING          = 0x02,  /// UTF8 STRING
        DOCUMENT        = 0x03,  /// Embedded document
        ARRAY           = 0x04,  ///
        BINARY          = 0x05,  /// Binary data
        UNDEFINED       = 0x06,  /// UNDEFINED - Deprecated
        OID             = 0x07,  /// ObjectID
        BOOLEAN         = 0x08,  /// Boolean - true or false
        DATE            = 0x09,  /// UTC datetime
        NULL            = 0x0a,  /// Null value
        REGEX           = 0x0b,  /// Regular expression
        DBPOINTER       = 0x0c,  /// DBPointer - Deprecated
        JS_CODE         = 0x0d,  /// JavaScript Code
        SYMBOL          = 0x0e,  ///
        JS_CODE_W_SCOPE = 0x0f,  /// JavaScript code w/ scope
        INT32           = 0x10,  /// 32-bit integer
        TIMESTAMP       = 0x11,  ///
        INT64           = 0x12,  /// 64-bit integer,
        UINT32          = 0x50, // 32 bit unsigend integer
        UINT64          = 0x52,  // 64 bit unsigned integer
        FLOAT           = 0x41,  // Float 32

        MAX             = 0x7f   /// Special type which compares higher than all other possible BSON element values
}


enum BinarySubType : ubyte
{
    generic     = 0x00,  /// Binary / Generic
    func        = 0x01,  ///
    binary      = 0x02,  /// Binary (Old)
    uuid        = 0x03,  ///
    md5         = 0x05,  ///
    userDefined = 0x80,   ///
        // Non statdard types
    INT32_array     = userDefined | Type.INT32,
    INT64_array     = userDefined | Type.INT64,
    DOUBLE_array    = userDefined | Type.DOUBLE,
    STRING_array    = userDefined | Type.STRING,
    BOOLEAN_array   = userDefined | Type.BOOLEAN,
    DOCUMENT_array  = userDefined | Type.DOCUMENT,
    UINT32_array    = userDefined | Type.UINT32,
    UINT64_array    = userDefined | Type.UINT64,
    FLOAT_array     = userDefined | Type.FLOAT,
    non             = 0xFF   /// Not defined
}


/**
 * BSON document representation, which is called "BSONObj" in C++.
 */
struct Document
{
  private:
    immutable ubyte[] data_;


  public:
    @safe
    nothrow this(immutable ubyte[] data)
    {
        data_ = data;
    }


    @property nothrow pure const
    {
        @safe
        bool empty()
        {
            return data_.length < 5;
        }


        @trusted
        size_t size()
        {
            return *cast(size_t*)(data_[0..4].ptr);
        }
        alias size length;
    }


    struct Range
    {
      private:
        immutable ubyte[] data_;
        size_t            index_;
        Element           element_;


      public:
        @safe
        this(immutable ubyte[] data)
        {
            data_ = data;

            if (data.length == 0) {
                index_ = 0;
            } else {
                index_ = 4;
                popFront();
            }
        }


        @property @safe nothrow const
        {
            bool empty()
            {
                return index_ >= data_.length;
            }


            /**
             * InputRange primitive operation that returns the currently iterated element.
             */
            ref const(Element) front()
            {
                return element_;
            }
        }


        /**
         * InputRange primitive operation that advances the range to its next element.
         */
        @trusted
        void popFront()
        {
            import std.conv;

            emplace!Element(&element_, data_[index_..$]);
            index_ += element_.size;
        }
    }


    Range opSlice()
    {
        return Range(data_);
    }


    @property @trusted
    string[] keys() const
    {
        import std.array;

        return array(map!"a.key"(Range(data_)));
    }


    @trusted const
    {
        import Integer=tango.text.convert.Integer ;
        // TODO: Replace with opIn?
        bool hasElement(in string key)
        {
            return !opIndex(key).isEod();
        }


        bool hasElement(in size_t index) {
            return hasElement(Integer.toString(index).idup);
        }

        Element opIndex(in string key)
        {
            foreach (ref element; Range(data_)) {
                if (element.key == key)
                    return element;
            }

            return Element();
        }

        Element opIndex(in size_t index) {
            return opIndex(Integer.toString(index).idup);
        }


    }


    bool opEquals(ref const Document other) const
    {
        return true;
    }


    version(none)
    int opCmp(ref const Document other) const
    {
        return 0;
    }

    immutable(ubyte)[] data() const pure nothrow {
        return data_;
    }
    @safe
    string toString() const
    {
        if (empty)
            return "{}";

        return "";
    }
}


unittest
{
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
        assert(doc.keys == ["foo", "bool", "num"]);
    }
    { // opIndex([])
        auto strElem = doc["foo"];
        assert(strElem.str == "bar");

        auto numElem = doc["num"];
        assert(numElem.get!int == 10);

        auto boolElem = doc["bool"];
        assert(boolElem.get!bool);
    }
}


/**
 * BSON element representation
 */
struct Element
{
  private:
    /*
     * -----
     * //data image:
     * +-----------------------------------+
     * | [type] | [key] | [val | unused... |
     * +-----------------------------------+
     *          ^ type offset(1)
     *                  ^ keySize
     *                         ^ size
     *                                     ^ data.length
     * -----
     */
    immutable ubyte[] data_;


  public:
    this(immutable ubyte[] data)
    {
        // In this time, Element does not parse a binary data.
        // This is lazy initialization for some efficient.
        data_ = data;
    }


    @property @safe const pure nothrow
    {
        bool isEod()
        {
            return data_.length == 0;
        }


        bool isNumber()
        {
            switch (type) {
            case Type.INT32, Type.INT64, Type.DOUBLE:
                return true;
            default:
                return false;
            }
        }


        bool isSimple()
        {
            switch (type) {
            case Type.INT32, Type.INT64, Type.DOUBLE, Type.STRING, Type.BOOLEAN, Type.DATE, Type.OID:
                return true;
            default:
                return false;
            }
        }


        bool isTrue()
        {
            switch (type) {
            case Type.INT32:
                return _int32() != 0;
            case Type.INT64:
                return _int64() != 0L;
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


        bool isDocument()
        {
            switch (type) {
            case Type.DOCUMENT, Type.ARRAY:
                return true;
            default:
                return false;
            }
        }

        bool isBinary() {
            return type == Type.BINARY;
        }

        BinarySubType subtype() {
            if ( (type == Type.BINARY) && (4<value.length) ) {
                return cast(BinarySubType)value[4];
            }
            else {
                return BinarySubType.non;
            }
            //return ((4<data_.length) )?data_[4]:BinarySubType.non;
        }



        // need mayEncapsulate?
    }

    @property @safe const pure nothrow
    {
        Type type()
        {
            if (isEod)
                return Type.NONE;
            return cast(Type)data_[0];
        }


        byte canonicalType()
        {
            Type t = type;

            final switch (t) {
            case Type.MIN, Type.MAX:
                return t;
            case Type.NONE, Type.UNDEFINED:
                return 0;
            case Type.NULL:
                return 5;
            case Type.DOUBLE, Type.INT32, Type.INT64:
                return 10;
            case Type.STRING, Type.SYMBOL:
                return 15;
            case Type.DOCUMENT:
                return 20;
            case Type.ARRAY:
                return 25;
            case Type.BINARY:
                return 30;
            case Type.OID:
                return 35;
            case Type.BOOLEAN:
                return 40;
            case Type.DATE, Type.TIMESTAMP:
                return 45;
            case Type.REGEX:
                return 50;
            case Type.DBPOINTER:
                return 55;
            case Type.JS_CODE:
                return 60;
            case Type.JS_CODE_W_SCOPE:
                return 65;
            case Type.FLOAT, Type.UINT32, Type.UINT64:
                return 70;

            }
        }
    }


    @property const pure nothrow
    {
        @trusted
        string key()
        {
            if (isEod)
                return null;

            immutable k = cast(string)data_[1..$];
            immutable strsize=strlen(k.ptr);
            immutable len=(strsize<k.length)?strsize:k.length;
            return k[0..len];
        }


        @safe
        size_t keySize()
        {
            return key.length;
        }

    }


    @property @safe const pure nothrow
    {
        immutable(ubyte[]) value()
        {
            if (isEod)
                return null;

            return data_[1 + rawKeySize..size];
        }


        size_t valueSize()
        {
            return value.length;
        }

    }
    //Binary buffer
    immutable(ubyte[]) binary_buffer() const  {
        auto v=value();
        immutable len=*cast(int*)(v.ptr);
        return v[5..len+5];
    }


    @property @trusted
    size_t size() const pure nothrow
    {
        size_t s;
        final switch (type) {
        case Type.MIN, Type.MAX, Type.NONE, Type.UNDEFINED, Type.NULL:
            break;
        case Type.BOOLEAN:
            s = 1;
            break;
        case Type.INT32, Type.UINT32, Type.FLOAT:
            s = 4;
            break;
        case Type.DOUBLE, Type.INT64, Type.DATE, Type.TIMESTAMP, Type.UINT64:
            s = 8;
            break;
        case Type.OID:
            s = 12;
            break;
        case Type.DOCUMENT, Type.JS_CODE_W_SCOPE, Type.ARRAY:
            s = bodySize;
            break;
        case Type.STRING, Type.SYMBOL, Type.JS_CODE:
            s = bodySize + 4;
            break;
        case Type.BINARY:
            s = bodySize + 4 + 1;
            break;
        case Type.DBPOINTER:
            s = bodySize + 4 + 12;
            break;
        case Type.REGEX:
            auto p1 = cast(immutable(char*))data_[1 + rawKeySize..$].ptr;
            size_t length1 = strlen(p1);
            auto p2 = cast(immutable(char*))data_[1 + rawKeySize + length1 + 1..$].ptr;
            size_t length2 = strlen(p2);
            s = length1 + 1 + length2 + 1;
            break;
        }

        return 1 + rawKeySize + s;
    }
    alias size length;

    // D's primitive type accessor like Variant

    @property const /* pure: check is not pure */
    {
        string get(T)() if (is(T == string))
        {
            check(Type.STRING);
            return str;
        }


        bool get(T)() if (is(T == bool))
        {
            check(Type.BOOLEAN);
            return _boolean();
        }


        int get(T)() if (is(T == int))
        {
            check(Type.INT32);
            return _int32();
        }


        long get(T)() if (is(T == long))
        {
            check(Type.INT64);
            return _int64();
        }


        double get(T)() if (is(T == double))
        {
            check(Type.DOUBLE);
            return _double();
        }


        Date get(T)() if (is(T == Date))
        {
            check(Type.DATE);
            return cast(Date)SysTime(_int64());
        }


        DateTime get(T)() if (is(T == DateTime))
        {
            check(Type.TIMESTAMP);
            return cast(DateTime)SysTime(_int64());
        }


        ObjectId get(T)() if (is(T == ObjectId))
        {
            check(Type.OID);
            return ObjectId(value);
        }


        /**
         * Returns an DOCUMENT document.
         */
        Document get(T)() if (is(T == Document))
        {
            if ( (type != Type.DOCUMENT) && (type != Type.ARRAY) ) {
                check(Type.DOCUMENT);
            }
            return Document(value);
        }

        T get(T)() if (!is(T == string) && is(T == immutable(U)[], U)) {
            static if ( is(T == immutable(U)[], U) ) {
                if ( type == Type.BINARY)  {
                    static if ( is(T == immutable(ubyte)[] ) ) {
                        return binary_buffer;

                    }
                    else if ( subtype == getSubtype!T ) {
                        auto buf=binary_buffer;
                        return (cast(immutable(U)*)(buf.ptr))[0..buf.length/U.sizeof];
                    }
                }
            }
            throw new BSONException("Invalide type expected "~to!string(subtype)~" but the type used is "~T.stringof);
            assert(0, "Unsupported type "~T.stringof);
        }


    }


    @property @trusted const pure nothrow
    {
        int as(T)() if (is(T == int))
        {
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


        int as(T)() if (is(T == uint))
        {
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


        long as(T)() if (is(T == long))
        {
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


        ulong as(T)() if (is(T == ulong))
        {
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

        double as(T)() if (is(T == double))
        {
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

    @property @trusted const nothrow
    {
        Tuple!(string, string) regex() pure
        {
            immutable start1  = 1 + rawKeySize;
            immutable pattern = cast(string)data_[start1..$];
            immutable length1 = strlen(pattern.ptr);
            immutable start2  = start1 + length1 + 1;
            immutable flags   = cast(string)data_[start2..$];
            immutable length2 = strlen(flags.ptr);
            return typeof(return)(pattern[start1..start1 + length1],
                                  flags[start2..start2 + length2]);
        }


        string str() pure
        {
            return cast(string)value[4..$ - 1];
        }
        alias str dbPointer;


        Date date()
        {
            return cast(Date)SysTime(_int64());
        }


        DateTime timestamp()
        {
            return cast(DateTime)SysTime(_int64());
        }


        string codeWScope() pure
        {
            return cast(string)value[8..$];
        }


        string codeWScopeData() pure
        {
            immutable code = codeWScope;
            return code[code.length + 1..$];
        }


        immutable(ubyte[]) binData() pure
        {
            return value[5..$];
        }
    }


    @safe
    bool opEquals(ref const Element other) const pure nothrow
    {
        size_t s = size;
        if (s != other.size)
            return false;
        return data_[0..s] == other.data_[0..s];
    }


    @safe
    int opCmp(ref const Element other) const pure nothrow
    {
        int typeDiff = canonicalType - other.canonicalType;
        if (typeDiff < 0)
            return -1;
        else if (typeDiff > 0)
            return 1;
        return compareValue(this, other);
    }


    @safe
    string toString() const
    {
        return toFormatString(true, true);
    }


    @trusted
    string toFormatString(bool includeKey = false, bool full = false) const
    {
        string result;
        if (!isEod)
            result = key ~ ": ";

        final switch (type) {
        case Type.MIN:
            result ~= "MinKey";
            break;
        case Type.MAX:
            result ~= "MaxKey";
            break;
        case Type.NONE:
            result ~= "End of Document";
            break;
        case Type.UNDEFINED:
            result ~= "UNDEFINED";
            break;
        case Type.NULL:
            result ~= "null";
            break;
        case Type.BOOLEAN:
            result ~= to!string(_boolean());
            break;
        case Type.INT32:
            result ~= to!string(_int32());
            break;
        case Type.UINT32:
            result ~= to!string(_uint32());
            break;
        case Type.INT64:
            result ~= to!string(_int64());
            break;
        case Type.UINT64:
            result ~= to!string(_uint64());
            break;
        case Type.DOUBLE:
            result ~= to!string(_double());
            break;
        case Type.FLOAT:
            result ~= to!string(_float());
            break;
        case Type.DATE:
            result ~= "new Date(" ~ date.toString() ~ ")";
            break;
        case Type.TIMESTAMP:
            result ~= "Timestamp " ~ timestamp.toString();
            break;
        case Type.OID:
            auto oid = get!ObjectId;
            result ~= "ObjectId(" ~ oid.toString() ~ ")";
            break;
        case Type.DOCUMENT:
            //result ~= DOCUMENT.toFormatString(false, full);
            break;
        case Type.ARRAY:
            //result ~= DOCUMENT.toFormatString(true, full);
            break;
        case Type.JS_CODE_W_SCOPE:
            result ~= "codeWScope(" ~ codeWScope ~ ")";
            // TODO: Add codeWScopeObject
            break;
        case Type.STRING, Type.SYMBOL, Type.JS_CODE:
            // TODO: Support ... representation with bool = true
            result ~= '"' ~ str ~ '"';
            break;
        case Type.BINARY:
            result ~= "binData";
            // need content?
            break;
        case Type.DBPOINTER:
            result ~= "DBRef(" ~ str ~ ")";
            break;
        case Type.REGEX:
            immutable re = regex;
            result ~= "/" ~ re.field[0] ~ "/" ~ re.field[1];
            break;
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

  private:
    @trusted
    void check(Type t) const /* pure */
    {
        if (t != type) {
            string typeName = to!string(t); // why is to! is not pure?
            string message;
            if (isEod)
                message = "Field not found: expected type = " ~ typeName;
            else
                message = "Wrong type for field: " ~ key ~ " != " ~ typeName ~ " expected " ~ to!string(type) ;

            throw new BSONException(message);
        }
    }


    @trusted const pure nothrow
    {
        bool _boolean()
        {
            return value[0] == 0 ? false : true;
        }


        int _int32()
        {
            return *cast(int*)(value.ptr);
        }

        uint _uint32()
        {
            return *cast(uint*)(value.ptr);
        }


        long _int64()
        {
            return *cast(long*)(value.ptr);
        }

        ulong _uint64()
        {
            return *cast(ulong*)(value.ptr);
        }


        double _double()
        {
            return *cast(double*)(value.ptr);
        }

        float _float()
        {
            return *cast(float*)(value.ptr);
        }
    }


    @property const pure nothrow
    {
        @safe
        size_t rawKeySize()
        {
            return key.length + 1;  // including null character termination
        }

        @trusted
        uint bodySize()
        {
            return *cast(uint*)(data_[1 + rawKeySize..$].ptr);
        }
    }
}


unittest
{
    struct ETest
    {
        ubyte[] data;
        Type    type;
        string  key;
        ubyte[] value;
        bool    isTrue;
        bool    isNumber;
        bool    isSimple;
    }

    Element test(ref const ETest set, string msg)
    {
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


@trusted
int wellOrderedCompare(ref const Element lhs, ref const Element rhs, bool considerKey = true) pure nothrow
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


@trusted
int compareValue(ref const Element lhs, ref const Element rhs) pure nothrow
{
    final switch (lhs.type) {
    case Type.MIN, Type.MAX, Type.NONE, Type.UNDEFINED,  Type.NULL:
        auto r = lhs.canonicalType - rhs.canonicalType;
        if (r < 0)
            return -1;
        return r == 0 ? 0 : 1;
    case Type.DOUBLE:
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
    case Type.FLOAT:
        if (rhs.type == Type.FLOAT) {
            immutable l = lhs.as!float;
            immutable r = rhs.as!float;

            if (l < r)
                return -1;
            return l == r ? 0 : 1;
        }
        goto Ldouble;
    case Type.INT32:
        if (rhs.type == Type.INT32) {
            immutable l = lhs.as!int;
            immutable r = rhs.as!int;

            if (l < r)
                return -1;
            return l == r ? 0 : 1;
        }
        goto Ldouble;
    case Type.UINT32:
        if (rhs.type == Type.UINT32) {
            immutable l = lhs.as!int;
            immutable r = rhs.as!int;

            if (l < r)
                return -1;
            return l == r ? 0 : 1;
        }
        goto Ldouble;
    case Type.INT64:
        if (rhs.type == Type.INT64) {
            immutable l = lhs.as!long;
            immutable r = rhs.as!long;

            if (l < r)
                return -1;
            return l == r ? 0 : 1;
        }
        goto Ldouble;
    case Type.UINT64:
        if (rhs.type == Type.UINT64) {
            immutable l = lhs.as!ulong;
            immutable r = rhs.as!ulong;

            if (l < r)
                return -1;
            return l == r ? 0 : 1;
        }
        goto Ldouble;
    case Type.STRING, Type.SYMBOL, Type.JS_CODE:
        import std.algorithm;

        immutable ls = lhs.bodySize;
        immutable rs = rhs.bodySize;
        immutable r  = memcmp(lhs.str.ptr, rhs.str.ptr, min(ls, rs));

        if (r != 0)
            return r;
        if (ls < rs)
            return -1;
        return ls == rs ? 0 : 1;
    case Type.DOCUMENT,  Type.ARRAY:
        // TODO
        return 0;
    case Type.BINARY:
        immutable ls = lhs.bodySize;
        immutable rs = rhs.bodySize;

        if ((ls - rs) != 0)
            return ls - rs < 0 ? -1 : 1;
        return memcmp(lhs.value[4..$].ptr, rhs.value[4..$].ptr, ls + 1);  // +1 for subtype
    case Type.OID:
        return memcmp(lhs.value.ptr, rhs.value.ptr, 12);
    case Type.BOOLEAN:
        return lhs.value[0] - rhs.value[0];
    case Type.DATE, Type.TIMESTAMP:
        // TODO: Fix for correct comparison
        // Following comparison avoids non-pure function call.
        immutable l = lhs._int64();
        immutable r = rhs._int64();

        if (l < r)
            return -1;
        return l == r ? 0 : 1;
    case Type.REGEX:
        immutable re1 = lhs.regex;
        immutable re2 = rhs.regex;

        immutable r = strcmp(re1.field[0].ptr, re2.field[0].ptr);
        if (r != 0)
            return r;
        return strcmp(re1.field[1].ptr, re2.field[1].ptr);
    case Type.DBPOINTER:
        immutable ls = lhs.valueSize;
        immutable rs = rhs.valueSize;

        if ((ls - rs) != 0)
            return ls - rs < 0 ? -1 : 1;
        return memcmp(lhs.str.ptr, rhs.str.ptr, ls);
    case Type.JS_CODE_W_SCOPE:
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
    }
}


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
 * Exception type used by mongo.bson module
 */
class BSONException : Exception
{
    this(string msg)
    {
        super(msg);
    }
}


/**
 * The BSON ObjectId Datatype
 *
 * See_Also:
 *  $(LINK2 http://www.mongodb.org/display/DOCS/Object+IDs, Object IDs)
 */
struct ObjectId
{
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


    @trusted
    shared static this()
    {
        // import std.md5;  // TODO: Will be replaced with std.digest
        import std.digest.md;
        import std.socket;

        ubyte[16] digest;

        digest=md5Of(Socket.hostName());
        //sum(digest, Socket.hostName());
        ourMachine[] = digest[0..3];
    }


    unittest
    {
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
    bool opEquals(ref const ObjectId other) const pure nothrow
    {
        return data == other.data;
    }


    @safe
    string toString() const pure nothrow
    {
        return data.toHex();
    }

    @safe
    immutable(ubyte)[12] id() const pure nothrow {
        return data;
    }
}


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


//private:


// Phobos does not have 0-filled hex conversion functions?


@trusted
string toHex(in ubyte[] nums) pure nothrow
{
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
ubyte[] fromHex(in string hex) pure nothrow
{
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
        else static if (is(T:string[])) {
            return STRING_array;
        }
        else static if (is(T:const(BSON!true)[]) || is(T:const(BSON!false)[])) {
            return DOCUMENT_array;
        }
        else  {
            static assert(0, "Unsupport type "~T.stringof);
        }
    }
}

unittest
{
    static struct Test
    {
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


class BSON(bool key_sort_flag=true) {
    package Type _type;
    package BinarySubType subtype;
    private BSON members; // List of members
    private immutable(char)[] _key;
    public bool typedarray; // Start standard type array as Binary data (like double[])
    public bool no_duble; // This will prevent the BSON object from creating double or multiple members
    struct CodeScope {
        immutable(char)[] code;
        BSON document;
    }
    private struct _Date  {
        short _year  = 1;
        Month _month = Month.jan;
        ubyte _day   = 1;
    }

    immutable(char)[] key() @safe pure nothrow const {
        return _key;
    }

    union Value {
        double number;
        float number32;
        immutable(char)[] text;
        bool boolean;
        BSON document;
        ObjectId oid;
        private _Date _date;
        @property final Date date() const {
            return Date(_date._year, _date._month, _date._day);
        }
        @property final void date(ref const(Date) d) nothrow {
            _date._year=d.year;
            _date._month=d.month;
            _date._day=d.day;
        }

//        Date date;
        int int32;
        long int64;
        uint uint32;
        ulong uint64;
        immutable(char)[][2] regex;
        CodeScope codescope;
        const(ubyte)[] binary;
        const(bool)[] bool_array;
        const(int)[] int32_array;
        const(uint)[] uint32_array;
        const(long)[] int64_array;
        const(ulong)[] uint64_array;
        const(float)[] float_array;
        const(double)[] double_array;
        string[] text_array;
        BSON[] bson_array;
/*
        immutable(char)[][] atext;
        int[] aint32;
        immuatble(long[] aint64;
*/
    };
    this() {
        _type=Type.DOCUMENT;
    }
    @property
    size_t id() const pure nothrow {
        return cast(size_t)(cast(void*)this);
    }
    auto get(T)() {
        static if (is(T==double)) {
            assert(_type == Type.DOUBLE);
            return value.number;
        }
        else static if (is(T==string)) {
            assert(_type == Type.STRING);
            return value.text;
        }
        else static if (is(T==bool)) {
            assert(_type == Type.BOOLEAN);
            return value.boolean;
        }
        else static if (is(T==BSON)) {
            assert(_type == Type.DOCUMENT);
            return value.document;
        }
        else static if (is(T==ObjectId)) {
            assert(_type == Type.OID);
            return value.oid;
        }
        else static if (is(T==int)) {
            assert(_type == Type.INT32);
            return value.int32;
        }
        else static if (is(T==long)) {
            assert(_type == Type.INT64);
            return value.int64;
        }
        else static if (is(T==Date)) {
            assert(_type == Type.DATE);
            return value.date;
        }
        else static if (is(T==CodeScope)) {
            assert(_type == Type.JS_CODE_SCOPE);
            return value.codescope;
        }
        else static if (is(T:const(ubyte)[])) {
            assert(_type == Type.BINARY);
            return value.binary;
        }
        else static if (is(T:const(bool)[])) {
            assert(_type == Type.ARRAY);
            assert(subtype == BinarySubType.BOOLEAN_array);
            return value.bool_array;
        }
        else static if (is(T:const(int)[])) {
            assert(_type == Type.ARRAY);
            assert(subtype == BinarySubType.INT32_array);
            return value.int32_array;
        }
        else static if (is(T:const(uint)[])) {
            assert(_type == Type.ARRAY);
            assert(subtype == BinarySubType.UINT32_array);
            return value.uint32_array;
        }
        else static if (is(T:const(ulong)[])) {
            assert(_type == Type.ARRAY);
            assert(subtype == BinarySubType.UINT64_array);
            return value.uint64_array;
        }
        else static if (is(T:const(long)[])) {
            assert(_type == Type.ARRAY);
            assert(subtype == BinarySubType.INT64_array);
            return value.int64_array;
        }
        else static if (is(T:const(float)[])) {
            assert(_type == Type.ARRAY);
            assert(subtype == BinarySubType.FLOAT_array);
            return value.float_array;
        }
        else static if (is(T:const(double)[])) {
            assert(_type == Type.ARRAY);
            assert(subtype == BinarySubType.DOUBLE_array);
            return value.double_array;
        }
        else static if (is(T:U[],U) && isStringType!U) {
            assert(_type == Type.ARRAY);
            assert(subtype == BinarySubType.STRING_array);
            return value.text_array;
        }
        else static if (is(T==BSON[])) {
            assert(_type == Type.ARRAY);
            assert(subtype == BinarySubType.DOCUMENT_array);
            return value.bson_array;
        }
        else {
            static assert(0, "Type "~T.stringof~ "is not supported by this function");
        }


    }
    package Value value;
    // package void sort_keys() {
    //     if ( (type == Type.DOCUMENT) ) {
    //         BSON[] barry;
    //         import std.algorithm.mutation : SwapStrategy;
    //         for(auto b=members; b !is null; b=b.members) {
    //             barry~=b;
    //         }
    //         if ( barry.length > 1 ) {
    //             // Sort by key
    //             sort!("a.key < b.key", SwapStrategy.stable)(barry);
    //             BSON prev;
    //             foreach(i,ref b;barry) {
    //                 if ( i > 0 ) {
    //                     prev.members=b;
    //                 }
    //                 prev=b;
    //             }
    //             prev.members=null;
    //             this.members=barry[0];
    //         }
    //     }
    // }
    bool isDocument() {
        return ( (type == Type.DOCUMENT) || (type == Type.ARRAY) );
    }
    @trusted
    bool append(T)(Type type, in string key, T x, BinarySubType subtype=BinarySubType.generic) {
        bool result=false;
        BSON elm=new BSON;
        with (Type) final switch (type) {
            case MIN:
            case NONE:
                break;
            case DOUBLE:
            case FLOAT:
                static if (is(T:double)) {
                    elm.value.number=cast(double)x;
                    result=true;
                }
                break;
            case REGEX:
                static if (is(T==U[],U)) {
                    if (x.length>1) {
                        immutable(char)[][2] regex;
                        static if (is(U==immutable(char)[])) {
                            regex[0]=x[0];
                            regex[1]=x[1];
                            elm.value.regex=regex;
                            result=true;
                        }
                        else static if (is(U:const(char)[])) {
                            regex[0]=x[0].idup;
                            regex[1]=x[1].idup;
                            elm.value.regex=regex;
                            result=true;
                        }
                    }
                }
                break;
            case STRING:
            case JS_CODE:
            case SYMBOL:
                static if (is(T==immutable(char)[])) {
                    elm.value.text=x;
                    result=true;
                }
                else static if (is(T:const(char)[])) {
                    elm.value.text=x.idup;
                    result=true;
                }
                break;
            case JS_CODE_W_SCOPE:
                static if (is(T==CodeScope)) {
                    elm.value.codescope=x;
                    result=true;
                }
                break;
            case DOCUMENT:
                static if (is(T:BSON)) {
                    elm.value.document=x;
                    result=true;
                }
                else {
                    assert(0, "Unsupported type "~T.stringof~" not a valid "~to!string(type));
                }
                break;
            case ARRAY:
                static if (is(T==BSON)) {
                    elm.value.document=x;
                    result=true;
                }
                else static if (is(T:U[],U) && !isStringType!T) {
                    static if (is(U:const(bool))) {
//                        elm.subtype=BinarySubType.BOOLEAN_array;
                        elm.value.bool_array=x;
                        result=true;
                    }
                    else static if (is(U:const(char)[])) {
                        elm.value.text_array=x;
                        result=true;
                    }
                    else static if (is(immutable U==immutable uint )) {
                        elm.value.uint32_array=x;
                        result=true;
                    }
                    else static if (is(immutable U==immutable int )) {
                        elm.value.int32_array=x;
                        result=true;
                    }
                    else static if (is(immutable U==immutable long )) {
                        elm.value.int64_array=x;
                        result=true;
                    }
                    else static if (is(immutable U==immutable float)) {
                        elm.value.float_array=x;
                    }
                    else static if (is(immutable U==immutable double)) {
                        elm.value.double_array=x;
                        result=true;
                    }
                    else static if (is(U:BSON)) {
                         elm.value.bson_array=x;
                        result=true;
                    }
                    else {
                        assert(0, "Unsupported type "~T.stringof);
                    }
                }
                else {
                     assert(0, "Unsupported type "~T.stringof~" does not seem to be a valid native array");
                }
                break;
            case BINARY:
                static if (is(T:U[],U)) {
                    static if (is(immutable U==immutable ubyte)) {
                        elm.value.binary=x;
                    }
                    else static if (is(immutable U==immutable int)) {
                        elm.value.int32_array=x; //(cast(immutable(ubyte)*)x.ptr)[0..x.length/int.sizeof];
                    }
                    else static if (is(immutable U==immutable uint)) {
                        elm.value.uint32_array=x;
                    }
                    else static if (is(immutable U==immutable long)) {
                        elm.value.int64_array=x;
                    }
                    else static if (is(immutable U==immutable ulong)) {
                        elm.value.uint64_array=x;
                    }
                    else static if (is(immutable U==immutable double)) {
                        elm.value.double_array=x;
                    }
                    else static if (is(immutable U==immutable float)) {
                        elm.value.float_array=x;
                    }
                }
                else {
                    static if (__traits(compiles,x.ptr)) {
                        elm.value.binary=((cast(ubyte*)x.ptr)[0..T.sizeof]).idup;
                    }
                    else {
                        elm.value.binary=((cast(ubyte*)&x)[0..T.sizeof]).idup;
                    }
                }
//                Stdout.formatln("Again before subtype Append int[] length={}",elm.value.int32_array.length);
                elm.subtype=subtype;
//                Stdout.formatln("Again Append int[] length={}",elm.value.int32_array.length);
                result=true;
                break;
            case UNDEFINED:
                result=true;
                break;
            case OID:
                static if (is(T==ObjectId)) {
                    result=true;
                    elm.value.oid=x;
                }
                break;
            case BOOLEAN:
                static if (is(T:long) || is(T:ulong) ) {
                    elm.value.boolean=x!=0;
                    result=true;
                }
                else static if (is(T:real)) {
                    elm.value.boolean=x!=0.0;
                    result=true;
                }
                else static if (is(T:bool)) {
                    elm.value.boolean=cast(bool)x;
                    result=true;
                }
                break;
            case DATE:
                static if (is(T:Date)) {
                    elm.value.date=x;
                    result=true;
                }
                break;
            case NULL:
                result=true;
                break;
            case DBPOINTER:
                break;
            case INT32:
                static if (is(T:int)) {
                    elm.value.int32=cast(int)x;
                    result=true;
                }
                break;
            case UINT32:
                static if (is(T:uint)) {
                    elm.value.uint32=cast(uint)x;
                    result=true;
                }
                break;
            case INT64:
                static if (is(T:long)) {
                    elm.value.int64=cast(long)x;
                    result=true;
                }
                break;
            case UINT64:
                static if (is(T:ulong)) {
                    elm.value.uint64=cast(ulong)x;
                    result=true;
                }
                break;
            case TIMESTAMP:
                static if (is(T==DateTime)) {
                    auto st=SysTime(x);
                    elm.value.int64=st.stdTime;
                    result=true;
                }
                break;
            case MAX:
            }
        if (result) {
            if ( no_duble ) {
                remove(key);
            }
            elm._type=type;
            elm.subtype=subtype;
            elm._key=key;
            elm.members=members;
            members=elm;
        }
        return result;
    }

    void opIndexAssign(T)(T x, in string key) {
        bool result;
        static if (is(T==bool)) {
            result=append(Type.BOOLEAN, key, x);
        }
        else static if (is(T:const(char)[])) {
            result=append(Type.STRING, key, x);
        }
        else static if (is(T==BSON)) {
            result=append(Type.DOCUMENT, key, x);
        }
        else static if (is(T:const(int))) {
            result=append(Type.INT32, key, x);
        }
        else static if (is(T:const(long))) {
            result=append(Type.INT64, key, x);
        }
        else static if (is(T==double)) {
            result=append(Type.DOUBLE, key, x);
        }
        else static if (is(T:const(ubyte)[])) {
            result=append(Type.BINARY, key, x);
        }
        else static if (is(T:const(Date))) {
            result=append(Type.DATE, key, x);
        }
        else static if (is(T:const(DateTime))) {
            result=append(Type.TIMESTAMP, key, x);
        }
        else static if (is(T:U[],U)) {
            if (typedarray && is(U : const(double) ) ) {
                result=append(Type.BINARY, key, x, getSubtype!T);
            }
            else {
                result=append(Type.ARRAY, key, x, getSubtype!T);
            }
        }
        else {
            writefln("Check %s", is(T:const(ubyte)[]));
            static assert(0, "opIndexAssign does not support type "~T.stringof~" use append member function instead");
        }
/*
        else static if (is(T.length) && is(T.opApply) {
           BSON[] bsons=new BSON[x.length];
           size_t i;
           foreach(v;x) {
               BSON elm=new BSON;
               elm[
               bson[i]=new BSON;
           }
        }
*/

    }

   BSON opIndex(const(char)[] key) {
        BSON result;
        foreach(b;this) {
            if ( b.key == key ) {
                result=b;
                break;
            }
        }
        if ( result ) {
            return result;
        }
        throw new BSONException("Member '"~key.idup~"' not defined");
        assert(0);
    }

    Type type() pure const nothrow {
        return _type;
    }

    immutable(char)[] toInfo() const {
        immutable(char)[] result;
        with(Type) final switch(_type) {
            case MIN:
            case MAX:
            case NONE:
            case UNDEFINED:
            case NULL:
                result=to!string(_type);
                break;
            case DOUBLE:
                result~=Format("{} {}", to!string(_type), value.number);
                break;
            case FLOAT:
                result~=Format("{} {}", to!string(_type), value.number32);
                break;
            case STRING:
            case REGEX:
            case JS_CODE:
            case SYMBOL:
                result~=Format("**{} {}", to!string(_type), value.text);
                break;
            case JS_CODE_W_SCOPE:
                result~=Format("{} {} {:X}", to!string(_type), value.codescope.code, value.codescope.document.id);
                break;
            case DOCUMENT:
            case ARRAY:
                result~=Format("##{} {:X}", to!string(_type), this.id);
                break;
            case BINARY:

                // Todo
                break;
            case OID:
                result~=Format("{} {:X}", to!string(_type), value.oid.id);
                break;
            case BOOLEAN:
                result~=Format("{} {}", to!string(_type), value.boolean);
                break;
            case DATE:
                result~=Format("{} {}", to!string(_type), value.date);
                break;
            case DBPOINTER:
                result=to!string(_type);
                break;
            case INT32:
                result~=Format("{} {}", to!string(_type), value.int32);
                break;
            case UINT32:
                result~=Format("{} {}", to!string(_type), value.uint32);
                break;
            case INT64:
                result~=Format("{} {}", to!string(_type), value.int64);
                break;
            case UINT64:
                result~=Format("{} {}", to!string(_type), value.uint64);
                break;
            case TIMESTAMP:
                result~=Format("{} {}", to!string(_type), value.int64);
                break;

            }
        return result;
    }

    string_t toText(string_t=string)() {
        string_t object_toText(BSON obj) {
            string_t buf;
            bool any=false;
            immutable bool array=(obj.type == Type.ARRAY);
            buf = (array)?"[":"{";
            foreach(k, b; obj) {
                if(any)
                    buf ~= ",\n";
                any = true;
                if (!array) {
                    buf ~= to!string_t(k);
                    buf ~= " : ";
                }
                if ( b.isDocument ) {
                    buf~=object_toText(b.value.document);
                }
                else {
                    buf~=b.toText!string_t;
                }
            }
            buf ~= (array)?"]":"}";
            return buf;
        }
        string_t binary_toText() {
            string_t buf;
            void loop(T)(T array) {
                bool any=false;
                foreach(n; array) {
                    if (any) {
                        buf ~=", ";
                    }
                    any=true;
                    buf~=to!string_t(n);
                }
            }

            buf="[";
            with (BinarySubType) switch (subtype) {
                case INT32_array:
                    loop(value.int32_array);
                    break;
                case UINT32_array:
                    loop(value.uint32_array);
                    break;
                case INT64_array:
                    loop(value.int64_array);
                    break;
                case UINT64_array:
                    loop(value.uint64_array);
                    break;
                case FLOAT_array:
                    loop(value.float_array);
                    break;
                case DOUBLE_array:
                    loop(value.double_array);
                    break;
                default:
                    loop(value.binary);

                }
            buf~="]";
            return buf;
        }

        with(Type) final switch(_type) {
            case MIN:
            case MAX:
            case NONE:
                return '"'~to!string_t(to!string(_type))~'"';
            case UNDEFINED:
                return "undefined";
            case NULL:
                return "null";
            case DOUBLE:
                return to!string_t(value.number);
            case FLOAT:
                return to!string_t(value.number32);
            case STRING:
            case REGEX:
            case JS_CODE:
            case SYMBOL:
                return to!string_t('"'~value.text~'"') ;
            case JS_CODE_W_SCOPE:
                return to!string_t("["~value.codescope.code~", "~to!string(value.codescope.document.id)~"]");
            case DOCUMENT:
            case ARRAY:
                return object_toText(this);
            case BINARY:
                return binary_toText();
            case OID:
                return to!string_t(toHex(value.oid.id));
            case BOOLEAN:
                return to!string_t(value.boolean);
            case DATE:
                return to!string_t('"'~value.date.toString~'"');
            case DBPOINTER:
                return to!string_t('"'~to!string(_type)~'"');
            case INT32:
                return to!string_t(value.int32);
            case UINT32:
                return to!string_t(value.uint32);
            case INT64:
                return '"'~to!string_t(value.int64)~'"';
            case UINT64:
                return '"'~to!string_t(value.uint64)~'"';
            case TIMESTAMP:
                return '"'~to!string_t(value.int64)~'"';
            }
        assert(0, "Unmatch type");
    }

    static void native_append(T)(T x, ref immutable(ubyte)[] data) {
        static if (is(T:const(bool))) {
            data~=(x)?one:zero;
        }
        else static if (is(T:const(int)) || is(T:const(long)) || is(T:const(double)) ) {
            data~=nativeToLittleEndian(x);
        }
        else static if (is(T:string)) {
            data~=nativeToLittleEndian(cast(uint)x.length+1);
            data~=x;
            data~=zero;
        }
        else static if (is(T:BSON)) {
            data~=x.expand;
        }
        else {
            static assert(0, "Unsupported type "~T.stringof);
        }

    }

    enum zero=cast(ubyte)0;
    enum one=cast(ubyte)1;

    void appendData(ref immutable(ubyte)[] data) {
        with(Type) final switch(_type) {
            case NULL:
                data~=zero;
                break;
            case NONE:
                break;
            case DOUBLE:
                data~=nativeToLittleEndian(value.number);
                break;
            case FLOAT:
                data~=nativeToLittleEndian(value.number32);
                break;
            case STRING:
            case SYMBOL:
            case JS_CODE:
                data~=nativeToLittleEndian(cast(uint)value.text.length+1);
                data~=value.text;
                data~=zero;
                break;
            case DOCUMENT:
                data~=value.document.expand;
                break;
            case ARRAY:
                if ( (subtype & BinarySubType.userDefined) == 0 ) {
                    data~=value.document.expand;
                }
                else {
                    immutable(ubyte)[] local;
                    void local_array_expand(T)(Type type) {
                        foreach(i,a;get!T) {
                            local~=subtype & 0x7F;
                            local~=tango.text.convert.Integer.toString(i);
                            local~=zero;
                            native_append(a, local);
                        }

                    }
                    with(BinarySubType) switch(subtype) {
                        case BOOLEAN_array:
                            local_array_expand!(const(bool)[])(_type);
                            break;
                        case INT32_array:
                            local_array_expand!(const(int)[])(_type);
                            break;
                        case UINT32_array:
                            local_array_expand!(const(uint)[])(_type);
                            break;
                        case INT64_array:
                            local_array_expand!(const(long)[])(_type);
                            break;
                        case UINT64_array:
                            local_array_expand!(const(ulong)[])(_type);
                            break;
                        case DOUBLE_array:
                            local_array_expand!(const(double)[])(_type);
                            break;
                        case FLOAT_array:
                            local_array_expand!(const(float)[])(_type);
                            break;
                        case STRING_array:
                            local_array_expand!(string[])(_type);
                            break;
                        case DOCUMENT_array:
                            local_array_expand!(BSON[])(_type);
                            break;
                        default:
                            throw new BSONException("Subtype "~to!string(subtype)~" not supported by "~to!string(_type));
                        }
                    data~=nativeToLittleEndian(cast(uint)(local.length+uint.sizeof+zero.sizeof));
                    data~=local;
                    data~=zero;
                }

                break;
            case BINARY:
                auto buf=subtype_buffer;
                data~=nativeToLittleEndian(cast(int)(buf.length));
                data~=subtype;
                data~=buf;
                break;
            case UNDEFINED:
            case MAX:
            case MIN:
                break;
            case OID:
                data~=value.oid.id;
                break;
            case BOOLEAN:
                data~=(value.boolean)?one:zero;
                break;
            case DATE:
                break;
            case REGEX:
                data~=value.regex[0];
                data~=zero;
                data~=value.regex[1];
                data~=zero;
                break;
            case DBPOINTER:
                break;
            case JS_CODE_W_SCOPE:
                immutable(ubyte)[] local=expand();
                // Size of block
                data~=nativeToLittleEndian(cast(uint)(local.length+uint.sizeof+value.text.length+1));
                data~=nativeToLittleEndian(cast(uint)(value.text.length+1));
                data~=value.text;
                data~=zero;
                data~=local;
                break;
            case INT32:
                data~=nativeToLittleEndian(value.int32);
                break;
            case UINT32:
                data~=nativeToLittleEndian(value.uint32);
                break;
            case TIMESTAMP:
            case INT64:
                data~=nativeToLittleEndian(value.int64);
                break;
            case UINT64:
                data~=nativeToLittleEndian(value.uint64);
                break;

            }
    }

    immutable(ubyte)[] expand() {
        immutable(ubyte)[] local_expand() {
            immutable(ubyte)[] data;
            foreach(e; iterator!key_sort_flag) {
                data~=e._type;
                data~=e.key;
                data~=zero;
                with(Type) final switch(e._type) {
                    case NONE:
                        data~=zero;
                        break;
                    case DOUBLE:
                        data~=nativeToLittleEndian(e.value.number);
                        break;
                    case FLOAT:
                        data~=nativeToLittleEndian(e.value.number32);
                        break;
                    case STRING:
                    case SYMBOL:
                    case JS_CODE:
                        data~=nativeToLittleEndian(cast(uint)e.value.text.length+1);
                        data~=e.value.text;
                        data~=zero;
                        //dgelm(data);
                        break;
                    case DOCUMENT:
                        data~=e.value.document.expand;
                        break;
                    case ARRAY:
                        e.appendData(data);
                        break;
                    case BINARY:
                        e.appendData(data);
                        break;
                    case UNDEFINED:
                    case NULL:
                    case MAX:
                    case MIN:
                        break;
                    case OID:
                        data~=e.value.oid.id;
                        break;
                    case BOOLEAN:
                        data~=(e.value.boolean)?one:zero;
                        break;
                    case DATE:
                        break;
                    case REGEX:
                        data~=e.value.regex[0];
                        data~=zero;
                        data~=e.value.regex[1];
                        data~=zero;
                        break;
                    case DBPOINTER:
                        break;
                    case JS_CODE_W_SCOPE:
                        immutable(ubyte)[] local=e.expand();
                        // Size of block
                        data~=nativeToLittleEndian(cast(uint)(local.length+uint.sizeof+e.value.text.length+1));
                        data~=nativeToLittleEndian(cast(uint)(e.value.text.length+1));
                        data~=e.value.text;
                        data~=zero;
                        data~=local;
                        break;
                    case INT32:
                        data~=nativeToLittleEndian(e.value.int32);
                        //dgelm(data);
                        break;
                    case UINT32:
                        data~=nativeToLittleEndian(e.value.uint32);
                        //dgelm(data);
                        break;
                    case TIMESTAMP:
                    case INT64:
                        data~=nativeToLittleEndian(e.value.int64);
                        //dgelm(data);
                        break;
                    case UINT64:
                        data~=nativeToLittleEndian(e.value.uint64);
                        //dgelm(data);
                        break;

                    }
            }
            return data;
        }
        immutable(ubyte)[] data;
        scope immutable(ubyte)[] local=local_expand();
        data~=nativeToLittleEndian(cast(uint)(local.length+uint.sizeof+zero.sizeof));
        data~=local;
        data~=zero;
        return data;
    }

//    version(none)
    unittest {
        BSON bson1=new BSON;


        bson1["int"]=3;
        bson1["number"]=1.7;
        bson1["bool"]=true;
        bson1["text"]="sometext";

        assert(!bson1.duble);
        {
            auto iter=bson1.iterator;
//            iter.popFront;
            assert(!iter.empty);
            assert(iter.front.key == "text");
            iter.popFront;
            assert(!iter.empty);
            assert(iter.front.key == "bool");
            iter.popFront;
            assert(iter.front.key == "number");
            iter.popFront;
            assert(iter.front.key == "int");
            iter.popFront;
            assert(iter.empty);
        }

        immutable(ubyte)[] data1;
        data1=bson1.expand();

        {
            auto doc=Document(data1);
            assert(doc.hasElement("int"));
            assert(doc.hasElement("bool"));
            assert(doc.hasElement("number"));
            assert(doc.hasElement("text"));
            assert(doc.keys.length == 4);
            assert(doc["int"].get!int == 3);
            assert(doc["bool"].get!bool);
            assert(doc["number"].get!double == 1.7);
            assert(doc["text"].get!string == "sometext");

        }

        BSON bson2=new BSON;
        bson2["x"] = 10;
        bson1["obj"]=bson2;

        data1=bson1.expand();
        {
            auto doc1b=Document(data1);
            assert(doc1b.hasElement("obj"));
            assert(doc1b["obj"].isDocument);
            auto subobj=doc1b["obj"].get!Document;

            assert(subobj.hasElement("x"));
            assert(subobj["x"].isNumber);
            assert(subobj["x"].get!int == 10);
        }

    }

    unittest {
        // Test D array types
        BSON bson;
        { // Boolean array
            immutable bools=[true, false, true];
            bson=new BSON;
            bson["bools"]=bools;
            auto doc=Document(bson.expand);
            assert(doc.hasElement("bools"));
            auto subarray=doc["bools"].get!Document;

            assert(subarray[0].get!bool == bools[0]);
            assert(subarray[1].get!bool == bools[1]);
            assert(subarray[2].get!bool == bools[2]);
        }
        { // Int array
            immutable(int[]) int32s=[7, 9, 13];
            bson=new BSON;
            bson["int32s"]=int32s;

            auto doc=Document(bson.expand);
            assert(doc.hasElement("int32s"));
            auto subarray=doc["int32s"].get!Document;

            assert(subarray[0].get!int == int32s[0]);
            assert(subarray[1].get!int == int32s[1]);
            assert(subarray[2].get!int == int32s[2]);
        }

        { // Int array
            immutable(long[]) int64s=[7, 9, 13];
            bson=new BSON;
            bson["int64s"]=int64s;

            auto doc=Document(bson.expand);
            assert(doc.hasElement("int64s"));
            auto subarray=doc["int64s"].get!Document;

            assert(subarray[0].get!long == int64s[0]);
            assert(subarray[1].get!long == int64s[1]);
            assert(subarray[2].get!long == int64s[2]);
        }
        { // double array
            immutable(double[]) doubles=[7.0, 9.0, 13.0];
            bson=new BSON;
            bson["doubles"]=doubles;

            auto doc=Document(bson.expand);
            assert(doc.hasElement("doubles"));
            auto subarray=doc["doubles"].get!Document;

            assert(subarray[0].get!double == doubles[0]);
            assert(subarray[1].get!double == doubles[1]);
            assert(subarray[2].get!double == doubles[2]);
        }
        { // string array
            string[] strings=["Hej", "med", "dig"];
            bson=new BSON;
            bson["strings"]=strings;

            auto doc=Document(bson.expand);
            assert(doc.hasElement("strings"));
            auto subarray=doc["strings"].get!Document;

            assert(subarray[0].get!string == strings[0]);
            assert(subarray[1].get!string == strings[1]);
            assert(subarray[2].get!string == strings[2]);
        }
        {
            BSON[] bsons;
            bson=new BSON;
            bson["x"]=10;
            bsons~=bson;
            bson=new BSON;
            bson["y"]="kurt";
            bsons~=bson;
            bson=new BSON;
            bson["z"]=true;
            bsons~=bson;
            bson=new BSON;

            bson["bsons"]=bsons;

            auto data=bson.expand;

            auto doc=Document(bson.expand);

            assert(doc.hasElement("bsons"));

            auto subarray=doc["bsons"].get!Document;
            assert(subarray[0].get!Document["x"].get!int == 10);
            assert(subarray[1].get!Document["y"].get!string == "kurt");
            assert(subarray[2].get!Document["z"].get!bool == true);

        }
    }

    unittest  {
        // Buffer as binary arrays
        BSON bson;
        {

            bson=new BSON;
            bson.typedarray=true;
            { // Typedarray int32
                immutable(int[]) int32s= [ -7, 9, -13];
                bson["int32s"]=int32s;
                auto doc = Document(bson.expand);


                assert(doc.hasElement("int32s"));
                auto element=doc["int32s"];
                assert(element.get!(immutable(int)[]).length == int32s.length);
                assert(element.get!(immutable(ubyte)[]) == cast(immutable(ubyte)[])int32s);
                assert(element.get!(immutable(int)[]) == int32s);
            }

            { // Typedarray uint32
                immutable(uint[]) uint32s= [ 7, 9, 13];
                bson["uint32s"]=uint32s;
                auto doc = Document(bson.expand);

                assert(doc.hasElement("uint32s"));
                auto element=doc["uint32s"];
                assert(element.get!(immutable(uint)[]).length == uint32s.length);
                assert(element.get!(immutable(ubyte)[]) == cast(immutable(ubyte)[])uint32s);
                assert(element.get!(immutable(uint)[]) == uint32s);
            }

            { // Typedarray int64
                immutable(long[]) int64s= [ -7_000_000_000_000, 9_000_000_000_000, -13_000_000_000_000];
                bson["int64s"]=int64s;
                auto doc = Document(bson.expand);

                assert(doc.hasElement("int64s"));
                auto element=doc["int64s"];
                assert(element.get!(immutable(long)[]).length == int64s.length);
                assert(element.get!(immutable(ubyte)[]) == cast(immutable(ubyte)[])int64s);
                assert(element.get!(immutable(long)[]) == int64s);
            }


            { // Typedarray uint64
                immutable(long[]) uint64s= [ -7_000_000_000_000, 9_000_000_000_000, -13_000_000_000_000];
                bson["uint64s"]=uint64s;
                auto doc = Document(bson.expand);

                assert(doc.hasElement("uint64s"));
                auto element=doc["uint64s"];
                assert(element.get!(immutable(long)[]).length == uint64s.length);
                assert(element.get!(immutable(ubyte)[]) == cast(immutable(ubyte)[])uint64s);
                assert(element.get!(immutable(long)[]) == uint64s);
            }

            { // Typedarray number64
                immutable(double[]) number64s= [ -7.7e9, 9.9e-4, -13e200];
                bson["number64s"]=number64s;
                auto doc = Document(bson.expand);

                assert(doc.hasElement("number64s"));
                auto element=doc["number64s"];
                assert(element.get!(immutable(double)[]).length == number64s.length);
                assert(element.get!(immutable(ubyte)[]) == cast(immutable(ubyte)[])number64s);
                assert(element.get!(immutable(double)[]) == number64s);
            }


            { // Typedarray number32
                immutable(float[]) number32s= [ -7.7e9, 9.9e-4, -13e20];
                bson["number32s"]=number32s;
                auto doc = Document(bson.expand);

                assert(doc.hasElement("number32s"));
                auto element=doc["number32s"];
                assert(element.get!(immutable(float)[]).length == number32s.length);
                assert(element.get!(immutable(ubyte)[]) == cast(immutable(ubyte)[])number32s);
                assert(element.get!(immutable(float)[]) == number32s);
            }

//            immutable(int[]) uint32s= [ 7, 9, -13];

//            assert(0);
        }
    }

    bool duble() {
        auto iter=iterator;
        for(; !iter.empty; iter.popFront) {
            auto dup_iter=iter;
            for(dup_iter.popFront; !dup_iter.empty; dup_iter.popFront) {
                if (dup_iter.front.key == iter.front.key) {
                    return true;
                }
            }
        }
        return false;
    }

    bool remove(string key) {
        auto iter=iterator;
        bool result;
        BSON prev;
        for(; !iter.empty; iter.popFront) {
            if ( iter.front.key == key ) {
                // If the key is found then remove it from the change
                if ( members == iter.front ) {
                    // Remove the root member
                    members=members.members;
                }
                else {
                    prev.members=iter.front.members;
                }
                result = true;
            }
            prev=iter.front;
        }
        return result;
    }

    unittest {
        // Remove and duble check
        BSON bson;
        bson=new BSON;
        bson["a"]=3;
        bson["b"]=13;

        assert(bson["a"].get!int == 3);
        bson["a"] = 4;

        uint i;
        foreach(b; bson) {
            if ( b.key == "a" ) {
                i++;
            }
        }
        assert(i == 2);
        assert(bson.duble);

        bson=new BSON;
        bson.no_duble=true;

        bson["a"] = 3;
        bson["b"] = 13;
        bson["a"] = 4;

        assert(!bson.duble);

        assert(bson["a"].get!int==4);

    }

    Iterator!F iterator(bool F=false)() {
        return Iterator!F(this);
    }

    const(ubyte)[] subtype_buffer() {
        with(BinarySubType) switch(subtype) {
            case generic:
            case func:
            case binary:
            case uuid:
            case md5:
            case userDefined:
                return value.binary;
            case INT32_array:
                return (cast(const(ubyte)*)(value.int32_array.ptr))[0..value.int32_array.length*int.sizeof];
            case UINT32_array:
                return (cast(const(ubyte)*)(value.uint32_array.ptr))[0..value.uint32_array.length*uint.sizeof];
            case INT64_array:
                return (cast(const(ubyte)*)(value.int64_array.ptr))[0..value.int64_array.length*long.sizeof];
            case UINT64_array:
                return (cast(const(ubyte)*)(value.uint64_array.ptr))[0..value.uint64_array.length*ulong.sizeof];
            case DOUBLE_array:
                return (cast(const(ubyte)*)(value.double_array.ptr))[0..value.double_array.length*double.sizeof];
            case FLOAT_array:
                return (cast(const(ubyte)*)(value.float_array.ptr))[0..value.float_array.length*float.sizeof];
            default:
                throw new BSONException("Binary suptype "~to!string(subtype)~" not supported for buffer");
            }

    }

    string[] keys() pure const nothrow {
        string[] result;
        void foreach_key(const BSON current) pure nothrow {
            if ( current !is null ) {
                foreach_key(current.members);
                result~=current.key;
            }
        }

        foreach_key(this.members);
        return result;
    }

    unittest {
        // Test keys function
        // and the sorted BSON
        {
            auto bson=new BSON!true;
            auto some_keys=["kurt", "abe", "ole"];
            bson[some_keys[0]]=0;
            bson[some_keys[1]]=1;
            bson[some_keys[2]]=2;
            auto keys=bson.keys;
            // writefln("keys=%s", keys);
            auto data=bson.expand;
            auto doc=Document(data);
            // writefln("doc.keys=%s", doc.keys);
            // Check that doc.keys are sorted
            assert(doc.keys == ["abe", "kurt", "ole"]);
        }
        {
            BSON!true[] array;
            for(int i=10; i>-7; i--) {
                auto len=new BSON!true;
                len["i"]=i;
                array~=len;
            }
            auto bson=new BSON!true;
            bson["array"]=array;
            auto data=bson.expand;
            auto doc=Document(data);
            auto doc_array=doc["array"].get!Document;
            foreach(i,k;doc_array.keys) {
                assert(to!string(i) == k);
            }
        }

    }

    int opApply(scope int delegate(BSON bson) dg) {
        return iterator.opApply(dg);
    }

    int opApply(scope int delegate(in string key, BSON bson) dg) {
        return iterator.opApply(dg);
    }

    struct Iterator(bool key_sort_flag) {
        private BSON owner;
        static if (key_sort_flag) {
            private string[] sorted_keys;
            private string[] current_keys;
        }
        else {
            private BSON current;
        }
        this(BSON owner) {
            this.owner=owner;
            static if ( key_sort_flag ) {
                sorted_keys=owner.keys;
                sort!("a < b", SwapStrategy.stable)(sorted_keys);
                current_keys=sorted_keys;
            }
            else {
                this.current=owner.members;
            }
        }
        void popFront()
            in {
                assert(owner !is null);
                static if ( !key_sort_flag ) {
                    assert(current !is owner,"Circular reference member "~current.key~" points to it self");
                }
            }
        body {
            static if ( key_sort_flag ) {
                current_keys=current_keys[1..$];
            }
            else {
                current=current.members;
            }
        }
        BSON front() {
            static if ( key_sort_flag ) {
                assert ( current_keys.length > 0 );
                return owner[current_keys[0]];
            }
            else {
                return current;
            }
        }
        bool empty() {
            static if ( key_sort_flag ) {
                return current_keys.length == 0;
            }
            else {
                return current is null;
            }
        }
        final int opApply(scope int delegate(BSON bson) dg) {
            int result;
            for(; !empty; popFront) {
                if ( (result=dg(front))!=0 ) {
                    break;
                }
            }
            return result;
        }
        final int opApply(scope int delegate(in string key, BSON bson) dg) {
            int result;
            for(; !empty; popFront) {
                if ( (result=dg(front.key, front))!=0 ) {
                    break;
                }
            }
            return result;
        }

    }

}


int[] doc2ints(Document doc) {
    int[] result;
    foreach(elm; doc.opSlice) {
        result~=elm.as!int;
    }
    return result;
}

double[] doc2doubles(Document doc) {
    double[] result;
    foreach(elm; doc.opSlice) {
        result~=elm.as!double;
    }
    return result;
}