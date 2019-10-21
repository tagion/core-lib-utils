// Written in the D programming language.

/**
 * Implements HiBON
 * Hash-invariant Binary Object Notation
 * Is inspired by BSON but us not compatible
 *
 * See_Also:
 *  $(LINK2 http://bsonspec.org/, BSON - Binary JSON)
 *
 */
module tagion.utils.HiBON;


import std.stdio;
//import std.datetime;   // Date, DateTime
import std.container : RedBlackTree;
import std.format;
import std.meta : staticIndexOf;
import std.algorithm.iteration : map, fold, each;
import std.traits : EnumMembers, ForeachType, Unqual, isMutable, isBasicType;
import std.bitmanip : write;
import std.conv : to;

import tagion.utils.Document;
import tagion.utils.HiBONBase;


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


version(none)
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


@safe class HiBON {
    alias Value=ValueT!(true, HiBON,  Document);

    this() {
        _members = new Members;
    }

    version(none)
    protected void append(ref ubyte[] buffer, ref size_t index) const {
        immutable size_index = index;
        buffer.binwrite(uint.init, &index);
        _members[].each!( a => a.append(buffer, index) ); //.fold!( (a, b) => (a + b) );
        buffer.binwrite(Type.NONE, &index);
        immutable size = cast(uint)(index - uint.sizeof);
        buffer.binwrite(size, size_index);
    }
    /*
            private static void appendNativeArray(E)(ref ubyte[] buffer, ref const(Member) member, ref size_t index) const
            in {
                assert(member.type is E);
            }
        do {
            immutable size_index = index;
            buffer.binwrite(uint.init, &index);
            auto array = member.by!E;
            with(Type) {
            foreach(i, a; array) {
                immutable key = i.to!string;
                static if ( E is NATIVE_STRING_ARRAY ) {
                    enum ElementType = STRING;
                }
                else {
                    enum ElementType = DOCUMENT;
                }
                Document.buildKey(buffer, ElementType, key, index);
                static if ( E is NATIVE_HIBON_ARRAY ) {
                    a.append(buffer, index);
                }
                else static if ( E is NATIVE_DOCUMENT_ARRAY ) {
                    a.array_write(a.get!(E).data, index);
                }
                else static if ( E is NATIVE_STRING_ARRAY ) {
                    a.array_write(a.get!(E), index);
                }
                else {
                    static assert(0, format("Unsupported array type %s", E));
                }
            }
            }
        }

        private void append(ref ubyte[] buffer, ref size_t index) const {
            immutable size_index = index;
            buffer.binwrite(uint.init, &index);
            foreach(m; this[]) {
                with(Type) {
                TypeCase:
                    switch(m.type) {
                        static foreach(E; EnumMembers!Type) {
                            static if (isHiBONType(E) || isNative(E)) {
                            case E:
                                static if ( E is DOCUMENT ) {
                                    m.by!DOCUMENT.append(buffer, index);
                                }
                                else static if (isNative(E)) {
                                    static if (E is NATIVE_DOCUMENT) {
                                        Document.build(buffer, DOCUMENT, m.key, m.by!E, index);
                                    }
                                    else {
                                        appendNativeArray!E(buffer, n, index);
                                    }
                                }
                                else {
                                    Document.build(buffer, E, m.key, m.by!E, index);
                                }
                                break TypeCase;
                            }
                        }
                    default:
                        assert(0, format("Illegal type %s", type));
                    }
                }
            }
            buffer.binwrite(Type.NONE, &index);
            immutable size = cast(uint)(index - uint.sizeof);
            buffer.binwrite(size, size_index);
        }

    */

    size_t size() const pure {
        size_t result = uint.sizeof+Type.sizeof;
        if (_members.length) {
            result += _members[].map!(a => a.size).fold!( (a, b) => a + b);
        }
        return result;
    }

    immutable(ubyte[]) serialize() const pure {
        scope buffer = new ubyte[size];
        size_t index;
        append(buffer, index);
        return buffer.idup;
    }

    private void append(ref ubyte[] buffer, ref size_t index) const pure {
        immutable size_index = index;
        buffer.binwrite(uint.init, &index);
        if (_members.length) {
            _members[].each!(a => a.append(buffer, index));
        }
        buffer.binwrite(Type.NONE, &index);
        immutable doc_size=cast(uint)(index - size_index - uint.sizeof);
        buffer.binwrite(doc_size, size_index);
    }

    @safe static class Member {
        string key;
        Type type;
        Value value;

        protected this() nothrow {
            value = uint.init;
            // empty
        }

        this(T)(T x, string key) if ( !is(T == const) ) { //const if ( is(T == const) ) {
            this.value = x;
            this.type  = Value.asType!T;
            this.key  = key;
        }

        @trusted
        this(T)(T x, string key) const if ( is(T == const) ) { //const if ( is(T == const) ) {
            static if ( is(T == class) || is(T == struct) ) {
                alias MutableT = Unqual!T;
                this.value = cast(MutableT)x;
            }
            // static if ( is(T:const(Document)) ) {
            //     pragma(msg, T);
            //     this.value = x;
            // }
            else {
                this.value = x;
            }
            this.type  = Value.asType!T;
            this.key  = key;
        }


        @trusted
        inout(HiBON) document() inout pure
            in {
                assert(type is Type.DOCUMENT);
            }
        do {
            return value.document;
        }

        static Member search(string key) {
            auto result=new Member();
            result.key = key;
            return result;
        }

        T get(T)() inout {
            enum E = Value.asType!T;
            .check(E is type, format("Expected HiBON type %s but apply type %s (%s)", type, E, T.stringof));
            return value.by!E;
        }

        auto by(Type type)() inout {
            return value.by!type;
        }

        @trusted
        size_t size() const pure {
            with(Type) {
            TypeCase:
                switch(type) {
                    foreach(E; EnumMembers!Type) {
                        static if(isHiBONType(E) || isNative(E)) {
                            case E:
                                static if ( E is Type.DOCUMENT ) {
                                    return Document.sizeKey(key)+value.by!(E).size;
                                }
                                else static if ( E is NATIVE_DOCUMENT ) {
                                    return Document.sizeKey(key)+value.by!(E).size+uint.sizeof;
                                }
                                else static if ( isNative(E) ) {
                                    size_t result = uint.sizeof + Type.sizeof;
                                    alias T = Value.TypeT!E;
                                    alias U = ForeachType!(T);
                                    foreach(i, e; value.by!(E)[]) {
                                        immutable key=i.to!string;
                                        result += Document.sizeKey(key);
                                        static if(E is NATIVE_HIBON_ARRAY) {
                                            result += e.size;
                                        }
                                        else static if (E is NATIVE_DOCUMENT_ARRAY) {
                                            result += e.size;
                                        }
                                        else static if (E is NATIVE_STRING_ARRAY) {
                                            result += uint.sizeof + e.length;
                                        }
                                    }
                                    return result;
                                }
                                else {
                                    const v = value.by!(E);
                                    return Document.sizeT(E, key, v);
                                }
                                break TypeCase;
                        }
                    }
                default:
                    // Empty
                }
                assert(0, format("Size of HiBON type %s is not valid", type));
            }
        }

        version(none)
        protected void appendList(Type E)(ref ubyte[] buffer, ref size_t index) const if ( isList!E ) {
            alias U=ForeachType!(Value.type!E);
            foreach(h; value.get!E) {
                const m= new const(Member)(h);
                m.append(buffer, index);
            }
        }

        void append(ref ubyte[] buffer, ref size_t index) const pure {
            with(Type) {
            TypeCase:
                switch(type) {
                    static foreach(E; EnumMembers!Type) {
                        static if(isHiBONType(E) || isNative(E)) {
                        case E:
                            alias T = Value.TypeT!E;
                            static if (E is DOCUMENT) {
                                Document.buildKey(buffer, E, key, index);
                                value.by!(E).append(buffer, index);
                            }
                            else static if (isNative(E)) {
                                static if (E is NATIVE_DOCUMENT) {
                                    Document.buildKey(buffer, DOCUMENT, key, index);
                                    const doc=value.by!(E);
                                    buffer.array_write(value.by!(E).data, index);
                                }
                                else {
                                    assert(0, format("%s is not implemented yet", E));
                                }
                            }
                            else {
                                Document.build(buffer, E, key, value.by!E, index);
                            }
                            break TypeCase;
                        }
                    }
                default:
                    assert(0, format("Illegal type %s", type));
                }
            }
        }
    }

    alias Members=RedBlackTree!(Member, (a, b) => (less_than(a.key, b.key)));

    protected Members _members;

    version(none)
    void check_type(DTYPE)(string file = __FILE__, size_t line = __LINE__ ){
        enum type_to_be_check=DtoHiBONType!DTYPE;
        static if ( is(typeof(type_to_be_check) == BinarySubType ) ) {
            .check(_type == Type.BINARY, format("Bad type %s expected %s", Type.BINARY, _type), file, line);
            .check(_subtype == type_to_be_check, format("Bad sub-type %s expected %s", type_to_be_check, _subtype), file, line);

        }
        else {
            .check(_type == type_to_be_check, format("Bad type %s expected %s", type_to_be_check, _type), file, line);
        }
    }

    auto opSlice() const {
        return _members[];
    }

    void opIndexAssign(T)(T x, in string key) {
        .check(is_key_valid(key), format("Key is not a valid format '%s'", key));
        Member new_member=new Member(x, key);
        _members.insert(new_member);
    }

    const(Member) opIndex(in string key) const {
        auto range=_members.equalRange(Member.search(key));
        .check(!range.empty, format("Member '%s' does not exist", key) );
        return range.front;
    }
/*
    Value opIndex(in string key) {
        auto range=_members.equalRange(Member.search(key)); //Member.search(key));
        .check(!range.empty, format("Member '%s' does not exist", key) );
        return range.front.value;
    }
*/

    /*
    inout(T) get(T)() inout {
        with(Type) final switch (type) {
                foreach (E; EnumMembers!Type) {
                case E:
                    alias T=ValueType!E;
                    static if ( isOneOf!(T, NonValidDataTypes) ) {
                        .check(0, format("Illigal HiBSON type %s", E));
                    }
                    else {
                        return value!E;
                    }
                    break;
                }
            }
        assert(0);
    }
    */
    version(none) {

        bool isDocument() const {
            return (type is Type.DOCUMENT);
        }

        bool isArray() const {
            return (type is Type.LIST);
        }
    }


    void append(T)(string key, T x) {
        alias BaseT=TypedefType!T;
        alias UnqualT=Unqual!BaseT;
        HiBON elm=new HiBON;
        scope(success) {
            // elm._type=type;
            // elm._subtype=binary_subtype;
            Member member={key : key, element : elm};
            _member.insert(member);
            // elm._key=key;
            // elm.members=members;
            // members=elm;
        }
        static if ( is(UnqualT == bool) ) {
            elm._type=Type.BOOLEAN;
            elm.value.boolean=x;
        }
        else static if ( is(UnqualT == double) ) {
            int x;
        }
    }

    unittest {
        import std.stdio;
        import std.conv : to;
        import std.typecons : Tuple, isTuple;
        // Note that the keys are in alphabetic order
        // Because the HiBON keys must be ordered
        alias Tabel = Tuple!(
            bool,   Type.BOOLEAN.stringof,
            float,  Type.FLOAT32.stringof,
            double, Type.FLOAT64.stringof,
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

        // Note that the keys are in alphabetic order
        // Because the HiBON keys must be ordered
        alias TabelArray = Tuple!(
            immutable(ubyte)[],  Type.BINARY.stringof,
            immutable(bool)[],   Type.BOOLEAN_ARRAY.stringof,
            immutable(float)[],  Type.FLOAT32_ARRAY.stringof,
            immutable(double)[], Type.FLOAT64_ARRAY.stringof,
            immutable(int)[],    Type.INT32_ARRAY.stringof,
            immutable(long)[],   Type.INT64_ARRAY.stringof,
            string,              Type.STRING.stringof,
            immutable(uint)[],   Type.UINT32_ARRAY.stringof,
            immutable(ulong)[],  Type.UINT64_ARRAY.stringof,
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

        { // empty
            auto hibon = new HiBON;
            assert(hibon.length is 0);

            assert(hibon.size is uint.sizeof+Type.sizeof);
            immutable data = hibon.serialize;

            const doc = Document(data);
            assert(doc.length is 0);
            assert(doc[].empty);
        }

        { // Single element
            auto hibon = new HiBON;
            enum pos=1;
            static assert(is(test_tabel.Types[pos] == float));
            hibon[test_tabel.fieldNames[pos]] = test_tabel[pos];

            assert(hibon.length is 1);

            const m=hibon[test_tabel.fieldNames[pos]];

            assert(m.type is Type.FLOAT32);
            assert(m.key is Type.FLOAT32.stringof);
            assert(m.get!(test_tabel.Types[pos]) == test_tabel[pos]);
            assert(m.by!(Type.FLOAT32) == test_tabel[pos]);

            immutable size = hibon.size;


            // This size of a HiBON with as single element of the type FLOAT32
            enum hibon_size
                = uint.sizeof                    // Size of the object in ubytes (uint(14))
                + Type.sizeof                    // The HiBON Type  (Type.FLOAT32)  1
                + ubyte.sizeof                   // Length of the key (ubyte(7))    2
                + Type.FLOAT32.stringof.length   // The key text string ("FLOAT32") 9
                + float.sizeof                   // The data            (float(1.23)) 13
                + Type.sizeof                    // The HiBON object ends with a (Type.NONE) 14
                ;

            const doc_size = Document.sizeT(Type.FLOAT32, Type.FLOAT32.stringof, test_tabel[pos]);

            assert(size is hibon_size);

            immutable data = hibon.serialize;

            const doc = Document(data);

            assert(doc.length is 1);
            const e = doc[Type.FLOAT32.stringof];

            assert(e.type is Type.FLOAT32);
            assert(e.key == Type.FLOAT32.stringof);
            assert(e.by!(Type.FLOAT32) == test_tabel[pos]);

        }

        { // HiBON Test for basic types
            auto hibon = new HiBON;

            string[] keys;
            foreach(i, t; test_tabel) {
                hibon[test_tabel.fieldNames[i]] = t;
                keys~=test_tabel.fieldNames[i];
            }

            size_t index;
            foreach(m; hibon[]) {
                assert(m.key == keys[index]);
                index++;
            }

            foreach(i, t; test_tabel) {
                enum key=test_tabel.fieldNames[i];
                const m = hibon[key];
                assert(m.key == key);
                assert(m.type.to!string == key);
                assert(m.get!(test_tabel.Types[i]) == t);
            }

            immutable data = hibon.serialize;
            const doc = Document(data);

            assert(doc.length is test_tabel.length);

            foreach(i, t; test_tabel) {
                enum key=test_tabel.fieldNames[i];
                const e = doc[key];
                assert(e.key == key);
                assert(e.type.to!string == key);
                assert(e.get!(test_tabel.Types[i]) == t);
            }
        }

        { // HiBON Test for basic-array types
            auto hibon = new HiBON;

            string[] keys;
            foreach(i, t; test_tabel_array) {
                hibon[test_tabel_array.fieldNames[i]] = t;
                keys~=test_tabel_array.fieldNames[i];
            }

            size_t index;
            foreach(m; hibon[]) {
                assert(m.key == keys[index]);
                index++;
            }

            foreach(i, t; test_tabel_array) {
                enum key=test_tabel_array.fieldNames[i];
                const m = hibon[key];
                assert(m.key == key);
                assert(m.type.to!string == key);
                assert(m.get!(test_tabel_array.Types[i]) == t);
            }

            immutable data = hibon.serialize;
            const doc = Document(data);

            assert(doc.length is test_tabel_array.length);

            foreach(i, t; test_tabel_array) {
                enum key=test_tabel_array.fieldNames[i];
                const e = doc[key];
                assert(e.key == key);
                assert(e.type.to!string == key);
                assert(e.get!(test_tabel_array.Types[i]) == t);
            }

        }

        { // HIBON test containg an child HiBON
            auto hibon = new HiBON;
            auto hibon_child = new HiBON;
            enum chile_name = "child";

            hibon["string"] = "Text";
            hibon["float"]  = float(1.24);

            immutable hibon_size_no_child = hibon.size;
            hibon[chile_name]      = hibon_child;
            hibon_child["int32"]= 42;

            immutable hibon_child_size    = hibon_child.size;
            immutable child_key_size = Document.sizeKey(chile_name);
            immutable hibon_size = hibon.size;
            assert(hibon_size is hibon_size_no_child+child_key_size+hibon_child_size);

            immutable data = hibon.serialize;
            const doc = Document(data);

        }

        { // Use of native Documet in HiBON
            auto native_hibon = new HiBON;
            native_hibon["int"] = int(42);
            immutable native_data = native_hibon.serialize;
            auto native_doc = Document(native_hibon.serialize);

            auto hibon = new HiBON;
            hibon["string"] = "Text";

            immutable hibon_no_native_document_size = hibon.size;
            hibon["native"] = native_doc;
            immutable data = hibon.serialize;
            const doc = Document(data);

            {
                const e = doc["string"];
                assert(e.type is Type.STRING);
                assert(e.get!string == "Text");
            }

            { // Check native document
                const e = doc["native"];

                assert(e.type is Type.DOCUMENT);
                const sub_doc =  e.get!Document;
                assert(sub_doc.length is 1);
                assert(sub_doc.data == native_data);
                const sub_e = sub_doc["int"];
                assert(sub_e.type is Type.INT32);
                assert(sub_e.get!int is 42);
            }

        }
    }
    version(none)
    void opIndexAssign(T, Index)(T x, const Index index) if (isIntegral!Index) {
        opIndexAssign(x, index.to!string);
    }

    version(none)
    void opIndexAssign(T)(T x, string key) {
        alias BaseT=TypedefType!T;
        alias UnqualT=Unqual!BaseT;
        static if (is(UnqualT == bool)) {
            insert(Type.BOOLEAN, key, x);
        }
        else static if (is(UnqualT == int)) {
            insert(Type.INT32, key, x);
        }
        else static if (is(UnqualT == long)) {
            insert(Type.INT64, key, x);
        }
        else static if (is(UnqualT == double)) {
            insert(Type.DOUBLE, key, x);
        }
        else static if (is(UnqualT == uint)) {
            insert(Type.UINT32, key, x);
        }
        else static if (is(UnqualT == ulong)) {
            insert(Type.UINT64, key, x);
        }
        else static if (is(UnqualT == float)) {
            insert(Type.FLOAT, key, x);
        }
        else static if (is(UnqualT == string)) {
            insert(Type.STRING, key, x);
        }
        else static if (is(UnqualT == Date)) {
            insert(Type.DATE, key, x);
        }
        // else static if (isGeneralType!(BaseT, DateTime)) {
        //     insert(Type.TIMESTAMP, key, x);
        // }
        // else static if (is(BaseT:string[])) {
        //     insert(Type.NATIVE_STRING_ARRAY, key, x);
        // }
        else static if (is(BaseT:const(HiBON))) {
            insert(Type.DOCUMENT, key, x);
        }
        else static if (is(BaseT:const(Document)) ) {
            insert(Type.NATIVE_DOCUMENT, key, x);
        }
        else static if (is(BaseT:const(Document[])) ) {
            insert(Type.NATIVE_ARRAY, key, x);
        }
        else static if (is(BaseT:const(HiBON[])) ) {
            insert(Type.NATIVE_HiBON_ARRAY, key, x);
        }
        else static if (isSubType!BaseT) {
            insert(Type.BINARY, key, x, getSubtype!BaseT);
        }
        else static if (is(BaseT:U[],U)) {
            insert(Type.ARRAY, key, x);
        }
        else static if (is(BaseT==enum) && is(BaseT : const(uint)) ) {
            insert(Type.UINT32, key, cast(uint)x);
        }
        else {
            static assert(0, format("opIndexAssign does not support type %s", T.stringof));
        }
    }
    /+
     unittest { // opIndexAssign type test
     auto hbson=new HiBON;
     {
     const bool x=true;
     enum type=typeof(x).stringof;
     hbson[type]=x;
     assert(hbson[type].type == Type.BOOLEAN);
     }

     {
     const int x=-42;
     enum type=typeof(x).stringof;
     hbson[type]=x;
     assert(hbson[type].type == Type.INT32);
     }

     {
     const long x=-42;
     enum type=typeof(x).stringof;
     hbson[type]=x;
     assert(hbson[type].type == Type.INT64);
     }

     {
     const double x=-42.42;
     enum type=typeof(x).stringof;
     hbson[type]=x;
     assert(hbson[type].type == Type.DOUBLE);
     }

     {
     const uint x=42;
     enum type=typeof(x).stringof;
     hbson[type]=x;
     assert(hbson[type].type == Type.UINT32);
     }

     {
     const ulong x=42;
     enum type=typeof(x).stringof;
     hbson[type]=x;
     assert(hbson[type].type == Type.UINT64);
     }

     {
     const float x=-42.42;
     enum type=typeof(x).stringof;
     hbson[type]=x;
     assert(hbson[type].type == Type.FLOAT);
     }

     {
     const string x="some_text";
     enum type=typeof(x).stringof;
     hbson[type]=x;
     assert(hbson[type].type == Type.STRING);
     }

     }
     +/
    // void setNull(string key) {
    //     append(Type.NULL, key, null);
    // }

    version(none)
    unittest { // bool bug-fix test
        auto hbson=new HiBON;
        const x=true;
        hbson["bool"]=x;
        immutable data=hbson.serialize;

        auto doc=Document(data);
        auto value=doc["bool"];
        assert(value.type == Type.BOOLEAN);
        assert(value.get!bool == true);
    }

    version(none)
    unittest { // Assign Document[]
        HiBON hbson;
        Document[] docs;
        hbson=new HiBON;
        hbson["int_0"]=0;
        docs~=Document(hbson.serialize);
        hbson=new HiBON;
        hbson["int_1"]=1;
        docs~=Document(hbson.serialize);
        hbson=new HiBON;
        hbson["int_2"]=2;
        docs~=Document(hbson.serialize);

        hbson=new HiBON;
        hbson["docs"]=docs;


        auto doc=Document(hbson.serialize);

        auto doc_docs=doc["docs"].get!Document;

        assert(doc_docs.length == 3);
        assert(equal(doc_docs.keys, ["0", "1", "2"]));
        assert(equal(doc_docs.indices, [0, 1, 2]));
        foreach(uint i;0..3) {
            assert(doc_docs.hasElement(i.to!string));
            assert(doc_docs.hasElement(i));
            auto e=(i.to!string in doc_docs);
            auto d=doc_docs[i].get!Document;
            assert(d["int_"~i.to!string].get!int == i);
        }
    }

    version(none)
    inout(HiBON) opIndex(const(char)[] key) inout {

        auto iter=Iterator!(const(HiBON), false)(this);
        foreach(b;iter) {
            if ( b.key == key ) {
                return b;
                break;
            }
        }
        throw new HiBONException("Member '"~key.idup~"' not defined");
        assert(0);
    }

    version(none)
    HiBON opIndex(string key) {
        immutable search={key : key};
        auto range=_members.lowerBound(search);
        check(!range.empty, format("Member '%s' not defined", key));
        return range.front;
    }

    version(none)
    bool hasElement(string key) const {
        immutable search={key : key};
        return !(_members.lowerBound(search).empty);
    }

    version(none)
    Type type() pure const nothrow {
        return _type;
    }

    version(none)
    string typeString() pure const  {
        if ( _type is Type.BINARY ) {
            return subtype.to!string;
        }
        else {
            return _type.to!string;
        }
    }

    version(none)
    @trusted
    immutable(char)[] toInfo() const {
        immutable(char)[] result;
        with(Type) final switch(_type) {
            case MIN:
            case MAX:
            case TRUNC:
            case NONE:
            case UNDEFINED:
            case NULL:
                result=to!string(_type);
                break;
            case DOUBLE:
                result~=format("%s %s", to!string(_type), value.number);
                break;
            case FLOAT:
                result~=format("%s %s", to!string(_type), value.number32);
                break;
            case STRING:
            case REGEX:
            case JS_CODE:
            case SYMBOL:
                result~=format("**%s %s", to!string(_type), value.text);
                break;
            case JS_CODE_W_SCOPE:
                result~=format("%s %s :%X", to!string(_type), value.codescope.code, value.codescope.document.id);
                break;
            case DOCUMENT:
            case ARRAY:
                result~=format("##%s :%X", to!string(_type), this.id);
                break;
            case BINARY:
                result~=format("%s.%s", to!string(_type), to!string(subtype));
                // Todo
                break;
            case OID:
                result~=format("%s :%X ", to!string(_type), value.oid.id);
                break;
            case BOOLEAN:
                result~=format("%s %s", to!string(_type), value.boolean);
                break;
            case DATE:
                result~=format("%s %s", to!string(_type), value.date);
                break;
            case DBPOINTER:
                result=to!string(_type);
                break;
            case INT32:
                result~=format("%s %s", to!string(_type), value.int32);
                break;
            case UINT32:
                result~=format("%s %s", to!string(_type), value.uint32);
                break;
            case INT64:
                result~=format("%s %s", to!string(_type), value.int64);
                break;
            case UINT64:
                result~=format("%s %s", to!string(_type), value.uint64);
                break;
            case TIMESTAMP:
                result~=format("%s %s", to!string(_type), value.int64);
                break;
            case HASHDOC:
                assert(0, "Hashdoc not implemented yet");
                break;

            case NATIVE_DOCUMENT:
                result~=format("%s %s", to!string(_type), value.document_array.length);
                break;
            case NATIVE_ARRAY:
                result~=format("%s %s", to!string(_type), value.binary.length);
                break;
            case NATIVE_HiBON_ARRAY:
                result~=format("%s %s", to!string(_type), value.hbson_array.length);
                break;
            case NATIVE_STRING_ARRAY:
                result~=format("%s %s", to!string(_type), value.text_array.length);
            }
        return result;
    }

    string_t toText(string_t=string)() {
        string_t object_toText(HiBON obj) {
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
                    if ( b.const_pointer ) {
                        buf~=object_toText(*(b.value.document));
                    }
                    else {
                        buf~=object_toText(b.value.document);
                    }
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
                // case UNDEFINED:
                //     return "undefined";
            case NULL:
                return "null";
            case DOUBLE:
                return to!string_t(value.number);
            case FLOAT:
                return to!string_t(value.number32);
            case STRING:
                // case REGEX:
                // case JS_CODE:
                // case SYMBOL:
                return to!string_t('"'~value.text~'"') ;
                // case JS_CODE_W_SCOPE:
                //     return to!string_t("["~value.codescope.code~", "~to!string(value.codescope.document.id)~"]");
            case DOCUMENT:
            case ARRAY:
                return object_toText(this);
            case BINARY:
                return binary_toText();
                // case OID:
                //     return to!string_t(toHex(value.oid.id));
            case BOOLEAN:
                return to!string_t(value.boolean);
            case DATE:
                return to!string_t('"'~value.date.toString~'"');
                // case DBPOINTER:
                //     return to!string_t('"'~to!string(_type)~'"');
            case INT32:
                return to!string_t(value.int32);
            case UINT32:
                return to!string_t(value.uint32);
            case INT64:
                return '"'~to!string_t(value.int64)~'"';
            case UINT64:
                return '"'~to!string_t(value.uint64)~'"';
                // case TIMESTAMP:
                //     return '"'~to!string_t(value.int64)~'"';
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
        else static if (is(T:const(HiBON))) {
            data~=x.serialize;
        }
        else static if (is(T:const(Document))) {
            data~=x.data;
        }
        else {
            static assert(0, "Unsupported type "~T.stringof);
        }

    }

    enum zero = cast(ubyte)0;
    enum one  = cast(ubyte)1;
    protected void append_native_array(T)(const Type t, ref immutable(ubyte)[] data) const {
        scope immutable(ubyte)[] local;
        foreach(i,a;get!T) {
            local~=t;
            local~=i.to!string;
            local~=zero;
            native_append(a, local);
        }
        data~=nativeToLittleEndian(cast(uint)(local.length+uint.sizeof+zero.sizeof));
        data~=local;
        data~=zero;
    }


    version(none)
    immutable(ubyte)[] serialize() const {
        immutable(ubyte)[] local_serialize() {
            immutable(ubyte)[] data;
            foreach(e; iterator!key_sort_flag) {
                data~=(e._type & Type.TRUNC);
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
                    case ARRAY:
                        data~=e.value.document.serialize;
                        break;
                    case NATIVE_HiBON_ARRAY:
                        e.append_native_array!(HiBON[])(DOCUMENT, data);
                        break;
                    case NATIVE_ARRAY:
                        e.append_native_array!(Document[])(DOCUMENT, data);
                        break;
                    case NATIVE_STRING_ARRAY:
                        e.append_native_array!(string[])(STRING, data);
                        break;
                    case BINARY:
                        e.append_binary(data);
                        break;
                    case HASHDOC:
                        assert(0, "Hashdoc not implemented yet");
                        break;
                    case UNDEFINED:
                    case NULL:
                    case MAX:
                    case MIN:
                    case TRUNC:
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
                        assert(0, format("%s not supported", DBPOINTER));
                        break;

                    case JS_CODE_W_SCOPE:
                        immutable(ubyte)[] local=e.serialize();
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
                    case NATIVE_DOCUMENT:
                        data~=e.value.binary;
                        break;
                    }
            }
            return data;
        }
        immutable(ubyte)[] data;
        scope immutable(ubyte)[] local=local_serialize();
        data~=nativeToLittleEndian(cast(uint)(local.length+uint.sizeof+zero.sizeof));
        data~=local;
        data~=zero;
        return data;
    }

    version(none)
    unittest {
        HiBON hbson1=new HiBON;


        hbson1["int"]=3;
        hbson1["number"]=1.7;
        hbson1["bool"]=true;
        hbson1["text"]="sometext";

        assert(!hbson1.duble);
        {
            auto iter=hbson1.iterator;
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
        data1=hbson1.serialize();

        {
            auto doc=Document(data1);
            assert(doc.hasElement("int"));
            assert(doc.hasElement("bool"));
            assert(doc.hasElement("number"));
            assert(doc.hasElement("text"));
            assert(doc.length == 4);
            assert(doc["int"].get!int == 3);
            assert(doc["bool"].get!bool);
            assert(doc["number"].get!double == 1.7);
            assert(doc["text"].get!string == "sometext");

        }

        HiBON hbson2=new HiBON;
        hbson2["x"] = 10;
        hbson1["obj"]=hbson2;

        data1=hbson1.serialize();
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

    version(none)
    unittest { // Test of serializing of a cost(HiBON)
        auto stream(const(HiBON) b) {
            return b.serialize;
        }
        {
            auto hbson = new HiBON;
            hbson["x"] = 10;
            hbson["s"] = "text";
            auto data_const=stream(hbson);
            assert(data_const == hbson.serialize);
        }
        { // const(HiBON) member
            auto hbson1=new HiBON;
            auto hbson2=new HiBON;
            auto sub_hbson=new HiBON;
            sub_hbson["x"]=10;
            hbson1["num"]=42;
            hbson2["num"]=42;
            hbson1["obj"]=cast(HiBON)sub_hbson;
            hbson2["obj"]=sub_hbson;
            assert(hbson1.serialize == hbson2.serialize);
            assert(stream(hbson1) == hbson2.serialize);
            assert(hbson1.serialize == stream(hbson2));
        }
    }

    version(none)
    unittest {
        // Test D array types
        HiBON hbson;
        { // Boolean array
            immutable bools=[true, false, true];
            hbson=new HiBON;
            hbson["bools"]=bools;

            auto doc=Document(hbson.serialize);
            assert(doc.hasElement("bools"));
            auto subarray=doc["bools"].get!(typeof(bools));

            assert(subarray[0] == bools[0]);
            assert(subarray[1] == bools[1]);
            assert(subarray[2] == bools[2]);
        }

        { // Int array
            immutable(int[]) int32s=[7, -9, 13];
            hbson=new HiBON;
            hbson["int32s"]=int32s;

            auto doc=Document(hbson.serialize);
            assert(doc.hasElement("int32s"));
            auto subarray=doc["int32s"].get!(typeof(int32s));

            assert(subarray[0] == int32s[0]);
            assert(subarray[1] == int32s[1]);
            assert(subarray[2] == int32s[2]);
        }

        { // Unsigned int array
            immutable(uint[]) uint32s=[7, 9, 13];
            hbson=new HiBON;
            hbson["uint32s"]=uint32s;

            auto doc=Document(hbson.serialize);
            assert(doc.hasElement("uint32s"));
            auto subarray=doc["uint32s"].get!(typeof(uint32s));

            assert(subarray[0] == uint32s[0]);
            assert(subarray[1] == uint32s[1]);
            assert(subarray[2] == uint32s[2]);
        }

        { // Long array
            immutable(long[]) int64s=[7, 9, -13];
            hbson=new HiBON;
            hbson["int64s"]=int64s;

            auto doc=Document(hbson.serialize);
            assert(doc.hasElement("int64s"));
            auto subarray=doc["int64s"].get!(typeof(int64s));

            assert(subarray[0] == int64s[0]);
            assert(subarray[1] == int64s[1]);
            assert(subarray[2] == int64s[2]);
        }

        { // Unsigned long array
            immutable(ulong[]) uint64s=[7, 9, 13];
            hbson=new HiBON;
            hbson["uint64s"]=uint64s;

            auto doc=Document(hbson.serialize);
            assert(doc.hasElement("uint64s"));
            auto subarray=doc["uint64s"].get!(typeof(uint64s));

            assert(subarray[0] == uint64s[0]);
            assert(subarray[1] == uint64s[1]);
            assert(subarray[2] == uint64s[2]);
        }

        { // double array
            immutable(double[]) doubles=[7.7, 9.9, 13.13];
            hbson=new HiBON;
            hbson["doubles"]=doubles;

            auto doc=Document(hbson.serialize);
            assert(doc.hasElement("doubles"));
            auto subarray=doc["doubles"].get!(typeof(doubles));

            assert(subarray[0] == doubles[0]);
            assert(subarray[1] == doubles[1]);
            assert(subarray[2] == doubles[2]);
        }


        { // float array
            immutable(float[]) floats=[7.7, 9.9, 13.13];
            hbson=new HiBON;
            hbson["floats"]=floats;

            auto doc=Document(hbson.serialize);
            assert(doc.hasElement("floats"));
            auto subarray=doc["floats"].get!(typeof(floats));

            assert(subarray[0] == floats[0]);
            assert(subarray[1] == floats[1]);
            assert(subarray[2] == floats[2]);
        }

        { // string array
            string[] strings=["Hej", "med", "dig"];
            hbson=new HiBON;
            hbson["strings"]=strings;

            auto doc=Document(hbson.serialize);
            assert(doc.hasElement("strings"));
            auto subarray=doc["strings"].get!Document;

            assert(subarray[0].get!string == strings[0]);
            assert(subarray[1].get!string == strings[1]);
            assert(subarray[2].get!string == strings[2]);
        }

        {
            HiBON[] hbsons;
            hbson=new HiBON;
            hbson["x"]=10;
            hbsons~=hbson;
            hbson=new HiBON;
            hbson["y"]="kurt";
            hbsons~=hbson;
            hbson=new HiBON;
            hbson["z"]=true;
            hbsons~=hbson;
            hbson=new HiBON;

            hbson["hbsons"]=hbsons;

            auto data=hbson.serialize;

            auto doc=Document(hbson.serialize);

            assert(doc.hasElement("hbsons"));

            auto subarray=doc["hbsons"].get!Document;
            assert(subarray[0].get!Document["x"].get!int == 10);
            assert(subarray[1].get!Document["y"].get!string == "kurt");
            assert(subarray[2].get!Document["z"].get!bool == true);
        }
    }

    version(none)
    unittest  {
        // Buffer as binary arrays
        HiBON hbson;
        {

            hbson=new HiBON;
//            hbson.typedarray=true;
            { // Typedarray int32
                immutable(int[]) int32s= [ -7, 9, -13];
                hbson["int32s"]=int32s;
                auto doc = Document(hbson.serialize);


                assert(doc.hasElement("int32s"));
                auto element=doc["int32s"];
                assert(element.get!(immutable(int)[]).length == int32s.length);
                assert(element.get!(immutable(ubyte)[]) == cast(immutable(ubyte)[])int32s);
                assert(element.get!(immutable(int)[]) == int32s);
            }

            { // Typedarray uint32
                immutable(uint[]) uint32s= [ 7, 9, 13];
                hbson["uint32s"]=uint32s;
                auto doc = Document(hbson.serialize);

                assert(doc.hasElement("uint32s"));
                auto element=doc["uint32s"];
                assert(element.get!(immutable(uint)[]).length == uint32s.length);
                assert(element.get!(immutable(ubyte)[]) == cast(immutable(ubyte)[])uint32s);
                assert(element.get!(immutable(uint)[]) == uint32s);
            }

            { // Typedarray int64
                immutable(long[]) int64s= [ -7_000_000_000_000, 9_000_000_000_000, -13_000_000_000_000];
                hbson["int64s"]=int64s;
                auto doc = Document(hbson.serialize);

                assert(doc.hasElement("int64s"));
                auto element=doc["int64s"];
                assert(element.get!(immutable(long)[]).length == int64s.length);
                assert(element.get!(immutable(ubyte)[]) == cast(immutable(ubyte)[])int64s);
                assert(element.get!(immutable(long)[]) == int64s);
            }


            { // Typedarray uint64
                immutable(long[]) uint64s= [ -7_000_000_000_000, 9_000_000_000_000, -13_000_000_000_000];
                hbson["uint64s"]=uint64s;
                auto doc = Document(hbson.serialize);

                assert(doc.hasElement("uint64s"));
                auto element=doc["uint64s"];
                assert(element.get!(immutable(long)[]).length == uint64s.length);
                assert(element.get!(immutable(ubyte)[]) == cast(immutable(ubyte)[])uint64s);
                assert(element.get!(immutable(long)[]) == uint64s);
            }

            { // Typedarray number64
                immutable(double[]) number64s= [ -7.7e9, 9.9e-4, -13e200];
                hbson["number64s"]=number64s;
                auto doc = Document(hbson.serialize);

                assert(doc.hasElement("number64s"));
                auto element=doc["number64s"];
                assert(element.get!(immutable(double)[]).length == number64s.length);
                assert(element.get!(immutable(ubyte)[]) == cast(immutable(ubyte)[])number64s);
                assert(element.get!(immutable(double)[]) == number64s);
            }


            { // Typedarray number32
                immutable(float[]) number32s= [ -7.7e9, 9.9e-4, -13e20];
                hbson["number32s"]=number32s;
                auto doc = Document(hbson.serialize);

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

    version(none)
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


    version(none)
    bool remove(string key) {
        auto iter=iterator;
        bool result;
        HiBON prev;
        for(; !iter.empty; iter.popFront) {
            if ( iter.front.key == key ) {
                // If the key is found then remove it from the change
                if ( members is iter.front ) {
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

    version(none)
    unittest {
        static if (!one_time_write) {
            // Remove and duble check
            HiBON hbson;
            hbson=new HiBON;
            hbson["a"]=3;
            hbson["b"]=13;

            assert(hbson["a"].get!int == 3);
            hbson["a"] = 4;

            uint i;
            foreach(b; hbson) {
                if ( b.key == "a" ) {
                    i++;
                }
            }
            assert(i == 2);
            assert(hbson.duble);

            hbson=new HiBON;
            hbson.no_duble=true;

            hbson["a"] = 3;
            hbson["b"] = 13;
            hbson["a"] = 4;

            assert(!hbson.duble);

            assert(hbson["a"].get!int==4);
        }
    }

    version(none)
    Iterator!(HiBON, F) iterator(bool F=false)() {
        return Iterator!(HiBON, F)(this);
    }

    version(none)
    Iterator!(const(HiBON), F) iterator(bool F=false)() const {
        return Iterator!(const(HiBON), F)(this);
    }


    version(none)
    @trusted
    protected immutable(ubyte)[] subtype_buffer() const {
        with(BinarySubType) final switch(subtype) {
            case GENERIC:
                return value.binary;

                // case FUNC:
                // case BINARY:
                // case UUID:
                // case MD5:
            case userDefined:
                check(0, format("The subtype %s should not be used as a type", subtype));
//                return value.binary;
            case INT32_array:
                return (cast(immutable(ubyte)*)(value.int32_array.ptr))[0..value.int32_array.length*int.sizeof];
            case UINT32_array:
                return (cast(immutable(ubyte)*)(value.uint32_array.ptr))[0..value.uint32_array.length*uint.sizeof];
            case INT64_array:
                return (cast(immutable(ubyte)*)(value.int64_array.ptr))[0..value.int64_array.length*long.sizeof];
            case UINT64_array:
                return (cast(immutable(ubyte)*)(value.uint64_array.ptr))[0..value.uint64_array.length*ulong.sizeof];
            case DOUBLE_array:
                return (cast(immutable(ubyte)*)(value.double_array.ptr))[0..value.double_array.length*double.sizeof];
            case FLOAT_array:
                return (cast(immutable(ubyte)*)(value.float_array.ptr))[0..value.float_array.length*float.sizeof];
            case DECIMAL_array:
                return (cast(immutable(decimal)*)(value.float_array.ptr))[0..value.float_array.length*decimal.sizeof];
            case BOOLEAN_array:
                return (cast(immutable(ubyte)*)(value.bool_array.ptr))[0..value.bool_array.length*bool.sizeof];
                // case BIGINT, not_defined:
                //     throw new HiBONException("Binary suptype "~to!string(subtype)~" not supported for buffer");

            }

    }

    version(none)
    protected void append_binary(ref immutable(ubyte)[] data) const {
        scope binary=subtype_buffer;
        data~=nativeToLittleEndian(cast(uint)(binary.length));
        data~=cast(ubyte)subtype;
        data~=binary;
    }


    version(none)
    Members.ConstRange keys() pure const nothrow {
        return KeyIterator(this);
    }

    size_t length() const {
        return _members.length;
    }

    version(none)
    unittest {
        // Test keys function
        // and the sorted HiBON
        {
            auto hbson=new HiBON!true;
            auto some_keys=["kurt", "abe", "ole"];
            hbson[some_keys[0]]=0;
            hbson[some_keys[1]]=1;
            hbson[some_keys[2]]=2;
            auto keys=hbson.keys;
            // writefln("keys=%s", keys);
            auto data=hbson.serialize;
            auto doc=Document(data);
            // writefln("doc.keys=%s", doc.keys);
            // Check that doc.keys are sorted
            assert(equal(doc.keys, ["abe", "kurt", "ole"]));
        }
        {
            import std.array : to_array=array;
            HiBON!true[] array;
            for(int i=10; i>-7; i--) {
                auto len=new HiBON!true;
                len["i"]=i;
                array~=len;
            }
            auto hbson=new HiBON!true;
            hbson["array"]=array;
            auto data=hbson.serialize;
            auto doc=Document(data);
            auto doc_array=doc["array"].get!Document;
            foreach(i,k; to_array(doc_array.keys)) {
                assert(to!string(i) == k);
            }
        }

    }

    version(none)
    int opApply(scope int delegate(HiBON hbson) @safe dg) {
        return iterator.opApply(dg);
    }

    version(none)
    int opApply(scope int delegate(in string key, HiBON hbson) @safe dg) {
        return iterator.opApply(dg);
    }

    version(none)
    @safe
    struct KeyIterator {
        protected Members.ConstRange range;
        this(const(HiBON) owner) nothrow {
            range=owner._members[];
        }
        void popFront() {
            range.popFront;
        }
        string front() pure const {
            return range.front.key;
        }
        bool empty() pure const {
            return range.empty;
        }
    }

    version(none)
    @safe
    struct Iterator(THiBON, bool key_sort_flag) {
        static assert( is (THiBON:const(HiBON)), format("Iterator only supports %s ",HiBON.stringof));
        private THiBON owner;
        enum owner_is_mutable=is(THiBON==HiBON);
        static if (key_sort_flag) {
            private string[] sorted_keys;
            private string[] current_keys;
        }
        else {
//            static assert(is(THiBON==HiBON), format("Non sorted HiBON does not support %s", THiBON.stringof));
            static if ( owner_is_mutable ) {
                private HiBON current;
            }
            else {
                private THiBON* current;
            }
        }
        this(THiBON owner) {
            this.owner=owner;
            static if ( key_sort_flag ) {
                void keylist(const(HiBON) owner) {
                    sorted_keys=owner.keys;
                    sort!(less_than, SwapStrategy.stable)(sorted_keys);
                    current_keys=sorted_keys;
                }
                keylist(owner);
            }
            else static if ( owner_is_mutable ) {
                current=owner.members;
            }
            else {
                current=&(owner.members);
            }
        }
        void popFront()
            in {
                static if ( !key_sort_flag ) {
                    assert(owner !is null);
                    static if ( owner_is_mutable ) {
                        assert(current !is owner,"Circular reference member "~current.key~" points to it self");
                    }
                    else {
                        if ( current ) {
                            assert(*current !is owner,"Circular reference member "~current.key~" points to it self");
                        }
                    }
                }
            }
        do {
            static if ( key_sort_flag ) {
                current_keys=current_keys[1..$];
            }
            else static if ( owner_is_mutable ) {
                current=current.members;
            }
            else {
                auto result() @trusted {
                    if ( current !is null ) {
                        return &(current.members);
                    }
                    else {
                        return null;
                    }
                }
                current=result();
            }
        }

        THiBON front() {
            static if ( key_sort_flag ) {
                assert ( current_keys.length > 0 );
                return owner[current_keys[0]];
            }
            else static if ( owner_is_mutable ) {
                return current;
            }
            else {
                return *current;
            }
        }

        bool empty() {
            static if ( key_sort_flag ) {
                return current_keys.length == 0;
            }
            else static if ( owner_is_mutable ) {
                return current is null;
            }
            else {
                auto result=(current is null) || (*current is null);
                scope(exit) {
                    if ( result ) {
                        current = null;
                    }
                }
                return result;
            }
        }

        final int opApply(scope int delegate(THiBON hbson) @safe dg) {
            int result;
            for(; !empty; popFront) {
                if ( (result=dg(front))!=0 ) {
                    break;
                }
            }
            return result;
        }

        final int opApply(scope int delegate(in string key, THiBON hbson) @safe dg) {
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



// int[] doc2ints(Document doc) {
//     int[] result;
//     foreach(elm; doc.opSlice) {
//         result~=elm.as!int;
//     }
//     return result;
// }

// double[] doc2doubles(Document doc) {
//     double[] result;
//     foreach(elm; doc.opSlice) {
//         result~=elm.as!double;
//     }
//     return result;
// }

version(none)
unittest { // HiBON with const member
    alias GHiBON=HiBON!true;
    auto hbson1=new GHiBON;
    auto hbson2=new GHiBON;
    hbson1["hugh"]="Some data";
    hbson1["age"]=42;
    hbson1["height"]=155.7;

    hbson2["obj"]=hbson1;
    immutable hbson1_data=hbson1.serialize;
    immutable hbson2_data=hbson2.serialize;

    auto doc1=Document(hbson1_data);
    auto doc2=Document(hbson2_data);

    assert(hbson1_data.length == doc1.data.length);
    assert(hbson1_data == doc1.data);

    assert(hbson2_data.length == doc2.data.length);
    assert(hbson2_data == doc2.data);

    void doc_hbson_const(GHiBON hbson, const(GHiBON) b) {

        hbson["obj"]=b;
    }

    auto hbson2c=new GHiBON;
    doc_hbson_const(hbson2c, hbson1);

    immutable hbson2c_data=hbson2c.serialize;
    auto doc2c=Document(hbson2c_data);
    assert(hbson2c_data == doc2c.data);
    assert(doc2c.data == doc2.data);

}

version(none)
unittest { // Test of Native Document type
    // The native document type is only used as an internal representation of the Document
    auto hbson1=new HiBON;
    auto hbson2=new HiBON;
    auto doc_hbson=new HiBON;
    doc_hbson["int"]=10;
    doc_hbson["bool"]=true;
    hbson1["obj"]=doc_hbson;

    // Test of using native Documnet as a object member
    auto doc=Document(doc_hbson.serialize);
    hbson2["obj"]=doc;
    auto data1=hbson1.serialize;
    // writefln("%s:%d", data1, data1.length);
    auto data2=hbson2.serialize;
    // writefln("%s:%d", data2, data2.length);
    assert(data1.length == data2.length);
    assert(data1 == data2);
}
