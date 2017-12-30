module bakery.utils.DList;

import std.stdio;

@safe
class UtilException : Exception {
    this( immutable(char)[] msg ) {
//        writefln("msg=%s", msg);
        super( msg );
    }
}

class DList(E) {
    struct Element {
        E entry;
        Element* next;
        Element* prev;
        this(E e) {
            entry=e;
        }
    }
    private Element* head;
    private Element* tail;
    // Number of element in the DList
    private uint count;
    void unshift(E e) {
        auto element=new Element(e);
        if ( head is null ) {
            head = tail =  element;
            count=1;
        }
        else {
            element.next=head;
            head.prev=element;
            head = element;
            count++;
        }

    }

    E shift() {
        scope(success) {
            head=head.next;
            head.prev = null;
            count--;
        }
        if ( head is null ) {
            throw new UtilException(this.stringof~" is empty");
        }
        return head.entry;
    }

    void push(E e) {
        auto element=new Element(e);
        if ( head is null ) {
            head = tail = element;
        }
        else {
            tail.next = element;
            element.prev = tail;
            tail = element;
        }
        count++;
    }

    ref E pop() {
        Element* result;
        if ( tail !is null ) {
            result = tail.prev;
            result.next = null;
            tail.prev = null;
            tail = result;
            count++;
        }
        else {
            writefln("pop head is null %s", head is null);
            throw new UtilException("Pop from an empty list");
        }
        return result.entry;
    }

    void remove(Element* e)
        in {
            assert(e !is null);
            if ( head is null ) {
                assert(count == 0);
            }
        }
    body {
        if ( head is null ) {
            throw new UtilException("Remove from an empty list");
        }
        if ( head is e ) {
            if ( head.next is null ) {
                head = tail =  null;
            }
            else {
                head = head.next;
                head.prev = null;
                if ( head is tail ) {
                    tail.prev = null;
                }
            }
        }
        else if ( tail is e ) {
            tail = tail.prev;
            if ( tail is null ) {
                head = null;
            }
            else {
                tail.next = null;
            }
        }
        else {
            e.next.prev = e.prev;
            e.prev.next = e.next;
        }
        count--;
    }

    void moveToFront(Element* e) {
        writefln("moveToFront e=%x", e);
        remove(e);
        unshift(e.entry);
    }

    uint length()
        out(result) {
            uint internal_count(Element* e, uint i=0) {
                writef("%s", i);
                if ( e is null ) {
                    return i;
                }
                else {
                    return internal_count(e.next, i+1);
                }

            }
            writefln("<");
            assert(result == internal_count(head));
        }
    body {
        return count;
    }

    Element* first() {
        return head;
    }
    Element* last() {
        return tail;
    }

    Iterator iterator(bool revert=false) {
        writefln("count=%d %x %x",count, cast(size_t)head, cast(size_t)tail);
        auto result=Iterator(this, revert);
        return result;
    }

    int opApply(scope int delegate(E e) dg) {
        auto I=iterator;
        int result;
        for(; (!I.empty) && (result == 0); I.popFront) {
            result=dg(I.front);
        }
        return result;
    }

    int opApplyReverse(scope int delegate(E e) dg) {
        auto I=iterator(true);
        int result;
        for(; (!I.empty) && (result == 0); I.popBack) {
            result=dg(I.front);
        }
        return result;
    }


    struct Iterator {
        private Element* cursor;
        this(DList l, bool revert) {
            writefln("count=%d %x %x",l.count, cast(size_t)l.head, cast(size_t)l.tail);

            if (revert) {
                cursor = l.tail;
            }
            else {
                cursor = l.head;
            }
            writefln("cursor=%x", cursor);
        }

        bool empty() const pure nothrow {
            return cursor is null;
        }

        Iterator* popFront() {
            if ( cursor !is null) {
                cursor = cursor.next;
            }
            return &this;
        }

        Iterator* popBack() {
            if ( cursor !is null) {
                cursor = cursor.prev;
            }
            return &this;
        }

        E front() {
            return cursor.entry;
        }

        Element* current() pure {
            debug {
                writefln("cursor=%x", cursor);
            }
            return cursor;
        }
    }

    ~this() {
        // Assist the GC to clean the chain
        Element* clear(ref Element* e) {
            if ( e !is null ) {
                e.prev=null;
                e=clear(e.next);
            }
            return null;
        }
        clear(head);
        tail=null;
    }

    invariant {
        if ( head is null ) {
            assert(tail is null);
        }
        else {
            assert(head.prev is null);
            assert(tail.next is null);
            if ( head is tail ) {
                assert(head.next is null);
                assert(tail.prev is null);
            }
        }

    }
}

unittest {
    { // Empty element test
        auto l=new DList!int;
//        auto e = l.shift;
//        assert(e is null);
        bool flag;
        assert(l.length == 0);
        try {
            flag=false;
            l.pop;
        }
        catch ( UtilException e ) {
            flag=true;
        }
        assert(flag);
        assert(l.length == 0);

        try {
            flag=false;
            l.shift;
        }
        catch ( UtilException e ) {
            flag=true;
        }
        assert(flag);
        assert(l.length == 0);
    }
    { // One element test
        auto l=new DList!int;
        l.unshift(7);
        assert(l.length == 1);
        auto first=l.first;
        auto last =l.last;
        assert(first !is null);
        assert(last !is null);
        assert(first is last);
        l.remove(first);
        assert(l.length == 0);
    }
    { // two element test
        auto l=new DList!int;
        l.unshift(7);
        l.unshift(4);
        assert(l.length == 2);
        auto first=l.first;
        auto last=l.last;
        assert(first.entry == 4);
        assert(last.entry == 7);
        // moveToFront test
        l.moveToFront(last);
        assert(l.length == 2);
        first=l.first;
        last=l.last;
        assert(first.entry == 7);
        assert(last.entry == 4);
    }
    { // More elements test
        import std.algorithm.comparison : equal;
        auto l=new DList!int;
        enum amount=4;
        foreach(i;0..amount) {
            l.push(i);
        }
        assert(l.length == amount);

        { // Forward iteration test
            auto I=l.iterator(false);
            uint i;
            for(i=0; !I.empty; I.popFront, i++) {
                assert(I.front == i);
            }
            writefln("i=%s",i);
            assert(i == amount);
            i=0;
            I=l.iterator(false);
            foreach(entry; I) {
                assert(entry == i);
                i++;
            }
            writefln("i=%s",i);
            assert(i == amount);
        }

        assert(l.length == amount);

        {  // Backward iteration test
            auto I=l.iterator(true);
            uint i;
            for(i=amount; !I.empty; I.popBack) {
                i--;
                assert(I.front == i);
            }
            writefln("i=%s",i);
            assert(i == 0);
            i=amount;
//            I=l.iterator(true);
            foreach_reverse(entry; l) {
                i--;
                writefln("entry=%s i=%s", entry, i);
                assert(entry == i);
            }
            assert(i == 0);
        }

        // moveToFront for the second element ( element number 1 )

        {
            auto I=l.iterator;
            I.popFront;
            auto current = I.current;
            l.moveToFront(current);
            assert(l.length == amount);
            // The element shoud now be ordred as
            // [1, 0, 2, 3]
            I=l.iterator;
            assert(equal(I, [1, 0, 2, 3]));
        }

        {
            auto I=l.iterator;
            I.popFront.popFront;
            auto current = I.current;
            l.moveToFront(current);
            assert(l.length == amount);
            // The element shoud now be ordred as
            // [1, 0, 2, 3]
            I=l.iterator;
            assert(equal(I, [2, 1, 0, 3]));
        }

        // foreach(i;0..amount) {
        //     assert(current.entry == i);
        //     current=current.next;
        // }

        // moveToFront second element( enumber 1 )


    }
}
