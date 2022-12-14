#pragma once
#include "GCState.h"
#include "spooky.h"
#include <iostream>

//#define ENSURE_THROW(cond, exception)	\
//	do { int __afx_condVal=!!(cond); assert(__afx_condVal); if (!(__afx_condVal)){exception;} } while (false)
//#define ENSURE(cond)		ENSURE_THROW(cond, throw std::runtime_error(#cond " failed") )
//#define ENSURE(cond)
enum _before_ { _BEFORE_, _END_ };
enum _after_ { _AFTER_, _START_ };
enum _sentinel_ { _SENTINEL_ };

class CircularDoubleList;

void merge_from_to(CircularDoubleList* source, CircularDoubleList* dest);
namespace GC {
    void merge_collected();
    void init_thread(bool combine_thread);
}
class CircularDoubleList 
{
    friend void merge_from_to(CircularDoubleList* source, CircularDoubleList* dest);
    friend void GC::merge_collected();
    friend void GC::init_thread(bool);
    //because every collectable class has to be derived from this, give things obscure names so they don't polute the namespace for user instance variables.
    CircularDoubleList* circular_double_list_prev;
    CircularDoubleList* circular_double_list_next;
protected:
    bool circular_double_list_is_sentinel;
public:

    //doesn't conform to a standard iterator, though I suppose I could make it
    //It is a bald iterator for a circular list with a sentinal.  
    //it's bidirectional, and it's always safe to delete at the iterator and then
    //shift forward or reverse... except that you should never delete the sentinal
    //execept from an already empty list.
    class circular_double_list_iterator
    {
        CircularDoubleList* center;
        CircularDoubleList* next;
        CircularDoubleList* prev;
    public:
        circular_double_list_iterator() {}
        circular_double_list_iterator(CircularDoubleList& c) :center(&c), next(c.circular_double_list_next), prev(c.circular_double_list_prev) {}
        circular_double_list_iterator& remove() { 
            //center->fake_delete();
            delete center; 
            return *this; 
        }

        CircularDoubleList& operator*() { return *center; }
        bool operator ++() { center = next; prev = center->circular_double_list_prev; next = center->circular_double_list_next;  return !center->sentinel(); }

        bool operator --() { center = prev; prev = center->circular_double_list_prev; next = center->circular_double_list_next;  return !center->sentinel(); }

        operator bool() { return !center->sentinel(); }
        bool operator !() { return center->sentinel(); }
    };
    virtual void fake_delete() { disconnect(); }
    circular_double_list_iterator iterate() { return circular_double_list_iterator(*this); }
    bool sentinel() const { return circular_double_list_is_sentinel; }

    virtual ~CircularDoubleList() { circular_double_list_next->circular_double_list_prev = circular_double_list_prev; circular_double_list_prev->circular_double_list_next = circular_double_list_next;}
    void disconnect() { circular_double_list_next->circular_double_list_prev = circular_double_list_prev; circular_double_list_prev->circular_double_list_next = circular_double_list_next; }
    CircularDoubleList(_sentinel_) :circular_double_list_prev(this), circular_double_list_next(this), circular_double_list_is_sentinel(true) {}
    CircularDoubleList(_before_, CircularDoubleList* e) :circular_double_list_prev(e->circular_double_list_prev), circular_double_list_next(e), circular_double_list_is_sentinel(false)
    {
        circular_double_list_next->circular_double_list_prev = circular_double_list_prev->circular_double_list_next = this;
    }
    CircularDoubleList(_after_, CircularDoubleList* e) :circular_double_list_prev(e), circular_double_list_next(e->circular_double_list_next),  circular_double_list_is_sentinel(false)
    {
        circular_double_list_next->circular_double_list_prev = circular_double_list_prev->circular_double_list_next = this;
    }
    
    CircularDoubleList(CircularDoubleList&&) = delete;
    CircularDoubleList() = delete;

    bool empty() { return circular_double_list_next == circular_double_list_prev; }
};
inline void merge_from_to(CircularDoubleList* source, CircularDoubleList* dest) {
    assert(source->sentinel());
    assert(dest->sentinel());

    if (!source->empty()) {
        source->circular_double_list_next->circular_double_list_prev = dest;
        source->circular_double_list_prev->circular_double_list_next = dest->circular_double_list_next;
        dest->circular_double_list_next->circular_double_list_prev = source->circular_double_list_prev;
        dest->circular_double_list_next = source->circular_double_list_next;
        source->circular_double_list_next = source->circular_double_list_prev = source;
    }
}

template <typename T>
struct RootPtr;
class Collectable;

class InstancePtrBase
{
protected:
public:
    GC::SnapPtr value;
    Collectable* get_collectable() { return GC::Handles[GC::load_snapshot(&value)].ptr; }
    void mark();
};

template <typename T>
class InstancePtr : public InstancePtrBase
{
//    void double_ptr_store( T* const v) { GC::double_ptr_store(&value, v == nullptr ? GC::NULLHandle : v->getHandle()); }
    void double_ptr_store(const T* v) { GC::double_ptr_store(&value, v->getHandle()); }
public:

    GC::Handle getHandle() const { return GC::load(&value); }
    T* get() const { return (T*)GC::Handles[GC::load(&value)].ptr; }

//    void store(T* const v) { GC::write_barrier(&value, v == nullptr ? GC::NULLHandle : v->getHandle()); }
    void store( T* const v) { GC::write_barrier(&value, v->getHandle()); }
    template<typename U>
    auto operator[](U i) const { return (*get())[i]; }
    T& operator*() const { return *get(); }
    T* operator -> () const { return get(); }

    InstancePtr() { GC::double_ptr_store(&value, GC::NULLHandle); }
 //   InstancePtr(GC::Handle const v) { double_ptr_store( v); }

    
    explicit InstancePtr(const T*  v) { double_ptr_store( v); }

    explicit InstancePtr(const InstancePtr<T>& o) {
        double_ptr_store(o.get());
    }

    void operator = (T *const o) {
        store(o);
    }

    void operator = (const InstancePtr<T>& o) {
        store(o.get());
    }

    explicit InstancePtr(const RootPtr<T>& o);

    void operator = (const RootPtr<T>& o);
};

template< class T, class U >
RootPtr<T> static_pointer_cast(const InstancePtr<U>& v) noexcept
{
    return RootPtr<T>(static_cast<T*>(v.get()));
}

template< class T, class U >
RootPtr<T> const_pointer_cast(const InstancePtr<U>& v) noexcept
{
    return RootPtr<T>(const_cast<T*>(v.get()));
}
template< class T, class U >
RootPtr<T> dynamic_pointer_cast(const InstancePtr<U>& v) noexcept
{
    return RootPtr<T>(dynamic_cast<T*>(v.get()));
}

template< class T, class U >
RootPtr<T> reinterpret_pointer_cast(const InstancePtr<U>& v) noexcept
{
    return RootPtr<T>(reinterpret_cast<T*>(v.get()));
}

class Collectable;
struct RootLetterBase;

//There is one ScanLists struct per thread
//ActiveIndex take the value 0 or 1 in each successive garbage collection
//collectables[ActiveIndex] is the double linked ring list that all new collectable objects go in
//between GCs it stores ALL collectable objects
//collectables[2] holds the sublist of objects that existed during the collection phase (when the write barrier didn't write the snapshot) even after the lists are merged so that the 
//the lists are scanned in order to restore the snapshot, no time is wasted on objects created AFTER the double write barrier was restored.
//the same goes for roots[ActiveIndex] and roots[2] but for root variables instead of objects
namespace GC {
    struct ScanLists
    {
        Collectable* collectables[3];
        RootLetterBase* roots[3];
    };

    extern ScanLists* ScanListsByThread[MAX_COLLECTED_THREADS];
    extern int ActiveIndex;
}

//Ok roots are complicated.  Because the snapshot of roots is important until the next garbage collection even if after a root disappears,
//root are divided into a letter and an envelope. The visible part is the envelope, and what that is destroyed the letter remains until the
//next collection.  However only roots that were active at the moment the collection started are need to be scanned, thus "was_owned" to hold
//that information. In theory we could delete root letters when there is no collection going on, but I realized that late and I would have
//to add another phase to the gc to enable that, because it is also not safe to delete roots when the gc is walking the root list to 
//restore the snapshot and that does happen outside of the collection phases.
//
struct RootLetterBase : public CircularDoubleList
{
    bool owned;
    bool was_owned;

    virtual GC::SnapPtr* double_ptr() { abort(); return nullptr; }
#ifndef NDEBUG
    bool deleted;

    void fake_delete() { deleted = true; disconnect(); }
    void memtest() { 
        ENSURE(!deleted); 
        if (deleted) std::cout << '.';
    }
    RootLetterBase(_sentinel_) : CircularDoubleList(_SENTINEL_), owned(true), was_owned(true), deleted(false)
    {  }
#else
    RootLetterBase(_sentinel_) : CircularDoubleList(_SENTINEL_), owned(true), was_owned(true)
    {  }
#endif

    RootLetterBase(RootLetterBase&&) = delete;
    RootLetterBase();
    virtual void mark() { abort(); }
    virtual ~RootLetterBase()
    {
    }
};

inline RootLetterBase::RootLetterBase():CircularDoubleList(_START_,GC::ScanListsByThread[GC::MyThreadNumber]->roots[GC::ActiveIndex]),owned(true),was_owned(true)
#ifndef NDEBUG
,deleted(false)
#endif
{
}

template <typename T>
struct RootLetter : public RootLetterBase
{
    InstancePtr<T> value;
    virtual GC::SnapPtr* double_ptr() { MEM_TEST(); 
    return &value.value; }
    virtual void mark() { 
        MEM_TEST();
        Collectable* c = value.get_collectable();
        if (c != collectable_null) 
            c->collectable_mark(); }

    RootLetter(RootLetter&&) = delete;

    RootLetter() {}
    RootLetter(const T* v) :value(v){}
};

template <typename T>
struct RootPtr
{
    RootLetter<T>* var;
    RootPtr<T>(RootPtr<T> &&) = default;
    void operator = ( T* const o)
    {
        var->value.store(o);
    }

    void operator = (const RootPtr<T>& v)
    {
        var->value.store(v.var->value.get());
    }


    void operator = (const InstancePtr<T>& v)
    {
        var->value.store(v.get());
    }


    //template <typename Y>
  //  void operator = (Y* const v)
  //  {
  //      var->value.store(v);
  //  }
    template <typename U>
    auto operator[](U i) { return (*get())[i]; }
    template <typename U>
    auto operator[](U i) const { return (*get())[i]; }
    T* get() const
    {
        return var->value.get();
    }
    GC::Handle getHandle() const
    {
        return var->value.getHandle();
    }

    T& operator*() const { return *get(); }

    T* operator -> () const
    {
        return var->value.get();
    }
 //   template <typename Y>
 //   RootPtr(Y* v) :var(new RootLetter<T>(v)) 
 //   {
 //       GC::log_alloc(sizeof(*var));
 //   }

    RootPtr(const T* v) : var(new RootLetter<T>(v))
    {
        GC::log_alloc(sizeof(*var));
    }
   // RootPtr(RootPtr<T>&& v) = delete;

    RootPtr(const InstancePtr<T>& v) :var(new RootLetter<T>(v.get())) {
#ifndef NDEBUG
        v->memtest();
        var->memtest();
#endif
        GC::log_alloc(sizeof(*var));
    }

    RootPtr(const RootPtr<T>& v) :var(new RootLetter<T>(v.var->value.get())) {
#ifndef NDEBUG
        v.var->memtest();
        var->memtest();
#endif
        GC::log_alloc(sizeof(*var));
    }

    template <typename Y>
    RootPtr (const RootPtr<Y>  &v) :var(new RootLetter<T>(v.var->value.get())){
#ifndef NDEBUG
        v.var->memtest();
        var->memtest();
#endif
        GC::log_alloc(sizeof(*var));
    }
//    template <typename Y>
//    RootPtr (const InstancePtr<Y> &v) :var(new RootLetter<T>(v.get())) {
//#ifdef NDEBUG
//        var->memtest();
//#endif
//        GC::log_alloc(sizeof(*var));
//    }
    RootPtr() :var(new RootLetter<T>)
    {
        GC::log_alloc(sizeof(*var));
    }
    ~RootPtr() { 
        var->owned = false; 
        if (GC::ThreadState != GC::PhaseEnum::COLLECTING) var->was_owned = false;
    }
};

template<typename T>
InstancePtr<T>::InstancePtr(const RootPtr<T>& o)
{
    double_ptr_store(o.get());
}

template<typename T>
void InstancePtr<T>::operator = (const RootPtr<T>& o)
{
    store(o.get());
}

template< class T, class U >
RootPtr<T> static_pointer_cast(const RootPtr<U>& v) noexcept
{
#ifndef NDEBUG
    v->memtest();
#endif
    return RootPtr<T>(static_cast<T*>(v.var->value.get()));
}

template< class T, class U >
RootPtr<T> const_pointer_cast(const RootPtr<U>& v) noexcept
{
#ifndef NDEBUG
    v->memtest();
#endif
    return RootPtr<T>(const_cast<T*>(v.var->value.get()));
}

template< class T, class U >
RootPtr<T> const_dynamic_cast(const RootPtr<U>& v) noexcept
{
#ifndef NDEBUG
    v->memtest();
#endif
    return RootPtr<T>(dynamic_cast<T*>(v.var->value.get()));
}

template< class T, class U >
RootPtr<T> const_reinterpret_cast(const RootPtr<U>& v) noexcept
{
#ifndef NDEBUG
    v->memtest();
#endif
    return RootPtr<T>(reinterpret_cast<T*>(v.var->value.get()));
}



namespace GC {
    void merge_collected();
    void _do_collection();
    void _do_restore_snapshot();
    void _end_collection_start_restore_snapshot();
    void _do_finalize_snapshot();
}

enum class CollectableEqualityClass
{
    by_address,
    by_string,
};

#define ONE_COLLECT_THREAD

class Collectable: public CircularDoubleList {
protected:
    friend void GC::merge_collected();
    friend void GC::_do_collection();
    friend void GC::_do_restore_snapshot();
    friend void GC::_end_collection_start_restore_snapshot();
    friend void GC::_do_finalize_snapshot();
//public:
//    bool deleted;
protected:
    //when not tracing contains self index
    //when tracing points back to where we came from or 0 if that was a root
    //when in a free list points to the next free element as an unbiased index into this block
    Collectable* collectable_back_ptr;

    unsigned int collectable_back_ptr_from_counter : 31;//came from nth snapshot ptr
#ifdef ONE_COLLECT_THREAD
    bool collectable_marked : 1; //only one collection thread
#else
    std::atomic_bool marked;
#endif
    virtual ~Collectable() 
    {
        GC::DeallocHandleInGC(myHandle);
    }
    Collectable(_sentinel_) : CircularDoubleList(_SENTINEL_), collectable_back_ptr(collectable_null) , collectable_marked(false), myHandle(GC::AllocateHandle())
#ifndef NDEBUG
        ,deleted(0)
#endif
    {
        GC::Handles[myHandle].ptr = this;
    }

public:
    GC::Handle myHandle;
#ifndef NDEBUG

    int32_t deleted;
    void memtest() const {
        ENSURE(deleted != 0xfeebfdcb);
        ENSURE(deleted == 0);
        if (deleted == 0xfeebfdcb) std::cout << ':';
}
    void fake_delete() {
        if (deleted == 0xfeebfdcb) {
            std::cout << '$';
            return;
        }
        else if (deleted != 0) {
            std::cout << 'C';
            //return;
        }
        if (collectable_marked != 0 && collectable_marked != 0xbf) {
            std::cout << 'c';
        }
        deleted = 0xfeebfdcb; disconnect();
        GC::DeallocHandleInGC(myHandle);
    }

#endif
    GC::Handle getHandle() const { return myHandle; }
    //to keep from blowing the stack when marking a long chain, this is coded as a loop instead of being recursive and back pointers and context
    //is stored in the objects themselves instead of on the stack.
    void collectable_mark()
    {
        MEM_TEST();
        Collectable* n=collectable_null;
        Collectable* c = this;
        //if (deleted) std::cout << '!';
        if (collectable_marked) return;
#ifdef ONE_COLLECT_THREAD
        collectable_marked = true;
        {
#else
        bool got_it = marked.exchange(true);
        if (!got_it) {
#endif
            int t = total_instance_vars() - 1;
            for (;;) {
                if (t >= 0) {
                    InstancePtrBase* b = c->index_into_instance_vars(t);
                    if (b != nullptr) {
                        n = b->get_collectable();
                        if (n != collectable_null) {
#ifndef NDEBUG
                            if (n->deleted) std::cout << '*';
#endif

                            if (!n->collectable_marked) {
#ifdef ONE_COLLECT_THREAD
                                n->collectable_marked = true;
                                {
#else
                                got_it = marked.exchange(true);
                                if (!got_it) {
#endif
                                    n->collectable_back_ptr_from_counter = t;
                                    n->collectable_back_ptr = c;
                                    c = n;
                                    t = c->total_instance_vars() - 1;
                                    continue;
                                }
                            }
                        }
                    }
                    --t;
                }
                else {
                    if (c == this) return;
                    n = c;
                    c = c->collectable_back_ptr;
                    t = n->collectable_back_ptr_from_counter - 1;
                }
            }
        }
    }
    //virtual int num_ptrs_in_snapshot() = 0;
    //virtual GC::SnapPtr* index_into_snapshot_ptrs(int num) = 0;
    //not snapshot, includes ones that could be null because they're live
    virtual int total_instance_vars() const = 0;
    void log_size(size_t s) { GC::log_alloc(s); }
    //virtual size_t my_size() const = 0;
    virtual InstancePtrBase* index_into_instance_vars(int num) = 0;
    virtual void clean_after_collect() {}
    virtual CollectableEqualityClass equality_class() const { return CollectableEqualityClass::by_address; }
    virtual bool equal(const Collectable *o)
    {
        CollectableEqualityClass oc = o->equality_class();
        if (equality_class() != oc) return false;
        return this == o;//assume  CollectableEqualityClass::by_address;
    }
    virtual uint64_t hash() const 
    {
        uint64_t buf = reinterpret_cast<uint64_t>(this);

        return spooky_hash64((void *)&buf,8,0xe0f4deda25b832e0);
    }
    Collectable(Collectable&&) = delete;

    Collectable() :CircularDoubleList(_START_, GC::ScanListsByThread[GC::MyThreadNumber]->collectables[GC::ActiveIndex]), collectable_back_ptr(collectable_null), collectable_marked(false), myHandle(GC::AllocateHandle())
#ifndef NDEBUG
        ,deleted(false)
#endif
    {
       GC::Handles[myHandle].ptr = this;
    }

};
struct CollectableString : public Collectable
{
    char* str;
    CollectableString(const char* s) :str(_strdup(s)) { log_size(sizeof(this)+strlen(s)); }
    ~CollectableString() { free (str); }
    virtual int total_instance_vars() const { return 0; }
    //virtual size_t my_size() const { return sizeof(*this); }
    virtual InstancePtrBase* index_into_instance_vars(int num) { return nullptr; }
    virtual void clean_after_collect() {}
    virtual CollectableEqualityClass equality_class() const { return CollectableEqualityClass::by_string; }
    virtual bool equal(const Collectable* o)
    {
        CollectableEqualityClass oc = o->equality_class();
        if (equality_class() != oc) return false;
        return strcmp(str,((CollectableString*)o)->str)==0;
    }
    virtual uint64_t hash() const
    {

        return spooky_hash64((void*)str,strlen(str), 0xc243487c4b5ee78e);
    }

};
template<>
struct std::hash<CollectableString>
{
    std::size_t operator()(CollectableString const& s) const noexcept
    {
        return (size_t)s.hash();
    }
};

inline std::ostream& operator<<(std::ostream& os, const RootPtr<CollectableString>& o) {
    return os << o->str;
}

template<typename T>
struct CollectableBlock : public Collectable
{
    InstancePtr<T> block[32];
    uint8_t size;
    uint8_t reserved;

    //size_t my_size() { return sizeof(this); }
    int total_instance_vars() { return reserved; }


    InstancePtrBase* index_into_instance_vars(int num) { return &block[num]; }
    bool push_back(const RootPtr<T>& o) {
        if (size == 32) return false;
        block[size++] = o;
        if (size >= reserved) reserved = size;
        return true;
    }
    bool pop_back(const RootPtr<T>& o) {
        if (size == 0) return false;
        o = block[--size];
        block[size] = collectable_null;
        return true;
    }
    bool pop_back(const InstancePtr<T>& o) {
        if (size == 0) return false;
        o = block[--size];
        block[size] = collectable_null;
        return true;
    }
    CollectableBlock() :size(0), reserved(0) { log_size(sizeof(this)); }
    InstancePtr<T>& operator [] (int i) {
        return block[i & 31];
    }
    InstancePtr<T>& insure (int i) {
        int s = i+1;
        if (s > size) size = s;
        if (size >= reserved) reserved = size;
        return block[i & 31];
    }
    void clear()
    {
        for (int i = 0; i < size; ++i) block[i] = collectable_null;
        size = 0;
    }
    void resize(int s)
    {
        assert(s < 32);
        while (size > s)block[--size] = collectable_null;
        size = s;
    }
};
template<typename T>
struct Collectable2Block : public Collectable
{
    InstancePtr< CollectableBlock<T> > block[32];
    //uint8_t b_size;
    uint8_t b_reserved;
    int size;

    int b_size(int i=0) { return ((size+i-1) >> 5)+1; }
   

    //size_t my_size() { return sizeof(this); }
    int total_instance_vars() { return b_reserved; }
    InstancePtrBase* index_into_instance_vars(int num) { return &block[num]; }
    Collectable2Block() :size(0), b_reserved(0) { log_size(sizeof(this)); }
    bool push_back(const RootPtr<T>& o) {
        if (size == 32*32) return false;
        ++size;
        if (b_size() > b_size(-1)) {
            if (block[b_size() - 1].get() == collectable_null) block[b_size() - 1] = new CollectableBlock<T>;
            if (b_size() > b_reserved) b_reserved = b_size();
        }
        return block[b_size() - 1]->push_back(o);
    }

    bool pop_back(RootPtr<T>& o)
    {
        if (size == 0) return false;
        block[b_size() - 1]->pop_back(o);
        --size;
        //b_size = ((--size-1) >> 5)+1;
        return true;
    }

    bool pop_back(InstancePtr<T>& o)
    {
        if (size == 0) return false;
        block[b_size() - 1]->pop_back(o);
        --size;
        //b_size = ((--size-1) >> 5) + 1;
        return true;
    }

    InstancePtr<T>& operator [] (int i) {
        return block[31 & (i >> 5)]->operator[](i);
    }
    InstancePtr<T>& insure(int i) {
        int j = 31&(i >> 5);
        for (int k = b_size()-1; k <= j; ++k) {
            if (block[k].get() == collectable_null) {
                block[k] = new CollectableBlock<T>;
                if (k<j) block[k]->insure((1<<6)-1);
            }
        }
        int s = i+1;
        if (s > size) size = s;
        if (b_reserved<b_size()) b_reserved = b_size();
        

        return block[j]->insure(i-(j<<5));
    }
    void clear()
    {
        GC::safe_point();
        for (int i = 0; i < b_size(); ++i) block[i]->clear();
        size = 0;
    }
    void resize(int s)
    {
        assert(s < 32*32);
        if (s == size) return;
        if (s > size) {
            insure(s - 1);
        }
        else {
            int j = (s - 1) >> 5;
            block[j]->resize(s - (j << 5) + 1);
            for (int k = j + 1; k < b_size(); ++k) block[k]->clear();
            size = s;
        }
    }
};

template<typename T>
struct Collectable3Block : public Collectable
{
    InstancePtr< Collectable2Block<T> > block[32];
    int b_size(int i = 0) { return ((size + i - 1) >> 10) + 1; }
    uint8_t b_reserved;
    int size;



    //size_t my_size() { return sizeof(this); }
    int total_instance_vars() { return b_reserved; }
    Collectable3Block() :size(0), b_reserved(0) { log_size(sizeof(this)); }
    InstancePtrBase* index_into_instance_vars(int num) { return &block[num]; }
    bool push_back(const RootPtr<T>& o) {
        if (size == 32 * 32 * 32) return false;
        ++size;
        if (b_size() > b_size(-1)) {
            if (block[b_size()-1].get() == collectable_null) block[b_size()-1] = new Collectable2Block<T>;
            if (b_size() > b_reserved) b_reserved = b_size();
        }
        return block[b_size() - 1]->push_back(o);
    }
    bool pop_back(RootPtr<T>& o)
    {
        if (size == 0) return false;
        block[b_size() - 1]->pop_back(o);
        --size;
        //b_size = ((--size-1) >> 5)+1;
        return true;
    }
    bool pop_back(InstancePtr<T>& o)
    {
        if (size == 0) return false;
        block[b_size() - 1]->pop_back(o);
        --size;
        //b_size = ((--size-1) >> 5)+1;
        return true;
    }
    InstancePtr<T>& operator [] (int i) {
        return block[31&(i >> 10)]->operator [](i);
    }
    InstancePtr<T>& insure(int i) {
        int j = 31 & (i >> 10);
        for (int k = b_size() - 1; k <= j; ++k) {
            if (block[k].get() == collectable_null) {
                block[k] = new Collectable2Block<T>;
                if (k < j) block[k]->insure((1 << 11) - 1);
            }
        }
        int s = i+1;
        if (s > size) size = s;        
        if (b_reserved < b_size()) b_reserved = b_size();


        return block[j]->insure(i-(j<<10));
    }
    void clear()
    {
        for (int i = 0; i < b_size(); ++i) {
            block[i]->clear();
            GC::safe_point();
        }
     
        size = 0;
    }
    void resize(int s)
    {
        assert(s < 32*32*32);
        if (s == size) return;
        if (s > size) {
            insure(s - 1);
        }
        else {
            int j = (s - 1) >> 10;
            block[j]->resize(s - (j << 10) + 1);
            for (int k = j + 1; k < b_size(); ++k) block[k]->clear();
            size = s;
        }
    }
};

template<typename T>
struct Collectable4Block : public Collectable
{
    InstancePtr< Collectable3Block<T> > block[32];

    uint8_t b_reserved;
    int size;
    int b_size(int i = 0) { return ((size + i - 1) >> 15)+1; }

    //size_t my_size() { return sizeof(this); }
    int total_instance_vars() { return b_reserved; }
    Collectable4Block() :size(0), b_reserved(0) { log_size(sizeof(this)); }
    InstancePtrBase* index_into_instance_vars(int num) { return &block[num]; }
    bool push_back(const RootPtr<T>& o) {
        if (size == 32 * 32 * 32) return false;
        ++size;
        if (b_size() > b_size(-1)) {
            if (block[b_size() - 1].get() == collectable_null) block[b_size() - 1] = new Collectable3Block<T>;
            if (b_size() > b_reserved) b_reserved = b_size();
        }
        return block[b_size() - 1]->push_back(o);

    }
    bool pop_back(RootPtr<T>& o)
    {
        if (size == 0) return false;
        block[b_size() - 1]->pop_back(o);
        --size;
        //b_size = ((--size-1) >> 5)+1;
        return true;
    }
    bool pop_back(InstancePtr<T>& o)
    {
        if (size == 0) return false;
        block[b_size() - 1]->pop_back(o);
        --size;
        //b_size = ((--size-1) >> 5)+1;
        return true;
    }
    InstancePtr<T>& operator [] (int i) {
        return block[31 & (i >> 15)]->operator [](i);
    }

    InstancePtr<T>& insure(int i) {
        int j = 31 & (i >> 15);
        for (int k = b_size() - 1; k <= j; ++k) {
            if (block[k].get() == collectable_null) {
                block[k] = new Collectable3Block<T>;
                if (k < j) block[k]->insure((1 << 16) - 1);
            }
        }
        int s = i+1;
        if (s > size) size = s;        
        if (b_reserved < b_size()) b_reserved = b_size();

        return block[j]->insure(i-(j<<15));
    }

    bool push_front(const RootPtr<T>& o) {
        if (size == 32 * 32 * 32 * 32) return false;
        if (size > 0) {
            insure(size) = (*this)[size - 1];
            for (int i = size - 1; i > 0; --i) {
                if ((i&1023)==0) GC::safe_point();
                (*this)[i] = (*this)[i - 1];
            }
            (*this)[0] = o;
        }
        else return push_back(o);
        return true;
    }

    void clear()
    {
        for (int i = 0; i < b_size(); ++i) {
            GC::safe_point();
            block[i]->clear();
        }
        size = 0;
    }
    void resize(int s)
    {
        assert(s < 32*32*32*32);
        if (s == size) return;
        if (s > size) {
            insure(s - 1);
        }
        else {
            int j = (s-1) >> 15;
            block[j]->resize(s - (j << 15)+1);
            for (int k = j + 1; k < b_size(); ++k) block[k]->clear();
            size = s;
        }
    }
};

template<typename T>
class VectorOfCollectable;

template<typename T>
struct CollectableInlineVector : public Collectable
{
    T* data;
    InstancePtrBase * * instance_counts;
    int total_vars;
    int size;
    /*
    void resize(int s) {
        if (s < 0)s = 0;
        else if (s > reserve) s = reserve;
        if (s<size){
            for (int i = size - 1; i >= s; --i) {
                data[i].~T();
            }
        }
        else if (s > size) {
            for (int i = size; i < s; ++i)
                new((void *)&data[i]) T();
        }
        size = s;
    }

    bool push_back(const T& s) {
        if (size == reserve) return false;
        data[size++].T(s);
        return true;
    }
    */
    T* operator [](int i)
    {
        return &data[i];
    }

    CollectableInlineVector(int s) : instance_counts(nullptr),size(s){
        data = new T [s];
        assert(data != nullptr);
        total_vars = 0;
        for (int i = 0; i < s; ++i) {
            total_vars += data[i].total_instance_vars();
        }
        instance_counts = new InstancePtrBase * [total_vars];
        int t = 0;
        for (int i = 0; i < s; ++i) {
            for (int j = data[i].total_instance_vars() - 1; j >= 0; --j) {
                instance_counts[t++] = data[i].index_into_instance_vars(j);
            }
        }
        log_size( sizeof(*this) + size * sizeof(T) + total_vars * sizeof(void*));
    }
    int total_instance_vars() const
    {
        return total_vars;
    }
//    size_t my_size() const 
//    {
//        return sizeof(*this)+size*sizeof(T)+ total_vars*sizeof(void *);
//    }
    InstancePtrBase* index_into_instance_vars(int num)
    {
        return instance_counts[num];
    }
    ~CollectableInlineVector()
    {
        delete [] data;
        delete[] instance_counts;
    }
};
/*
template <typename T>
class CollectableInlineVector : public Collectable {
    InstancePtr< CollectableInlineVectorUse<T> > data;
public:
    int total_instance_vars() const { return 1;  }
    InstancePtrBase* index_into_instance_vars(int num) { return &data; }
    size_t my_size() const { return sizeof(*this) + data->my_size(); }

    //bool empty() const { return data->size == 0; }
    //void clear() const { data->resize(0); }
    //int capacity() const { return data->reserve;   }
    //void reserve(int i)
    //{
    //    if (i > data->reserve) {
    //        RootPtr< CollectableInlineVectorUse<T> > new_data = cnew( CollectableInlineVectorUse<T>(i));
    //        for (int j = 0; j < data->size; ++j) new_data->push_back(data.data[j]);
    //        data = new_data;
    //    }
    //}
    CollectableInlineVector(int i) { data = cnew  (CollectableInlineVectorUse<T>(i)); /data->resize(i); }
    //CollectableInlineVector() { data = cnew(  CollectableInlineVectorUse<T> (8)); }
    //int size() { return data->size(); }
    //void resize(int i)
    //{
    //    if (i > size) {
    //        if (i > data->reserve) {
    //            reserve(i * 2);
    //        }
    //    }
    //    data->resize(i);
    //}
    //void push_back(const RootPtr<T> v)
    //{
    //    if (data->size == data->reserve) resize(data->reserve << 1);
    //    if (v == collectable_null) data->resize(size() + 1);
    //    else data->push_back(*v.get());
    //}
    T* operator[](int i) { return (*data)[i]; }
};
*/
template<typename T>
struct CollectableVectoreUse : public Collectable
{
    int size;
    int scan_size;
    int reserved;

    std::unique_ptr<InstancePtr<T> > data;

    CollectableVectoreUse(int s) :size(0), scan_size(0), reserved(s), data(new InstancePtr<T>[s]) { log_size(sizeof(*this) + sizeof(InstancePtr<T>) * reserved); }
    int total_instance_vars() const {
        MEM_TEST();
        return scan_size;
    }
    InstancePtrBase* index_into_instance_vars(int num) {
        MEM_TEST();
        return data.get() + num;
    }
    //size_t my_size() const { return sizeof(*this) + sizeof(InstancePtr<T>) * reserved; }


    bool push_back(const RootPtr<T>& o) {
        MEM_TEST();
        if (size >= reserved) return false;
        (data.get())[size++] = o;
        if (size > scan_size || GC::ThreadState != GC::PhaseEnum::COLLECTING) scan_size = size;
        return true;
    }
    bool pop_back(RootPtr<T>& o) {
        MEM_TEST();
        if (size == 0) return false;
        o = (data.get())[--size].get();
        (data.get())[size] = (T*)collectable_null;
        assert(GC::ThreadState != GC::PhaseEnum::NOT_MUTATING);
        if (scan_size > size && GC::ThreadState != GC::PhaseEnum::COLLECTING) scan_size = size;
        return true;
    }
    bool pop_back(InstancePtr<T>& o) {
        MEM_TEST();
        if (size == 0) return false;
        o = (data.get())[--size];
        (data.get())[size] = (T*)collectable_null;
        assert(GC::ThreadState != GC::PhaseEnum::NOT_MUTATING);
        if (scan_size > size && GC::ThreadState != GC::PhaseEnum::COLLECTING) scan_size = size;
        return true;
    }
    InstancePtr<T>& at (int i) {
        MEM_TEST();
        if (i < 0 || i >= size) throw std::out_of_range("CollectableVector index out of range");
        return (data.get())[i];
    }
    InstancePtr<T>& operator[](int i) {
        MEM_TEST();
        return (data.get())[i];
    }
    void clear()
    {
        MEM_TEST();
        for (int i = 0; i < size; ++i) (data.get())[i] = (T*)collectable_null;
        size = 0;
        assert(GC::ThreadState != GC::PhaseEnum::NOT_MUTATING);
        if (GC::ThreadState != GC::PhaseEnum::COLLECTING) scan_size =0 ;
    }
    bool resize(int s, const RootPtr<T>& exemplar)
    {
        MEM_TEST();
        if (s > reserved) return false;
        if (s < size) while (size > s)(data.get())[--size] = (T*)collectable_null;
        else while (size < s)(data.get())[size++] = exemplar;
        assert(GC::ThreadState != GC::PhaseEnum::NOT_MUTATING);
        if (size > scan_size || GC::ThreadState != GC::PhaseEnum::COLLECTING) scan_size = size;

        return true;
    }
    bool resize(int s, InstancePtr<T>& exemplar)
    {
        MEM_TEST();
        if (s > reserved) return false;
        if (s < size) while (size > s)(data.get())[--size] = (T*)collectable_null;
        else while (size < s)(data.get())[size++] = exemplar;
        assert(GC::ThreadState != GC::PhaseEnum::NOT_MUTATING);
        if (size > scan_size || GC::ThreadState != GC::PhaseEnum::COLLECTING) scan_size = size;
        return true;
    }
    bool resize(int s)
    {
        MEM_TEST();
        if (s > reserved) return false;
        if (s < size) while (size > s)(data.get())[--size] = (T*)collectable_null;
        size = s;
        assert(GC::ThreadState != GC::PhaseEnum::NOT_MUTATING);
        if (size > scan_size || GC::ThreadState != GC::PhaseEnum::COLLECTING) scan_size = size;
        return true;
    }
    bool push_front(const RootPtr<T>& o)
    {
        MEM_TEST();
        if (size >= reserved) return false;
        if (size > 0) {
            (*this)[size] = (*this)[size - 1];
            for (int i = size - 1; i > 0; --i) {
                if ((i & 1023) == 0) GC::safe_point();
                (*this)[i] = (*this)[i - 1];
            }
            (*this)[0] = o;
            ++size;
        }
        else return push_back(o);
        if (size > scan_size || GC::ThreadState != GC::PhaseEnum::COLLECTING) scan_size = size;
        return true;
    }
    void update_scan_size()
    {
        assert(GC::ThreadState != GC::PhaseEnum::NOT_MUTATING);
        if (size > scan_size || GC::ThreadState != GC::PhaseEnum::COLLECTING) scan_size = size;
    }
};


template<typename T>
class CollectableVector : public Collectable
{

    friend class circular_double_list_iterator;
    friend class const_iterator;

    InstancePtr<CollectableVectoreUse<T> > data;
    public:
    int total_instance_vars() const {
        MEM_TEST();
        return 1;
    }
    InstancePtrBase* index_into_instance_vars(int num) {
        MEM_TEST();
        return &data;
    }
    //size_t my_size() const { return sizeof(*this); }

    struct iterator;
    struct const_iterator {
        RootPtr<const CollectableVector<T> > v;
        int pos;
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = int;

        explicit const_iterator() :pos(0) {  }
        explicit const_iterator(RootPtr<CollectableVector<T> >& v) :pos(0) {  }

        const_iterator(const CollectableVector<T>::const_iterator &t) :v(t.v), pos(t.pos) {}
        const_iterator(const CollectableVector<T>::iterator &t) :v(t.v), pos(t.pos) {}

        const_iterator& operator =(CollectableVector<T>::const_iterator t)
        {
            v = t.v;
            pos = t.pos;
            return *this;
        }

        const_iterator& operator =(CollectableVector<T>::iterator t)
        {
            v = t.v;
            pos = t.pos;
            return *this;
        }

        const_iterator& operator++() { ++pos; return *this; }
        const_iterator& operator--() { --pos; return *this; }

        const_iterator& operator+=(int i) { pos += i; return *this; }
        const_iterator& operator-=(int i) { pos -= i; return *this; }

        const_iterator operator+(int i) { return const_iterator(v, pos + i); }
        const_iterator operator-(int i) { return const_iterator(v, pos - i); }

        RootPtr<T> operator++(int) { int p = pos;  ++pos; return v[p]; }
        RootPtr<T> operator--(int) { int p = pos;  --pos; return v[p]; }

        RootPtr<T> operator*() { return v[pos]; }
        const InstancePtr<T>* operator->() { return &v[pos]; }

        bool operator == (CollectableVector<T>::iterator i) {
            return pos == i.pos && v.data.get() == i.pos.data.get();
        }
        bool operator < (CollectableVector<T>::iterator i) {
            return pos < i.pos&& v.data.get() == i.pos.data.get();
        }
        bool operator > (CollectableVector<T>::iterator i) {
            return pos > i.pos && v.data.get() == i.pos.data.get();
        }
        bool operator >= (CollectableVector<T>::iterator i) {
            return pos >= i.pos && v.data.get() == i.pos.data.get();
        }
        bool operator <= (CollectableVector<T>::iterator i) {
            return pos <= i.pos && v.data.get() == i.pos.data.get();
        }
        bool operator != (CollectableVector<T>::iterator i) {
            return pos != i.pos && v.data.get() == i.pos.data.get();
        }
        bool operator == (CollectableVector<T>::const_iterator i) {
            return pos == i.pos && v.data.get() == i.pos.data.get();
        }
        bool operator < (CollectableVector<T>::const_iterator i) {
            return pos < i.pos&& v.data.get() == i.pos.data.get();
        }
        bool operator > (CollectableVector<T>::const_iterator i) {
            return pos > i.pos && v.data.get() == i.pos.data.get();
        }
        bool operator >= (CollectableVector<T>::const_iterator i) {
            return pos >= i.pos && v.data.get() == i.pos.data.get();
        }
        bool operator <= (CollectableVector<T>::const_iterator i) {
            return pos <= i.pos && v.data.get() == i.pos.data.get();
        }
        bool operator != (CollectableVector<T>::const_iterator i) {
            return pos != i.pos && v.data.get() == i.pos.data.get();
        }
    };
    struct iterator{
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = int;

        RootPtr<CollectableVector<T> > v;
        int pos;
        explicit iterator():pos(0) {  }
        explicit iterator(const RootPtr<CollectableVector<T> > &v) :v(v),pos(0) {  }
        explicit iterator(const RootPtr<CollectableVector<T> >& v, int p) :v(v),pos(p) {  }
        iterator(const CollectableVector<T>::const_iterator &t) :v(t.v), pos(t.pos) {}
        iterator(const CollectableVector<T>::iterator &t) :v(t.v), pos(t.pos) {}
        
        iterator& operator =(CollectableVector<T>::const_iterator t)
        {
            v = t.v;
            pos = t.pos;
            return *this;
        }

        iterator& operator =(CollectableVector<T>::iterator t)
        {
            v = t.v;
            pos = t.pos;
            return *this;
        }

        iterator& operator++() { ++pos; return *this; }
        iterator& operator--() { --pos; return *this; }

        iterator& operator+=(int i) { pos+=i; return *this; }
        iterator& operator-=(int i) { pos-=i; return *this; }

        iterator operator+(int i) { return iterator(v, pos + i); }
        iterator operator-(int i) { return iterator(v, pos - i); }

        RootPtr<T> operator++(int) { int p = pos;  ++pos; return v[p]; }
        RootPtr<T> operator--(int) { int p = pos;  --pos; return v[p]; }

        InstancePtr<T>& operator*() { return v->operator[](pos); }
        InstancePtr<T>* operator->() { return &v[pos]; }

        bool operator == (CollectableVector<T>::iterator i) {
            return pos == i.pos && v->data.get() == i.v->data.get();
        }
        bool operator < (CollectableVector<T>::iterator i) {
            return pos < i.pos && v->data.get() == i.v->data.get();
        }
        bool operator > (CollectableVector<T>::iterator i) {
            return pos > i.pos && v->data.get() == i.v->data.get();
        }
        bool operator >= (CollectableVector<T>::iterator i) {
            return pos >= i.pos && v->data.get() == i.v->data.get();
        }
        bool operator <= (CollectableVector<T>::iterator i) {
            return pos <= i.pos && v->data.get() == i.v->data.get();
        }
        bool operator != (CollectableVector<T>::iterator i) {
            return pos != i.pos && v->data.get() == i.v->data.get();
        }
        bool operator == (CollectableVector<T>::const_iterator i) {
            return pos == i.pos && v->data.get() == i.v->data.get();
        }
        bool operator < (CollectableVector<T>::const_iterator i) {
            return pos < i.pos&& v->data.get() == i.v->data.get();
        }
        bool operator > (CollectableVector<T>::const_iterator i) {
            return pos > i.pos && v->data.get() == i.v->data.get();
        }
        bool operator >= (CollectableVector<T>::const_iterator i) {
            return pos >= i.pos && v->data.get() == i.v->data.get();
        }
        bool operator <= (CollectableVector<T>::const_iterator i) {
            return pos <= i.pos && v->data.get() == i.v->data.get();
        }
        bool operator != (CollectableVector<T>::const_iterator i) {
            return pos != i.pos && v->data.get() == i.v->data.get();
        }
    };
    iterator begin() {
        MEM_TEST();
        return iterator(this);
    }
    const_iterator begin() const {
        MEM_TEST();
        return const_iterator(this);
    }
    const_iterator cbegin() {
        MEM_TEST();
        return const_iterator(this);
    }
    iterator end() {
        MEM_TEST();
        return iterator(this,size());
    }
    const_iterator end() const {
        MEM_TEST();
        return const_iterator(this,size());
    }
    const_iterator cend() {
        MEM_TEST();
        return const_iterator(this, size());
    }

    bool empty() const { return size() == 0; }

    void assign(const RootPtr<CollectableVector<T> > &o)
    {
        MEM_TEST();
        if (this == o.get()) return;
        clear();
        int s = o->size();
        for (int i = 0; i < s; ++i) push_back(o->at(i));
    }
    void assign(InstancePtr<CollectableVector<T> >& o)
    {
        MEM_TEST();
        if (this == o.get()) return;
        clear();
        int s = o->size();
        for (int i = 0; i < s; ++i) push_back(o->at(i));
    }

    CollectableVector() : data(new CollectableVectoreUse<T>(8)) { log_size(sizeof(*this)); }
    CollectableVector(int s) : data(new  CollectableVectoreUse<T>(s<<1)){ log_size(sizeof(*this)); }
    CollectableVector(int s, const RootPtr<T>& exemplar) : data(new  CollectableVectoreUse<T>(s << 1)){ resize(s, exemplar); log_size(sizeof(*this));
    }
    CollectableVector(int s, InstancePtr<T>& exemplar) : data(new  CollectableVectoreUse<T>(s << 1)) { resize(s, exemplar); log_size(sizeof(*this));
    }
    void push_back(const RootPtr<T>& o)
    {
        MEM_TEST();
        if (!data->push_back(o)) {
            reserve(size()+1);
            data->push_back(o);
        }
    }
    int size() const 
    { 
        MEM_TEST();
        return data->size;
    }
    bool pop_back(RootPtr<T>& o) 
    {
        MEM_TEST();
        return data->pop_back(o);
    }
    bool pop_back(InstancePtr<T>& o)
    {
        MEM_TEST();
        return data->pop_back(o);
    }
    RootPtr<T> operator[](int i) const
    {
        MEM_TEST();
        return data->data.get()[i];
    }
    InstancePtr<T>& operator[](int i)
    {
        MEM_TEST();
        return data->data.get()[i];
    }
    RootPtr<T> at(int i) const
    {
        MEM_TEST();
        return data->at(i);
    }
    InstancePtr<T>& at (int i)
    {
        MEM_TEST();
        return data->at(i);
    }
    void clear() 
    { 
        MEM_TEST();
        data->clear();
    }

    iterator erase(const_iterator t) {
        MEM_TEST();
        if (t.pos >= size) return end();
        int s = size() - 1;
        for (int i=t.pos;i<s;++i)
        {
            data->data.get()[i] = data->data.get()[i + 1];
        }
        data->data.get()[s] = collectable_null;
        data->size = s;
        data->update_scan_size();
        return iterator(*this,t.pos+1);
    }

    iterator erase(const_iterator f, const_iterator t) {
        MEM_TEST();
        if (f.pos >= size) return end();
        if (f.pos >= t.pos) return iterator(*this, f.pos);

        int e = t.pos;
        if (e > size())e = size();
        int d = e - f.pos;
        int s = size() - d;
        int i;
        for (i = f.pos; i < s; ++i)
        {
            data->data.get()[i] = data->data.get()[i + d];
        }
        for (;i<size();++i)
            data->data.get()[i] = collectable_null;

        data->size = s;
        data->update_scan_size();

        return iterator(*this, f.pos + 1);
    }

    iterator insert(const_iterator f, const RootPtr<T>& a)
    {
        MEM_TEST();
        int p = f.pos;
        int s = size();
        if (p > s) p = s;
        if (p < 0)p = 0;
        reserve(s + 1);
        for (int i = s; i > p; --i)data->data.get()[i] = data->data.get()[i - 1];
        data->data.get()[p] = a;
        ++data->size;
        data->update_scan_size();

        return iterator(this, p);
    }

    iterator insert(const_iterator f, int n, const RootPtr<T>& a)
    {
        MEM_TEST();
        int p = f.pos;
        if (n<1) return iterator(*this, p);
        int s = size();
        if (p > s) p = s;
        if (p < 0)p = 0;
        reserve(s + n);
        for (int i = s-1; i >= p; --i)data->data.get()[i+n] = data->data.get()[i];
        for (int i=0;i<n;++i) data->data.get()[p+i] = a;
        data->size+=n;
        return iterator(this, p+n);
    }
    iterator insert(const_iterator f, const_iterator t, const RootPtr<T>& a)
    {
        MEM_TEST();
        int n = t.pos - f.pos;
        int p = f.pos;
        if (n < 1) return iterator(*this, p);
        int s = size();
        if (p > s) p = s;
        if (p < 0)p = 0;
        reserve(s + n);
        for (int i = s - 1; i >= p; --i)data->data.get()[i + n] = data->data.get()[i];
        for (int i = 0; i < n; ++i) {
            data->data.get()[p + i] = *f;
            ++f;
        }
        data->size += n;
        data->update_scan_size();

        return iterator(this, p + n);
    }
    void swap(CollectableVector& o)
    {
        MEM_TEST();
        RootPtr<T> t = data;
        data = o.data;
        o.data = t;
    }
    void resize(int s, const RootPtr<T>& exemplar)
    {
        MEM_TEST();
        if (!data->resize(s, exemplar)) {
            RootPtr<CollectableVectoreUse<T> > data_held_for_collect = data;
            data = new CollectableVectoreUse<T>(this, s << 1);
            InstancePtr<T>* source = data_held_for_collect.data.get();
            InstancePtr<T>* dest = data.data.get();

            int i;
            for (i = 0; i < data_held_for_collect->size; ++i) {
                if ((i & 1023) == 0) GC::safe_point();
                dest[i] = source[i];
            }
            for (; i < data->size; ++i) {
                if ((i & 1023) == 0) GC::safe_point();
                dest[i] = exemplar;
            }
            data->size = s;
            data->update_scan_size();

        }
    }   
    void resize(int s, InstancePtr<T>& exemplar)
    {
        MEM_TEST();
        if (!data->resize(s,exemplar)) {
            RootPtr<CollectableVectoreUse<T> > data_held_for_collect = data;
            data = new CollectableVectoreUse<T>(this, s << 1);
            InstancePtr<T>* source = data_held_for_collect.data.get();
            InstancePtr<T>* dest = data.data.get();

            int i;
            for (i = 0; i < data_held_for_collect->size; ++i) { 
                if ((i & 1023) == 0) GC::safe_point();
                dest[i] = source[i];
            }
            for (; i < data->size; ++i) {
                if ((i & 1023) == 0) GC::safe_point();
                dest[i] = exemplar;
            }
            data->size = s;
            data->update_scan_size();

        }
    }
    void resize(int s) {
        MEM_TEST();
        reserve(s);
        data->size = s;
        data->update_scan_size();

    }
    //sets the reservation to at least s, not the size
    void reserve(int s)
    {
        MEM_TEST();
        if (data->reserved < s) {
            RootPtr<CollectableVectoreUse<T> > data_held_for_collect ( data);
            data = new CollectableVectoreUse<T>( s << 1);
            InstancePtr<T> * source = data_held_for_collect->data.get();
            InstancePtr<T> * dest = data->data.get();

            for (int i = data_held_for_collect->size - 1; i >= 0; --i) { 
                if ((i & 1023) == 0) GC::safe_point();
                dest[i] = source[i];
            }
            data->size = data_held_for_collect->size;
            data->update_scan_size();

        }
    }
    void push_front(const RootPtr<T>& o) {
        MEM_TEST();
        int s = size()+1;
 //       reserve(s);
        if (!data->push_front(o)) {
            RootPtr<CollectableVectoreUse<T> > data_held_for_collect ( data);
            data = new  CollectableVectoreUse<T>(s << 1);
            InstancePtr<T>* source = data_held_for_collect->data.get();
            InstancePtr<T>* dest = data->data.get();

            int i;
            for (i = s-2; i >= 0; --i) {
                if ((i & 1023) == 0) GC::safe_point();
                dest[i+1] = source[i];
            }
            dest[0] = o;
            data->size = s;
            data->update_scan_size();

        }
    }
    RootPtr<T> front() const {
        MEM_TEST();
        return at(0);
    }

    InstancePtr<T>& front() {
        MEM_TEST();
        return at(0);
    }

    RootPtr<T> back() const {
        MEM_TEST();
        return at(size() - 1);
    }


    InstancePtr<T>& back() {
        MEM_TEST();
        return at(size() - 1);
    }

};

template<typename T>
class SharableVector : Collectable
{
    InstancePtr< Collectable4Block<T> > blocks;
public:
    int total_instance_vars() const {
        return 1;
    }
    InstancePtrBase* index_into_instance_vars(int num) {
        return &blocks;
    }
    //size_t my_size() const { return sizeof(*this); }


    SharableVector() :blocks(new Collectable4Block<T>) { log_size(sizeof(*this)); }
    bool push_back(const RootPtr<T>& o) 
    {
        return blocks->push_back(o);
    }
    bool pop_back(RootPtr<T>& o)
    {
        return blocks->pop_back(o);
    }
    bool pop_back(InstancePtr<T>& o)
    {
        return blocks->pop_back(o);
    }
    bool push_front(const RootPtr<T>& o)
    {
        return blocks->push_front(o);
    }
    void clear()
    {
        blocks->clear();
    }
    void resize(int s)
    {
        blocks->resize(s);
    }
    int size() { return blocks->size; }
    InstancePtr<T>& at (int i) {
        if (i < 0 || i >= size()) throw std::out_of_range("SharableVector index out of range");
        return (*blocks.get())[i];
    }
    InstancePtr<T>& operator[](int i) {
        return (*blocks.get())[i];
    }
    InstancePtr<T>& insure (int i) {
        return blocks->insure(i);
    }
    InstancePtr<T>& front() {
        return at(0);
    }
    InstancePtr<T>& back() {
        return at(size()-1);
    }

};

class CollectableSentinel : public Collectable
{
public:
    CollectableSentinel() :Collectable(_SENTINEL_) {
        circular_double_list_is_sentinel = false;
    }
    virtual int total_instance_vars() const { return 0; }
    //not snapshot, includes ones that could be null because they're live
    //virtual size_t my_size() const { return 0; }
    virtual InstancePtrBase* index_into_instance_vars(int num) { return nullptr; }
};
