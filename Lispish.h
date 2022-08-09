#pragma once
#include "parser.h"


enum class SexpType {
	nil_,
	string_,
	int_,
	uint_,
	double_,
	atom_,
	cons_
};

class Sexp : public Collectable
{
public:
	virtual bool operator == (RootPtr<Sexp> o) = 0;
	bool eq(RootPtr<Sexp> o) { return this == &*o; }
	virtual SexpType type() = 0;
	int total_instance_vars() const { return 0; }
	InstancePtrBase* index_into_instance_vars(int num) { return nullptr; }
	virtual RootPtr<Sexp> operator [](int i);
};
class SexpNil : public Sexp
{
public:
	SexpType type() { return SexpType::nil_; }
	virtual uint64_t hash() const
	{
		return 0x0123456789abcdef;
	}
	bool operator == (RootPtr<Sexp> o) {
		return type() == o->type();
	}
};

class SexpString : public Sexp
{
public:
	SexpType type() { return SexpType::string_; }
	GraphemeString value;
	virtual uint64_t hash() const
	{
		return value.hash1();
	}
	SexpString(GraphemeString g) :value(g) {}
	bool operator == (RootPtr<Sexp> o) {
		if (o->type() == type()) {
			return static_pointer_cast<SexpString>(o)->value == value;
		}
		return false;
	}
};

class SexpInt : public Sexp
{
public:
	SexpType type() { return SexpType::int_; }
	int64_t value;
	virtual uint64_t hash() const
	{
		return spooky_hash64((void*)&value, 8, 0xb1b7dff016a9cc83);
	}
	SexpInt(int64_t g) :value(g) {}
	bool is_8_bits() { return (int8_t)value == value; }
	bool is_16_bits() { return (int16_t)value == value; }
	bool is_32_bits() { return (int32_t)value == value; }
	bool operator == (RootPtr<Sexp> o) {
		if (o->type() == type()) {
			return static_pointer_cast<SexpInt>(o)->value == value;
		}
		return false;
	}
};

class SexpUInt : public Sexp
{
public:
	SexpType type() { return SexpType::uint_; }
	uint64_t value;
	virtual uint64_t hash() const
	{
		return spooky_hash64((void*)&value, 8, 0x6f619a06d8d49a74);
	}
	SexpUInt(uint64_t g) :value(g) {}
	bool is_8_bits() { return (uint8_t)value == value; }
	bool is_16_bits() { return (uint16_t)value == value; }
	bool is_32_bits() { return (uint32_t)value == value; }
	bool operator == (RootPtr<Sexp> o) {
		if (o->type() == type()) {
			return static_pointer_cast<SexpUInt>(o)->value == value;
		}
		return false;
	}
};

class SexpDouble : public Sexp
{
public:
	SexpType type() { return SexpType::double_; }
	double value;
	virtual uint64_t hash() const
	{
		return spooky_hash64((void*)&value, 8, 0x8f5b519323bb7a30);
	}
	SexpDouble(double  g) :value(g) {}
	bool operator == (RootPtr<Sexp> o) {
		if (o->type() == type()) {
			return static_pointer_cast<SexpDouble>(o)->value == value;
		}
		return false;
	}
};

class SexpAtom : public Sexp
{
public:
	SexpType type() { return SexpType::atom_; }
	Atom value;
	virtual uint64_t hash() const
	{
		return spooky_hash64((void*)&value, 4, 0x09a38b2ba5edaf0e);
	}
	SexpAtom(Atom  g) :value(g) {}
	bool operator == (RootPtr<Sexp> o) {
		if (o->type() == type()) {
			return static_pointer_cast<SexpAtom>(o)->value == value;
		}
		return false;
	}
};

class SexpCons : public Sexp
{
public:
	int total_instance_vars() const { return 2; }
	InstancePtrBase* index_into_instance_vars(int num) { if (num == 0) return &car; return &cdr; }
	SexpType type() { return SexpType::cons_; }
	InstancePtr<Sexp> car;
	InstancePtr<Sexp> cdr;
	virtual uint64_t hash() const
	{
		uint64_t both[2];
		both[0] = car->hash();
		both[1] = cdr->hash();
		return spooky_hash64((void*)&both, 16, 0x9c5a20659cb5d452);
	}
	SexpCons(RootPtr<Sexp> car, RootPtr<Sexp> cdr) :car(car), cdr(cdr) {}
	SexpCons(RootPtr<Sexp> car);
	SexpCons();
	SexpCons(RootPtr<SexpCons> o) :car(o->car), cdr(o->cdr) {}
	bool operator == (RootPtr<Sexp> o) {
		if (o->type() == type()) {
			auto c = static_pointer_cast<SexpCons>(o);
			return car->eq(c->car) && cdr->eq(c->cdr);
		}
		return false;
	}
	RootPtr<Sexp> operator [](int i)
	{
		if (i == 0) return car;
		return cdr[i - 1];
	}
};

extern RootPtr<Sexp> *_SNil_;
#define SNil (*_SNil_)

RootPtr<Sexp> copy(RootPtr<Sexp> a);
RootPtr<Sexp> deep_copy(RootPtr<Sexp> a);
RootPtr<SexpCons> append(RootPtr<SexpCons> a, RootPtr<Sexp> b);
RootPtr<SexpCons> append_(RootPtr<SexpCons> a, RootPtr<Sexp> b);

typedef RootPtr<Sexp>(*sexp_filter)(RootPtr<Sexp>);

void doleaves(sexp_filter f, RootPtr<Sexp> a);
RootPtr<Sexp> mapleaves(sexp_filter f, RootPtr<Sexp> a);
RootPtr<Sexp> mapcar(sexp_filter f, RootPtr<Sexp> a);
void docar(sexp_filter f, RootPtr<Sexp> a);
RootPtr<Sexp> maplist(sexp_filter f, RootPtr<Sexp> a);
void dolist(sexp_filter f, RootPtr<Sexp> a);
RootPtr<Sexp> reverse(RootPtr<Sexp> l);
RootPtr<Sexp> reverse_all(RootPtr<Sexp> l);