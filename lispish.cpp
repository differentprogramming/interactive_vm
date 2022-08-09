#include "Lispish.h"
RootPtr<Sexp> * _SNil_;

RootPtr<Sexp> Sexp::operator [](int i)
{
	if (i == 0) return this;
	return SNil;
}

SexpCons::SexpCons(RootPtr<Sexp> car) :car(car), cdr(SNil) {}
SexpCons::SexpCons() :car(SNil), cdr(SNil) {}

RootPtr<Sexp> copy(RootPtr<Sexp> a)
{
	if (a->type() == SexpType::cons_) {
		auto b = static_pointer_cast<SexpCons>(a);
		return new SexpCons(b->car, copy(b->cdr));
	}
	return a;
}
RootPtr<Sexp> deep_copy(RootPtr<Sexp> a)
{
	if (a->type() == SexpType::cons_) {
		auto b = static_pointer_cast<SexpCons>(a);
		return new SexpCons(deep_copy(b->car), deep_copy(b->cdr));
	}
	return a;
}
RootPtr<SexpCons> append(RootPtr<SexpCons> a, RootPtr<Sexp> b)
{
	if (a->cdr->type() != SexpType::cons_) return new SexpCons(a->car, b);
	return new SexpCons(a->car, append(static_pointer_cast<SexpCons>(a->cdr), b));
}
RootPtr<SexpCons> append_(RootPtr<SexpCons> a, RootPtr<Sexp> b)
{
	RootPtr<SexpCons> r = a;
	while (r->cdr->type() == SexpType::cons_) r = static_pointer_cast<SexpCons>(r->cdr);
	r->cdr = b;
	return a;
}

typedef RootPtr<Sexp> (*sexp_filter)(RootPtr<Sexp>);

void doleaves(sexp_filter f, RootPtr<Sexp> a)
{
	if (a->type() != SexpType::cons_) {
		f(a);
		return;
	}
	RootPtr<SexpCons> l = static_pointer_cast<SexpCons>(a);
	if (l->cdr->type() == SexpType::nil_) {
		doleaves(f, l->car);
		return;
	}

	doleaves(f, l->car);
	doleaves(f, l->cdr);
}

RootPtr<Sexp> mapleaves(sexp_filter f, RootPtr<Sexp> a)
{
	if (a->type() != SexpType::cons_) {
		return f(a);
	}
	RootPtr<SexpCons> l = static_pointer_cast<SexpCons>(a);
	if (l->cdr->type()==SexpType::nil_) return new SexpCons(mapleaves(f, l->car), SNil);

	return new SexpCons(mapleaves(f,l->car), mapleaves(f, l->cdr));
}

RootPtr<Sexp> mapcar(sexp_filter f, RootPtr<Sexp> a)
{
	if (a->type() != SexpType::cons_) return SNil;
	RootPtr<SexpCons> l = static_pointer_cast<SexpCons>(a);
	return new SexpCons(f(l->car), mapcar(f, l->cdr));
}


void docar(sexp_filter f, RootPtr<Sexp> a)
{
	if (a->type() == SexpType::cons_) {
		RootPtr<SexpCons> l = static_pointer_cast<SexpCons>(a);
		f(l->car);
		docar(f, l->cdr);
	}
}

RootPtr<Sexp> maplist(sexp_filter f, RootPtr<Sexp> a)
{
	if (a->type() != SexpType::cons_) return SNil;
	RootPtr<SexpCons> l = static_pointer_cast<SexpCons>(a);
	return new SexpCons(f(l), maplist(f, l->cdr));
}

void dolist(sexp_filter f, RootPtr<Sexp> a)
{
	if (a->type() == SexpType::cons_) {
		RootPtr<SexpCons> l = static_pointer_cast<SexpCons>(a);
		f(l);
		dolist(f, l->cdr);
	}
}

//Note there is no correct way to reverse a list that ends in a dotted pair
RootPtr<Sexp> reverse(RootPtr<Sexp> l)
{
	if (l->type() != SexpType::cons_) return l;
	RootPtr<Sexp> r(SNil);
	while (l->type() == SexpType::cons_) {
		RootPtr<SexpCons> ls= static_pointer_cast<SexpCons>(l);
		r = new SexpCons(ls->car, r);
		l = ls->cdr;
	};
	return r;
}

RootPtr<Sexp> reverse_all(RootPtr<Sexp> l)
{
	if (l->type() != SexpType::cons_) return l;
	RootPtr<Sexp> r(SNil);
	while (l->type() == SexpType::cons_) {
		RootPtr<SexpCons> ls = static_pointer_cast<SexpCons>(l);
		r = new SexpCons(reverse_all(ls->car), r);
		l = ls->cdr;
	};
	return r;
}
