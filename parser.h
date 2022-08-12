#pragma once
#include "grapheme.h"
#include "CollectableHash.h"
#include <unordered_map>
typedef int32_t Atom;

enum class LLex {
    IDENT,
    dontcare,
    plus,
    minus,
    mul,
    div,
    rem,
    assign,
    eq,
    le,
    ge,
    gt,
    lt,
    shiftl,
    shiftr,
    ne,
    and,
    or ,
    xor,
    not,
    band,
    bor,
    bxor,
    bnot,
    visible,
    atomic,
    ordered,
    relaxed,
    sequential,
    thread,
    join,
    visible_lifo,
    mutex,
    rwmutex,
    event,
    hold,
    read_hold,
    modify_hold,
    i32,
    u32,
    i64,
    u64,
    i16,
    u16,
    i8,
    u8,
    string,
    float_,
    double_,
    any,
    const_,
    let,
    define,
    break_,
    let_loop,
    while_,
    until_,
    continue_,
    cond,
    lambda,
    call_cc,
    continuation,
    continuable,
    search,
    fail,
	bool_,
    yes,
    no,
    cut,
    amb,
    constructor,
    destructor,
    on_unwind,
    on_wind,
    finalize,
    impl,
    switch_,
    inline_,
    float_intrinsic,
    tuple,
    struct_,
    alg,
    ref,
    array,
    cast,
    ptr,
	deref,
	take_addr,
    vector,
    return_,
    co_return_,
    return_from,
    call,
    real,
    int_,
    lp,
    rp,
    dot_call,
    dot,
    dot_pair,
    skip,
    NUMBER_OF_TOKENS
};
//if I want to make this really threadsafe eventually, replace atom_to_name with something like CollectableInlineVector and put a mutex over all access to name_to_atom
extern std::unordered_map < GraphemeString, Atom> name_to_atom;
extern std::vector <GraphemeString*> atom_to_name;

//std::mutex intern_mutex;

Atom intern(GraphemeString a);
bool nfa_parse(Atom& found, GraphemeString& source, int& pos, int& startpos); //throws std::runtime_err
