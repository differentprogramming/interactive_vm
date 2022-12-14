#pragma once
#include "grapheme.h"
#include "CollectableHash.h"
#include <unordered_map>
typedef int32_t Atom;

enum class LLex {
    IDENT,
    dontcare,
    plus,
    plus_asn,
    minus,
    minus_asn,
    mul,
    mul_asn,
    div,
    div_asn,
    rem,
    rem_asn,
    assign,
    eq,
    le,
    ge,
    gt,
    lt,
    shiftl,
    shiftl_asn,
    shiftr,
    shiftr_asn,
    ne,
    and,
    or ,
    xor,
    not,
    band,
    band_asn,
    bor,
    bor_asn,
    bxor,
    bxor_asn,
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
    var,
    phi,
    let,
    set,
    define,
    break_,
    let_loop,
    for_,
    while_,
    until_,
    continue_,
    cond,
    else_,
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
    fallthrough,
    inline_,
    sin,
    cos,
    asin,
    acos,
    exp,
    ln,
    exp10,
    log,
    exp2,
    log2,
    sqrt,
    tan,
    atan,
    atan2,
    pow,
    nan,
    abs,
    fabs,
    sinh,
    cosh,
    asinh,
    acosh,
    tanh,
    atanh,
    frexp,
    ldexp,
    expm1,
    modf,
    cbrt,
    hypot,
    erf,
    erc,
    tgamma,
    lgamma,
    ceil,
    floor,
    fmod,
    trunc,
    round,
    lround,
    rint,
    lrint,
  //  remainder,
    remquo,
    nextafter,
    fdim,
    fmax,
    fmin,
    fma,
    fpclassify,
    isfinite,
    isinf,
    isnan,
    isnormal,
    signbit,
    isunordered,
    infinity,
    huge_val,
    huge_valf,

    tuple,
    struct_,
    alg,
    ref,
    array,
    cast,
    ptr,
    get_addr,
    gen_reg,
    float_reg,
    in_mem,
	deref,
	take_addr,
    type,
	union_,
    vector,
    return_,
    export_,
    co_return_,
    module_,
    return_from,
    call,
    real,
    int_,
    lp,
    rp,
    dot_call,
    dot,
    as_it_points_to,
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
