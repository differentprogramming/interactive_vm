//===- KaleidoscopeJIT.h - A simple JIT for Kaleidoscope --------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Contains a simple JIT definition for use in the kaleidoscope tutorials.
//
//===----------------------------------------------------------------------===//

#include "stdafx.h"

#ifndef LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H
#define LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H
#ifdef NO_NO_NO
#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#endif
#include <memory>

#define report(f) _RPTF0(_CRT_WARN, f)
#define report1(f,a) _RPTF1(_CRT_WARN, f,a)
#define report2(f,a,b) _RPTF2(_CRT_WARN, f,a,b)
#define report3(f,a,b,c) _RPTF3(_CRT_WARN, f,a,b,c)
#define report4(f,a,b,c,d) _RPTF4(_CRT_WARN, f,a,b,c,d)
#define report5(f,a,b,c,d,e) _RPTF5(_CRT_WARN, f,a,b,c,d,e)
#define report6(f,a,b,c,d,e,g) _RPTF6(_CRT_WARN, f,a,b,c,d,e,g)

#ifdef NO_NO_NO
namespace llvm {
    namespace orc {

        class KaleidoscopeJIT {
        private:
            std::unique_ptr<ExecutionSession> ES;

            DataLayout DL;
            MangleAndInterner Mangle;

            RTDyldObjectLinkingLayer ObjectLayer;
            IRCompileLayer CompileLayer;

            JITDylib& MainJD;

        public:
            KaleidoscopeJIT(std::unique_ptr<ExecutionSession> ES,
                JITTargetMachineBuilder JTMB, DataLayout DL)
                : ES(std::move(ES)), DL(std::move(DL)), Mangle(*this->ES, this->DL),
                ObjectLayer(*this->ES,
                    []() { return std::make_unique<SectionMemoryManager>(); }),
                CompileLayer(*this->ES, ObjectLayer,
                    std::make_unique<ConcurrentIRCompiler>(std::move(JTMB))),
                MainJD(this->ES->createBareJITDylib("<main>")) {
                MainJD.addGenerator(
                    cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(
                        DL.getGlobalPrefix())));
                if (JTMB.getTargetTriple().isOSBinFormatCOFF()) {
                    ObjectLayer.setOverrideObjectFlagsWithResponsibilityFlags(true);
                    ObjectLayer.setAutoClaimResponsibilityForObjectSymbols(true);
                }
            }

            ~KaleidoscopeJIT() {
                if (auto Err = ES->endSession())
                    ES->reportError(std::move(Err));
            }

            static Expected<std::unique_ptr<KaleidoscopeJIT>> Create() {
                auto EPC = SelfExecutorProcessControl::Create();
                if (!EPC)
                    return EPC.takeError();

                auto ES = std::make_unique<ExecutionSession>(std::move(*EPC));

                JITTargetMachineBuilder JTMB(
                    ES->getExecutorProcessControl().getTargetTriple());

                auto DL = JTMB.getDefaultDataLayoutForTarget();
                if (!DL)
                    return DL.takeError();

                return std::make_unique<KaleidoscopeJIT>(std::move(ES), std::move(JTMB),
                    std::move(*DL));
            }

            const DataLayout& getDataLayout() const { return DL; }

            JITDylib& getMainJITDylib() { return MainJD; }

            Error addModule(ThreadSafeModule TSM, ResourceTrackerSP RT = nullptr) {
                if (!RT)
                    RT = MainJD.getDefaultResourceTracker();
                return CompileLayer.add(RT, std::move(TSM));
            }

            Expected<JITEvaluatedSymbol> lookup(StringRef Name) {
                return ES->lookup({ &MainJD }, Mangle(Name.str()));
            }
        };

    } // end namespace orc
} // end namespace llvm
#endif
#endif // LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H

#include <cassert>
#include <utility>

#ifdef NO_NO_NO
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#endif
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>

#include "grapheme.h"
#include <unordered_map>
#include <set>
#include <unordered_set>
//#define DEBUGSETS
#ifdef DEBUGSETS
class debug_set {
    std::vector<GraphemeString> collection;
public:
    GraphemeString& operator[](int i) { return collection[i];  }
    void insert(GraphemeString& o) { 
        for (int i = (int)size() - 1; i >= 0; --i) if (collection[i] == o) return;
        collection.push_back(o); 
    }
    size_t size() const { return collection.size(); }
    auto cbegin() { return collection.cbegin(); }
    auto cend() {
        return collection.cend();
    }
    int count(GraphemeString& o)
    {
        for (int i = (int)collection.size() - 1; i >= 0; --i) if (collection[i] == o) return 1;
        return 0;
    }

};
class debug_cat_set 
{
    std::vector<utf8proc_category_t> collection;
public:
    utf8proc_category_t& operator[](int i) { return collection[i]; }
    void insert(utf8proc_category_t o) {
        for (int i = (int)size() - 1; i >= 0; --i) if (collection[i] == o) return;
        collection.push_back(o); 
    }
    auto cbegin() { return collection.cbegin(); }
    auto cend() {
        return collection.cend();
    }
    void clear() { collection.clear();  }
    size_t size() const { return collection.size(); }
    int count(debug_cat_set &n) {
        for (int j = n.size() - 1; j >= 0; --j) {
            utf8proc_category_t o = n.collection[j];
            for (int i = (int)size() - 1; i >= 0; --i) if (collection[i] == o) return 1;
        }
        return 0;
    }
};
int cat_set_count(debug_cat_set& s, debug_cat_set& n)
{
    return s.count(n);
}
#else
#define debug_set std::unordered_set<GraphemeString>
#define debug_cat_set std::unordered_set<utf8proc_category_t>
int cat_set_count(debug_cat_set& s, debug_cat_set& n)
{
    for (auto a : n) {
        if (s.count(a) != 0) return 1;
    }
    return 0;
}

#endif

#ifdef NOOOOO

#else
class lexer_generator
{
    void make_nfa(GraphemeString &name, GraphemeString &expression);
    void make_skip_nfa(GraphemeString &expression);
public:
    lexer_generator& prod(const char* _name, const wchar_t* _expression)
    {
        GraphemeString name(_name);
        GraphemeString expression(_expression);
        make_nfa(name, expression);
        return *this;
    }
    lexer_generator&  prod(const char* _name,const char* _expression)
    {
        
        GraphemeString name(_name);
        
        GraphemeString expression(_expression);
        
        make_nfa(name, expression);
        
        return *this;
    }

    lexer_generator& prod(const wchar_t* _name, const wchar_t* _expression)
    {
        GraphemeString name(_name);
        GraphemeString expression(_expression);
        make_nfa(name, expression);
        return *this;
    }

    lexer_generator& prod(const wchar_t* _name, const char* _expression)
    {
        GraphemeString name(_name);
        GraphemeString expression(_expression);
        make_nfa(name, expression);
        return *this;
    }

    lexer_generator& skip(const wchar_t* _expression)
    {
        GraphemeString expression(_expression);
        make_skip_nfa(expression);
        return *this;
    }

    lexer_generator& skip(const char* _expression) {
        GraphemeString expression(_expression);
        make_skip_nfa(expression);
        return *this;
    }

};
lexer_generator LexerGen;
LPSTR UnicodeToUTF8(LPCTSTR s);
struct unicode_property {
    utf8proc_category_t value;
    const char* name;
    const char* description;
};

unicode_property unicode_properties[] =
{
    {UTF8PROC_CATEGORY_CN ,"","Other, not assigned"},
    {UTF8PROC_CATEGORY_LU ,"","Letter, uppercase"},
    {UTF8PROC_CATEGORY_LL ,"","Letter, lowercase"},
    {UTF8PROC_CATEGORY_LT ,"","Letter, titlecase"},
    {UTF8PROC_CATEGORY_LM ,"","Letter, modifier"},
    {UTF8PROC_CATEGORY_LO ,"","Letter, other"},
    {UTF8PROC_CATEGORY_MN ,"","Mark, nonspacing"},
    {UTF8PROC_CATEGORY_MC ,"","Mark, spacing combining"},
    {UTF8PROC_CATEGORY_ME ,"","Mark, enclosing"},
    {UTF8PROC_CATEGORY_ND ,"","Number, decimal digit"},
    {UTF8PROC_CATEGORY_NL ,"","Number, letter"},
    {UTF8PROC_CATEGORY_NO ,"","Number, other"},
    {UTF8PROC_CATEGORY_PC ,"","Punctuation, connector"},
    {UTF8PROC_CATEGORY_PD ,"","Punctuation, dash"},
    {UTF8PROC_CATEGORY_PS ,"","Punctuation, open"},
    {UTF8PROC_CATEGORY_PE ,"","Punctuation, close"},
    {UTF8PROC_CATEGORY_PI ,"","Punctuation, initial quote"},
    {UTF8PROC_CATEGORY_PF ,"","Punctuation, final quote"},
    {UTF8PROC_CATEGORY_PO ,"","Punctuation, other"},
    {UTF8PROC_CATEGORY_SM ,"","Symbol, math"},
    {UTF8PROC_CATEGORY_SC ,"","Symbol, currency"},
    {UTF8PROC_CATEGORY_SK ,"","Symbol, modifier"},
    {UTF8PROC_CATEGORY_SO ,"","Symbol, other"},
    {UTF8PROC_CATEGORY_ZS ,"","Separator, space"},
    {UTF8PROC_CATEGORY_ZL ,"","Separator, line"},
    {UTF8PROC_CATEGORY_ZP ,"","Separator, paragraph"},
    {UTF8PROC_CATEGORY_CC ,"","Other, control"},
    {UTF8PROC_CATEGORY_CF ,"","Other, format"},
    {UTF8PROC_CATEGORY_CS ,"","Other, surrogate"},
    {UTF8PROC_CATEGORY_CO ,"","Other, private use"},
  
};


int lex_error_position;
static debug_cat_set grapheme_cat_singleton;
debug_cat_set& grapheme_cats(GraphemeString& o)
{
    grapheme_cat_singleton.clear();
    for (int i = 0; i < o.codepoint_length(); ++i)
    {
        if (o.codepoint_at(i) != 0) {
            grapheme_cat_singleton.insert(utf8proc_category(o.codepoint_at(i)));
        }
    }
    return grapheme_cat_singleton;
}


struct nfa
{
    bool can_end;
    int end_priority;
    debug_cat_set has_category;
    debug_cat_set lacks_category;
    debug_set matches;
    debug_set lacks;
    int match_nfa; 
    int range_positive_count;
    int range_negative_count;
    int cat_positive_count;
    int cat_negative_count;

    int no_match_nfa;
    int or_nfa;
    bool epsilon;
    GraphemeString name;

    nfa & nfa :: operator= (const nfa&) = default;
    nfa(const nfa&) = default;

    nfa(GraphemeString& n) :name(n),match_nfa(-1),no_match_nfa(-1),or_nfa(-1), epsilon(true), can_end(false),end_priority(0), range_positive_count(0),range_negative_count(0), cat_positive_count(0), cat_negative_count(0) {}
    void matches_ascii_range(char a, char b, bool negate = false) {
        epsilon = false;
        char buf[2];
        buf[1] = 0;
        for (char i = a; i <= b; ++i) {
            buf[0] = i;
            if (negate) {
                if (i == '\n') lacks.insert(GraphemeString("\r\n"));
                lacks.insert(GraphemeString(buf));
                ++range_negative_count;
            }
            else {
                ++range_positive_count;
                if (i == '\n') matches.insert(GraphemeString("\r\n"));
                matches.insert(GraphemeString(buf));
            }
        }
    }

    void matches_char(char a, bool negate = false) {
        epsilon = false;
        char buf[2];
        buf[0] = a;
        buf[1] = 0;

        if (negate) { 
            lacks.insert(GraphemeString(buf)); 
            if (a == '\n') lacks.insert(GraphemeString("\r\n"));
        }
        else { 
            matches.insert(GraphemeString(buf)); 
            if (a == '\n') matches.insert(GraphemeString("\r\n"));
        }
    }
    //assumes that any ^ or ] or \p{ or :xxxxx: has already been processed
    //for use inside range, special characters aren't special
    GraphemeString read_char(GraphemeString& s, int& pos, GraphemeString & production_name) {
        //while (s[pos] == " ")++pos;
        auto w = s[pos];
        if (w == "\\"){
            uint8_t buf[2];
            buf[1] = 0;
            ++pos;
            auto w2 = s[pos];
            uint8_t special = 0;
            if (w2 == "e") special = 0x1b;
            else if (w2 == "t") special = 9;
            else if (w2 == "v") special = 0xb;
            else if (w2 == "n") special = 0xa;
            else if (w2 == "r") special = 0xd;
            else if (w2 == "b") special = 8;
            else if (w2 == "f") special = 0xc;
            else if (w2 == "a") special = 7;
            else if (w2 == "e") special = 0x1b;
            else if (w2 == "") {
                uint8_t errorbuf[200];
                (GraphemeString("in production ") + production_name + " character expected after backslash.").fill_utf8(errorbuf);
                lex_error_position = pos;
                throw std::runtime_error((char*)errorbuf);
            }
            buf[0] = special;
            ++pos;
            if (special != 0) {
                report1("read_char found special character %d\n",(int)buf[0]);
                return GraphemeString(buf);
            }
            report1("read_char found `%s`\n",w2.str());
            return w2;
        }
        ++pos;
        return w;
    }
    bool ProcessPossibleCharClass(GraphemeString& s, int& pos, bool negate, GraphemeString& production_name) 
    {
        uint8_t errorbuf[200];
        //while (s[pos] == " ")++pos;
        auto w = s[pos];
        if (w == ":") {
            if (s[pos + 5] == ":") {
                if (s.slice(pos + 1, pos + 4) == "word") {
                    pos += 6;
                    matches_ascii_range('a', 'z', negate);
                    matches_ascii_range('A', 'Z', negate);
                    epsilon = false;
                    return true;
                }
            }
            else if (s[pos + 6] == ":") {
                auto w = s.slice(pos + 1, pos + 5);
                if (w == "alnum") {
                    pos += 7;
                    matches_ascii_range('a', 'z', negate);
                    matches_ascii_range('A', 'Z', negate);
                    epsilon = false;
                    return true;
                }
                else if (w == "ascii") {
                    pos += 7;
                    matches_ascii_range(0, 127,negate);

                    epsilon = false;
                    return true;
                }
                else if (w == "blank") {
                    matches_char(9, negate);
                    matches_char(0x20, negate);
                    pos += 7;
                    epsilon = false;
                    return true;
                }
                else if (w == "cntrl") {
                    pos += 7;
                    matches_ascii_range(1, 10, negate);

                    matches_ascii_range(0xe, 0x1f, negate);
                    matches_char(0x7f, negate);
                    epsilon = false;
                    return true;
                }
                else if (w == "digit") {
                    pos += 7;
                    matches_ascii_range('0', '9', negate);
                    epsilon = false;
                    return true;
                }
                else if (w == "lower") {
                    pos += 7;
                    matches_ascii_range('a', 'z', negate);
                    epsilon = false;
                    return true;
                }
                else if (w == "graph") {
                    pos += 7;
                    matches_ascii_range('!', '~', negate);
                    epsilon = false;
                    return true;
                }
                else if (w == "print") {
                    pos += 7;
                    matches_ascii_range(' ', '~', negate);
                    matches_char('\t', negate);
                    matches_char('\n', negate);
                    matches_char('\r', negate);

                    matches_char(0x0b, negate);
                    matches_char(0x20, negate);
                    epsilon = false;
                    return true;
                }
                else if (w == "punct") {
                    pos += 7;
                    matches_ascii_range('!', '\\', negate);
                    matches_ascii_range(':', '@', negate);
                    matches_ascii_range('[', '`', negate);
                    matches_ascii_range('{', '~', negate);
                    epsilon = false;
                    return true;
                }
                else if (w == "space") {
                    pos += 7;
                    matches_char(' ', negate);
                    matches_char('\t', negate);
                    matches_char('\n', negate);
                    matches_char('\r', negate);

                    matches_char(0x0b, negate);
                    matches_char(0x20, negate);
                    epsilon = false;
                    return true;
                }
                else if (w == "upper") {
                    pos += 7;
                    matches_ascii_range('A', 'Z', negate);
                    epsilon = false;
                    return true;
                }
            }
            else if (s[pos + 7] == ":") {
                if (s.slice(pos + 1, pos + 6) == "xdigit") {
                    pos += 8;
                    matches_ascii_range('a', 'f', negate);
                    matches_ascii_range('A', 'F', negate);
                    matches_ascii_range('0', '9', negate);
                    epsilon = false;
                    return true;
                }
            }
            (GraphemeString("in production ") + production_name + " char class expected after : or colon has to be escaped with at backslash.").fill_utf8(errorbuf);
            lex_error_position = pos;
            throw std::runtime_error((char*)errorbuf);

        }
        else {
            GraphemeString unicode_spec = s.slice(pos, pos + 2);
            if (unicode_spec == "\\p{" || unicode_spec == "\\P{") {
#ifdef ITSACOMMENT
                UTF8PROC_CATEGORY_CN = 0, /**< Other, not assigned */
                    UTF8PROC_CATEGORY_LU = 1, /**< Letter, uppercase */
                    UTF8PROC_CATEGORY_LL = 2, /**< Letter, lowercase */
                    UTF8PROC_CATEGORY_LT = 3, /**< Letter, titlecase */
                    UTF8PROC_CATEGORY_LM = 4, /**< Letter, modifier */
                    UTF8PROC_CATEGORY_LO = 5, /**< Letter, other */
                    UTF8PROC_CATEGORY_MN = 6, /**< Mark, nonspacing */
                    UTF8PROC_CATEGORY_MC = 7, /**< Mark, spacing combining */
                    UTF8PROC_CATEGORY_ME = 8, /**< Mark, enclosing */
                    UTF8PROC_CATEGORY_ND = 9, /**< Number, decimal digit */
                    UTF8PROC_CATEGORY_NL = 10, /**< Number, letter */
                    UTF8PROC_CATEGORY_NO = 11, /**< Number, other */
                    UTF8PROC_CATEGORY_PC = 12, /**< Punctuation, connector */
                    UTF8PROC_CATEGORY_PD = 13, /**< Punctuation, dash */
                    UTF8PROC_CATEGORY_PS = 14, /**< Punctuation, open */
                    UTF8PROC_CATEGORY_PE = 15, /**< Punctuation, close */
                    UTF8PROC_CATEGORY_PI = 16, /**< Punctuation, initial quote */
                    UTF8PROC_CATEGORY_PF = 17, /**< Punctuation, final quote */
                    UTF8PROC_CATEGORY_PO = 18, /**< Punctuation, other */
                    UTF8PROC_CATEGORY_SM = 19, /**< Symbol, math */
                    UTF8PROC_CATEGORY_SC = 20, /**< Symbol, currency */
                    UTF8PROC_CATEGORY_SK = 21, /**< Symbol, modifier */
                    UTF8PROC_CATEGORY_SO = 22, /**< Symbol, other */
                    UTF8PROC_CATEGORY_ZS = 23, /**< Separator, space */
                    UTF8PROC_CATEGORY_ZL = 24, /**< Separator, line */
                    UTF8PROC_CATEGORY_ZP = 25, /**< Separator, paragraph */
                    UTF8PROC_CATEGORY_CC = 26, /**< Other, control */
                    UTF8PROC_CATEGORY_CF = 27, /**< Other, format */
                    UTF8PROC_CATEGORY_CS = 28, /**< Other, surrogate */
                    UTF8PROC_CATEGORY_CO = 29, /**< Other, private use */
            } utf8proc_category_t;
#endif
            GraphemeString us_rest = s.slice(pos + 3, pos + 5); 
            std::vector<utf8proc_category_t> cat;

            if (us_rest == "CN}" || us_rest == "cn}") {
                cat.push_back(UTF8PROC_CATEGORY_CN);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "LU}" || us_rest == "lu}" || s.slice(pos, pos+10) == "\\p{:upper:}"){
                cat.push_back(UTF8PROC_CATEGORY_LU);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "LL}" || us_rest == "ll}" || s.slice(pos, pos + 10) == "\\p{:lower:}") {
                cat.push_back(UTF8PROC_CATEGORY_LL);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "LT}" || us_rest == "lt}") {
                cat.push_back(UTF8PROC_CATEGORY_LT);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "LM}" || us_rest == "lm}") {
                cat.push_back(UTF8PROC_CATEGORY_LM);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (s.slice(pos, pos + 10) == "\\p{:graph:}") {
                cat.push_back(UTF8PROC_CATEGORY_LU);
                cat.push_back(UTF8PROC_CATEGORY_LL);
                cat.push_back(UTF8PROC_CATEGORY_LT);
                cat.push_back(UTF8PROC_CATEGORY_LM);
                cat.push_back(UTF8PROC_CATEGORY_LO);
                cat.push_back(UTF8PROC_CATEGORY_ME);
                cat.push_back(UTF8PROC_CATEGORY_ND);
                cat.push_back(UTF8PROC_CATEGORY_NL);
                cat.push_back(UTF8PROC_CATEGORY_NO);
                cat.push_back(UTF8PROC_CATEGORY_PC);
                cat.push_back(UTF8PROC_CATEGORY_PD);
                cat.push_back(UTF8PROC_CATEGORY_PS);
                cat.push_back(UTF8PROC_CATEGORY_PE);
                cat.push_back(UTF8PROC_CATEGORY_PI);
                cat.push_back(UTF8PROC_CATEGORY_PF);
                cat.push_back(UTF8PROC_CATEGORY_PO);
                cat.push_back(UTF8PROC_CATEGORY_SM);
                cat.push_back(UTF8PROC_CATEGORY_SC);
                cat.push_back(UTF8PROC_CATEGORY_SK);
                cat.push_back(UTF8PROC_CATEGORY_SO);
                if (negate) cat_negative_count+=20;
                else cat_positive_count+=20;
            }
            else if (s.slice(pos, pos + 9) == "\\p{:word:}") {
                cat.push_back(UTF8PROC_CATEGORY_LU);
                cat.push_back(UTF8PROC_CATEGORY_LL);
                cat.push_back(UTF8PROC_CATEGORY_LT);
                cat.push_back(UTF8PROC_CATEGORY_LM);
                cat.push_back(UTF8PROC_CATEGORY_LO);
                if (negate) cat_negative_count += 5;
                else cat_positive_count += 5;
            }
            else if (s.slice(pos, pos + 10) == "\\p{:alnum:}") {
                cat.push_back(UTF8PROC_CATEGORY_LU);
                cat.push_back(UTF8PROC_CATEGORY_LL);
                cat.push_back(UTF8PROC_CATEGORY_LT);
                cat.push_back(UTF8PROC_CATEGORY_LM);
                cat.push_back(UTF8PROC_CATEGORY_LO);
                cat.push_back(UTF8PROC_CATEGORY_ND);
                cat.push_back(UTF8PROC_CATEGORY_NL);
                cat.push_back(UTF8PROC_CATEGORY_NO);
                if (negate) cat_negative_count += 8;
                else cat_positive_count += 8;
            }
            else if (s.slice(pos, pos + 10) == "\\p{:digit:}") {
                cat.push_back(UTF8PROC_CATEGORY_ND);
                cat.push_back(UTF8PROC_CATEGORY_NL);
                cat.push_back(UTF8PROC_CATEGORY_NO);
                if (negate) cat_negative_count += 3;
                else cat_positive_count += 3;
            }
            else if (s.slice(pos, pos + 10) == "\\p{:space:}") {
                cat.push_back(UTF8PROC_CATEGORY_ZS);
                cat.push_back(UTF8PROC_CATEGORY_ZL);
                cat.push_back(UTF8PROC_CATEGORY_ZP);
                if (negate) cat_negative_count += 3;
                else cat_positive_count += 3;
                matches_char(' ', negate);
                matches_char('\t', negate);
                matches_char('\n', negate);
                matches_char('\r', negate);

                matches_char(0x0b, negate);
                matches_char(0x20, negate);
            }
            else if (s.slice(pos, pos + 10) == "\\p{:punct:}") {

                cat.push_back(UTF8PROC_CATEGORY_PC);
                cat.push_back(UTF8PROC_CATEGORY_PD);
                cat.push_back(UTF8PROC_CATEGORY_PS);
                cat.push_back(UTF8PROC_CATEGORY_PE);
                cat.push_back(UTF8PROC_CATEGORY_PI);
                cat.push_back(UTF8PROC_CATEGORY_PF);
                cat.push_back(UTF8PROC_CATEGORY_PO);
                cat.push_back(UTF8PROC_CATEGORY_SM);
                cat.push_back(UTF8PROC_CATEGORY_SC);
                cat.push_back(UTF8PROC_CATEGORY_SK);
                cat.push_back(UTF8PROC_CATEGORY_SO);
                if (negate) cat_negative_count += 11;
                else cat_positive_count += 11;
                matches_ascii_range('!', '\\', negate);
                matches_ascii_range(':', '@', negate);
                matches_ascii_range('[', '`', negate);
                matches_ascii_range('{', '~', negate);

            }
            else if (us_rest == "LO}" || us_rest == "lo}") {
                cat.push_back(UTF8PROC_CATEGORY_LO);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "MN}" || us_rest == "mn}") {
                cat.push_back(UTF8PROC_CATEGORY_MN);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "MC}" || us_rest == "mc}") {
                cat.push_back(UTF8PROC_CATEGORY_MC);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "ME}" || us_rest == "me}") {
                cat.push_back(UTF8PROC_CATEGORY_ME);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "ND}" || us_rest == "nd}") {
                cat.push_back(UTF8PROC_CATEGORY_ND);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "NL}" || us_rest == "nl}") {
                cat.push_back(UTF8PROC_CATEGORY_NL);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "NO}" || us_rest == "no}") {
                cat.push_back(UTF8PROC_CATEGORY_NO);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "PC}" || us_rest == "pc}") {
                cat.push_back(UTF8PROC_CATEGORY_PC);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "PD}" || us_rest == "pd}") {
                cat.push_back(UTF8PROC_CATEGORY_PD);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "PS}" || us_rest == "ps}") {
                cat.push_back(UTF8PROC_CATEGORY_PS);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "PE}" || us_rest == "pe}") {
                cat.push_back(UTF8PROC_CATEGORY_PE);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "PI}" || us_rest == "pi}") {
                cat.push_back(UTF8PROC_CATEGORY_PI);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "PF}" || us_rest == "pf}") {
                cat.push_back(UTF8PROC_CATEGORY_PF);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "PO}" || us_rest == "po}") {
                cat.push_back(UTF8PROC_CATEGORY_PO);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "SM}" || us_rest == "sm}") {
                cat.push_back(UTF8PROC_CATEGORY_SM);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "SC}" || us_rest == "sc}") {
                cat.push_back(UTF8PROC_CATEGORY_SC);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "SK}" || us_rest == "sk}") {
                cat.push_back(UTF8PROC_CATEGORY_SK);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "SO}" || us_rest == "so}") {
                cat.push_back(UTF8PROC_CATEGORY_SO);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "ZS}" || us_rest == "zs}") {
                cat.push_back(UTF8PROC_CATEGORY_ZS);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "ZL}" || us_rest == "zl}") {
                cat.push_back(UTF8PROC_CATEGORY_ZL);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "ZP}" || us_rest == "zp}") {
                cat.push_back(UTF8PROC_CATEGORY_ZP);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "CC}" || us_rest == "cc}") {
                cat.push_back(UTF8PROC_CATEGORY_CC);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "CF}" || us_rest == "cf}") {
                cat.push_back(UTF8PROC_CATEGORY_CF);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "CS}" || us_rest == "cs}") {
                cat.push_back(UTF8PROC_CATEGORY_CS);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else if (us_rest == "CO}" || us_rest == "CO}") {
                cat.push_back(UTF8PROC_CATEGORY_CO);
                if (negate) ++cat_negative_count;
                else ++cat_positive_count;
            }
            else return false;
            while (cat.size() > 0) {
                if (negate) {
                    lacks_category.insert(cat.back());
                }
                else {
                    has_category.insert(cat.back());
                }
                cat.pop_back();
            }
            epsilon = false;
            pos += 3;
            while (s[pos] != "}")++pos;
            ++pos;
            return true;
        } else {
                auto first = read_char(s,pos, production_name);
                int32_t first_codepoint;
                first.fill_codepoints(&first_codepoint, false);
                //while (s[pos] == " ")++pos;
                if (s[pos] == "-") {
                    ++pos;
                    if (first.codepoint_length() > 1) {
                        (GraphemeString("in production ") + production_name + " multi-codepoint character "+first+" can't be used in a range.").fill_utf8(errorbuf);
                        lex_error_position = pos;
                        throw std::runtime_error((char*)errorbuf);
                    }
                    //while (s[pos] == " ")++pos;
                    if (s[pos] == "]" || s[pos] == "^" || s[pos]=="" || (s[pos] == "\\" && (s[pos+1] == "p"|| s[pos + 1] == "P"))) {
                        (GraphemeString("in production ") + production_name + " end of range missing.").fill_utf8(errorbuf);
                        lex_error_position = pos;
                        throw std::runtime_error((char*)errorbuf);
                    }
                    auto second = read_char(s, pos, production_name);
                    if (second.codepoint_length() > 1) {
                        (GraphemeString("in production ") + production_name + " multi-codepoint character " + second + " can't be used in a range.").fill_utf8(errorbuf);
                        lex_error_position = pos;
                        throw std::runtime_error((char*)errorbuf);
                    }
                    epsilon = false;
                    int32_t second_codepoint;
                    
                    second.fill_codepoints(&second_codepoint, false);
                    if (first_codepoint > second_codepoint) {
                        int32_t t = first_codepoint;
                        first_codepoint = second_codepoint;
                        second_codepoint = t;
                    }
                    report3("%s codepoint range from %d to %d\n",(negate?"Subtracting":"Adding"),first_codepoint,second_codepoint);
                    for (int32_t i = first_codepoint; i <= second_codepoint; ++i) {
                        int32_t b[2];
                        b[0] = i;
                        b[1] = 0;
                        if (negate) {
                            ++range_negative_count;
                            if (i == '\n') lacks.insert(GraphemeString("\r\n"));
                            lacks.insert(GraphemeString(b));
                        }
                        else {
                            ++range_positive_count;
                            if (i == '\n') matches.insert(GraphemeString("\r\n"));
                            matches.insert(GraphemeString(b));
                        }
                    }
                    return true;
                }
                int32_t b[2];
                b[0] = first_codepoint;
                b[1] = 0;
                epsilon = false;
                if (negate) {
                    ++range_negative_count;
                    report1("subtract codepoint from charset %d\n", (int)b[0]);
                    lacks.insert(GraphemeString(b));
                }
                else {
                    ++range_positive_count;
                    report1("add codepoint to charset %d\n", (int)b[0]);
                    matches.insert(GraphemeString(b));
                }
                return true;
            }
        }
        return false;
    }
};
std::vector<nfa> nfas;
std::vector<int> nfa_start_states;
std::unordered_map < GraphemeString, int> name_to_nfa;

struct last_found_nfa
{
    int found;
    int priority;

    int pos;
    last_found_nfa(int pos) :found(-1), priority(-3), pos(pos) {}
};

int next_nfa(GraphemeString &next_char, int pos, int current, last_found_nfa& last_endpoint, std::vector<int> &splits)
{
    report5("next_nfa %d (at pos %d `%s`), %s%s ",current,pos,next_char.str(), (nfas[current].can_end?"can end ":""),(nfas[current].epsilon?"epsilon ":""));
    report5("#matches %d%s lacks %d%s%s\n", (int)nfas[current].matches.size(), 
        (nfas[current].matches.count(next_char)?" in match ":""), 
        (int)nfas[current].lacks.size(), 
        (nfas[current].lacks.count(next_char) ? " in lacks " : ""), 
        (nfas[current].or_nfa != -1?"has_or ":""));
    if (nfas[current].can_end){
        report("FOUND POSSIBLE END\n");
        if (last_endpoint.pos < pos || nfas[current].end_priority > last_endpoint.priority) {
            report1("found valid end from %d at priority %d\n", current, nfas[current].end_priority);
            last_endpoint.found = current;
            last_endpoint.priority = nfas[current].end_priority;
            last_endpoint.pos = pos;
        }
    }
    if (nfas[current].or_nfa != -1) {
        report2("pushing or %d from %d\n", nfas[current].or_nfa,current);
        splits.push_back(nfas[current].or_nfa);
    }
    if (nfas[current].epsilon && nfas[current].match_nfa != -1) {
        report2("forward on epsilon from %d to %d\n", current, nfas[current].match_nfa);
        return next_nfa(next_char, pos, nfas[current].match_nfa, last_endpoint, splits); 
    }

    else {
        auto cats = grapheme_cats(next_char);
        if ((((nfas[current].range_positive_count == 0 && nfas[current].range_negative_count != 0) || 
                nfas[current].matches.count(next_char) != 0)
        || ((nfas[current].cat_positive_count == 0 && nfas[current].cat_negative_count != 0) ||
            cat_set_count(nfas[current].has_category,cats) != 0)) && (0 == cat_set_count(nfas[current].lacks_category,cats) && 0 == nfas[current].lacks.count(next_char))){
            report3("forward on match from %d to %d %s\n", current, nfas[current].match_nfa,(nfas[current].matches.count(next_char) != 0?"on positive match":""));
            return nfas[current].match_nfa;
        }
        else {
            if (nfas[current].no_match_nfa == -1) return -1;
            return next_nfa(next_char, pos, nfas[current].no_match_nfa, last_endpoint, splits);
        }
    }
}

bool nfa_parse(GraphemeString* &found, GraphemeString& source, int& pos, int &startpos)
{
    startpos = pos;
    for (;;) {
        last_found_nfa last(pos);
        int cur_pos = pos;
        if (source[pos] == "")return false;
        std::vector<int> concurrent = nfa_start_states;
        while (concurrent.size() > 0) {
            report2("***about to start loop over %d elements for `%s`\n", (int)concurrent.size(),source[cur_pos].str());
            for (int state_index = 0; state_index < concurrent.size(); ++state_index) {
                report4("at parallel state %d of %d, nfa# %d called %s \n", state_index, (int)concurrent.size(), concurrent[state_index], nfas[concurrent[state_index]].name.str());
                int next_state = next_nfa(source[cur_pos], cur_pos, concurrent[state_index], last, concurrent);
                if (next_state < 0) {
                    report2("state ended %d of %d\n", state_index, (int)concurrent.size());
                    concurrent.erase(concurrent.cbegin() + state_index);
                    --state_index;
                }
                else concurrent[state_index] = next_state;
            }
            ++cur_pos;
        }
        if (last.found >= 0) {
            if (nfas[last.found].end_priority == -2) {
                report3("SKIPPING %s from %d to %d\n", nfas[last.found].name.str(), pos, last.pos);
                pos = last.pos;
                startpos = pos;
                if (source[pos] == "")return false;
                continue;//skip
            }
            found = &nfas[last.found].name;
            report3("FOUND TOKEN %s from %d to %d\n",found->str(),pos, last.pos);
            pos = last.pos-1;

            return true;
        }
        report("NO TOKENS FOUND");
        return false;
    }
}
/* nfa_end_ret should have success setable 
 * nfa_start_ret should have or settable 
 */
/*
//character see https://github.com/kkos/oniguruma/blob/master/doc/RE
// https://github.com/KenDickey/Cuis-Smalltalk-UniCodePoint-Properties/blob/master/System-Text-UnicodeSupport.pck.st
//\special character t v n r b f a e \777 \xhh \x{7HHHHHHH} \uHHHH "
  \t           horizontal tab         (0x09)
  \v           vertical tab           (0x0B)
  \n           newline (line feed)    (0x0A)
  \r           carriage return        (0x0D)
  \b           backspace              (0x08)
  \f           form feed              (0x0C)
  \a           bell                   (0x07)
  \e           escape                 (0x1B)
  \nnn         octal char                    (encoded byte value)
  \xHH         hexadecimal char              (encoded byte value)
  \x{7HHHHHHH} (1-8 digits) hexadecimal char (code point value)
  \o{17777777777} (1-11 digits) octal char   (code point value)
  \uHHHH       hexadecimal char              (code point value)//
// 
//[] range, character or type list
//\p{} unicode elemement
//:ascii class:
alnum
alpha
ascii
blank
cntrl
digit
graph 
punct
space
upper
xdigit
word

//:^ascii class:
//alignment ^ $ 
//(parse_reg_or)
*/
bool parse_reg_or(GraphemeString& s, int& pos, int& nfa_start_ret, int& nfa_end_ret, GraphemeString& production_name, int &priority);
bool parse_element(GraphemeString& s, int& pos, int& nfa_start_ret, int& nfa_end_ret, GraphemeString& production_name, int &priority) 
{
    uint8_t errorbuf[200];
    
    while (s[pos] == " ") ++pos;
    if (s[pos] == "") return false;
    if (s[pos] == "(") {
        ++pos;
        int pri = priority;
        bool succeeded = parse_reg_or(s, pos, nfa_start_ret, nfa_end_ret, production_name,pri);
        
        if (pri < priority)priority = pri;
        
        while (s[pos] == " ") ++pos;
        
        if (s[pos] != ")") {
            
            (GraphemeString("in production ") + production_name + " ) expected.").fill_utf8(errorbuf);
            lex_error_position = pos;
            throw std::runtime_error((char*)errorbuf);
        }
        ++pos;
        if (!succeeded){//epsilon
            int epsilon = nfas.size();
            nfas.push_back(nfa(production_name));
            nfa_start_ret = nfa_end_ret = epsilon;
        }
        return true;
    }
    else if (s[pos] == "\\") {
        
        ++pos;
        uint8_t special = 0;
        if (s[pos] == "e") special = 0x1b;
        else if (s[pos] == "t") special = 9;
        else if (s[pos] == "v") special = 0xb;
        else if (s[pos] == "n") special = 0xa;
        else if (s[pos] == "r") special = 0xd;
        else if (s[pos] == "b") special = 8;
        else if (s[pos] == "f") special = 0xc;
        else if (s[pos] == "a") special = 7;
        else if (s[pos] == "e") special = 0x1b;
        else if (s[pos] == "") {
            
            (GraphemeString("in production ") + production_name + " character expected after backslash.").fill_utf8(errorbuf);
            lex_error_position = pos;
            throw std::runtime_error((char*)errorbuf);
        }
        
        int r = nfas.size();
        nfas.push_back(nfa(production_name));
        nfas[r].epsilon = false;
        
        nfa_start_ret = nfa_end_ret = r;
        if (special != 0) {
            nfas[r].matches_char(special);
        }else nfas[r].matches.insert(s[pos++]);
        
        nfa_start_ret = nfa_end_ret = r;
        return true;
    }
    else if (s[pos] == ":") {
       
        if (s[pos + 5] == ":") {
            if (s.slice(pos + 1, pos + 4) == "word") {
                if (priority > -1) priority = -1;
                pos += 6;
               
                int r = nfas.size();
                nfas.push_back(nfa(production_name));
                nfas[r].epsilon = false;
                nfas[r].matches_ascii_range('a', 'z');
                nfas[r].matches_ascii_range('A', 'Z');
                nfa_start_ret = nfa_end_ret = r;
                return true;
            }
        }
        else if (s[pos + 6] == ":") {
            auto w = s.slice(pos + 1, pos + 5);
            if (w == "alnum") {
                if (priority > -1) priority = -1;
                pos += 7;
                int r = nfas.size();
                nfas.push_back(nfa(production_name));
                nfas[r].epsilon = false;
                nfas[r].matches_ascii_range('a', 'z');
                nfas[r].matches_ascii_range('A', 'Z');
                nfas[r].matches_ascii_range('0', '9');
                nfa_start_ret = nfa_end_ret = r;
                return true;
            }
            else if (w == "ascii") {
                if (priority > -1) priority = -1;
                pos += 7;
                int r = nfas.size();
                nfas.push_back(nfa(production_name));
                nfas[r].epsilon = false;
                nfas[r].matches_ascii_range(0, 127);

                nfa_start_ret = nfa_end_ret = r;
                return true;
                pos += 7;
            }
            else if (w == "blank") {
                if (priority > -1) priority = -1;
                int r = nfas.size();
                nfas.push_back(nfa(production_name));
                nfas[r].epsilon = false;
                nfas[r].matches_char(9);
                nfas[r].matches_char(0x20);
                nfa_start_ret = nfa_end_ret = r;
                return true;
                pos += 7;
            }
            else if (w == "cntrl") {
                if (priority > -1) priority = -1;
                pos += 7;
                int r = nfas.size();
                nfas.push_back(nfa(production_name));
                nfas[r].epsilon = false;
                nfas[r].matches_ascii_range(1, 10);
                nfas[r].matches_ascii_range(0xe, 0x1f);
//                nfas[r].matches_char(GraphemeString("\r\n"));
                nfas[r].matches_char(0x7f);
                nfa_start_ret = nfa_end_ret = r;
                return true;
            }
            else if (w == "digit") {
                if (priority > -1) priority = -1;
                pos += 7;
                int r = nfas.size();
                nfas.push_back(nfa(production_name));
                nfas[r].epsilon = false;
                nfas[r].matches_ascii_range('0', '9');
                nfa_start_ret = nfa_end_ret = r;
                return true;
            }
            else if (w == "lower") {
                if (priority > -1) priority = -1;
                pos += 7;
                int r = nfas.size();
                nfas.push_back(nfa(production_name));
                nfas[r].epsilon = false;
                nfas[r].matches_ascii_range('a', 'z');
                nfa_start_ret = nfa_end_ret = r;
                return true;
            }
            else if (w == "graph") {
                if (priority > -1) priority = -1;
                pos += 7;
                int r = nfas.size();
                nfas.push_back(nfa(production_name));
                nfas[r].epsilon = false;
                nfas[r].matches_ascii_range('!', '~');
                nfa_start_ret = nfa_end_ret = r;
                return true;
            }
            else if (w == "print") {
                if (priority > -1) priority = -1;
                pos += 7;
                int r = nfas.size();
                nfas.push_back(nfa(production_name));
                nfas[r].epsilon = false;
                nfas[r].matches_ascii_range(' ', '~');
                nfas[r].matches_char('\t');
                nfas[r].matches_char('\n');
                nfas[r].matches_char('\r');

                nfas[r].matches_char(0x0b);
                nfas[r].matches_char(0x20);
                nfa_start_ret = nfa_end_ret = r;
                return true;
            }
            else if (w == "punct") {
                if (priority > -1) priority = -1;
                pos += 7;
                int r = nfas.size();
                nfas.push_back(nfa(production_name));
                nfas[r].epsilon = false;
                nfas[r].matches_ascii_range('!', '\\');
                nfas[r].matches_ascii_range(':', '@');
                nfas[r].matches_ascii_range('[', '`');
                nfas[r].matches_ascii_range('{', '~');
                nfa_start_ret = nfa_end_ret = r;
                return true;
            }
            else if (w == "space") {
                if (priority > -1) priority = -1;
                pos += 7;
                int r = nfas.size();
                nfas.push_back(nfa(production_name));
                nfas[r].epsilon = false;
                nfas[r].matches_char(' ');
                nfas[r].matches_char('\t');
                nfas[r].matches_char('\n');
                nfas[r].matches_char('\r');

                nfas[r].matches_char(0x0b);
                nfas[r].matches_char(0x20);
                nfa_start_ret = nfa_end_ret = r;
                return true;
            }
            else if (w == "upper") {
            if (priority > -1) priority = -1;
            pos += 7;
                int r = nfas.size();
                nfas.push_back(nfa(production_name));
                nfas[r].epsilon = false;
                nfas[r].matches_ascii_range('A', 'Z');
                nfa_start_ret = nfa_end_ret = r;
                return true;
            }
        }
        else if (s[pos + 7] == ":") {
            if (s.slice(pos + 1, pos + 6) == "xdigit") {
                if (priority > -1) priority = -1;
                pos += 8;
                int r = nfas.size();
                nfas.push_back(nfa(production_name));
                nfas[r].epsilon = false;
                nfas[r].matches_ascii_range('a', 'f');
                nfas[r].matches_ascii_range('A', 'F');
                nfas[r].matches_ascii_range('0', '9');
                nfa_start_ret = nfa_end_ret = r;
                return true;
            }
        }
        (GraphemeString("in production ") + production_name + " char class expected after : or colon has to be escaped with at backslash.").fill_utf8(errorbuf);
        lex_error_position = pos;
        throw std::runtime_error((char*)errorbuf);
    }
    else if (s[pos] == "[") {
        ++pos;
        if (priority > -1) priority = -1;
        bool neg = false;
        int r = nfas.size();
        nfas.push_back(nfa(production_name));
        while (s[pos] != "" && s[pos]!="]") {
            if (s[pos] == "^") {
                ++pos;
                neg = true;
            }
            if (!nfas[r].ProcessPossibleCharClass(s, pos, neg, production_name)) {
                (GraphemeString("in production ") + production_name + " malformed range.").fill_utf8(errorbuf);
                lex_error_position = pos;
                throw std::runtime_error((char*)errorbuf);

            }
        }
        if (s[pos] == "") {
            (GraphemeString("in production ") + production_name + " expected end of range.").fill_utf8(errorbuf);
            lex_error_position = pos;
            throw std::runtime_error((char*)errorbuf);
        }
        ++pos;
        nfa_start_ret = nfa_end_ret = r;
        return true;
    }else if (s[pos] == "|" || s[pos] == "?" || s[pos] == "*" || s[pos] == "+" || s[pos] == ")") {
        return false;
    }else if ( s[pos] == "]" || s[pos] == "}") {

        (GraphemeString("in production ") + production_name + " character or character class expected.").fill_utf8(errorbuf);
        lex_error_position = pos;
        throw std::runtime_error((char*)errorbuf);
    }

    int r = nfas.size();
    nfas.push_back(nfa(production_name));
    nfas[r].epsilon = false;
    report2("element matched %s for state %d\n", s[pos].str(),r);
    nfas[r].matches.insert(s[pos++]);
    
    nfa_start_ret = nfa_end_ret= r;
    return true;


}

//parse * + or ?
bool parse_post(GraphemeString& s, int& pos, int& nfa_start_ret, int& nfa_end_ret, GraphemeString& production_name, int &priority) 
{
    int se,ne;
    int pri = priority;
    
    if (s[pos] != "" && parse_element(s, pos, se, ne, production_name,pri)) {
        
        if (pri < priority) priority = pri;
        while (s[pos] == " ") ++pos;
        
        if (s[pos] == "?") {
            ++pos;
            if (s[pos] == "+") {
                ++pos;
                if (se == ne) {
                    int f = nfas.size();
                    nfas.push_back(nfa(production_name));
                    nfas[se].match_nfa = f;
                    nfas[se].no_match_nfa = f;
                    nfa_start_ret = se;
                    nfa_end_ret = f;
                    goto doneit;
                }
            }
            int after = nfas.size();
            nfas.push_back(nfa(production_name));
            int can_or = nfas.size();
            nfas.push_back(nfa(production_name));

            nfas[can_or].match_nfa = se;
            nfas[se].or_nfa = after;
            nfas[ne].match_nfa = after;
            nfa_start_ret = can_or;
            nfa_end_ret = after;
            
        }
        else if (s[pos] == "*") {
            ++pos;
            if (s[pos]=="+"){
                ++pos;
                if (se == ne) {
                    int f = nfas.size();
                    nfas.push_back(nfa(production_name));
                    nfas[se].match_nfa = se;
                    nfas[se].no_match_nfa = f;
                    nfa_start_ret = se;
                    nfa_end_ret = f;
                    goto doneit;
                }
            }
            int loop = nfas.size();
            nfas.push_back(nfa(production_name));
            int after = nfas.size();
            nfas.push_back(nfa(production_name));
            nfas[after].or_nfa = se;
            nfas[loop].match_nfa = after;
            nfas[ne].match_nfa = loop;
            nfa_start_ret = loop;
            nfa_end_ret = after;
            

        }
        else if (s[pos] == "+") {
            ++pos;
            if (s[pos] == "+") {
                ++pos;
                if (se == ne) {
                    int d = nfas.size();
                    nfa temp(nfas[se]);
                    nfas.push_back(temp);
                    int f = nfas.size();
                    nfas.push_back(nfa(production_name));
                    nfas[se].match_nfa = d;
                    nfas[d].match_nfa = d;
                    nfas[d].no_match_nfa = f;
                    nfa_start_ret = se;
                    nfa_end_ret = f;
                    goto doneit;
                }
            }
            int loop = nfas.size();
            nfas.push_back(nfa(production_name));
            nfas[ne].match_nfa = loop;
            nfas[loop].or_nfa = se;
            nfa_start_ret = se;
            nfa_end_ret = loop;
            report3("********** + loop=%d start=%d end=%d\n", loop, se, ne);
            
        }
        else {
            
            nfa_start_ret = se;
            nfa_end_ret = ne;
        }
doneit:
        while (s[pos] == " ") ++pos;

        if (s[pos] == "?" || s[pos] == "*" || s[pos] == "+") {

            uint8_t errorbuf[200];
            (GraphemeString("in production ") + production_name + " can't chain post op operators.").fill_utf8(errorbuf);
            lex_error_position = pos;
            throw std::runtime_error((char*)errorbuf);
        }

        return true;
    }
    else return false;
}

bool parse_concat(GraphemeString& s, int& pos, int& nfa_start_ret, int& nfa_end_ret, GraphemeString& production_name, int& priority)
{
    int nep;
    bool first = true;
    
    for (;;) {
        int ns, ne;
        int p = pos;
        int pri = priority;

        if (s[p]!="" && parse_post(s, p, ns, ne, production_name, pri)) {
            
            if (pri < priority) priority = pri;
            pos = p;
            if (first) {
                first = false;
                nfa_start_ret = ns;
            }
            else {
                nfas[nep].match_nfa = ns;
                
            }
            nep = ne;
        }
        else {
            if (first) return false;
            nfa_end_ret = nep;
            return true;
        }
    }
}

//note, ending with a | makes and or to empty match the same as enclosing the or in a ()?
bool parse_reg_or(GraphemeString& s, int& pos, int& nfa_start_ret, int& nfa_end_ret,  GraphemeString &production_name, int &priority)
{
    
    //int p = pos;
    int ns, ne;
    int f = nfas.size();

//    bool first = true;
    nfas.push_back(nfa(production_name));
    
    int pri = priority;
    if (s[pos] != "" && parse_concat(s, pos, ns, ne, production_name,pri)) {
        
        if (pri < priority) priority = pri;
//        int b = nfas.size();
//        nfas.push_back(nfa(production_name));
//        nfas[b].match_nfa = ns;
        
        nfa_start_ret = ns;
        for (;;) {
            //pos = p;
            nfas[ne].match_nfa = f;

            while (s[pos] == " ") ++pos;
            if (s[pos] != "|") {

                nfa_end_ret = f;
                return true;
            }

            report1("found | in %s\n",production_name.str());
            ++pos;//past |
            int ns2, ne2;
            pri = priority;

            if (s[pos] != "" && parse_concat(s, pos, ns2, ne2, production_name, pri)) {

                if (pri < priority) priority = pri;
//                first = false;
                nfas[ns].or_nfa = ns2;

                ns = ns2;
                ne = ne2;
            }
            else {
                uint8_t errorbuf[200];
                (GraphemeString("in production ") + production_name + " alternate after | not found").fill_utf8(errorbuf);
                lex_error_position = pos;
                throw std::runtime_error((char*)errorbuf);
            }
        }
    }
    nfas.pop_back();
    return false;
}

void lexer_generator::make_nfa(GraphemeString &name, GraphemeString &expression) {
    //parse_reg_or(GraphemeString& s, int& pos, int& nfa_start_ret, int& nfa_end_ret,  GraphemeString &production_name)
    int pos = 0;
    int priority = 0;
    int nfa_start, nfa_end;
    
    if (expression[pos]!= "" && parse_reg_or(expression, pos, nfa_start, nfa_end, name, priority))
    {
        
        nfa_start_states.push_back(nfa_start);
        nfas[nfa_end].can_end = true;
        nfas[nfa_end].end_priority = priority;
        name_to_nfa.insert(std::make_pair(name, nfa_start));
        
    }
    else {
        
        uint8_t errorbuf[200];
        (GraphemeString("in production ") + name + " can't parse.").fill_utf8(errorbuf);
        lex_error_position = pos;
        throw std::runtime_error((char*)errorbuf);
    }
}
void lexer_generator::make_skip_nfa(GraphemeString &expression) {
    GraphemeString name("skip");
    int pos = 0;
    int priority = -2;
    int nfa_start, nfa_end;
    if (parse_reg_or(expression, pos, nfa_start, nfa_end, name, priority))
    {
        report1("*******skip nfa %d is possible end*****************", nfa_end);
        nfa_start_states.push_back(nfa_start);
        nfas[nfa_end].can_end = true;
        nfas[nfa_end].end_priority = -2;
    }
    else {
        uint8_t errorbuf[200];
        (GraphemeString("in production ") + name + " can't parse.").fill_utf8(errorbuf);
        lex_error_position = pos;
        throw std::runtime_error((char*)errorbuf);
    }
}
#include "CollectableHash.h"

RootPtr<CollectableString> int_to_collectable_string(int a)
{
    std::stringstream ss;
    ss << a;
    return new CollectableString(ss.str().c_str());
}

GraphemeString int_to_grapheme_string(int a)
{
    std::stringstream ss;
    ss << a;
    return GraphemeString(ss.str().c_str());
}


std::string scan(LPSTR source);

std::string mainish(LPSTR source)
{
    GraphemeString b(source);
    //   GraphemeStringBuilder b;

    //   b << nye << hindi << emojis << diacritics;
       //b << korean << zalgo;
    std::ostringstream t;

    RootPtr< CollectableKeyHashTable<CollectableString, GraphemeString> > ckh = new CollectableKeyHashTable<CollectableString, GraphemeString>(GraphemeString(""));
    RootPtr< CollectableValueHashTable<GraphemeString, CollectableString> > cvh = new CollectableValueHashTable<GraphemeString, CollectableString>();
    RootPtr< CollectableHashTable<CollectableString, CollectableString> > ch = new CollectableHashTable<CollectableString, CollectableString>();
    RootPtr< HashTable<GraphemeString, GraphemeString> > h = new HashTable<GraphemeString, GraphemeString>(GraphemeString(""));


    for (int i = 0; i < 10000; ++i) {
        RootPtr<CollectableString> cs = int_to_collectable_string(i);
        GraphemeString gs = int_to_grapheme_string(i);

        ckh->insert_or_assign(cs, gs);
        cvh->insert_or_assign(gs, cs);
        h->insert_or_assign(gs, gs);
        ch->insert_or_assign(cs,cs);
    }
    bool s = true;
    for (int i = 0; i < 10000; ++i) {
        RootPtr<CollectableString> cs = int_to_collectable_string(i);
        GraphemeString gs = int_to_grapheme_string(i);

        s = s && ckh->contains(cs) && ckh[cs]== gs;
        if (!s) {
            t << i<<" failed at 1\n";
            goto done;
        }
        s = s && cvh->contains(gs) && cvh[gs]->equal(cs.get());
        if (!s) {
            t << i << " failed at 2\n";
            goto done;
        }
        s = s && h->contains(gs) && h[gs]== gs;
        if (!s) {
            t << i << " failed at 3\n";
            goto done;
        }
        s = s && ch->contains(cs) && ch[cs]->equal(cs.get());
        if (!s) {
            t << i << " failed at 4\n";
            goto done;
        }
    }
    for (int i = 0; i < 10000; i+=2) {
        RootPtr<CollectableString> cs = int_to_collectable_string(i);
        GraphemeString gs = int_to_grapheme_string(i);

        ckh->erase(cs);
        cvh->erase(gs);
        h->erase(gs);
        ch->erase(cs);
    }

    for (int i = 0; i < 10000; ++i) {
        RootPtr<CollectableString> cs = int_to_collectable_string(i);
        GraphemeString gs = int_to_grapheme_string(i);

        if ((i & 1) != 0) {
            s = s && ckh->contains(cs) && ckh[cs] == gs;
            if (!s) {
                t << i <<" = " << cs << " contains" << (ckh->contains(cs) ? "true" : "false") << " value " << ckh[cs] << " should be " << gs << " failed at 5\n";
                //goto done;
                s = true;
            }
            s = s && cvh->contains(gs) && cvh[gs]->equal(cs.get());
            if (!s) {
                t << i << " failed at 6\n";
                //goto done;
                s = true;
            }
            s = s && h->contains(gs) && h[gs] == gs;
            if (!s) {
                t << i << " failed at 7\n";
                //goto done;
                s = true;
            }
            s = s && ch->contains(cs) && ch[cs]->equal(cs.get());
            if (!s) {
                t << i << " failed at 8\n";
                //goto done;
                s = true;
            }
        }
        else {
            s = s && !ckh->contains(cs);
            if (!s) {
                t << i << " = " << cs << " contains" << (ckh->contains(cs) ? "true" : "false") << " value " << ckh[cs] << " should be " << gs << " failed at 9\n";
                goto done;
            }
            s = s && !cvh->contains(gs);
            if (!s) {
                t << i << " failed at 10\n";
                goto done;
            }
            s = s && !h->contains(gs);
            if (!s) {
                t << i << " failed at 11\n";
                goto done;
            }
            s = s && !ch->contains(cs);
            if (!s) {
                t << i << " failed at 12\n";
                goto done;
            }
        }
    }
    for (int i = 10000; i < 50000; ++i) {
        RootPtr<CollectableString> cs = int_to_collectable_string(i);
        GraphemeString gs = int_to_grapheme_string(i);

        ckh->insert_or_assign(cs, gs);
        cvh->insert_or_assign(gs, cs);
        h->insert_or_assign(gs, gs);
        ch->insert_or_assign(cs, cs);
    }
    for (int i = 0; i < 10000; ++i) {
        RootPtr<CollectableString> cs = int_to_collectable_string(i);
        GraphemeString gs = int_to_grapheme_string(i);

        if (i>=10000 || (i & 1) != 0) {
            s = s && ckh->contains(cs) && ckh[cs] == gs;
            if (!s) {
                t << i << " failed at 13\n";
                goto done;
            }
            s = s && cvh->contains(gs) && cvh[gs]->equal(cs.get());
            if (!s) {
                t << i << " failed at 14\n";
                goto done;
            }
            s = s && h->contains(gs) && h[gs] == gs;
            if (!s) {
                t << i << " failed at 15\n";
                goto done;
            }
            s = s && ch->contains(cs) && ch[cs]->equal(cs.get());
            if (!s) {
                t << i << " failed at 16\n";
                goto done;
            }
        }
        else {
            s = s && !ckh->contains(cs);
            if (!s) {
                t << i << " failed at 17\n";
                goto done;
            }
            s = s && !cvh->contains(gs);
            if (!s) {
                t << i << " failed at 18\n";
                goto done;
            }
            s = s && !h->contains(gs);
            if (!s) {
                t << i << " failed at 19\n";
                goto done;
            }
            s = s && !ch->contains(cs);
            if (!s) {
                t << i << " failed at 20\n";
                goto done;
            }
        }
    }
    if (s) t << "success!";
done:;
    /*
        char* src = "a\r\nb";

        for (int i = 0; 0 != src[i]; ++i) {
            t << (int)src[i] << ' ';
        }

        t << '\n';
        GraphemeString b(src);

        for (int i = 0; 0 != b.grapheme_at(i); ++i) {
            t << b.grapheme_at(i) <<' '<< b.grapheme_at(i,1) << ',';
        }
    */
    /*
        char* n;

        n = UnicodeToUTF8(emojis);
        t << "emojis ="<<n<<'\n';
        free(n);

        n = UnicodeToUTF8(hindi);
        t << "hindi =" << n << '\n';
        free(n);

        n = UnicodeToUTF8(nye);
        t << "nye =" << n << '\n';
        free(n);

        n = UnicodeToUTF8(diacritics);
        t << "diacritics =" << n << '\n';
        free(n);

        n = UnicodeToUTF8(korean);
        t << "korean =" << n << '\n';
        free(n);

        n = UnicodeToUTF8(zalgo);
        t << "zalgo =" << n << '\n';
        free(n);
        */

        /*
            GraphemeString bt(b.build());
            t << "build " << hindi.grapheme_num_codepoints(3) << " done\n";
            t << "hindi" << hindi << '\n';
            for (auto a : hindi)t << "forward char '" << a << "'\n";
            for (auto a = hindi.rbegin(); a != hindi.rend();++a) {
                t << "char '" << *a << "'\n";
            }
            t << "emojis" << emojis << '\n';
            for (auto a : emojis)t << "forward char '" << a << "'\n";
            for (auto a = emojis.rbegin(); a != emojis.rend(); ++a) {
                t << "char '" << *a << "'\n";
            }
            t << "nye" << nye << '\n';
            for (auto a : nye)t << "forward char '" << a << "'\n";
            for (auto a = nye.rbegin(); a != nye.rend(); ++a) {
                t << "char '" << *a << "'\n";
            }
            t << "diacritics" << diacritics << '\n';
            for (auto a : diacritics)t << "forward char '" << a << "'\n";
            for (auto a = diacritics.rbegin(); a != diacritics.rend(); ++a) {
                t << "char '" << *a << "'\n";
            }
            t << "korean" << korean << '\n';
            for (auto a : korean)t << "forward char '" << a << "'\n";
            for (auto a = korean.rbegin(); a != korean.rend(); ++a) {
                t << "char '" << *a << "'\n";
            }
            t << "zalgo" << zalgo << '\n';
            for (auto a : zalgo)t << "forward char '" << a << "'\n";
            for (auto a = zalgo.rbegin(); a != zalgo.rend(); ++a) {
                t << "char '" << *a << "'\n";
            }
            */
            //bool nfa_parse(GraphemeString* &found, GraphemeString& source, int& pos)
         /*   GraphemeString test("121 auto");
            t << " '123'.size() " << GraphemeString("123") <<" = " << GraphemeString("123").size() << " byte length " << GraphemeString("123").byte_length() << " codepoint length " << GraphemeString("123").codepoint_length() << '\n';
            GraphemeString t2("0123456789");
            t << " 2-5 " << t2.slice(2, 5) << " [3] " << t2[3] << "slice eq test " << (t2[2] == "2" ? " pass0 " : " fail0 ") << (t2[1] == "1" ? " pass1 " : " fail1 ")
                << (t2.slice(2, 3) == "23" ? " pass2 " : " fail2 ") << '\n';


            t << "n " << (test[0]=="1"?"pass0 ":"fail0 ") << " r " << (test[1] == GraphemeString("2a")[0] ? "pass1 " : "fail1 ")  << " n "  << (test[2] == "1" ? "pass2 " : "fail2 ") <<
                " s " << (test[3] == " " ? "pass3 " : "fail3 ") << " a " << (test[4] == "a" ? "pass4 " : "fail4 ") << "' \n";
        */
    return t.str();
}
std::string scan(LPSTR source)
{
    GraphemeString b(source);
    std::ostringstream t;
    GraphemeString* found;
    int pos = 0;
    int startpos = 0;
    ///*
    try {
        while (nfa_parse(found, b, pos, startpos)) {
            t << *found << ": `" << b.slice(startpos, pos) << "`\n";
            report2("***%s: `%s`****\n",found->str(), b.slice(startpos, pos).str());
            ++pos;
        }
    }
    catch (std::runtime_error err)
    {
        t << err.what()<<'\n';
    }
    //*/
    return t.str();
}

extern COutputWnd* output_window;
void init_parser()
{
    _CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_DEBUG);
    std::ostringstream t;
    try{
#define FULLTEST
#ifdef FULLTEST
        /*
qualified_name:  (IDENT ':')* IDENT

constant_exp
    : NUM_INT
    | YES
    | NO
    | NIL
    | ATOMCONST
    | NUM_REAL
    | STRING_LITERAL
    | specify

type: explicit_type
    | CLASS qualified_name
    | LOGICAL (type)?
    | | (FUNCTION|GENERATOR) '(' (type (',' type)*)? ')' (RETURNING type)?

//can new an explicit type
explicit_type
    : simple_type
    | ARRAY NUM_INT? (OF type)?
    | QUEUE (OF type)?
    | MESSAGE QUEUE (OF type)?
    | TABLE (BY type)? (OF type)?
    | LIST (OF type)?
    | TREE (OF type)?
    | POINTER TO type
    | RECORD qualified_name

simple_type
    : 'big'
    | 'integer'
    | 'byte'
    | 'real'
    | 'string'
    | 'whether'
    | 'atom'
    | 'boxed'
    | 'continuation'
  
    ;        */
        LexerGen
            .prod("IDENT", "[\\p{:word:}_][_\\p{:alnum:}]*+")
            .prod("LITERAL", "auto|double|int|struct|break|else|long|switch|case|enum|register|typedef|char|extern|return|union|const|float|short|unsigned|continue|for|signed|void|default|goto|sizeof|volatile|do|if|static|while|_Bool|_Imaginary|restrict|_Complex|inline|_Alignas|_Generic|_Thread_local|_Alignof|_Noreturn|_Atomic|_Static_assert")
            .prod("STRING", "\"(\\\\([^xu]|x[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]|u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F])|[^\"\\\\]*+)*\"s?")
            .prod("NUM_REAL", ":digit:++.:digit:*+([Ee][+\\-]?:digit:++)?|.:digit:++([Ee][+\\-]?:digit:++)?|:digit:++[Ee][+\\-]?:digit:++")
            .prod("NUM_INT", ":digit:++|0x[:digit:a-fA-F]++|0b[01]++")
            .prod("(","\\(")
            .prod(")", "\\)")
            .prod(".", ".")

            .skip("/\\*[^*]*+(\\*[^/][^*]*+)*\\*/")
        .skip("//[^\\n\\r]*+[\\r\\n]")
        .skip("[\\p{:space:}]++")
        ;
#else
        
        LexerGen
            .prod("LITERAL", "auto|double|int|struct|break|else|long|switch|case|enum|register|typedef|char|extern|return|union|const|float|short|unsigned|continue|for|signed|void|default|goto|sizeof|volatile|do|if|static|while|_Bool|_Imaginary|restrict|_Complex|inline|_Alignas|_Generic|_Thread_local|_Alignof|_Noreturn|_Atomic|_Static_assert")
            ;
        LexerGen
            .skip(":space:+");
        
#endif
    }
    catch (std::runtime_error err)
    {
        t << err.what() << '\n';
        output_window->append_string(t.str().c_str());
    }
}

#endif