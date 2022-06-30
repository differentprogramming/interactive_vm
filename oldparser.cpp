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

#ifndef LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H
#define LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H

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
#include <memory>

#define report(f) _RPTF0(_CRT_WARN, f)
#define report1(f,a) _RPTF1(_CRT_WARN, f,a)
#define report2(f,a,b) _RPTF2(_CRT_WARN, f,a,b)
#define report3(f,a,b,c) _RPTF3(_CRT_WARN, f,a,b,c)
#define report4(f,a,b,c,d) _RPTF4(_CRT_WARN, f,a,b,c,d)
#define report5(f,a,b,c,d,e) _RPTF5(_CRT_WARN, f,a,b,c,d,e)
#define report6(f,a,b,c,d,e,g) _RPTF6(_CRT_WARN, f,a,b,c,d,e,g)


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

#endif // LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H

#include <cassert>
#include <utility>


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
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>

#include "grapheme.h"
class debug_set {
    std::vector<GraphemeString> collection;
public:
    void insert(GraphemeString& o) { collection.push_back(o); }
    size_t size() const { return collection.size(); }
    int count(GraphemeString& o)
    {
        for (int i = (int)collection.size() - 1; i >= 0; --i) if (collection[i] == o) return 1;
        return 0;
    }

};
class debug_cat_set :public std::vector<utf8proc_category_t>
{
public:
    int count(debug_cat_set &n) {
        for (int j = n.size() - 1; j >= 0; --j) {
            utf8proc_category_t o = n[j];
            for (int i = (int)size() - 1; i >= 0; --i) if ((*this)[i] == o) return 1;
        }
        return 0;
    }
};


#ifdef NOOOOO
extern COutputWnd* output_window;
extern CMFCStatusBar* status_bar;

std::string errbuf;
//std::ostringstream *myout;
llvm::raw_string_ostream *myerr;

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

extern "C" DLLEXPORT double putchard(double x) {
    *myerr << (char)round(x);
    return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X) {
    char buf[30];
    sprintf_s(buf, sizeof(buf)-1, "%f\n", X);
    *myerr << buf;
    return 0;
}

char buffer[10000];

using namespace llvm;
using namespace llvm::orc;
//My Lexer

const char* Keywords[] = { "array" //0
,"and"
,"as"
,"ast"
,"atom"
,"become"
,"big"
,"biop"
,"boxed"
,"break"
,"byte"     //10
,"cast"
,"call"
,"case"
,"class"
,"cut"
,"declare"
,"delete"
,"default"
,"do"
,"continue" //20
,"constant"
,"continuation"
,"ctree"
,"else"
,"elseif"
,"endfunction"
,"endgenerator"
,"endswitch"
,"endif"
,"endwhile" //30
,"function"
,"generator"
,"if"
,"index"
,"integer"
,"interface"
,"list"
,"logical"
,"match"
,"maybe"    //40
,"mod"
,"new"
,"nil"
,"no"
,"not"
,"object"
,"of"
,"or"
,"one"
,"pointer"  //50
,"post"
,"pre"
,"real"
,"record"
,"returning"
,"send"
,"sizeof"
,"string"
,"super"
,"switch"   //60
,"table"
,"then"
,"to"
,"unify"
,"until"
,"value"
,"var"
,"where"
,"whether"
,"while"    //70
,"bxor"
,"yes"
,"-"
,"*"
,"&"
,"!"
,"~"
,"++"
,"--"
,"="        //80
,"+="
,"-="
, "*="
,"/="
,"mod="
,"<<="
,">>="
,"band="
,"bor="
,"bxor="    //90
,"^"
,"<="
,">="
,"<"
,">"
,"=="
,"not="
,"<=>"
,".."
,"."    //100
,"|"
,">>"
,"<<"
,"+"
,"\\"
,"/"
,"?="
,"?"
,"#"
,"#|" //110
,"`"    
,","
,"::"
,"("
,")"
,"["
,"]"
,"{"
,"}"
,"'"//120
,":"    
,";"
,nullptr
};

SolidAsciiTokenizer Tokenizer(false, Keywords);

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//
#ifdef OH_OH_NO
// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
    tok_eof = -1,

    // commands
    tok_def = -2,
    tok_extern = -3,

  // primary
  tok_identifier = -4,
  tok_number = -5,

  // control
  tok_if = -6,
  tok_then = -7,
  tok_else = -8,
  tok_for = -9,
  tok_in = -10,

  // operators
  tok_binary = -11,
  tok_unary = -12
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number

/// gettok - Return the next token from standard input.
static int gettok() {
    static int LastChar = ' ';

    // Skip any whitespace.
    while (isspace(LastChar))
        LastChar = getchar();

    if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
        IdentifierStr = LastChar;
        while (isalnum((LastChar = getchar())))
            IdentifierStr += LastChar;

    if (IdentifierStr == "def")
      return tok_def;
    if (IdentifierStr == "extern")
      return tok_extern;
    if (IdentifierStr == "if")
      return tok_if;
    if (IdentifierStr == "then")
      return tok_then;
    if (IdentifierStr == "else")
      return tok_else;
    if (IdentifierStr == "for")
      return tok_for;
    if (IdentifierStr == "in")
      return tok_in;
    if (IdentifierStr == "binary")
      return tok_binary;
    if (IdentifierStr == "unary")
      return tok_unary;
    return tok_identifier;
  }

    if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
        std::string NumStr;
        do {
            NumStr += LastChar;
            LastChar = getchar();
        } while (isdigit(LastChar) || LastChar == '.');

        NumVal = strtod(NumStr.c_str(), nullptr);
        return tok_number;
    }

    if (LastChar == '#') {
        // Comment until end of line.
        do
            LastChar = getchar();
        while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

        if (LastChar != EOF)
            return gettok();
    }

    // Check for end of file.  Don't eat the EOF.
    if (LastChar == EOF)
        return tok_eof;

    // Otherwise, just return the character as its ascii value.
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}
#endif
//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace {

    /// ExprAST - Base class for all expression nodes.
    class ExprAST {
    public:
        virtual ~ExprAST() = default;

        virtual Value* codegen() = 0;
    };

    /// NumberExprAST - Expression class for numeric literals like "1.0".
    class NumberExprAST : public ExprAST {
        double Val;

    public:
        NumberExprAST(double Val) : Val(Val) {}

        Value* codegen() override;
    };

    /// VariableExprAST - Expression class for referencing a variable, like "a".
    class VariableExprAST : public ExprAST {
        std::string Name;

    public:
        VariableExprAST(const std::string& Name) : Name(Name) {}

        Value* codegen() override;
    };

/// UnaryExprAST - Expression class for a unary operator.
class UnaryExprAST : public ExprAST {
    TOKENS Opcode;
  std::unique_ptr<ExprAST> Operand;

public:
  UnaryExprAST(TOKENS Opcode, std::unique_ptr<ExprAST> Operand)
      : Opcode(Opcode), Operand(std::move(Operand)) {}

  Value *codegen() override;
};
    /// BinaryExprAST - Expression class for a binary operator.
    class BinaryExprAST : public ExprAST {
        TOKENS Op;
        std::unique_ptr<ExprAST> LHS, RHS;

    public:
        BinaryExprAST(TOKENS Op, std::unique_ptr<ExprAST> LHS,
            std::unique_ptr<ExprAST> RHS)
            : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

        Value* codegen() override;
    };

    /// CallExprAST - Expression class for function calls.
    class CallExprAST : public ExprAST {
        std::string Callee;
        std::vector<std::unique_ptr<ExprAST>> Args;

    public:
        CallExprAST(const std::string& Callee,
            std::vector<std::unique_ptr<ExprAST>> Args)
            : Callee(Callee), Args(std::move(Args)) {}

        Value* codegen() override;
    };

/// IfExprAST - Expression class for if/then/else.
class IfExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Cond, Then, Else;

public:
  IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
            std::unique_ptr<ExprAST> Else)
      : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

  Value *codegen() override;
};

/// ForExprAST - Expression class for for/in.
class ForExprAST : public ExprAST {
  std::string VarName;
  std::unique_ptr<ExprAST> Start, End, Step, Body;

public:
  ForExprAST(const std::string &VarName, std::unique_ptr<ExprAST> Start,
             std::unique_ptr<ExprAST> End, std::unique_ptr<ExprAST> Step,
             std::unique_ptr<ExprAST> Body)
      : VarName(VarName), Start(std::move(Start)), End(std::move(End)),
        Step(std::move(Step)), Body(std::move(Body)) {}

  Value *codegen() override;
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;
  bool IsOperator;
  unsigned Precedence; // Precedence if a binary op.

public:
  PrototypeAST(const std::string &Name, std::vector<std::string> Args,
               bool IsOperator = false, unsigned Prec = 0)
      : Name(Name), Args(std::move(Args)), IsOperator(IsOperator),
        Precedence(Prec) {}

  Function *codegen();
  const std::string &getName() const { return Name; }

  bool isUnaryOp() const { return IsOperator && Args.size() == 1; }
  bool isBinaryOp() const { return IsOperator && Args.size() == 2; }

  //char getOperatorName() const {
  //  assert(isUnaryOp() || isBinaryOp());
  //  return Name[Name.size() - 1];
 // }

  unsigned getBinaryPrecedence() const { return Precedence; }
};

    /// FunctionAST - This class represents a function definition itself.
    class FunctionAST {
        std::unique_ptr<PrototypeAST> Proto;
        std::unique_ptr<ExprAST> Body;

    public:
        FunctionAST(std::unique_ptr<PrototypeAST> Proto,
            std::unique_ptr<ExprAST> Body)
            : Proto(std::move(Proto)), Body(std::move(Body)) {}

        Function* codegen();
    };

} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
//static int CurTok;
//static int getNextToken() { return CurTok = gettok(); }

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<std::string, int> BinopPrecedence;

static int BiopPrecedence[TK_NUM_TOKENS];
static int PreopPrecedence[TK_NUM_TOKENS];
static int PostopPrecedence[TK_NUM_TOKENS];
enum class Associativity {
    Left = 1,
    Right = 2,
    NonAssoc = 3,
    BiopMask = 3,
    Pre = 4,
    Post = 8,
    NotOperator = 16
};
static enum class Associativity OpAssoc[TK_NUM_TOKENS];

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
    if (((int)OpAssoc[Tokenizer.cur_token().token_number] & (int)Associativity::BiopMask) != 0) 
        return BiopPrecedence[Tokenizer.cur_token().token_number];
    if (Tokenizer.cur_token().token_number == TK_IDENT) {
        char buf[30];
        Tokenizer.cur_token().get_text(buf, sizeof(buf));
        if (0!=BinopPrecedence.count(buf)) return BinopPrecedence[buf];
    }
    return -1;
}

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char* Str) {
    *myerr << "Error: "<<Str<<"\n";
    return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char* Str) {
    LogError(Str);
    return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();

/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
    auto Result = std::make_unique<NumberExprAST>(Tokenizer.cur_token().as_double());
    Tokenizer.tokenize(); // consume the number
    return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
    Tokenizer.tokenize(); // eat (.
    auto V = ParseExpression();
    if (!V)
        return nullptr;

    if (Tokenizer.cur_token().token_number != TK_RP)
        return LogError("expected ')'");
    Tokenizer.tokenize(); // eat ).
    return V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
    char buf[100];
    Tokenizer.cur_token().get_text(buf, 100);
    std::string IdName = buf;

    Tokenizer.tokenize(); // eat identifier.

    if (Tokenizer.cur_token().token_number != TK_LP) // Simple variable ref.
        return std::make_unique<VariableExprAST>(IdName);

    // Call.
    Tokenizer.tokenize(); // eat (
    std::vector<std::unique_ptr<ExprAST>> Args;
    if (Tokenizer.cur_token().token_number != TK_RP) {
        while (true) {
            if (auto Arg = ParseExpression())
                Args.push_back(std::move(Arg));
            else
                return nullptr;

            if (Tokenizer.cur_token().token_number == TK_RP)
                break;

            if (Tokenizer.cur_token().token_number != TK_COMMA)
                return LogError("Expected ')' or ',' in argument list");
            Tokenizer.tokenize();
        }
    }

    // Eat the ')'.
    Tokenizer.tokenize();

    return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// ifexpr ::= 'if' expression 'then' expression 'else' expression
static std::unique_ptr<ExprAST> ParseIfExpr() {
    Tokenizer.tokenize(); // eat the if.

  // condition.
  auto Cond = ParseExpression();
  if (!Cond)
    return nullptr;

  if (Tokenizer.cur_token().token_number != TK_THEN)
    return LogError("expected then");
  Tokenizer.tokenize(); // eat the then

  auto Then = ParseExpression();
  if (!Then)
    return nullptr;

  if (Tokenizer.cur_token().token_number != TK_ELSE)
    return LogError("expected else");

  Tokenizer.tokenize();

  auto Else = ParseExpression();
  if (!Else)
    return nullptr;

  return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then),
                                      std::move(Else));
}

/// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
static std::unique_ptr<ExprAST> ParseForExpr() {
    Tokenizer.tokenize(); // eat the for.

  if (Tokenizer.cur_token().token_number != TK_IDENT)
    return LogError("expected identifier after for");

  char buf[100];
  Tokenizer.cur_token().get_text(buf, 100);
  std::string IdName = buf;

  Tokenizer.tokenize(); // eat identifier.

  if (Tokenizer.cur_token().token_number != TK_EQUAL)
    return LogError("expected '=' after for");
  Tokenizer.tokenize(); // eat '='.

  auto Start = ParseExpression();
  if (!Start)
    return nullptr;
  if (Tokenizer.cur_token().token_number != TK_COMMA)
    return LogError("expected ',' after for start value");
  Tokenizer.tokenize();

  auto End = ParseExpression();
  if (!End)
    return nullptr;

  // The step value is optional.
  std::unique_ptr<ExprAST> Step;
  if (Tokenizer.cur_token().token_number == TK_COMMA) {
    Tokenizer.tokenize();
    Step = ParseExpression();
    if (!Step)
      return nullptr;
  }

  if (Tokenizer.cur_token().token_number != TK_DO)
    return LogError("expected 'do' after for");
  Tokenizer.tokenize(); // eat 'in'.

  auto Body = ParseExpression();
  if (!Body)
    return nullptr;

  return std::make_unique<ForExprAST>(IdName, std::move(Start), std::move(End),
                                       std::move(Step), std::move(Body));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
    switch (Tokenizer.cur_token().token_number) {
    default:
        return LogError("unknown token when expecting an expression");
    case TK_IDENT:
        return ParseIdentifierExpr();
    case TK_INTEGER_CONST:
    case TK_REAL_CONST:
        return ParseNumberExpr();
    case TK_LP:
        return ParseParenExpr();
  case TK_IF:
    return ParseIfExpr();
  case TK_TO:
    return ParseForExpr();
  }
}

/// unary
///   ::= primary
///   ::= '!' unary
static std::unique_ptr<ExprAST> ParseUnary() {
  // If the current token is not an operator, it must be a primary expr.
    // why comma when ParsePrimary can't handle commas?
    switch (Tokenizer.cur_token().token_number) {
    case TK_IDENT:
    case TK_INTEGER_CONST:
    case TK_REAL_CONST:
    case TK_LP:
    case TK_IF:
    case TK_TO:
    case TK_COMMA:
        return ParsePrimary();
    }
  // If this is a unary operator, read it.
  int Opc = Tokenizer.cur_token().token_number;
  Tokenizer.tokenize();
  if (auto Operand = ParseUnary())
    return std::make_unique<UnaryExprAST>(Opc, std::move(Operand));
  return nullptr;
}

/// binoprhs
///   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
    std::unique_ptr<ExprAST> LHS) {
    // If this is a binop, find its precedence.
    while (true) {
        int TokPrec = GetTokPrecedence();

        // If this is a binop that binds at least as tightly as the current binop,
        // consume it, otherwise we are done.
        if (TokPrec < ExprPrec)
            return LHS;

        // Okay, we know this is a binop.
        TOKENS BinOp = (TOKENS)Tokenizer.cur_token().token_number;
        Tokenizer.tokenize(); // eat binop

    // Parse the unary expression after the binary operator.
    auto RHS = ParseUnary();
    if (!RHS)
      return nullptr;

        // If BinOp binds less tightly with RHS than the operator after RHS, let
        // the pending operator take RHS as its LHS.
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS)
                return nullptr;
        }

        // Merge LHS/RHS.
        LHS =
            std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    }
}

/// expression
///   ::= primary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParseUnary();
  if (!LHS)
    return nullptr;

    return ParseBinOpRHS(0, std::move(LHS));
}

/// prototype
///   ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
  std::string FnName;
  char buf[100];

  unsigned Kind = 0; // 0 = identifier, 1 = unary, 2 = binary.
  unsigned BinaryPrecedence = 30;
/*
static int BiopPrecedence[TK_NUM_TOKENS];
static int PreopPrecedence[TK_NUM_TOKENS];
static int PostopPrecedence[TK_NUM_TOKENS];
enum class Associativity {
    Left = 1,
    Right = 2,
    NonAssoc = 3,
    BiopMask = 3,
    Pre = 4,
    Post = 8,
    NotOperator = 16
};
static enum class Associativity OpAssoc[TK_NUM_TOKENS];*/

  switch (Tokenizer.cur_token().token_number) {
  default:
    return LogErrorP("Expected function name in prototype");
  case TK_IDENT:
    
    Tokenizer.cur_token().get_text(buf, 100);
    FnName = buf;
    Tokenizer.tokenize();
    Kind = 0;
    break;
  case TK_PRE:
  case TK_POST:
      Tokenizer.tokenize();
    if (Tokenizer.cur_token().token_number != TK_IDENT && (Tokenizer.cur_token().token_number < TK_MINUS || Tokenizer.cur_token().token_number> TK_COLONCOLON))
      return LogErrorP("Expected unary operator");
    FnName = "unary";
    Tokenizer.cur_token().get_text(buf, 100);
    FnName += buf;
    Kind = 1;
    Tokenizer.tokenize();
    break;
  case TK_BIOP:
    Tokenizer.tokenize();
    if (Tokenizer.cur_token().token_number != TK_IDENT && (Tokenizer.cur_token().token_number <TK_MINUS || Tokenizer.cur_token().token_number> TK_COLONCOLON))
      return LogErrorP("Expected binary operator");
    FnName = "binary";
    Tokenizer.cur_token().get_text(buf, 100);
    FnName += buf;
    Kind = 2;
    Tokenizer.tokenize();

    // Read the precedence if present.
    if (Tokenizer.cur_token().token_number == TK_INTEGER_CONST) {
      if (Tokenizer.cur_token().int_value < 1 || Tokenizer.cur_token().int_value > 100)
        return LogErrorP("Invalid precedence: must be 1..100");
      BinaryPrecedence = (unsigned)Tokenizer.cur_token().int_value;
      Tokenizer.tokenize();
    }
    break;
  }

  if (Tokenizer.cur_token().token_number != TK_LP)
    return LogErrorP("Expected '(' in prototype");

    std::vector<std::string> ArgNames;
    while (Tokenizer.tokenize() && Tokenizer.cur_token().token_number == TK_IDENT) {
        Tokenizer.cur_token().get_text(buf, 100);
        ArgNames.push_back(buf);
        Tokenizer.tokenize();
        if (Tokenizer.cur_token().token_number != TK_COMMA) break;

    }

    if (Tokenizer.cur_token().token_number != TK_RP)
        return LogErrorP("Expected ')' in prototype");

    // success.
    Tokenizer.tokenize(); // eat ')'.

     // Verify right number of names for operator.
    if (Kind && ArgNames.size() != Kind)
        return LogErrorP("Invalid number of operands for operator");

    return std::make_unique<PrototypeAST>(FnName, ArgNames, Kind != 0,
        BinaryPrecedence);
}

/// definition ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition() {
    Tokenizer.tokenize(); // eat def.
    auto Proto = ParsePrototype();
    if (!Proto)
        return nullptr;

    if (auto E = ParseExpression())
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    return nullptr;
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
    if (auto E = ParseExpression()) {
        // Make an anonymous proto.
        auto Proto = std::make_unique<PrototypeAST>("__anon_expr",
            std::vector<std::string>());
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

/// external ::= 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern() {
    Tokenizer.tokenize(); // eat extern.
    return ParsePrototype();
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, Value *> NamedValues;
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
static ExitOnError ExitOnErr;

Value* LogErrorV(const char* Str) {
    LogError(Str);
    return nullptr;
}

Function *getFunction(std::string Name) {
  // First, see if the function has already been added to the current module.
  if (auto *F = TheModule->getFunction(Name))
    return F;

  // If not, check whether we can codegen the declaration from some existing
  // prototype.
  auto FI = FunctionProtos.find(Name);
  if (FI != FunctionProtos.end())
    return FI->second->codegen();

  // If no existing prototype exists, return null.
  return nullptr;
}

Value *NumberExprAST::codegen() {
  return ConstantFP::get(*TheContext, APFloat(Val));
}

Value* VariableExprAST::codegen() {
    // Look this variable up in the function.
    Value* V = NamedValues[Name];
    if (!V)
        return LogErrorV("Unknown variable name");
    return V;
}

Value *UnaryExprAST::codegen() {
  Value *OperandV = Operand->codegen();
  if (!OperandV)
    return nullptr;

  Function *F = getFunction(std::string("unary") + Keywords[Opcode]);
  if (!F)
    return LogErrorV("Unknown unary operator");

  return Builder->CreateCall(F, OperandV, "unop");
}

Value *BinaryExprAST::codegen() {
  Value *L = LHS->codegen();
  Value *R = RHS->codegen();
  if (!L || !R)
    return nullptr;

    switch (Op) {
    case TK_PLUS:
        return Builder->CreateFAdd(L, R, "addtmp");
    case TK_MINUS:
        return Builder->CreateFSub(L, R, "subtmp");
    case TK_ASTERIX:
        return Builder->CreateFMul(L, R, "multmp");
    case TK_LT:
        L = Builder->CreateFCmpULT(L, R, "cmptmp");
        // Convert bool 0/1 to double 0.0 or 1.0
        return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
    default:
        return LogErrorV("invalid binary operator");
    }
  // If it wasn't a builtin binary operator, it must be a user defined one. Emit
  // a call to it.
  Function *F = getFunction(std::string("binary") + Keywords[Op]);
  assert(F && "binary operator not found!");

  Value *Ops[] = {L, R};
  return Builder->CreateCall(F, Ops, "binop");
}

Value *CallExprAST::codegen() {
  // Look up the name in the global module table.
  Function *CalleeF = getFunction(Callee);
  if (!CalleeF)
    return LogErrorV("Unknown function referenced");

    // If argument mismatch error.
    if (CalleeF->arg_size() != Args.size())
        return LogErrorV("Incorrect # arguments passed");

    std::vector<Value*> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
        ArgsV.push_back(Args[i]->codegen());
        if (!ArgsV.back())
            return nullptr;
    }

    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

Value *IfExprAST::codegen() {
  Value *CondV = Cond->codegen();
  if (!CondV)
    return nullptr;

  // Convert condition to a bool by comparing non-equal to 0.0.
  CondV = Builder->CreateFCmpONE(
      CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");

  Function *TheFunction = Builder->GetInsertBlock()->getParent();

  // Create blocks for the then and else cases.  Insert the 'then' block at the
  // end of the function.
  BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
  BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
  BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont");

  Builder->CreateCondBr(CondV, ThenBB, ElseBB);

  // Emit then value.
  Builder->SetInsertPoint(ThenBB);

  Value *ThenV = Then->codegen();
  if (!ThenV)
    return nullptr;

  Builder->CreateBr(MergeBB);
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = Builder->GetInsertBlock();

  // Emit else block.
  TheFunction->getBasicBlockList().push_back(ElseBB);
  Builder->SetInsertPoint(ElseBB);

  Value *ElseV = Else->codegen();
  if (!ElseV)
    return nullptr;

  Builder->CreateBr(MergeBB);
  // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
  ElseBB = Builder->GetInsertBlock();

  // Emit merge block.
  TheFunction->getBasicBlockList().push_back(MergeBB);
  Builder->SetInsertPoint(MergeBB);
  PHINode *PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");

  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
}

// Output for-loop as:
//   ...
//   start = startexpr
//   goto loop
// loop:
//   variable = phi [start, loopheader], [nextvariable, loopend]
//   ...
//   bodyexpr
//   ...
// loopend:
//   step = stepexpr
//   nextvariable = variable + step
//   endcond = endexpr
//   br endcond, loop, endloop
// outloop:
Value *ForExprAST::codegen() {
  // Emit the start code first, without 'variable' in scope.
  Value *StartVal = Start->codegen();
  if (!StartVal)
    return nullptr;

  // Make the new basic block for the loop header, inserting after current
  // block.
  Function *TheFunction = Builder->GetInsertBlock()->getParent();
  BasicBlock *PreheaderBB = Builder->GetInsertBlock();
  BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);

  // Insert an explicit fall through from the current block to the LoopBB.
  Builder->CreateBr(LoopBB);

  // Start insertion in LoopBB.
  Builder->SetInsertPoint(LoopBB);

  // Start the PHI node with an entry for Start.
  PHINode *Variable =
      Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, VarName);
  Variable->addIncoming(StartVal, PreheaderBB);

  // Within the loop, the variable is defined equal to the PHI node.  If it
  // shadows an existing variable, we have to restore it, so save it now.
  Value *OldVal = NamedValues[VarName];
  NamedValues[VarName] = Variable;

  // Emit the body of the loop.  This, like any other expr, can change the
  // current BB.  Note that we ignore the value computed by the body, but don't
  // allow an error.
  if (!Body->codegen())
    return nullptr;

  // Emit the step value.
  Value *StepVal = nullptr;
  if (Step) {
    StepVal = Step->codegen();
    if (!StepVal)
      return nullptr;
  } else {
    // If not specified, use 1.0.
    StepVal = ConstantFP::get(*TheContext, APFloat(1.0));
  }

  Value *NextVar = Builder->CreateFAdd(Variable, StepVal, "nextvar");

  // Compute the end condition.
  Value *EndCond = End->codegen();
  if (!EndCond)
    return nullptr;

  // Convert condition to a bool by comparing non-equal to 0.0.
  EndCond = Builder->CreateFCmpONE(
      EndCond, ConstantFP::get(*TheContext, APFloat(0.0)), "loopcond");

  // Create the "after loop" block and insert it.
  BasicBlock *LoopEndBB = Builder->GetInsertBlock();
  BasicBlock *AfterBB =
      BasicBlock::Create(*TheContext, "afterloop", TheFunction);

  // Insert the conditional branch into the end of LoopEndBB.
  Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

  // Any new code will be inserted in AfterBB.
  Builder->SetInsertPoint(AfterBB);

  // Add a new entry to the PHI node for the backedge.
  Variable->addIncoming(NextVar, LoopEndBB);

  // Restore the unshadowed variable.
  if (OldVal)
    NamedValues[VarName] = OldVal;
  else
    NamedValues.erase(VarName);

  // for expr always returns 0.0.
  return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}

Function *PrototypeAST::codegen() {
  // Make the function type:  double(double,double) etc.
  std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(*TheContext));
  FunctionType *FT =
      FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);

    Function* F =
        Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

    // Set names for all arguments.
    unsigned Idx = 0;
    for (auto& Arg : F->args())
        Arg.setName(Args[Idx++]);

    return F;
}

Function *FunctionAST::codegen() {
  // Transfer ownership of the prototype to the FunctionProtos map, but keep a
  // reference to it for use below.
  auto &P = *Proto;
  FunctionProtos[Proto->getName()] = std::move(Proto);
  Function *TheFunction = getFunction(P.getName());
  if (!TheFunction)
    return nullptr;

  // If this is an operator, install it.
  if (P.isBinaryOp())
    BinopPrecedence[P.getName()] = P.getBinaryPrecedence();

  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
  Builder->SetInsertPoint(BB);

    // Record the function arguments in the NamedValues map.
    NamedValues.clear();
    for (auto& Arg : TheFunction->args())
        NamedValues[std::string(Arg.getName())] = &Arg;

    if (Value* RetVal = Body->codegen()) {
        // Finish off the function.
        Builder->CreateRet(RetVal);

        // Validate the generated code, checking for consistency.
        verifyFunction(*TheFunction);

    // Run the optimizer on the function.
    TheFPM->run(*TheFunction);

    return TheFunction;
  }

  // Error reading body, remove function.
  TheFunction->eraseFromParent();

  if (P.isBinaryOp())
    BinopPrecedence.erase(P.getName());
  return nullptr;
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static void InitializeModuleAndPassManager() {
  // Open a new context and module.
  TheContext = std::make_unique<LLVMContext>();
  TheModule = std::make_unique<Module>("my cool jit", *TheContext);
  TheModule->setDataLayout(TheJIT->getDataLayout());

  // Create a new builder for the module.
  Builder = std::make_unique<IRBuilder<>>(*TheContext);

  // Create a new pass manager attached to it.
  TheFPM = std::make_unique<legacy::FunctionPassManager>(TheModule.get());

  // Do simple "peephole" optimizations and bit-twiddling optzns.
  TheFPM->add(createInstructionCombiningPass());
  // Reassociate expressions.
  TheFPM->add(createReassociatePass());
  // Eliminate Common SubExpressions.
  TheFPM->add(createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  TheFPM->add(createCFGSimplificationPass());

  TheFPM->doInitialization();
}

static void HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    if (auto *FnIR = FnAST->codegen()) {
      *myerr << "Read function definition:";
      FnIR->print(*myerr);
      *myerr<<"\n";
      ExitOnErr(TheJIT->addModule(
          ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
      InitializeModuleAndPassManager();
    }
  } else {
    // Skip token for error recovery.
    Tokenizer.tokenize();
  }
}

static void HandleExtern() {
  if (auto ProtoAST = ParseExtern()) {
    if (auto *FnIR = ProtoAST->codegen()) {
      *myerr << "Read extern: ";
      FnIR->print(*myerr);
      *myerr << "\n";
      FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
    }
  } else {
    // Skip token for error recovery.
    Tokenizer.tokenize();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto FnAST = ParseTopLevelExpr()) {
    if (FnAST->codegen()) {
      // Create a ResourceTracker to track JIT'd memory allocated to our
      // anonymous expression -- that way we can free it after executing.
      auto RT = TheJIT->getMainJITDylib().createResourceTracker();

      auto TSM = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
      ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
      InitializeModuleAndPassManager();

      // Search the JIT for the __anon_expr symbol.
      auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));

      // Get the symbol's address and cast it to the right type (takes no
      // arguments, returns a double) so we can call it as a native function.
      double (*FP)() = (double (*)())(intptr_t)ExprSymbol.getAddress();
      *myerr << "Evaluated to "<< FP()<<'\n';

      // Delete the anonymous expression module from the JIT.
      ExitOnErr(RT->remove());
    }
  } else {
    // Skip token for error recovery.
    Tokenizer.tokenize();
  }
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
    while (true) {
        *myerr<< "ready> ";
        switch (Tokenizer.cur_token().token_number) {
        case TK_EOF:
            return;
        case TK_SEMICOLON: // ignore top-level semicolons.
            Tokenizer.tokenize();
            break;
        case TK_FUNCTION://instead of define
            HandleDefinition();
            break;
        case TK_DECLARE://instead of extern
            HandleExtern();
            break;
        default:
            HandleTopLevelExpression();
            break;
        }
    }
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

std::string mainish(LPSTR source)
{
    errbuf.clear();
    myerr = new raw_string_ostream(errbuf);

    // Prime the first token.
    Tokenizer.source.resize(strlen(source)+1);
    Tokenizer.set_to_beginning_of_file();
    strncpy(&Tokenizer.source[0], source, Tokenizer.source.size());

    Tokenizer.tokenize();

 

    // Run the main "interpreter loop" now.
    MainLoop();
  

    // Print out all of the generated code.
    TheModule->print(*myerr, nullptr);
    myerr->flush();
    return  errbuf;
}
void init_parser()
{
    for (int i = 0; i < TK_NUM_TOKENS; ++i) {
        BiopPrecedence[i] = 0;
        PreopPrecedence[i] = 0;
        PostopPrecedence[i] = 0;
        OpAssoc[i] = Associativity::NotOperator;
    }
    // Install standard binary operators.
    // 1 is lowest precedence.
/*
*
*
1	()   []   ->   .   ::	Function call, scope, array/member access
2	   ~   -   +   *   &   sizeof   type cast   ++   --  	(most) unary operators, sizeof and type casts (right to left)
3	*   /   % MOD	Multiplication, division, modulo
4	+   -	Addition and subtraction
5	<<   >>	Bitwise shift left and right
6	&	Bitwise AND
6	^	Bitwise exclusive OR (XOR)
6	|	Bitwise inclusive (normal) OR
7	<   <=   >   >=	Comparisons: less-than and greater-than
7	==   !=	Comparisons: equal and not equal

8	and
8	or
9   not
10	? :	Conditional expression (ternary)
11	=   +=   -=   *=   /=   %=   &=   |=   ^=   <<=   >>=	Assignment operators (right to left)
12	,
12  ;

    TK_AND,              and
        TK_NOT,          not
        TK_OR,           or
        TK_TO,
        TK_BXOR,
        TK_MINUS,
        TK_ASTERIX,
        TK_AMPERSAND,
        TK_EXCLAMATION,
        TK_TILDE,
        TK_PLUSPLUS,
        TK_MINUSMINUS,
        TK_EQUAL,
        TK_PLUSEQ,
        TK_MINUSEQ,
        TK_ASTERIXEQ,
        TK_SLASHEQ,
        TK_MODEQ,
        TK_LTLTEQ,
        TK_GTGTEQ,
        TK_BANDEQ,
        TK_BOREQ,
        TK_BXOREQ,
        TK_CAROT,
        TK_LE,
        TK_GE,
        TK_LT,
        TK_GT,
        TK_EQEQ,
        TK_NOTEQ,
        TK_LTEQGT,
        TK_DOTDOT,
        TK_PERIOD,
        TK_PIPE,
        TK_GTGT,
        TK_LTLT,
        TK_PLUS,
        TK_SLASH,
        TK_QMARKEQ,
        TK_QMARK,
        TK_BSLASH,
        TK_HASH,
        TK_HASHPIPE,

static int BiopPrecedence[TK_NUM_TOKENS];
static int PreopPrecedence[TK_NUM_TOKENS];
static int PostopPrecedence[TK_NUM_TOKENS];
enum class Associativity {
    Left    =   1,
    Right   =   2,
    NonAssoc=   3,
    BiopMask=   3,
    Pre     =   4,
    Post    =   8,
    NotOperator=16
};
static enum class Associativity OpAssoc[TK_NUM_TOKENS];
        */
    BiopPrecedence[TK_LT] = 10;
    OpAssoc[TK_LT] = Associativity::Left;
    BiopPrecedence[TK_PLUS] = 20;
    OpAssoc[TK_PLUS] = Associativity::Left;
    BiopPrecedence[TK_MINUS] = 20;
    OpAssoc[TK_MINUS] = Associativity::Left;
    BiopPrecedence[TK_ASTERIX] = 40; // highest.
    OpAssoc[TK_ASTERIX] = Associativity::Left;

    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();
    TheJIT = ExitOnErr(KaleidoscopeJIT::Create());

    InitializeModuleAndPassManager();

}
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
/*
int main()
{
    std::ifstream t("C:\\local\\tinycc\\pre.c");
    std::stringstream buffer;
    buffer << t.rdbuf();
    std::string b = buffer.str();
    lexer_iterator<std::string::iterator> lexer(b.begin(), b.end());

    do
    {
        std::cout << lexer->value << token_names[(int)lexer->type + 1] << '\n';
        lexer.next();
    } while (lexer->type != token_type::ENDOFFILE);
    return 0;
}*/
LPSTR UnicodeToUTF8(LPCTSTR s);
/*
GraphemeString nye(L"ñññññññ"),
 hindi(L"अनुच्छेद"),
 emojis(L"🌷🎁💩😜👍🏳️‍🌈"),
 diacritics(L"Ĺo͂řȩm̅"),
 korean(L"뎌쉐"),
 zalgo(L"Z͑ͫ̓ͪ̂ͫ̽͏̴̙̤̞͉͚̯̞̠͍A̴̵̜̰͔ͫ͗͢L̠ͨͧͩ͘G̴̻͈͍͔̹̑͗̎̅͛́Ǫ̵̹̻̝̳͂̌̌͘!͖̬̰̙̗̿̋ͥͥ̂ͣ̐́́͜͞");
 
GraphemeString source1(L"ñññññññअनुच्छेद🌷🎁💩😜👍🏳️‍🌈Ĺo͂řȩm̅뎌쉐Z͑ͫ̓ͪ̂ͫ̽͏̴̙̤̞͉͚̯̞̠͍A̴̵̜̰͔ͫ͗͢L̠ͨͧͩ͘G̴̻͈͍͔̹̑͗̎̅͛́Ǫ̵̹̻̝̳͂̌̌͘!͖̬̰̙̗̿̋ͥͥ̂ͣ̐́́͜͞");
GraphemeString source2 = source1.slice(7, -1);

GraphemeString nye = source1.slice(0, 6).deep_copy(),
hindi = source2.slice(0, 4).deep_copy(),
emojis = source2.slice(5, 10).deep_copy(),
diacritics = source2.slice(11, 15).deep_copy(),
korean = source2.slice(16, 17).deep_copy(),
zalgo = source2.slice(18,-1).deep_copy();
*/
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

#include <unordered_map>
#include <set>
int lex_error_position;
static debug_cat_set grapheme_cat_singleton;
debug_cat_set& grapheme_cats(GraphemeString& o)
{
    grapheme_cat_singleton.clear();
    for (int i = 0; i < o.codepoint_length(); ++i)
    {
        if (o.codepoint_at(i) != 0) {
            grapheme_cat_singleton.push_back(utf8proc_category(o.codepoint_at(i)));
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
                    lacks_category.push_back(cat.back());
                }
                else {
                    has_category.push_back(cat.back());
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
            nfas[current].has_category.count(cats) != 0)) && (0 == nfas[current].lacks_category.count(cats) && 0 == nfas[current].lacks.count(next_char))){
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
std::string mainish(LPSTR source)
{

    GraphemeString b(source);
 //   GraphemeStringBuilder b;

 //   b << nye << hindi << emojis << diacritics;
    //b << korean << zalgo;
    std::ostringstream t;
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
        LexerGen
            .prod("OPEN_BRACKET", "\\[")
            .prod("CLOSE_BRACKET", "\\]")
            .prod("OPEN_PAREN", "\\(")
            .prod("CLOSE_PAREN", "\\)")
            .prod("OPEN_BRACE", "\\{")
            .prod("CLOSE_BRACE", "\\}")
            .prod("PERIOD", ".")
            .prod("AMP", "&")
            .prod("PIPE", "\\|")
            .prod("AND", "&&")
            .prod("OR", "\\|\\|")
            .prod("PLUS", "\\+")
            .prod("WIDEPLUS", u8"＋")
            .prod("MINUS", "\\-")
            .prod("TILDE", "~")
            .prod("MUL", "\\*")
            .prod("DIV", "/")
            .prod("BANG", "!")
            .prod("MOD", "%")
            .prod("ASSIGN", "=")
            .prod("POINTSTO", "->")
            .prod("ADD_ASSIGN", "\\+=")
            .prod("SUB_ASSIGN", "\\-=")
            .prod("MUL_ASSIGN", "\\*=")
            .prod("DIV_ASSIGN", "/=")
            .prod("AND_ASSIGN", "&=")
            .prod("OR_ASSIGN", "\\|=")
            .prod("XOR_ASSIGN", "^=")
            .prod("MOD_ASSIGN", "%=")
            .prod("SHL_ASSIGN", "<<=")
            .prod("SHR_ASSIGN", ">>=")
            .prod("XOR", "^")
            .prod("QMARK", "\\?")
            .prod("SHL", "<<")
            .prod("SHR", ">>")
            .prod("LT", "<")
            .prod("GT", ">")
            .prod("LE", "<=")
            .prod("GE", ">=")
            .prod("NE", "!=")
            .prod("EQ", "==")
            .prod("PREPROCESS_LINE", "#[^\\n]*+")//{}{}{} not processed correctly
            .prod("IDENT", "[\\p{:word:}_][_\\p{:alnum:}]*+")
            .prod("LITERAL", "auto|double|int|struct|break|else|long|switch|case|enum|register|typedef|char|extern|return|union|const|float|short|unsigned|continue|for|signed|void|default|goto|sizeof|volatile|do|if|static|while|_Bool|_Imaginary|restrict|_Complex|inline|_Alignas|_Generic|_Thread_local|_Alignof|_Noreturn|_Atomic|_Static_assert")
            .prod("COMMA", ",")
            .prod("COLON", "\\:")
            .prod("SEMICOLON", ";")
            .prod("STRING", "(L|u|U|u8)?R?\"(\\\\([\"'\\\\/bfrntav0]|[0-7][0-7][0-7]|x[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]|u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F])|[^\"\\\\])*\"s?")
            .prod("CHAR", "(L|u|U|u8)?R?'(\\\\([\"'\\\\/bfrntav0]|[0-7][0-7][0-7]|x[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]|u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F])|[^'\\\\])*'")
            .prod("NUMBER", "-?([\\p{:digit:}]*+)(.[\\p{:digit:}]++)?([Ee][+\\-]?([\\p{:digit:}]*+))?(u|U)?(d|f|LL|L)?")

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