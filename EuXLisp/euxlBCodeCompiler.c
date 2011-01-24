/// Copyright 1988 David Michael Betz
/// Copyright 1994 Russell Bradford
/// Copyright 2010, 2011 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                           EuLisp System 'EuXLisp'
///-----------------------------------------------------------------------------
//
//  EuXLisp is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
///-----------------------------------------------------------------------------
/// Title: Byte-code compiler
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"
#include "euxlBCodes.h"

#include <dlfcn.h>

///-----------------------------------------------------------------------------
/// Macros
///-----------------------------------------------------------------------------
#define EUXL_NOISY_LOAD

// Size of code buffer
#define euxmCodeBufferSize    10000

// Continuation types
#define euxmContReturn        -1
#define euxmContNext          -2

#define euxmSpaces() (spacy + 20 - (indent > 20 ? 20 : indent < 0 ? 0 : indent))

///  Instruction output formats
#define FMT_NONE        0
#define FMT_BYTE        1
#define FMT_LOFF        2
#define FMT_WORD        3
#define FMT_EOFF        4
#define FMT_LOFFL       5

///-----------------------------------------------------------------------------
/// Local variables
///-----------------------------------------------------------------------------
static const char *module_search_path[] =
{
    MODULE_SEARCH_PATH,
    0
};

static euxlValue info; // compiler info

///  Code buffer
static unsigned char cbuff[euxmCodeBufferSize]; // base of code buffer
static int cbase;                               // base for current function
static int cptr;                                // code buffer pointer

typedef struct
{
    int ot_code;
    char *ot_name;
    int ot_fmt;
} byteCodeFmtDef;

byteCodeFmtDef otab[] =
{
    {OP_BRT, "BRT", FMT_WORD},
    {OP_BRF, "BRF", FMT_WORD},
    {OP_BR, "BR", FMT_WORD},
    {OP_LIT, "LIT", FMT_LOFF},
    {OP_GREF, "GREF", FMT_LOFF},
    {OP_GSET, "GSET", FMT_LOFF},
    {OP_EREF, "EREF", FMT_EOFF},
    {OP_ESET, "ESET", FMT_EOFF},
    {OP_SAVE, "SAVE", FMT_WORD},
    {OP_CALL, "CALL", FMT_BYTE},
    {OP_RETURN, "RETURN", FMT_NONE},
    {OP_T, "T", FMT_NONE},
    {OP_NIL, "NIL", FMT_NONE},
    {OP_PUSH, "PUSH", FMT_NONE},
    {OP_CLOSE, "CLOSE", FMT_NONE},
    {OP_DELAY, "DELAY", FMT_NONE},

    {OP_FRAME, "FRAME", FMT_BYTE},
    {OP_MVARG, "MVARG", FMT_BYTE},
    {OP_MVOARG, "MVOARG", FMT_BYTE},
    {OP_MVRARG, "MVRARG", FMT_BYTE},
    {OP_ADROP, "ADROP", FMT_NONE},
    {OP_ALAST, "ALAST", FMT_NONE},

    {OP_AREF, "AREF", FMT_LOFF},
    {OP_ASET, "ASET", FMT_LOFF},

    {OP_CNM, "CALL-NEXT-METHOD", FMT_NONE},

    {OP_GREFL, "GREFL", FMT_LOFFL},
    {OP_GSETL, "GSETL", FMT_LOFFL},
    {OP_LITL, "LITL", FMT_LOFFL},

    {0, 0, 0}
};

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static void compileExpr(euxlValue expr, int cont);
static int inNtab(euxlValue expr, int cont);
static int inFtab(euxlValue expr, int cont);
static void compileDefun(euxlValue form, int cont);
static void compileDefconstant(euxlValue form, int cont);
static void compileDeflocal(euxlValue form, int cont);
static void compileSet(euxlValue form, int cont);
static void compileSetVar(euxlValue form, int cont);
static void compileQuote(euxlValue form, int cont);
static void compileLambda(euxlValue form, int cont);
static void compileFunction(euxlValue fun, euxlValue fargs, euxlValue body);
static void parseLambdaList(euxlValue fargs, euxlValue body);
static int findInternalDefinitions(euxlValue body, euxlValue last);
static void compileDelay(euxlValue form, int cont);
static void compileLet(euxlValue form, int cont);
static void compileNamedLet(euxlValue form, int cont);
static void compileLetNamedLet(euxlValue name, euxlValue form, int cont);
static void compileLetrec(euxlValue form, int cont);
static void compileLetstar(euxlValue form, int cont);
static void CompileLetstarCont(euxlValue blist, euxlValue body);
static int pushDummyValues(euxlValue blist);
static int pushInitExpressions(euxlValue blist);
static void parseLetVariables(euxlValue blist, euxlValue body);
static void setBoundVariables(euxlValue blist);
static euxlValue makeCodeObject(euxlValue fun);
static void compileCond(euxlValue form, int cont);
static void compileAnd(euxlValue form, int cont);
static void compileOr(euxlValue form, int cont);
static void compileIf(euxlValue form, int cont);
static void compileProgn(euxlValue form, int cont);
static void compileWhile(euxlValue form, int cont);
static void compileAccess(euxlValue form, int cont);
static void compileSetaccess(euxlValue form, int cont);
static void compileCall(euxlValue form, int cont);
static int pushArgs(euxlValue form);
static void compileNary(int op, int n, euxlValue form, int cont);
static int pushNargs(euxlValue form, euxlValue body, int n);
static void compileLiteral(euxlValue lit, int cont);
static void compileIdentifier(euxlValue sym, int cont);
static void compileContinuation(int cont);
static int addLevel();
static void removeLevel(int oldcbase);
static int findVariable(euxlValue sym, int *plev);
static int findVariableCurrentFrame(euxlValue sym);
static int findLiteral(euxlValue lit);
static void compileVariable(int op, euxlValue sym);
static void compileEVariable(int op, int lev, int off);
static int putCodeByte(int b);
static int putCodeWord(int w);
static void fixup(int chn);

static void compileDefmodule(euxlValue form, int cont);
static void compileExport(euxlValue form, int cont);
static void compileExpose(euxlValue form, int cont);
static void compileEnterModule(euxlValue form, int cont);
static void compileReenterModule(euxlValue form, int cont);
static void compileImport(euxlValue form, int cont);
static void reinternModuleSymbols(euxlValue body);

static void compileDefineGeneric(euxlValue form, int cont);
static void compileDefineMethod(euxlValue form, int cont);
static void compileCnm(euxlValue form, int cont);
static void compileNextMethodp(euxlValue form, int cont);
static void compileDefclass(euxlValue form, int cont);

static void compileDefcondition(euxlValue form, int cont);

static euxlValue reinternSymbol(euxlValue a);
static euxlValue filterImports(euxlValue implist, euxlValue sofar);

static void processFFI(const char* modname, euxlValue forms);
static FILE* createModuleFFI(const char* modname);
static void compileDefextern(euxlValue form, int cont);
static FILE* processDefextern(FILE* fffile, euxlValue form);
static void closeModuleFFI(FILE* fffile);
static void compileModuleFFI(const char* modname);
static void loadModuleFFI(const char* modname);

// Byte-coded functions
typedef struct
{
    char *name;
    int code, args;
} byteCodedFunDef;

static byteCodedFunDef byteCodeFunTab[] =
{
    {"atom?", OP_ATOM, 1},
    {"eq", OP_EQ, 2},
    {"null?", OP_NULL, 1},
    {"not", OP_NULL, 1},
    {"cons", OP_CONS, 2},
    {"car", OP_CAR, 1},
    {"cdr", OP_CDR, 1},
    {"set-car", OP_SETCAR, 2},
    {"set-cdr", OP_SETCDR, 2},
    {"%+", OP_ADD, -2},
    {"%-", OP_SUB, -2},
    {"%*", OP_MUL, -2},
    {"%/", OP_DIV, -2},
    {"%quotient", OP_QUO, -2},
    {"%<", OP_LSS, -2},
    {"%=", OP_EQL, -2},
    {"%>", OP_GTR, -2},
    {"class-of", OP_CLASSOF, 1},
    {"%GETIVAR", OP_GETIVAR, 2},
    {"%SETIVAR", OP_SETIVAR, 3},
    {"get", OP_GET, 2},
    {"put", OP_PUT, 3},
    {"current-module", OP_CURMOD, 0},
    {"cons?", OP_CONSP, 1},
    {"symbol?", OP_SYMBOLP, 1},
    {"vector?", OP_VECTORP, 1},
    {"append", OP_APPEND, -2},
    {"list", OP_LIST, -2},
    {"list-size", OP_SIZE, 1},
    {"reverse", OP_REVERSE, 1},
    {"caar", OP_CAAR, 1},
    {"cadr", OP_CADR, 1},
    {"cdar", OP_CDAR, 1},
    {"cddr", OP_CDDR, 1},
    {"get-syntax", OP_GETSYNTAX, 2},
    {"put-syntax", OP_PUTSYNTAX, 3},
    #ifndef NO_CHECK_REF
    {"check-ref", OP_CHECKREF, 2},
    #endif
    {(char *)0, 0, 0}
};

specialFormDef specialFormTab[] =
{
    {"quote", compileQuote},
    {"lambda", compileLambda},
    {"delay", compileDelay},
    {"let", compileLet},
    {"let*", compileLetstar},
    {"letrec", compileLetrec},
    {"%defun", compileDefun},
    {"defconstant", compileDefconstant},
    {"deflocal", compileDeflocal},
    {"setq", compileSet},
    {"if", compileIf},
    {"cond", compileCond},
    {"progn", compileProgn},
    {"sequence", compileProgn},
    {"and", compileAnd},
    {"or", compileOr},
    {"while", compileWhile},
    {"access", compileAccess},
    {"defmodule", compileDefmodule},
    {"export", compileExport},
    {"expose", compileExpose},
    {"enter-module", compileEnterModule},
    {"!>", compileEnterModule},
    {"reenter-module", compileReenterModule},
    {"!>>", compileReenterModule},
    {"%import", compileImport},
    {"%defgeneric", compileDefineGeneric},
    {"%defmethod", compileDefineMethod},
    {"call-next-method", compileCnm},
    {"next-method?", compileNextMethodp},
    {"defclass", compileDefclass},
    {"defcondition", compileDefcondition},
    {"defextern", compileDefextern},
    {(char *)0, (void (*)())0}
};

///-----------------------------------------------------------------------------
/// Utility Functions
///-----------------------------------------------------------------------------
#include <stdarg.h>

static euxlValue concat(const char *str, ...)
{
    // Sum sizes of all the argument strings
    // in order to allocate the correct result string
    size_t rlen = 1;

    va_list ap;
    va_start(ap, str);

    for (const char *s = str; s != NULL; s = va_arg(ap, const char *))
    {
        rlen += strlen(s);
    }

    va_end(ap);


    // Allocate the result string and concatenate arguments
    euxlValue result = euxcNewString(rlen);
    char *rp = euxmGetString(result);

    va_start(ap, str);

    for (const char *s = str; s != NULL; s = va_arg(ap, const char *))
    {
        size_t len = strlen(s);
        rp = memcpy(rp, s, len) + len;
    }

    va_end(ap);

    // Terminate the result string
    *rp++ = '\0';

    return result;
}

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxmCompileError - signal a compilation error
static void euxmCompileError(const char *msg, euxlValue arg)
{
    euxcCerror(msg, arg, euxls_compile_error);
}

///  euxcCompile - Compile an expression
euxlValue euxcCompile(euxlValue expr, euxlValue ctenv)
{
    // initialize the compile time environment
    info = euxcCons(euxmNil, euxmNil);
    euxmStackCheckPush(info);
    euxmSetCar(info, euxcNewFrame(ctenv, 1));
    euxmSetCdr(info, euxcCons(euxmNil, euxmNil));

    // setup the base of the code for this function
    cbase = cptr = 0;

    // setup the entry code
    putCodeByte(OP_FRAME);
    putCodeByte(1);

    // compile the expression
    compileExpr(expr, euxmContReturn);

    // build the code object
    euxmSetStackTop(makeCodeObject(euxmNil));

    return (euxmStackPop());
}

///  euxcCompileFunction - Compile a function
euxlValue euxcCompileFunction
(
    euxlValue fun,
    euxlValue fargs,
    euxlValue body,
    euxlValue ctenv
)
{
    // initialize the compile time environment
    info = euxcCons(euxmNil, euxmNil);
    euxmStackCheckPush(info);
    euxmSetCar(info, euxcNewFrame(ctenv, 1));
    euxmSetCdr(info, euxcCons(euxmNil, euxmNil));

    // setup the base of the code for this function
    cbase = cptr = 0;

    // compile the lambda list and the function body
    parseLambdaList(fargs, body);
    compileProgn(body, euxmContReturn);

    // build the code object
    euxmSetStackTop(makeCodeObject(fun));
    return (euxmStackPop());
}

///  compileExpr - compile an expression
//    (deflocal a (let ((lambda (lambda (lambda) lambda))) (lambda lambda)))
static void compileExpr(euxlValue expr, int cont)
{
    euxmStackCheckPush(expr);

    if (euxmConsp(expr))
    {
        euxlValue sym = euxmCar(expr);
        int lev, off;

        if
        (
            // ((foo 1) 2)
            !euxmSymbolp(sym)
            // (let ((euxmCar euxmCdr)) (euxmCar x))
         || (off = findVariable(sym, &lev))
         || euxmGetModule(sym) != euxcRootModule
         || (!inNtab(expr, cont) && !inFtab(expr, cont))
        )
        {
            compileCall(expr, cont);
        }
    }
    else if (euxmSymbolp(expr))
    {
        compileIdentifier(expr, cont);
    }
    else
    {
        compileLiteral(expr, cont);
    }

    euxmStackDrop(1);
}

///  inNtab - Check for a function in byteCodeFunTab
static int inNtab(euxlValue expr, int cont)
{
    euxlValue sym = euxmCar(expr);

    char *pname = euxmGetString(euxmGetPName(sym));

    for (byteCodedFunDef *nptr = byteCodeFunTab; nptr->name; ++nptr)
    {
        if (strcmp(pname, nptr->name) == 0)
        {
            compileNary(nptr->code, nptr->args, expr, cont);
            return (euxmTrue);
        }
    }

    return (euxmFalse);
}

///  inFtab - Check for a function in specialFormTab
static int inFtab(euxlValue expr, int cont)
{
    euxlValue sym = euxmCar(expr);
    char *pname = euxmGetString(euxmGetPName(sym));

    for (specialFormDef *fptr = specialFormTab; fptr->name; ++fptr)
    {
        if (strcmp(pname, fptr->name) == 0)
        {
            (*fptr->fun) (euxmCdr(expr), cont);
            return (euxmTrue);
        }
    }

    return (euxmFalse);
}

///  compileDefun - handle the (defun ... ) expression
static void compileDefun(euxlValue form, int cont)
{
    if (euxmAtom(form))
    {
        euxmCompileError("expecting function name symbol in defun", form);
    }

    euxlValue funname = euxmCar(form);
    if (!euxmSymbolp(funname) || euxmKeywordp(funname))
    {
        euxmCompileError("expecting a function name symbol in defun", funname);
    }

    form = euxmCdr(form);
    if (!euxmConsp(form))
    {
        euxmCompileError("expecting argument list in defun", funname);
    }

    euxlValue args = euxmCar(form);
    euxlValue body = euxmCdr(form);

    compileFunction(funname, args, body);

    // Define the variable value
    int off = findVariableCurrentFrame(funname);
    if (off)
    {
        compileEVariable(OP_ESET, 0, off);
    }
    else
    {
        compileVariable(OP_GSET, funname);
    }

    compileLiteral(funname, cont);
}

///  compileDefconstant - handle the (defconstant ... ) expression
static void compileDefconstant(euxlValue form, int cont)
{
    if (euxmAtom(form))
    {
        euxmCompileError("expecting symbol in defconstant", form);
    }

    euxlValue sym = euxmCar(form);
    if (!euxmSymbolp(sym))
    {
        euxmCompileError("expecting a symbol in defconstant", sym);
    }

    if (euxmKeywordp(sym))
    {
        euxmCompileError("trying to set a keyword in defconstant", sym);
    }

    form = euxmCdr(form);

    if (form && euxmAtom(form))
    {
        euxmCompileError("expecting value expression in defconstant", form);
    }

    // compile the value expression
    compileExpr(form == euxmNil ? euxmNil : euxmCar(form), euxmContNext);

    // Define the variable value
    int off = findVariableCurrentFrame(sym);
    if (off)
    {
        euxmCompileError("defconstant not at top level", sym);
    }

    compileVariable(OP_GSET, sym);
    euxmSetConstant(sym, euxs_t);

    compileLiteral(sym, cont);
}


///  compileDeflocal - handle the (deflocal ... ) expression
static void compileDeflocal(euxlValue form, int cont)
{
    if (euxmAtom(form))
    {
        euxmCompileError("expecting symbol in deflocal", form);
    }

    euxlValue sym = euxmCar(form);
    if (!euxmSymbolp(sym))
    {
        euxmCompileError("expecting a symbol in deflocal", sym);
    }

    if (euxmKeywordp(sym))
    {
        euxmCompileError("trying to set a keyword in deflocal", sym);
    }

    form = euxmCdr(form);

    if (form && euxmAtom(form))
    {
        euxmCompileError("expecting value expression in deflocal", form);
    }

    // compile the value expression
    compileExpr(form == euxmNil ? euxmNil : euxmCar(form), euxmContNext);

    // Define the variable value
    int off = findVariableCurrentFrame(sym);
    if (off)
    {
        euxmCompileError("deflocal not at top level", sym);
    }

    compileVariable(OP_GSET, sym);

    compileLiteral(sym, cont);
}

///  compileSet - compile the (set! ... ) expression
static void compileSet(euxlValue form, int cont)
{
    if (euxmAtom(form))
    {
        euxmCompileError("expecting symbol or ACCESS form", form);
    }
    else if (euxmSymbolp(euxmCar(form)))
    {
        compileSetVar(form, cont);
    }
    else if (euxmConsp(euxmCar(form)))
    {
        compileSetaccess(form, cont);
    }
    else
    {
        euxmCompileError("expecting symbol or ACCESS form", form);
    }
}

///  compileSetVar - compile the (set! var value) expression
static void compileSetVar(euxlValue form, int cont)
{
    // get the variable name
    euxlValue sym = euxmCar(form);

    if (euxmKeywordp(sym))
    {
        euxmCompileError("trying to set a keyword", sym);
    }

    // compile the value expression
    form = euxmCdr(form);
    if (euxmAtom(form))
    {
        euxmCompileError("expecting value expression", form);
    }
    compileExpr(euxmCar(form), euxmContNext);

    // set the variable value
    int lev, off = findVariable(sym, &lev);
    if (off)
    {
        compileEVariable(OP_ESET, lev, off);
    }
    else if (euxmConstantp(sym))
    {
        euxmCompileError("trying to set a constant binding", sym);
    }
    else
    {
        compileVariable(OP_GSET, sym);
    }

    compileContinuation(cont);
}

///  compileQuote - compile the (quote ... ) expression
static void compileQuote(euxlValue form, int cont)
{
    if (euxmAtom(form))
    {
        euxmCompileError("expecting quoted expression", form);
    }
    compileLiteral(euxmCar(form), cont);
}

///  compileLambda - compile the (lambda ... ) expression
static void compileLambda(euxlValue form, int cont)
{
    if (euxmAtom(form))
    {
        euxmCompileError("expecting argument list", form);
    }
    compileFunction(euxmNil, euxmCar(form), euxmCdr(form));
    compileContinuation(cont);
}

///  compileFunction - compile the function
static void compileFunction(euxlValue fun, euxlValue fargs, euxlValue body)
{
    // establish a new environment frame
    int oldcbase = addLevel();

    // compile the lambda list and the function body
    parseLambdaList(fargs, body);
    compileProgn(body, euxmContReturn);

    // build the code object
    euxmStackCheckPush(makeCodeObject(fun));

    // restore the previous environment
    removeLevel(oldcbase);

    // compile code to create a closure
    compileLiteral(euxmStackPop(), euxmContNext);
    putCodeByte(OP_CLOSE);
}

///  parseLambdaList - parse the formal argument list
static void parseLambdaList(euxlValue fargs, euxlValue body)
{
    // setup the entry code
    putCodeByte(OP_FRAME);
    int frame = putCodeByte(0);

    // initialize the argument name list and slot number
    euxlValue restarg = euxmNil;
    euxlValue last = euxmNil;
    int slotn = 1;

    // handle each required argument
    euxlValue arg;
    while (euxmConsp(fargs) && (arg = euxmCar(fargs)) != euxmNil)
    {
        // make sure the argument is a symbol
        if (!euxmSymbolp(arg))
        {
            euxmCompileError("variable in lambda-list must be a symbol", arg);
        }

        // and not a keyword
        if (euxmKeywordp(arg))
        {
            euxmCompileError("trying to bind a keyword in lambda-list", arg);
        }

        // add the argument name to the name list
        euxlValue new = euxcCons(arg, euxmNil);
        if (last)
        {
            euxmSetCdr(last, new);
        }
        else
        {
            euxmSetElement(euxmCar(euxmCar(info)), 0, new);
        }
        last = new;

        // generate an instruction to move the argument into the frame
        putCodeByte(OP_MVARG);
        putCodeByte(slotn++);

        // move the formal argument list pointer ahead
        fargs = euxmCdr(fargs);
    }

    // Check for the a dotted tail
    if (restarg == euxmNil && euxmSymbolp(fargs))
    {
        restarg = fargs;

        if (euxmKeywordp(restarg))
        {
            euxmCompileError("trying to bind keyword in lambda-list", restarg);
        }

        // add the argument name to the name list
        euxlValue new = euxcCons(restarg, euxmNil);
        if (last)
        {
            euxmSetCdr(last, new);
        }
        else
        {
            euxmSetElement(euxmCar(euxmCar(info)), 0, new);
        }
        last = new;

        // make the #!rest argument list
        putCodeByte(OP_MVRARG);
        putCodeByte(slotn++);
        fargs = euxmNil;
    }

    // Check for the end of the argument list
    if (fargs != euxmNil)
    {
        euxmCompileError("bad argument list tail in lambda-list", fargs);
    }

    // make sure the user didn't supply too many arguments
    if (restarg == euxmNil)
    {
        putCodeByte(OP_ALAST);
    }

    // scan the body for internal definitions
    slotn += findInternalDefinitions(body, last);

    // fixup the frame instruction
    cbuff[cbase + frame] = slotn;
}

///  findInternalDefinitions - find internal definitions
static int findInternalDefinitions(euxlValue body, euxlValue last)
{
    int n = 0;

    // look for all (defun ...) forms
    for
    (
        euxlValue defun = euxmInternAndExport("defun");
        euxmConsp(body);
        body = euxmCdr(body)
    )
    {
        if (euxmConsp(euxmCar(body)) && euxmCar(euxmCar(body)) == defun)
        {
            // the rest of the (defun ...) form
            euxlValue sym = euxmCdr(euxmCar(body));

            if (euxmConsp(sym))
            {
                // make sure there is a second subform

                // get the second subform
                sym = euxmCar(sym);

                if (euxmSymbolp(sym))
                {
                    euxlValue new = euxcCons(sym, euxmNil);
                    if (last)
                    {
                        euxmSetCdr(last, new);
                    }
                    else
                    {
                        euxmSetElement(euxmCar(euxmCar(info)), 0, new);
                    }
                    last = new;
                    ++n;
                }
            }
        }
    }

    return (n);
}

///  compileDelay - compile the (delay ... ) expression
static void compileDelay(euxlValue form, int cont)
{
    // Check argument list
    if (euxmAtom(form))
    {
        euxmCompileError("expecting delay expression", form);
    }

    // establish a new environment frame
    int oldcbase = addLevel();

    // setup the entry code
    putCodeByte(OP_FRAME);
    putCodeByte(1);

    // compile the expression
    compileExpr(euxmCar(form), euxmContReturn);

    // build the code object
    euxmStackCheckPush(makeCodeObject(euxmNil));

    // restore the previous environment
    removeLevel(oldcbase);

    // compile code to create a closure
    compileLiteral(euxmStackPop(), euxmContNext);
    putCodeByte(OP_DELAY);
    compileContinuation(cont);
}

///  compileLet - compile the (let ... ) expression
static void compileLet(euxlValue form, int cont)
{
    // handle named let
    if (euxmConsp(form) && euxmSymbolp(euxmCar(form)))
    {
        compileNamedLet(form, cont);
    }
    else // handle unnamed let
    {
        compileLetNamedLet(euxmNil, form, cont);
    }
}

///  compileNamedLet - compile the (let name ... ) expression
static void compileNamedLet(euxlValue form, int cont)
{
    int nxt;

    // save a continuation
    if (cont != euxmContReturn)
    {
        putCodeByte(OP_SAVE);
        nxt = putCodeWord(0);
    }

    // establish a new environment frame
    int oldcbase = addLevel();
    euxmSetElement(euxmCar(euxmCar(info)), 0, euxcCons(euxmCar(form), euxmNil));

    // setup the entry code
    putCodeByte(OP_FRAME);
    putCodeByte(2);

    // compile the let expression
    compileLetNamedLet(euxmCar(form), euxmCdr(form), euxmContReturn);

    // build the code object
    euxmStackCheckPush(makeCodeObject(euxls_letname));

    // restore the previous environment
    removeLevel(oldcbase);

    // compile code to create a closure
    compileLiteral(euxmStackPop(), euxmContNext);
    putCodeByte(OP_CLOSE);

    // apply the function
    putCodeByte(OP_CALL);
    putCodeByte(1);

    // target for the continuation
    if (cont != euxmContReturn)
    {
        fixup(nxt);
    }
}

///  Compile a let or named let expression
static void compileLetNamedLet(euxlValue name, euxlValue form, int cont)
{
    // make sure there is a binding list
    if (euxmAtom(form) || !euxmListp(euxmCar(form)))
    {
        euxmCompileError("expecting binding list", form);
    }

    int nxt = 0;

    // save a continuation
    if (cont != euxmContReturn)
    {
        putCodeByte(OP_SAVE);
        nxt = putCodeWord(0);
    }

    // euxmStackPush the initialization expressions
    int n = pushInitExpressions(euxmCar(form));

    // establish a new environment frame
    int oldcbase = addLevel();

    // compile the binding list
    parseLetVariables(euxmCar(form), euxmCdr(form));

    // compile the body of the let/letrec
    compileProgn(euxmCdr(form), euxmContReturn);

    // build the code object
    euxmStackCheckPush(makeCodeObject(euxls_letname));

    // restore the previous environment
    removeLevel(oldcbase);

    // compile code to create a closure
    compileLiteral(euxmStackPop(), euxmContNext);
    putCodeByte(OP_CLOSE);

    // store the procedure
    int lev = 0, off = findVariable(name, &lev);
    if (name && off)
    {
        compileEVariable(OP_ESET, lev, off);
    }

    // apply the function
    putCodeByte(OP_CALL);
    putCodeByte(n);

    // target for the continuation
    if (cont != euxmContReturn)
    {
        fixup(nxt);
    }
}

///  compileLetstar - compile the (let* ... ) expression
static void compileLetstar(euxlValue form, int cont)
{
    // make sure there is a binding list
    if (euxmAtom(form) || !euxmListp(euxmCar(form)))
    {
        euxmCompileError("expecting binding list", form);
    }

    // handle the case where there are bindings
    if (euxmConsp(euxmCar(form)))
    {
        int nxt;

        // save a continuation
        if (cont != euxmContReturn)
        {
            putCodeByte(OP_SAVE);
            nxt = putCodeWord(0);
        }

        // build the nested lambda expressions
        CompileLetstarCont(euxmCar(form), euxmCdr(form));

        // target for the continuation
        if (cont != euxmContReturn)
        {
            fixup(nxt);
        }
    }
    else // handle the case where there are no bindings
    {
        compileProgn(euxmCdr(form), cont);
    }
}

///  CompileLetstarCont - helper function for let*
static void CompileLetstarCont(euxlValue blist, euxlValue body)
{
    // euxmStackPush the next initialization expressions
    euxmStackCheckPush(euxcCons(euxmCar(blist), euxmNil));
    int n = pushInitExpressions(euxmStackTop());

    // establish a new environment frame
    int oldcbase = addLevel();

    // handle the case where there are more bindings
    if (euxmConsp(euxmCdr(blist)))
    {
        parseLetVariables(euxmStackTop(), euxmNil);
        CompileLetstarCont(euxmCdr(blist), body);
    }
    else // handle the last binding
    {
        parseLetVariables(euxmStackTop(), body);
        compileProgn(body, euxmContReturn);
    }

    // build the code object
    euxmSetStackTop(makeCodeObject(euxls_letname));

    // restore the previous environment
    removeLevel(oldcbase);

    // compile code to create a closure
    compileLiteral(euxmStackPop(), euxmContNext);
    putCodeByte(OP_CLOSE);

    // apply the function
    putCodeByte(OP_CALL);
    putCodeByte(n);
}

///  compileLetrec - compile the (letrec ... ) expression
static void compileLetrec(euxlValue form, int cont)
{
    // make sure there is a binding list
    if (euxmAtom(form) || !euxmListp(euxmCar(form)))
    {
        euxmCompileError("expecting binding list", form);
    }

    int nxt = 0;

    // save a continuation
    if (cont != euxmContReturn)
    {
        putCodeByte(OP_SAVE);
        nxt = putCodeWord(0);
    }

    // euxmStackPush the initialization expressions
    int n = pushDummyValues(euxmCar(form));

    // establish a new environment frame
    int oldcbase = addLevel();

    // compile the binding list
    parseLetVariables(euxmCar(form), euxmCdr(form));

    // compile instructions to set the bound variables
    setBoundVariables(euxmCar(form));

    // compile the body of the let/letrec
    compileProgn(euxmCdr(form), euxmContReturn);

    // build the code object
    euxmStackCheckPush(makeCodeObject(euxls_letname));

    // restore the previous environment
    removeLevel(oldcbase);

    // compile code to create a closure
    compileLiteral(euxmStackPop(), euxmContNext);
    putCodeByte(OP_CLOSE);

    // apply the function
    putCodeByte(OP_CALL);
    putCodeByte(n);

    // target for the continuation
    if (cont != euxmContReturn)
    {
        fixup(nxt);
    }
}

///  pushDummyValues - euxmStackPush dummy values for a 'letrec' expression
static int pushDummyValues(euxlValue blist)
{
    int n = 0;
    if (euxmConsp(blist))
    {
        putCodeByte(OP_NIL);
        for (; euxmConsp(blist); blist = euxmCdr(blist), ++n)
        {
            putCodeByte(OP_PUSH);
        }
    }
    return (n);
}

///  pushInitExpressions - euxmStackPush init expressions for a 'let' expression
static int pushInitExpressions(euxlValue blist)
{
    int n;
    if (euxmConsp(blist))
    {
        n = pushInitExpressions(euxmCdr(blist));
        if (euxmConsp(euxmCar(blist)) && euxmConsp(euxmCdr(euxmCar(blist))))
        {
            if (euxmCdr(euxmCdr(euxmCar(blist))) != euxmNil)
            {
                //***HGW compileExpr(euxmCar(euxmCdr(euxmCdr
                //(euxmCdr(euxmCar(blist))))), euxmContNext);
                //putCodeByte(OP_PUSH);
                n++;
                compileExpr
                (
                    euxmCar(euxmCdr(euxmCdr(euxmCar(blist)))),
                    euxmContNext
                );
            }
            else
            {
                compileExpr(euxmCar(euxmCdr(euxmCar(blist))), euxmContNext);
            }
        }
        else
        {
            putCodeByte(OP_NIL);
        }
        putCodeByte(OP_PUSH);
        return (n + 1);
    }
    return (0);
}

///  parseLetVariables - parse the binding list
static void parseLetVariables(euxlValue blist, euxlValue body)
{
    // setup the entry code
    putCodeByte(OP_FRAME);
    int frame = putCodeByte(0);

    // initialize the argument name list and slot number
    euxlValue last = euxmNil;
    int slotn = 1;

    // handle each required argument
    euxlValue arg;
    while (euxmConsp(blist) && (arg = euxmCar(blist)) != euxmNil)
    {
        if (euxmKeywordp(arg) || (euxmConsp(arg) && euxmKeywordp(euxmCar(arg))))
        {
            euxmCompileError("trying to bind a keyword in let", arg);
        }

        euxlValue new = NULL;

        // make sure the argument is a symbol
        if (euxmSymbolp(arg))
        {
            new = euxcCons(arg, euxmNil);
        }
        else if (euxmConsp(arg) && euxmSymbolp(euxmCar(arg)))
        {
            if (euxmCdr(euxmCdr(arg)) != euxmNil)
            {
                //xlerror("Multi-binding in let not supported", arg);
                new = euxcCons(euxmCar(arg), euxmNil);
                // add the argument name to the name list
                if (last)
                {
                    euxmSetCdr(last, new);
                }
                else
                {
                    euxmSetElement(euxmCar(euxmCar(info)), 0, new);
                }
                last = new;

                // generate an instruction to move the argument into the frame
                putCodeByte(OP_MVARG);
                putCodeByte(slotn++);

                new = euxcCons(euxmCar(euxmCdr(arg)), euxmNil);
            }
            else
            {
                new = euxcCons(euxmCar(arg), euxmNil);
            }
        }
        else
        {
            euxmCompileError("invalid binding in let", arg);
        }

        // add the argument name to the name list
        if (last)
        {
            euxmSetCdr(last, new);
        }
        else
        {
            euxmSetElement(euxmCar(euxmCar(info)), 0, new);
        }
        last = new;

        // generate an instruction to move the argument into the frame
        putCodeByte(OP_MVARG);
        putCodeByte(slotn++);

        // move the formal argument list pointer ahead
        blist = euxmCdr(blist);
    }
    putCodeByte(OP_ALAST);

    // scan the body for internal definitions
    slotn += findInternalDefinitions(body, last);

    // fixup the frame instruction
    cbuff[cbase + frame] = slotn;
}

///  setBoundVariables - set bound variables in a 'letrec' expression
static void setBoundVariables(euxlValue blist)
{
    for (; euxmConsp(blist); blist = euxmCdr(blist))
    {
        if (euxmConsp(euxmCar(blist)) && euxmConsp(euxmCdr(euxmCar(blist))))
        {
            compileExpr(euxmCar(euxmCdr(euxmCar(blist))), euxmContNext);
            int lev, off = findVariable(euxmCar(euxmCar(blist)), &lev);
            if (off)
            {
                compileEVariable(OP_ESET, lev, off);
            }
            else
            {
                euxmCompileError
                (
                    "compiler error -- can't find",
                    euxmCar(euxmCar(blist))
                );
            }
        }
    }
}

///  makeCodeObject - build a code object
static euxlValue makeCodeObject(euxlValue fun)
{
    // create a code object
    euxlValue code = euxcNewCode
    (
        euxmFirstLiteral + euxcListSize(euxmCar(euxmCdr(info)))
    );
    euxmStackCheckPush(code);
    euxmSetBCode(code, euxcNewString(cptr - cbase));

    // function name
    euxmSetCName(code, fun);

    // lambda list variables
    euxmSetVNames(code, euxmGetElement(euxmCar(euxmCar(info)), 0));

    // copy the literals into the code object
    euxlValue p;
    int i;
    for
    (
        i = euxmFirstLiteral, p = euxmCar(euxmCdr(info));
        euxmConsp(p);
        p = euxmCdr(p), ++i
    )
    {
        euxmSetElement(code, i, euxmCar(p));
    }

    // copy the byte codes
    unsigned char *cp;
    for
    (
        i = cbase, cp = (unsigned char *)euxmGetString(euxmGetBCode(code));
        i < cptr;
    )
    {
        *cp++ = cbuff[i++];
    }

    // return the new code object
    return (euxmStackPop());
}

///  compileCond - compile the (cond ... ) expression
static void compileCond(euxlValue form, int cont)
{
    if (euxmConsp(form))
    {
        int end;
        for (end = 0; euxmConsp(form); form = euxmCdr(form))
        {
            if (euxmAtom(euxmCar(form)))
            {
                euxmCompileError("expecting a cond clause", form);
            }

            compileExpr(euxmCar(euxmCar(form)), euxmContNext);
            putCodeByte(OP_BRF);
            int nxt = putCodeWord(0);

            if (euxmCdr(euxmCar(form)))
            {
                compileProgn(euxmCdr(euxmCar(form)), cont);
            }
            else
            {
                compileContinuation(cont);
            }

            if (cont == euxmContNext)
            {
                putCodeByte(OP_BR);
                end = putCodeWord(end);
            }
            fixup(nxt);
        }
        fixup(end);
    }
    else
    {
        putCodeByte(OP_NIL);
    }

    compileContinuation(cont);
}

///  compileAnd - compile the (and ... ) expression
static void compileAnd(euxlValue form, int cont)
{
    if (euxmConsp(form))
    {
        int end;
        for (end = 0; euxmConsp(form); form = euxmCdr(form))
        {
            if (euxmCdr(form))
            {
                compileExpr(euxmCar(form), euxmContNext);
                putCodeByte(OP_BRF);
                end = putCodeWord(end);
            }
            else
            {
                compileExpr(euxmCar(form), cont);
            }
        }
        fixup(end);
    }
    else
    {
        putCodeByte(OP_T);
    }

    compileContinuation(cont);
}

///  compileOr - compile the (or ... ) expression
static void compileOr(euxlValue form, int cont)
{
    if (euxmConsp(form))
    {
        int end;
        for (end = 0; euxmConsp(form); form = euxmCdr(form))
        {
            if (euxmCdr(form))
            {
                compileExpr(euxmCar(form), euxmContNext);
                putCodeByte(OP_BRT);
                end = putCodeWord(end);
            }
            else
            {
                compileExpr(euxmCar(form), cont);
            }
        }
        fixup(end);
    }
    else
    {
        putCodeByte(OP_NIL);
    }

    compileContinuation(cont);
}

///  compileIf - compile the (if ... ) expression
static void compileIf(euxlValue form, int cont)
{
    // compile the test expression
    if (euxmAtom(form))
    {
        euxmCompileError("expecting test expression", form);
    }
    compileExpr(euxmCar(form), euxmContNext);

    // skip around the 'then' clause if the expression is false
    putCodeByte(OP_BRF);
    int nxt = putCodeWord(0);

    // skip to the 'then' clause
    form = euxmCdr(form);
    if (euxmAtom(form))
    {
        euxmCompileError("expecting then clause", form);
    }

    // compile the 'then' and 'else' clauses
    if (euxmConsp(euxmCdr(form)))
    {
        int end;
        if (cont == euxmContNext)
        {
            compileExpr(euxmCar(form), euxmContNext);
            putCodeByte(OP_BR);
            end = putCodeWord(0);
        }
        else
        {
            compileExpr(euxmCar(form), cont);
            end = -1;
        }
        fixup(nxt);
        compileExpr(euxmCar(euxmCdr(form)), cont);
        nxt = end;
    }
    else // compile just a 'then' clause
    {
        compileExpr(euxmCar(form), cont);
    }

    // handle the end of the statement
    if (nxt >= 0)
    {
        fixup(nxt);
        compileContinuation(cont);
    }
}

///  compileProgn - compile the (progn ... ) expression
static void compileProgn(euxlValue form, int cont)
{
    if (euxmConsp(form))
    {
        for (; euxmConsp(form); form = euxmCdr(form))
        {
            if (euxmConsp(euxmCdr(form)))
            {
                compileExpr(euxmCar(form), euxmContNext);
            }
            else
            {
                compileExpr(euxmCar(form), cont);
            }
        }
    }
    else
    {
        putCodeByte(OP_NIL);
        compileContinuation(cont);
    }
}

///  compileWhile - compile the (while ... ) expression
static void compileWhile(euxlValue form, int cont)
{
    // make sure there is a test expression
    if (euxmAtom(form))
    {
        euxmCompileError("expecting test expression", form);
    }

    // skip around the 'body' to the test expression
    putCodeByte(OP_BR);
    int nxt = putCodeWord(0);

    // compile the loop body
    int loop = cptr - cbase;
    compileProgn(euxmCdr(form), euxmContNext);

    // label for the first iteration
    fixup(nxt);

    // compile the test expression
    nxt = cptr - cbase;
    compileExpr(euxmCar(form), euxmContNext);

    // skip around the 'body' if the expression is false
    putCodeByte(OP_BRT);
    putCodeWord(loop);

    // compile the continuation
    compileContinuation(cont);
}

///  compileAccess - compile the (access var env) expression
static void compileAccess(euxlValue form, int cont)
{
    // get the variable name
    if (euxmAtom(form) || !euxmSymbolp(euxmCar(form)))
    {
        euxmCompileError("expecting symbol", form);
    }

    euxlValue sym = euxmCar(form);

    // compile the environment expression
    form = euxmCdr(form);
    if (euxmAtom(form))
    {
        euxmCompileError("expecting environment expression", form);
    }

    compileExpr(euxmCar(form), euxmContNext);

    // get the variable value
    compileVariable(OP_AREF, sym);
    compileContinuation(cont);
}

///  compileSetaccess - compile the (set! (access var env) value) expression
static void compileSetaccess(euxlValue form, int cont)
{
    // make sure this is an access form
    euxlValue aform = euxmCar(form);
    if
    (
        euxmAtom(aform)
     || euxmCar(aform) != euxcEnterModule("access", euxcRootModule)
    )
    {
        euxmCompileError("expecting an ACCESS form", aform);
    }

    // get the variable name
    aform = euxmCdr(aform);
    if (euxmAtom(aform) || !euxmSymbolp(euxmCar(aform)))
    {
        euxmCompileError("expecting symbol", aform);
    }
    euxlValue sym = euxmCar(aform);

    // compile the environment expression
    aform = euxmCdr(aform);
    if (euxmAtom(aform))
    {
        euxmCompileError("expecting environment expression", aform);
    }
    compileExpr(euxmCar(aform), euxmContNext);
    putCodeByte(OP_PUSH);

    // compile the value expression
    form = euxmCdr(form);
    if (euxmAtom(form))
    {
        euxmCompileError("expecting value expression", form);
    }
    compileExpr(euxmCar(form), euxmContNext);

    // set the variable value
    compileVariable(OP_ASET, sym);
    compileContinuation(cont);
}

///  compileCall - compile a function call
static void compileCall(euxlValue form, int cont)
{
    int nxt = 0;

    // save a continuation
    if (cont != euxmContReturn)
    {
        putCodeByte(OP_SAVE);
        nxt = putCodeWord(0);
    }

    // compile each argument expression
    int n = pushArgs(euxmCdr(form));

    // compile the function itself
    compileExpr(euxmCar(form), euxmContNext);

    // apply the function
    putCodeByte(OP_CALL);
    putCodeByte(n);

    // target for the continuation
    if (cont != euxmContReturn)
    {
        fixup(nxt);
    }
}

///  pushArgs - compile the arguments for a function call
static int pushArgs(euxlValue form)
{
    if (euxmConsp(form))
    {
        int n = pushArgs(euxmCdr(form));
        compileExpr(euxmCar(form), euxmContNext);
        putCodeByte(OP_PUSH);
        return (n + 1);
    }
    return (0);
}

///  compileNary - compile nary operator expressions
static void compileNary(int op, int n, euxlValue form, int cont)
{
    if (n < 0 && (n = (-n)) != euxcListSize(euxmCdr(form)))
    {
        compileCall(form, cont);
    }
    else
    {
        pushNargs(euxmCar(form), euxmCdr(form), n);
        putCodeByte(op);
        compileContinuation(cont);
    }
}

///  pushNargs - compile the arguments for an inline function call
static int pushNargs(euxlValue fun, euxlValue body, int n)
{
    if (euxmConsp(body))
    {
        if (n == 0)
        {
            euxmCompileError("too many arguments", fun);
        }
        if (pushNargs(fun, euxmCdr(body), n - 1))
        {
            putCodeByte(OP_PUSH);
        }
        compileExpr(euxmCar(body), euxmContNext);
        return (euxmTrue);
    }
    if (n)
    {
        euxmCompileError("too few arguments", fun);
    }
    return (euxmFalse);
}

///  compileLiteral - compile a literal
static void compileLiteral(euxlValue lit, int cont)
{
    if (lit == euxmNil)
    {
        putCodeByte(OP_NIL);
    }
    else if (lit == euxs_t)
    {
        putCodeByte(OP_T);
    }
    else
    {
        int index = findLiteral(lit);

        if (index < 256)
        {
            putCodeByte(OP_LIT);
            putCodeByte(index);
        }
        else
        {
            putCodeByte(OP_LITL);
            putCodeWord(index);
        }
    }

    compileContinuation(cont);
}

///  compileIdentifier - compile an identifier
static void compileIdentifier(euxlValue sym, int cont)
{
    int lev, off;
    if (sym == euxs_t)
    {
        putCodeByte(OP_T);
    }
    else if ((off = findVariable(sym, &lev)))
    {
        compileEVariable(OP_EREF, lev, off);
    }
    else
    {
        compileVariable(OP_GREF, sym);
    }

    compileContinuation(cont);
}

///  compileContinuation - compile a continuation
static void compileContinuation(int cont)
{
    switch (cont)
    {
        case euxmContReturn:
            putCodeByte(OP_RETURN);
            break;
        case euxmContNext:
            break;
    }
}

///  addLevel - add a nesting level
static int addLevel()
{
    // establish a new environment frame
    euxmSetCar(info, euxcNewFrame(euxmCar(info), 1));
    euxmSetCdr(info, euxcCons(euxmNil, euxmCdr(info)));

    // setup the base of the code for this function
    int oldcbase = cbase;
    cbase = cptr;

    // return the old code base
    return (oldcbase);
}

///  removeLevel - remove a nesting level
static void removeLevel(int oldcbase)
{
    // restore the previous environment
    euxmSetCar(info, euxmCdr(euxmCar(info)));
    euxmSetCdr(info, euxmCdr(euxmCdr(info)));

    // restore the base and code pointer
    cptr = cbase;
    cbase = oldcbase;
}

///  findVariable - find an environment variable
static int findVariable(euxlValue sym, int *plev)
{
    int lev, off;
    euxlValue e, a;
    for (e = euxmCar(info), lev = 0; euxmEnvp(e); e = euxmCdr(e), ++lev)
    {
        for
        (
            a = euxmGetElement(euxmCar(e), 0), off = 1;
            euxmConsp(a);
            a = euxmCdr(a), ++off
        )
        {
            if (sym == euxmCar(a))
            {
                *plev = lev;
                return off;
            }
        }
    }

    return 0;
}

///  find an environment variable in the current frame
static int findVariableCurrentFrame(euxlValue sym)
{
    euxlValue a = euxmGetElement(euxmCar(euxmCar(info)), 0);

    for (int off = 1; euxmConsp(a); a = euxmCdr(a), ++off)
    {
        if (sym == euxmCar(a))
        {
            return off;
        }
    }

    return 0;
}

///  litequal - test for equality that distinguishes symbols
//    from  different modules
static int litequal(euxlValue a, euxlValue b)
{
    if (a == b)
    {
        return euxmTrue;
    }
    if (euxmSymbolp(a))
    {
        return euxmFalse;
    }

    return euxcEqual(a, b);
}

///  findLiteral - find a literal in the literal frame
static int findLiteral(euxlValue lit)
{
    int o = euxmFirstLiteral;
    euxlValue t, p;

    if ((t = euxmCar(euxmCdr(info))) != euxmNil)
    {
        for (p = euxmNil; euxmConsp(t); p = t, t = euxmCdr(t), ++o)
        {
            if (litequal(lit, euxmCar(t)))
            {
                return (o);
            }
        }
        euxmSetCdr(p, euxcCons(lit, euxmNil));
    }
    else
    {
        euxmSetCar(euxmCdr(info), euxcCons(lit, euxmNil));
    }

    return (o);
}

///  compileVariable - compile a variable reference
static void compileVariable(int op, euxlValue sym)
{
    int index = findLiteral(sym);

    if (index < 256)
    {
        putCodeByte(op);
        putCodeByte(index);
    }
    else
    {
        putCodeByte(op + L_OFF);
        putCodeWord(index);
    }
}

///  compileEVariable - compile an environment variable reference
static void compileEVariable(int op, int lev, int off)
{
    putCodeByte(op);
    putCodeByte(lev);
    putCodeByte(off);
}

///  putCodeByte - put a code byte into data space
static int putCodeByte(int b)
{
    if (cptr >= euxmCodeBufferSize)
    {
        euxcAbort("insufficient code space");
    }
    int adr = (cptr - cbase);
    cbuff[cptr++] = b;
    return (adr);
}

///  putCodeWord - put a code word into data space
static int putCodeWord(int w)
{
    int adr = putCodeByte(w >> 8);
    putCodeByte(w);
    return (adr);
}

///  fixup - fixup a reference chain
static void fixup(int chn)
{
    // store the value into each location in the chain
    int val = cptr - cbase;
    int hval = val >> 8;

    while (chn)
    {
        int nxt = (cbuff[cbase + chn] << 8) | (cbuff[cbase + chn + 1]);
        cbuff[cbase + chn] = hval;
        cbuff[cbase + chn + 1] = val;
        chn = nxt;
    }
}

///  euxcListSize - find the size of a list
int euxcListSize(euxlValue list)
{
    int len;

    for (len = 0; euxmConsp(list); list = euxmCdr(list))
    {
        ++len;
    }

    return (len);
}

///  Check if symbol is in list -- error if from different module
euxlValue euxcFindSym(euxlValue symbol, euxlValue list)
{
    char *name = euxmGetString(euxmGetPName(symbol));
    euxlValue module = euxmGetModule(symbol);

    for (euxlValue sym = list; sym; sym = euxmCdr(sym))
    {
        if (strcmp(name, euxmGetString(euxmGetPName(euxmCar(sym)))) == 0)
        {
            if (euxmGetModule(euxmCar(sym)) == module)
            {
                return euxmCar(sym);
            }
            else
            {
                euxmCompileError
                (
                    "symbol name clash in defmodule or import",
                    euxmCar(sym)
                );
            }
        }
    }

    return euxmNil;
}

///  loadModule - try to load a module
static int loadModule(euxlValue sym)
{
    static int indent = 0;
    static char *spacy = "                    ";

    char name[256];
    if (euxmSymbolp(sym))
    {
        strcpy(name, euxmGetString(euxmGetPName(sym)));
    }
    else
    {
        strcpy(name, euxmGetString(sym));
    }
    strcat(name, ".em");

    euxmStackCheckPush(sym);

    char path[256];
    FILE *fp = euxcPathOpen(name, "EU_MODULE_PATH", module_search_path, path);

    if (fp == NULL)
    {
        euxmStackDrop(1);
        return euxmFalse;   // fail
    }

    euxlValue file = euxcMakeStream(fp, euxmPortFlagInput);
    euxmStackCheckPush(file);

    if (!quiet)
    {
        char buf[256];
        if (strcmp(path, ".") == 0)
        {
            sprintf(buf, "%s<reading %s>\n", euxmSpaces(), name);
        }
        else
        {
            sprintf(buf, "%s<reading %s/%s>\n", euxmSpaces(), path, name);
        }
        euxcPutString(euxmGetValue(euxls_stderr), buf);
    }
    indent += 2;

    euxlValue curmod = euxcCurrentModule;
    euxcCurrentModule = euxcRootModule;       // read the form in root module
    #ifdef EUXM_TRACE_MODULE
    euxcPutString(euxlStdout(), "<2curmod=root>");
    #endif

    euxlValue expr;
    int readit = euxcRead(file, &expr);
    euxmSetFile(file, NULL);
    euxcOSClose(fp);

    if (readit)
    {
        if
        (
            !euxmConsp(expr)
         || !euxmSymbolp(euxmCar(expr))
         || strcmp(euxmGetString(euxmGetPName(euxmCar(expr))), "defmodule")
        )
        {
            euxcCurrentModule = curmod;
            #ifdef EUXM_TRACE_MODULE
            euxcPutString(euxlStdout(), "<3curmod=");
            euxcPrin1(euxcCurrentModule, euxlStdout());
            euxcPutString(euxlStdout(), ">");
            #endif
            euxmCompileError("not a defmodule in file", sym);
        }
        euxmStackCheckPush(expr);
        compileDefmodule(euxmCdr(expr), euxmContNext);
        euxmStackDrop(1);
    }   // else fail
    else
    {
        euxmCompileError("error in reading module file", sym);
    }

    euxcCurrentModule = curmod;
    #ifdef EUXM_TRACE_MODULE
    euxcPutString(euxlStdout(), "<4curmod=");
    euxcPrin1(euxcCurrentModule, euxlStdout());
    euxcPutString(euxlStdout(), ">");
    #endif

    indent -= 2;
    if (!quiet)
    {
        char buf[256];
        sprintf(buf, "%s<read %s>\n", euxmSpaces(), name);
        euxcPutString(euxmGetValue(euxls_stderr), buf);
    }

    euxmStackDrop(2);
    return euxmTrue;
}

///  sym a string or symbol
euxlValue euxcFindOrLoadModule(euxlValue sym)
{
    euxmStackCheckPush(sym);
    euxlValue mod = euxcFindModule(sym);

    if (mod == euxmNil)
    {
        (void)loadModule(sym);
        mod = euxcFindModule(sym);
    }

    euxmStackDrop(1);

    return mod;
}

///  addImportedSymbols
static void addImportedSymbols(euxlValue array, euxlValue syms)
{
    for (; syms; syms = euxmCdr(syms))
    {
        euxlValue sym = euxmCar(syms);
        char *name = euxmGetString(euxmGetPName(sym));
        int hval = euxcHash(name, euxmSymbolTableSize);
        if (euxcFindSym(sym, euxmGetElement(array, hval)) == euxmNil)
        {
            euxlValue symlist = euxcCons(sym, euxmGetElement(array, hval));
            euxmSetElement(array, hval, symlist);
        }
    }
}

///  euxmCheckSymbolList
static void euxmCheckSymbolList(euxlValue symlist)
{
    for (; symlist; symlist = euxmCdr(symlist))
    {
        if (!euxmSymbolp(euxmCar(symlist)))
        {
            euxmCompileError
            (
                "not a symbol in only/except list",
                euxmCar(symlist)
            );
        }
    }
}

///  sameName
static int sameName(euxlValue a, euxlValue b)
{
    return
    (
        strcmp
        (
            euxmGetString(euxmGetPName(a)),
            euxmGetString(euxmGetPName(b))
        ) == 0
    );
}

///  filterAll
static euxlValue filterAll(euxlValue modname, euxlValue sofar)
{
    euxmStackCheckPush(sofar);
    euxlValue module = euxcFindOrLoadModule(modname);
    euxmStackDrop(1);

    if (module == euxmNil)
    {
        euxmCompileError("no such module in defmodule", modname);
    }

    for
    (
        euxlValue syms = euxmGetModuleExports(module);
        syms;
        syms = euxmCdr(syms)
    )
    {
        sofar = euxcCons(euxmCar(syms), sofar);
    }

    return sofar;
}

///  filterOnly
static euxlValue filterOnly
(
    euxlValue symlist,
    euxlValue implist,
    euxlValue sofar
)
{
    euxmCheckSymbolList(symlist);

    euxmStackCheckPush(sofar);
    euxlValue syms = filterImports(implist, euxmNil);
    euxmStackDrop(1);

    euxmStackPush(syms);

    for (; symlist; symlist = euxmCdr(symlist))
    {
        euxlValue sym = euxcMember(euxmCar(symlist), syms, sameName);
        if (sym != euxmNil)
        {
            sofar = euxcCons(euxmCar(sym), sofar);
        }
    }

    euxmStackDrop(1);

    return sofar;
}

///  filterExcept
static euxlValue filterExcept
(
    euxlValue symlist,
    euxlValue implist,
    euxlValue sofar
)
{
    euxmCheckSymbolList(symlist);

    euxmStackCheckPush(sofar);
    euxlValue syms = filterImports(implist, euxmNil);
    euxmStackDrop(1);

    euxmStackPush(syms);

    for (; syms; syms = euxmCdr(syms))
    {
        if (euxcMember(euxmCar(syms), symlist, sameName) == euxmNil)
        {
            sofar = euxcCons(euxmCar(syms), sofar);
        }
    }

    euxmStackDrop(1);

    return sofar;
}

///  euxmCheckRenameList
static void euxmCheckRenameList(euxlValue renamelist)
{
    if (!euxmListp(renamelist))
    {
        euxmCompileError("malformed rename directive in defmodule", renamelist);
    }

    for (euxlValue list = renamelist; list; list = euxmCdr(list))
    {
        euxlValue rename = euxmCar(list);

        if (!euxmConsp(rename) || !euxmConsp(euxmCdr(rename)))
        {
            euxmCompileError("malformed rename pair in defmodule", rename);
        }

        if (!euxmSymbolp(euxmCar(rename)))
        {
            euxmCompileError
            (
                "not a symbol in rename in defmodule",
                euxmCar(rename)
            );
        }

        if (!euxmSymbolp(euxmCar(euxmCdr(rename))))
        {
            euxmCompileError
            (
                "not a symbol in rename in defmodule",
                euxmCar(euxmCdr(rename))
            );
        }
    }

    // Check for repeats (rename ((+ add) (- add)) ...)
    // bad (rename ((+ plus)
    // (+ add)) ...) seems OK
    for (; renamelist; renamelist = euxmCdr(renamelist))
    {
        euxlValue rename = euxmCar(renamelist);
        euxlValue old = euxmCar(rename);
        euxlValue new = euxmCar(euxmCdr(rename));
        for (euxlValue list = euxmCdr(renamelist); list; list = euxmCdr(list))
        {
            rename = euxmCar(list);
            if ((new == euxmCar(euxmCdr(rename))) && (old != euxmCar(rename)))
            {
                euxmCompileError("repeat in rename in defmodule", rename);
            }
        }
    }
}

///  import all symbols, flag some as renamed
static euxlValue filter_rename
(
    euxlValue renamelist,
    euxlValue implist,
    euxlValue sofar
)
{
    euxmStackCheck(3);
    euxmStackPush(sofar);
    euxlValue syms = filterImports(implist, euxmNil);
    euxmStackPush(syms);

    euxmCheckRenameList(renamelist);

    euxlValue except = euxmNil;

    for (; renamelist; renamelist = euxmCdr(renamelist))
    {
        euxlValue rename = euxmCar(renamelist);

        euxlValue oldname = euxmCar(rename);
        euxlValue newname = euxmCar(euxmCdr(rename));

        // renaming to self?
        if (oldname == newname)
        {
            continue;
        }

        euxlValue found = euxcMember(oldname, syms, sameName);
        if (found == euxmNil)
        {
            euxmCompileError
            (
                "no such imported symbol to rename in defmodule",
                oldname
            );
        }

        oldname = euxmCar(found);

        // don't import this one
        except = euxcCons(oldname, except);

        // run down the renaming chain
        while ((rename = euxcGetSyntax(oldname, euxls_rename_flag)) != euxmNil)
        {
            oldname = rename;
        }
        euxmStackPush(except);
        newname = reinternSymbol(newname);
        euxcPutSyntax(newname, oldname, euxls_rename_flag);
        euxmStackDrop(1);
    }

    euxmStackPush(except);

    for (; syms; syms = euxmCdr(syms))
    {
        if (euxcMember(euxmCar(syms), except, sameName) == euxmNil)
        {
            sofar = euxcCons(euxmCar(syms), sofar);
        }
    }

    euxmStackDrop(3);

    return sofar;
}

///  filterImports
static euxlValue filterImports(euxlValue implist, euxlValue sofar)
{
    for (; implist; implist = euxmCdr(implist))
    {
        euxlValue imp = euxmCar(implist);

        if (euxmSymbolp(imp))
        {
            sofar = filterAll(imp, sofar);
        }
        else if
        (
            !euxmConsp(imp)
         || !euxmSymbolp(euxmCar(imp))
         || !euxmConsp(euxmCdr(imp))
         || !euxmListp(euxmCar(euxmCdr(imp)))
        )
        {
            euxmCompileError
            (
                "malformed import directive in defmodule",
                euxmCar(implist)
            );
        }
        else if (euxmCar(imp) == euxls_only)
        {
            sofar = filterOnly
            (
                euxmCar(euxmCdr(imp)),
                euxmCdr(euxmCdr(imp)),
                sofar
            );
        }
        else if (euxmCar(imp) == euxls_except)
        {
            sofar = filterExcept
            (
                euxmCar(euxmCdr(imp)),
                euxmCdr(euxmCdr(imp)),
                sofar
            );
        }
        else if (euxmCar(imp) == euxls_rename)
        {
            sofar = filter_rename
            (
                euxmCar(euxmCdr(imp)),
                euxmCdr(euxmCdr(imp)),
                sofar
            );
        }
        else
        {
            euxmCompileError("bad import directive in defmodule", euxmCar(imp));
        }
    }

    return sofar;
}

///  processImportDirective
static void processImportDirective(euxlValue array, euxlValue implist)
{
    euxlValue symlist = filterImports(implist, euxmNil);
    euxmStackCheckPush(symlist);

    addImportedSymbols(array, symlist);

    euxmStackDrop(1);
}

///  processExportDirective
static void processExportDirective(euxlValue export_syms)
{
    euxlValue exports = euxmGetModuleExports(euxcCurrentModule);

    for (euxlValue syms=export_syms; syms; syms = euxmCdr(syms))
    {
        euxlValue sym = euxmCar(syms);

        if (!euxmSymbolp(sym))
        {
            euxmCompileError("non-symbol in export", sym);
        }
        else
        {
            // Intern the exported symbol into the euxcCurrentModule
            euxlValue mod_sym = euxmInternAndExport
            (
                euxmGetString(euxmGetPName(sym))
            );
            exports = euxcCons(mod_sym, exports);

            #ifdef DEBUG
            printf
            (
                "exporting symbol name %s in module %s as ",
                euxmGetString(euxmGetPName(sym)),
                euxmGetString(euxmGetModuleName(euxmGetModule(mod_sym)))
            );
            euxmCompileErrorPrint(mod_sym);
            #endif
        }
    }

    euxmSetModuleExports(euxcCurrentModule, exports);
}

///  processExposeDirective
static void processExposeDirective(euxlValue explist)
{
    euxlValue syms = filterImports(explist, euxmNil);
    euxmStackCheckPush(syms);

    euxlValue exports = euxmGetModuleExports(euxcCurrentModule);

    for (; syms; syms = euxmCdr(syms))
    {
        euxlValue sym = euxmCar(syms);

        if (!euxmSymbolp(sym))
        {
            euxmCompileError("non-symbol in expose", sym);
        }
        else
        {
            // Intern the exported symbol into the euxcCurrentModule
            euxlValue mod_sym = euxmInternAndExport
            (
                euxmGetString(euxmGetPName(sym))
            );
            exports = euxcCons(mod_sym, exports);

            #ifdef DEBUG
            printf
            (
                "exposing symbol name %s in module %s as ",
                euxmGetString(euxmGetPName(sym)),
                euxmGetString(euxmGetModuleName(euxmGetModule(mod_sym)))
            );
            euxmCompileErrorPrint(mod_sym);
            #endif
        }
    }

    euxmSetModuleExports(euxcCurrentModule, exports);

    euxmStackDrop(1);
}

///  processModuleDirectives
static void processModuleDirectives(euxlValue array, euxlValue directives)
{
    euxmStackCheck(2);
    euxmStackPush(array);
    euxmStackPush(directives);

    while (directives)
    {
        euxlValue directive = euxmCar(directives);
        if (!euxmSymbolp(directive))
        {
            euxmCompileError("malformed directive in defmodule", directive);
        }

        directives = euxmCdr(directives);

        if (!euxmConsp(directives))
        {
            euxmCompileError("missing value for directive", directive);
        }

        euxlValue value = euxmCar(directives);

        // For the moment treat "import" and "syntax" equivalently
        // This supports but does not require the code to correspond to the
        // current EuLisp definition
        if (directive == euxls_import || directive == euxls_syntax)
        {
            processImportDirective(array, value);
        }
        else if (directive == euxls_export)
        {
            processExportDirective(value);
        }
        else if (directive == euxls_expose)
        {
            processExposeDirective(value);
        }
        else
        {
            euxlValue out;
            out = euxmGetValue(euxls_stderr);
            euxcPutString(out, "*** ignoring directive \"");
            euxcPrin1(directive, out);
            euxcPutString(out, "\" in defmodule\n");
        }

        directives = euxmCdr(directives);
    }

    euxmStackDrop(2);
}

///  intern symbol in current module
//    can't use symbol name directly as it might move during a GC
//    in euxmInternAndExport
static euxlValue reinternSymbol(euxlValue sym)
{
    if (euxmKeywordp(sym) || euxmGetModule(sym) == euxcReinternModule)
    {
        return sym;
    }

    char buf[euxmStringMax + 1];
    strcpy(buf, euxmGetString(euxmGetPName(sym)));

    return euxmInternAndExport(buf);
}

///  make all symbols in a vector be in current module
static void reintern_vector_symbols(euxlValue form)
{
    int len = euxmGetSize(form);
    for (int i = 0; i < len; i++)
    {
        euxlValue elt = euxmGetElement(form, i);
        if (euxmSymbolp(elt))
        {
            euxmSetElement(form, i, reinternSymbol(elt));
        }
        else
        {
            reinternModuleSymbols(elt);
        }
    }
}

///  make all symbols in a defmodule body be in current module
//    classes: lists & vectors only
static void reinternModuleSymbols(euxlValue body)
{
    for (; euxmConsp(body); body = euxmCdr(body))
    {
        euxlValue form = euxmCar(body);
        if (euxmSymbolp(form))
        {
            euxmSetCar(body, reinternSymbol(form));
        }
        else if (euxmConsp(form))
        {
            reinternModuleSymbols(form);
        }
        else if (euxmVectorp(form))
        {
            reintern_vector_symbols(form);
        }
        if (euxmSymbolp(euxmCdr(body))) // (a b . c)
        {
            euxmSetCdr(body, reinternSymbol(euxmCdr(body)));
        }
    }

    if (euxmVectorp(body))  // (a b . #(c d))
    {
        reintern_vector_symbols(body);
    }
}

///  euxlReintern
euxlValue euxlReintern()
{
    static char *functionName = "reintern";

    euxlValue body = euxmGetArgList();
    euxmLastArg();

    reinternModuleSymbols(euxmCdr(body)); // (begin@ROOT .... )

    return euxs_t;
}

#if 1
///  euxlReinternSyntax
euxlValue euxlReinternSyntax()
{
    static char *functionName = "reintern-syntax";

    euxlValue sym = euxmGetArgSymbol();
    euxmLastArg();

    return reinternSymbol(sym);
}
#else
euxlValue reinternSyntaxForm(), reinternSyntaxVector();

///  reinternSyntaxForm
euxlValue reinternSyntaxForm(euxlValue form)
{
    if (euxmSymbolp(form))
    {
        return reinternSymbol(form);
    }
    else if (euxmConsp(form))
    {
        return euxcCons(euxmCar(form), reinternSyntaxForm(euxmCdr(form)));
    }
    else if (euxmVectorp(form))
    {
        return reinternSyntaxVector(form);
    }
    else
    {
        return form;
    }
}

///  reinternSyntaxVector
euxlValue reinternSyntaxVector(euxlValue form)
{
    int len = euxmGetSize(form);
    euxlValue new = euxcNewVector(len);
    euxmStackCheckPush(new);
    for (int i = 0; i < len; i++)
    {
        euxmSetElement(new, i, reinternSyntaxForm(euxmGetElement(form, i)));
    }
    euxmStackDrop(1);
    return new;
}

///  euxlReinternSyntax
euxlValue euxlReinternSyntax()
{
    static char *functionName = "reintern-syntax";

    euxlValue form = euxmGetArg();
    euxmLastArg();

    euxmStackCheckPush(form);

    if (euxmSymbolp(form) || euxmConsp(form) || euxmVectorp(form))
    {
        form = reinternSyntaxForm(form);
    }

    euxmStackDrop(1);
    return form;
}
#endif

///  euxlModuleDirectives
euxlValue euxlModuleDirectives()
{
    static char *functionName = "module-directives";

    euxlValue array = euxmGetArgVector();
    euxlValue form = euxmGetArgList();
    euxmLastArg();

    processModuleDirectives(array, form);

    return euxs_t;
}

///  loadDependentModules2 - load those modules that this one depends on
//    implist is a list of module descriptors (i.e., names or filters)
static void loadDependentModules2(euxlValue implist)
{
    for (; implist; implist = euxmCdr(implist))
    {
        euxlValue imp = euxmCar(implist);
        if (euxmSymbolp(imp))
        {
            euxlValue module = euxcFindOrLoadModule(imp);
            if (module == euxmNil)
            {
                euxmCompileError("no such module in defmodule", imp);
            }
        }
        else if
        (
            !euxmConsp(imp)
         || !euxmSymbolp(euxmCar(imp))
         || !euxmConsp(euxmCdr(imp))
         || !euxmListp(euxmCar(euxmCdr(imp)))
        )
        {
            euxmCompileError("malformed import directive in defmodule", imp);
        }
        else if
        (
            euxmCar(imp) == euxls_only
         || euxmCar(imp) == euxls_except
         || euxmCar(imp) == euxls_rename
        )
        {
            loadDependentModules2(euxmCdr(euxmCdr(imp)));
        }
        else
        {
            euxmCompileError("bad import directive in defmodule", euxmCar(imp));
        }
    }
}

///  loadDependentModules
static void loadDependentModules(euxlValue directives)
{
    while (directives)
    {
        euxlValue directive = euxmCar(directives);
        if (!euxmSymbolp(directive))
        {
            euxmCompileError("malformed directive in defmodule", directive);
        }

        directives = euxmCdr(directives);
        if (!euxmConsp(directives))
        {
            euxmCompileError("missing value for directive", directive);
        }

        if (directive == euxls_import || directive == euxls_syntax)
        {
            loadDependentModules2(euxmCar(directives));
        }

        directives = euxmCdr(directives);
    }
}

///  compileDefmodule - compile a defmodule
//     (defmodule foo (import a ..) body ...) ->
//     (load-dependent-modules (import a ..))
//     (set-module foo)
//     (module-directives (import ...) current-module-array)
//     (reintern (begin body ...))
//     ((compile (begin body ...)))     ; compile and run
//     (set-module root)
static void compileDefmodule(euxlValue form, int cont)
{
    if (euxcCurrentModule != euxcRootModule)
    {
        euxmCompileError("only use defmodule in root module", form);
    }

    if (euxmAtom(form))
    {
        euxmCompileError("expecting module name in defmodule", form);
    }

    if (!euxmSymbolp(euxmCar(form)) && !euxmStringp(euxmCar(form)))
    {
        euxmCompileError("expecting module name in defmodule", form);
    }

    // copy as strings can move in GC
    char modname[euxmStringMax];
    if (euxmSymbolp(euxmCar(form)))
    {
        strcpy(modname, euxmGetString(euxmGetPName(euxmCar(form))));
    }
    else
    {
        strcpy(modname, euxmGetString(euxmCar(form)));
    }

    if (euxmAtom(euxmCdr(form)) || !euxmListp(euxmCar(euxmCdr(form))))
    {
        euxmCompileError("expecting module import list in defmodule", form);
    }

    euxmStackCheck(3);
    euxmStackPush(form);
    euxlValue newmod = euxcMakeModule(modname);
    euxmStackPush(newmod);

    euxlValue array = euxmGetModuleSymbols(newmod);

    loadDependentModules(euxmCar(euxmCdr(form)));

    euxlValue expr = euxcCons(euxls_set_module, euxcCons(newmod, euxmNil));
    compileExpr(expr, euxmContNext);

    array = euxmGetModuleSymbols(newmod);
    expr = euxcCons
    (
        euxls_module_directives,
        euxcCons
        (
            array,
            euxcCons
            (
                euxcCons
                (
                    euxls_quote,
                    euxcCons(euxmCar(euxmCdr(form)), euxmNil)
                ),
                euxmNil
            )
        )
    );
    compileExpr(expr, euxmContNext);

    euxcCurrentModule = newmod;
    processFFI(modname, euxmCdr(euxmCdr(form)));
    euxcCurrentModule = euxcRootModule;

    euxlValue body = euxmCdr(euxmCdr(form));
    body = euxcCons(euxls_progn, body);
    body = euxcCons(euxls_quote, euxcCons(body, euxmNil));
    body = euxcCons(body, euxmNil);
    euxmStackPush(body);
    expr = euxcCons(euxls_reintern, body);
    compileExpr(expr, euxmContNext);
    euxmStackDrop(1);

    #ifdef EUXL_NOISY_LOAD
    char buf[128];
    euxlValue euxls_print = NULL;

    if (!quiet)
    {
        euxmStackPush(body);
        sprintf(buf, "<%s...", modname);
        euxls_print = euxcEnterModule("%print", euxcRootModule);
        expr = euxcCons(euxls_print, euxcCons(euxcMakeString(buf), euxmNil));
        compileExpr(expr, euxmContNext);
        euxmStackDrop(1);
    }
    #endif

    body = euxcCons(euxls_compile, body);
    body = euxcCons(body, euxmNil);     // call the compiled expression
    compileExpr(body, euxmContNext);

    expr = euxcCons(euxls_set_module, euxcCons(euxcRootModule, euxmNil));
    compileExpr(expr, euxmContNext);

    euxcModuleList = euxcCons(newmod, euxcModuleList);

    #ifdef EUXL_NOISY_LOAD
    if (!quiet)
    {
        sprintf(buf, "done>\n");
        expr = euxcCons(euxls_print, euxcCons(euxcMakeString(buf), euxmNil));
        compileExpr(expr, euxmContNext);
    }
    #endif

    if (euxmSymbolp(euxmCar(form)))
        compileLiteral(euxmGetPName(euxmCar(form)), cont);
    else
        compileLiteral(euxmCar(form), cont);

    euxcCurrentModule = euxcRootModule;
    #ifdef EUXM_TRACE_MODULE
    euxcPutString(euxlStdout(), "<5curmod=");
    euxcPrin1(euxcCurrentModule, euxlStdout());
    euxcPutString(euxlStdout(), ">");
    #endif

    euxmStackDrop(2);
}

///  compileExport
static void compileExport(euxlValue form, int cont)
{
    euxlValue exports = euxmGetModuleExports(euxcCurrentModule);
    euxmStackCheckPush(form);

    for (euxlValue syms = form; syms; syms = euxmCdr(syms))
    {
        euxlValue sym = euxmCar(syms);
        if (!euxmSymbolp(sym))
        {
            euxmCompileError("non-symbol in export", sym);
        }
        else
        {
            exports = euxcCons(sym, exports);
        }
    }

    euxmSetModuleExports(euxcCurrentModule, exports);

    euxmStackDrop(1);

    putCodeByte(OP_T);
    compileContinuation(cont);
}

///  euxcAppend
euxlValue euxcAppend(euxlValue a, euxlValue b)
{
    if (a == euxmNil)
    {
        return b;
    }

    return euxcCons(euxmCar(a), euxcAppend(euxmCdr(a), b));
}

///  compileExpose
static void compileExpose(euxlValue form, int cont)
{
    euxlValue exports = euxmGetModuleExports(euxcCurrentModule);
    euxmStackCheckPush(form);

    for (euxlValue syms = form; syms; syms = euxmCdr(syms))
    {
        euxlValue sym = euxmCar(syms);

        if (!euxmSymbolp(sym) && !euxmStringp(sym))
        {
            euxmCompileError("bad module name in expose", sym);
        }
        else
        {
            euxlValue mod = euxcFindOrLoadModule(sym);
            if (mod == euxmNil)
            {
                euxmCompileError("no such module in expose", sym);
            }
            euxmStackCheckPush(exports);
            exports = euxcAppend(euxmGetModuleExports(mod), exports);
            euxmStackDrop(1);
        }
    }

    euxmSetModuleExports(euxcCurrentModule, exports);

    putCodeByte(OP_T);
    compileContinuation(cont);

    euxmStackDrop(1);
}

///  compileEnterModule
static void compileEnterModule(euxlValue form, int cont)
{
    if (euxmAtom(form))
    {
        euxcFail("module name expected in enter-module", euxls_syntax_error);
    }

    euxlValue sym = euxmCar(form);
    if (!euxmSymbolp(sym) && !euxmStringp(sym))
    {
        euxmCompileError("bad module name in enter-module", sym);
    }

    euxmStackCheckPush(form);

    euxlValue mod = euxcFindOrLoadModule(sym);
    if (mod == euxmNil)
    {
        euxmCompileError("unknown module in enter-module", sym);
    }

    compileExpr
    (
        euxcCons(euxls_set_module, euxcCons(mod, euxmNil)),
        euxmContNext
    );

    putCodeByte(OP_T);
    cont = euxmContReturn;
    compileContinuation(cont);

    euxmStackDrop(1);
}

///  compileReenterModule
static void compileReenterModule(euxlValue form, int cont)
{
    if (euxmAtom(form))
    {
        euxcFail("module name expected in reenter-module", euxls_syntax_error);
    }

    euxlValue sym = euxmCar(form);
    if (!euxmSymbolp(sym) && !euxmStringp(sym))
        euxmCompileError("bad module name in reenter-module", sym);

    euxmStackCheckPush(form);

    int loaded = loadModule(sym);

    euxlValue mod = euxcFindModule(sym);
    if (mod == euxmNil)
    {
        euxmCompileError("unknown module in reenter-module", sym);
    }

    if (!loaded)
    {
        euxmCompileError("can't find module in reenter-module", sym);
    }

    compileExpr
    (
        euxcCons(euxls_set_module, euxcCons(mod, euxmNil)),
        euxmContNext
    );

    putCodeByte(OP_T);
    cont = euxmContReturn;
    compileContinuation(cont);

    euxmStackDrop(1);
}

///  Import directive for interactive use
static void compileImport(euxlValue form, int cont)
{
    euxmStackCheckPush(form);

    euxlValue mod = euxcFindOrLoadModule(euxmCar(euxmCdr(form)));
    if (mod == euxmNil)
    {
        euxmCompileError("unknown module in import", euxmCar(euxmCdr(form)));
    }

    euxlValue array = euxmGetModuleSymbols(euxcCurrentModule);

    putCodeByte(OP_SAVE);
    int nxt = putCodeWord(0);
    compileExpr(euxmCar(form), euxmContNext);
    putCodeByte(OP_PUSH);
    compileVariable(OP_GREF, euxcEnterModule("set-module", euxcRootModule));
    putCodeByte(OP_CALL);
    putCodeByte(1);
    fixup(nxt);

    form = euxmCdr(form);
    if (euxmStringp(euxmCar(form)))
    {
        euxmSetCar
        (
            form,
            euxcEnterModule(euxmGetString(euxmCar(form)), euxcRootModule)
        );
    }
    form = euxcCons(form, euxmNil);
    form = euxcCons(euxls_import, form);
    euxmStackDrop(1);
    euxmStackPush(form);

    putCodeByte(OP_SAVE);
    nxt = putCodeWord(0);
    compileLiteral(form, euxmContNext);
    putCodeByte(OP_PUSH);
    compileLiteral(array, euxmContNext);
    putCodeByte(OP_PUSH);
    compileVariable(OP_GREF, euxls_module_directives);
    putCodeByte(OP_CALL);
    putCodeByte(2);
    fixup(nxt);

    compileLiteral(euxmGetModuleExports(mod), cont);

    euxmStackDrop(1);
}

///  genargs
static void genargs(euxlValue gf, euxlValue args)
{
    if (!euxmConsp(args))
    {
        // mark if optional args allowed
        euxmSetGenericOpt(gf, args == euxmNil ? euxmNil : euxs_t);
        return;
    }

    genargs(gf, euxmCdr(args));

    putCodeByte(OP_PUSH);

    if (euxmSymbolp(euxmCar(args)))
    {
        compileLiteral(euxlc_object, euxmContNext);
    }
    else if
    (
        euxmConsp(euxmCar(args))
     && euxmSymbolp(euxmCar(euxmCar(args)))
     && euxmConsp(euxmCdr(euxmCar(args)))
     && euxmSymbolp(euxmCar(euxmCdr(euxmCar(args))))
    )
    {
        euxlValue sym = euxmCar(euxmCdr(euxmCar(args)));
        int lev, off = findVariable(sym, &lev);
        if (off)
        {
            compileEVariable(OP_EREF, lev, off);
        }
        else
        {
            compileVariable(OP_GREF, sym);
        }
    }
    else
    {
        euxmCompileError("bad argument for defgeneric", euxmCar(args));
    }

    putCodeByte(OP_CONS);
}

///  get classes of required args for gf
static void compileSet_genargs(euxlValue gf, euxlValue args)
{
    putCodeByte(OP_SAVE);
    int nxt = putCodeWord(0);
    putCodeByte(OP_NIL);
    genargs(gf, args);
    putCodeByte(OP_PUSH);
    compileLiteral(gf, euxmContNext);
    putCodeByte(OP_PUSH);
    compileVariable(OP_GREF, euxls_set_generic_args);
    putCodeByte(OP_CALL);
    putCodeByte(2);
    fixup(nxt);
}

///  compileDefineGeneric
static void compileDefineGeneric(euxlValue form, int cont)
{
    if (euxmAtom(form))
    {
        euxcFail("missing body in defgeneric", euxls_syntax_error);
    }

    euxlValue name = euxmCar(form);
    if (!euxmSymbolp(name))
    {
        euxmCompileError("bad name in defgeneric", name);
    }

    euxlValue args = euxmCar(euxmCdr(form));

    if (!euxmListp(args))
    {
        euxmCompileError("bad arglist in defgeneric", euxmCar(form));
    }

    if (!euxmConsp(args))
    {
        euxmCompileError
        (
            "must have at least one required arg in defgeneric",
            euxmCar(form)
        );
    }

    euxlValue gf = euxcNewGeneric();
    euxmStackCheckPush(gf);

    euxmSetGenericName(gf, name);
    euxmSetGenericArgs(gf, euxmNil);
    euxmSetGenericOpt(gf, euxmNil);
    euxmSetGenericMethods(gf, euxmNil);
    euxmSetGenericCache1(gf, euxmNil);
    euxmSetGenericCache2(gf, euxmNil);

    compileLiteral(gf, euxmContNext);
    int off = findVariableCurrentFrame(name);
    if (off)
    {
        compileEVariable(OP_ESET, 0, off);
    }
    else
    {
        compileVariable(OP_GSET, name);
    }

    compileSet_genargs(gf, args);

    euxmStackDrop(1);
    compileLiteral(name, cont);
}

///  defineMethodArgs
static euxlValue defineMethodArgs(euxlValue arglist)
{
    if (!euxmConsp(arglist))
    {
        return arglist;
    }

    euxlValue args = euxmNil;
    euxlValue tail = euxmNil;

    euxmStackCheck(1);

    for (; euxmConsp(arglist); arglist = euxmCdr(arglist))
    {
        euxlValue arg = euxmCar(arglist);
        if (euxmConsp(arg))
        {
            arg = euxmCar(arg);
        }
        if (!euxmSymbolp(arg))
        {
            euxmCompileError
            (
                "argument must be a symbol in defmethod",
                euxmCar(arglist)
            );
        }
        if (args)
        {
            euxmSetCdr(tail, euxcCons(arg, euxmNil));
            tail = euxmCdr(tail);
        }
        else
        {
            tail = args = euxcCons(arg, euxmNil);
            euxmStackPush(args);
        }
    }

    if (euxmSymbolp(arglist))       // optional args
    {
        euxmSetCdr(tail, arglist);
    }
    else if (arglist != euxmNil)
    {
        euxmCompileError
        (
            "rest argument must be a symbol in defmethod",
            arglist
        );
    }

    euxmStackDrop(1);

    return args;
}

///  the required args classes
static euxlValue defineMethodClasses(euxlValue arglist)
{
    if (!euxmConsp(arglist))
    {
        return euxmNil;
    }

    euxlValue euxls_object_class = euxcEnterModule("<object>", euxcRootModule);
    euxlValue classes = euxmNil;
    euxlValue tail = euxmNil;

    euxmStackCheck(1);

    for (; euxmConsp(arglist); arglist = euxmCdr(arglist))
    {
        euxlValue arg = euxmCar(arglist);
        if (euxmConsp(arg))
        {
            arg = euxmCdr(arg);
            if (!euxmConsp(arg))
            {
                euxmCompileError
                (
                    "malformed argument in defmethod",
                    euxmCar(arglist)
                );
            }
            arg = euxmCar(arg);
            if (!euxmSymbolp(arg))
            {
                euxmCompileError
                (
                    "expecting a class name in defmethod",
                    euxmCar(arglist)
                );
            }
        }
        else if (!euxmSymbolp(arg))
        {
            euxmCompileError
            (
                "malformed argument in defmethod",
                euxmCar(arglist)
            );
        }
        else
        {
            arg = euxls_object_class;
        }

        if (classes)
        {
            euxmSetCdr(tail, euxcCons(arg, euxmNil));
            tail = euxmCdr(tail);
        }
        else
        {
            tail = classes = euxcCons(arg, euxmNil);
            euxmStackPush(classes);
        }
    }

    euxmStackDrop(1);

    return classes;
}

///  euxcCons up the method domain
static int push_method_domain(euxlValue classes)
{
    if (classes == euxmNil)
    {
        return 0;
    }

    int len = push_method_domain(euxmCdr(classes));
    int lev, off = findVariable(euxmCar(classes), &lev);
    if (off)
    {
        compileEVariable(OP_EREF, lev, off);
    }
    else
    {
        compileVariable(OP_GREF, euxmCar(classes));
    }

    putCodeByte(OP_PUSH);

    return len + 1;
}

///  compileDefineMethod
static void compileDefineMethod(euxlValue form, int cont)
{
    if (euxmAtom(form))
    {
        euxcFail("missing body in defmethod", euxls_syntax_error);
    }

    euxlValue name = euxmCar(form);
    if (!(euxmSymbolp(name) || (euxmConsp(name) && euxmSymbolp(euxmCar(name)))))
    {
        euxmCompileError("bad name in defmethod", name);
    }

    euxlValue arglist = euxmCar(euxmCdr(form));

    if (!euxmListp(arglist))
    {
        euxmCompileError("bad arglist in defmethod", euxmCar(form));
    }

    if (!euxmConsp(arglist))
    {
        euxmCompileError
        (
            "must have at least one required arg in defmethod",
            euxmCar(form)
        );
    }

    euxmStackCheckPush(form);
    euxlValue classes = defineMethodClasses(arglist);
    euxmStackCheckPush(classes);
    euxlValue args = defineMethodArgs(arglist);
    euxmStackCheckPush(args);

    euxmStackDrop(1);
    args = euxcCons(euxls_arg_list, args);
    args = euxcCons(euxls_next_methods, args);
    euxmStackCheckPush(args);

    putCodeByte(OP_SAVE);  // add-method
    int nxt1 = putCodeWord(0);

    // optional args?
    euxlValue opts;
    for (opts = args; euxmConsp(opts); opts = euxmCdr(opts));
    if (opts == euxmNil)
    {
        putCodeByte(OP_NIL);
    }
    else
    {
        putCodeByte(OP_T);
    }
    putCodeByte(OP_PUSH);

    putCodeByte(OP_SAVE);  // list
    int nxt2 = putCodeWord(0);

    int len = push_method_domain(classes);

    compileVariable(OP_GREF, euxls_list);
    putCodeByte(OP_CALL);
    putCodeByte(len);
    fixup(nxt2);
    putCodeByte(OP_PUSH);

    euxlValue body = euxmCdr(euxmCdr(form));
    compileFunction(name, args, body); // the method function
    putCodeByte(OP_PUSH);

    compileExpr(name, euxmContNext);      // the generic

    putCodeByte(OP_PUSH);

    euxlValue sym = euxcEnterModule("make-and-add-method", euxcRootModule);
    compileVariable(OP_GREF, sym);

    putCodeByte(OP_CALL);
    putCodeByte(4);        // gf, closure, domain, optargs?
    fixup(nxt1);

    compileLiteral(name, cont);

    euxmStackDrop(3);
    return;

}

///  compileCnm
static void compileCnm(euxlValue form, int cont)
{
    if (form != euxmNil)
    {
        euxmCompileError("extra forms in call-next-method", form);
    }

    int nxt = 0;
    if (cont != euxmContReturn)
    {
        putCodeByte(OP_SAVE);
        nxt = putCodeWord(0);
    }

    int lev, off = findVariable(euxls_arg_list, &lev);
    if (off)   // arg list
    {
        compileEVariable(OP_EREF, lev, off);
    }
    else
    {
        euxcFail("call-next-method outside of a method", euxls_syntax_error);
    }

    putCodeByte(OP_PUSH);

    if ((off = findVariable(euxls_next_methods, &lev)))       // method list
    {
        compileEVariable(OP_EREF, lev, off);
    }
    else
    {
        euxcFail("call-next-method outside of a method", euxls_syntax_error);
    }

    putCodeByte(OP_CNM);

    if (cont != euxmContReturn)
    {
        fixup(nxt);
    }

    compileContinuation(cont);

}

///  compileNextMethodp
static void compileNextMethodp(euxlValue form, int cont)
{
    if (form != euxmNil)
    {
        euxmCompileError("extra forms in next-method?", form);
    }

    int lev, off = findVariable(euxls_next_methods, &lev);
    if (off)       // arg list
    {
        compileEVariable(OP_EREF, lev, off);
    }
    else
    {
        euxcFail("next-method? called outside of a method", euxls_syntax_error);
    }

    putCodeByte(OP_NULL);
    putCodeByte(OP_NULL);

    compileContinuation(cont);

}

///  compileSuperclass
static int compileSuperclass(euxlValue super)
{
    if (super == euxmNil)
    {
        return 0;
    }

    putCodeByte(OP_NIL);
    putCodeByte(OP_PUSH);

    int lev, off = findVariable(super, &lev);
    if (off)
    {
        compileEVariable(OP_EREF, lev, off);
    }
    else
    {
        compileVariable(OP_GREF, super);
    }

    putCodeByte(OP_CONS);
    putCodeByte(OP_PUSH);

    compileLiteral(euxls_superclasses, euxmContNext);
    putCodeByte(OP_PUSH);

    return 2;

}

///  compileAbstractp
static int compileAbstractp(euxlValue classopts)
{
    for (; classopts; classopts = euxmCdr(euxmCdr(classopts)))
    {
        if (euxmCar(classopts) == euxls_abstractp)
        {
            compileLiteral(euxmCar(euxmCdr(classopts)), euxmContNext);
            putCodeByte(OP_PUSH);
            compileLiteral(euxls_abstractp, euxmContNext);
            putCodeByte(OP_PUSH);
            return 2;
        }
    }

    return 0;
}

///  compileSlots
static int compileSlots(euxlValue slots)
{
    if (slots == euxmNil)
    {
        return 0;
    }

    putCodeByte(OP_SAVE);
    int nxt1 = putCodeWord(0);

    slots = euxcReverseList(slots);
    euxmStackCheckPush(slots);

    int nargs1;
    for (nargs1 = 0; slots; slots = euxmCdr(slots), nargs1++)
    {
        euxlValue slot = euxmCar(slots);

        if (slot == euxmNil)
        {
            euxcFail("missing slot name in defclass", euxls_syntax_error);
        }

        int nargs2 = 0;
        putCodeByte(OP_SAVE);
        int nxt2 = putCodeWord(0);

        if (euxmSymbolp(slot))
        {
            compileLiteral(slot, euxmContNext);
            putCodeByte(OP_PUSH);
            nargs2++;
            compileLiteral(euxls_name, euxmContNext);
            putCodeByte(OP_PUSH);
            nargs2++;
        }
        else
        {
            // slot name
            euxlValue slotname = euxmCar(slot);
            compileLiteral(slotname, euxmContNext);
            putCodeByte(OP_PUSH);
            nargs2++;
            compileLiteral(euxls_name, euxmContNext);
            putCodeByte(OP_PUSH);
            nargs2++;

            slot = euxmCdr(slot);

            // slot keyword
            euxlValue key = euxcFindKey(euxls_keyword, slot, euxls_unbound);
            if (key != euxls_unbound)
            {
                compileLiteral(key, euxmContNext);
                putCodeByte(OP_PUSH);
                nargs2++;
                compileLiteral(euxls_keyword, euxmContNext);
                putCodeByte(OP_PUSH);
                nargs2++;
            }

            // slot default
            euxlValue defn = euxcFindKey(euxls_default, slot, euxls_unbound);
            if (defn != euxls_unbound)
            {
                defn = euxcCons(defn, euxmNil);
                compileFunction(slotname, euxmNil, defn);
                putCodeByte(OP_PUSH);
                nargs2++;
                compileLiteral(euxls_default, euxmContNext);
                putCodeByte(OP_PUSH);
                nargs2++;
            }

            // slot requiredp
            euxlValue reqd = euxcFindKey(euxls_requiredp, slot, euxls_unbound);
            if (reqd != euxls_unbound)
            {
                compileLiteral(reqd, euxmContNext);
                putCodeByte(OP_PUSH);
                nargs2++;
                compileLiteral(euxls_requiredp, euxmContNext);
                putCodeByte(OP_PUSH);
                nargs2++;
            }
        }

        compileVariable(OP_GREF, euxls_list);
        putCodeByte(OP_CALL);
        putCodeByte(nargs2);
        fixup(nxt2);
        putCodeByte(OP_PUSH);
    }

    compileVariable(OP_GREF, euxls_list);
    putCodeByte(OP_CALL);
    putCodeByte(nargs1);
    fixup(nxt1);
    putCodeByte(OP_PUSH);
    compileLiteral(euxls_direct_slots, euxmContNext);
    putCodeByte(OP_PUSH);

    euxmStackDrop(1);

    return 2;
}

///  compileKeywords
static int compileKeywords(euxlValue slots, euxlValue classopts)
{
    int nargs = 0;

    euxmStackCheck(1);

    euxlValue keys = euxmNil;

    // slot keywords
    for (; slots; slots = euxmCdr(slots))
    {
        euxlValue slot = euxmCar(slots);
        if (euxmConsp(slot))
        {
            euxlValue val = euxcFindKey
            (
                euxls_keyword,
                euxmCdr(slot),
                euxls_unbound
            );
            if (val != euxls_unbound)
            {
                keys = euxcCons(val, keys);
            }
        }
    }

    // class keywords
    for (; classopts; classopts = euxmCdr(euxmCdr(classopts)))
    {
        euxlValue kwd = euxmCar(classopts);
        if (kwd == euxls_keywords)
        {
            if (!euxmListp(euxmCar(euxmCdr(classopts))))
            {
                euxmCompileError
                (
                    "bad keyword list in defclass",
                    euxmCar(euxmCdr(classopts))
                );
            }
            euxmStackPush(keys);
            keys = euxcAppend(euxmCar(euxmCdr(classopts)), keys);
            euxmStackDrop(1);
        }
        else if
        (
            kwd != euxls_constructor
         && kwd != euxls_predicate
         && kwd != euxls_class
         && kwd != euxls_abstractp
        )
        {
            compileExpr(euxmCar(euxmCdr(classopts)), euxmContNext);
            putCodeByte(OP_PUSH);
            compileLiteral(euxmCar(classopts), euxmContNext);
            putCodeByte(OP_PUSH);
            nargs += 2;
        }
    }

    if (keys != euxmNil)
    {
        compileLiteral(keys, euxmContNext);
        putCodeByte(OP_PUSH);
        compileLiteral(euxls_direct_keywords, euxmContNext);
        putCodeByte(OP_PUSH);
        nargs += 2;
    }

    return nargs;
}

/// mkarg
static euxlValue mkarg(int n)
{
    char buf[128];

    sprintf(buf, "arg%d", n);
    return euxmInternAndExport(buf);
}

///  compileGeneralConstructor
static void compileGeneralConstructor(euxlValue classname, euxlValue name)
{
    if (!euxmSymbolp(name))
    {
        euxmCompileError("bad name for constructor in defclass", name);
    }

    euxlValue args = mkarg(0);
    euxlValue body = euxcCons(args, euxmNil);
    body = euxcCons(classname, body);
    body = euxcCons(euxls_make, body);
    body = euxcCons(euxls_apply, body); // (apply make foo args)
    body = euxcCons(body, euxmNil);
    euxmStackCheckPush(body);
    compileFunction(name, args, body);
    euxmStackDrop(1);

    int lev, off = findVariable(name, &lev);
    if (off)
    {
        compileEVariable(OP_ESET, lev, off);
    }
    else
    {
        compileVariable(OP_GSET, name);
    }
}


///  compileConstructor
static void compileConstructor(euxlValue classname, euxlValue classopts)
{
    for (; classopts; classopts = euxmCdr(euxmCdr(classopts)))
    {
        if (euxmCar(classopts) == euxls_constructor)
        {
            euxlValue name = euxmCar(euxmCdr(classopts));
            euxlValue keys;
            if (euxmConsp(name))
            {
                keys = euxmCdr(name);
                name = euxmCar(name);
            }
            else
            {
                compileGeneralConstructor(classname, name);
                return;
            }

            if (!euxmSymbolp(name))
                euxmCompileError("bad name for constructor in defclass", name);

            euxlValue args = euxmNil;
            euxlValue body = euxmNil;
            euxmStackCheck(3);
            keys = euxcReverseList(keys);
            euxmStackPush(keys);

            for (int count = 0; keys; keys = euxmCdr(keys), count++)
            {
                euxmStackPush(args);
                euxmStackPush(body);
                euxlValue arg = mkarg(count);
                euxmStackDrop(2);
                args = euxcCons(arg, args);
                euxmStackPush(args);
                body = euxcCons(arg, body);
                body = euxcCons(euxmCar(keys), body);
                euxmStackDrop(1);
            }

            euxmStackDrop(1);
            euxmStackPush(args);
            body = euxcCons(classname, body);
            body = euxcCons(euxls_make, body);  // (make cl a: arg1 b: arg2)
            body = euxcCons(body, euxmNil);
            euxmStackPush(body);
            compileFunction(name, args, body);
            euxmStackDrop(2);

            int lev, off = findVariable(name, &lev);
            if (off)
            {
                compileEVariable(OP_ESET, lev, off);
            }
            else
            {
                compileVariable(OP_GSET, name);
            }
        }
    }
}

///  compilePredicate
static void compilePredicate(euxlValue classname, euxlValue classopts)
{
    euxlValue euxls_claseuxls_of =
        euxcEnterModule("class-of", euxcRootModule);
    euxlValue euxls_subeuxmClassp =
        euxcEnterModule("subclass?", euxcRootModule);

    euxmStackCheck(2);

    int previous = euxmFalse;

    for (; classopts; classopts = euxmCdr(euxmCdr(classopts)))
    {

        if (euxmCar(classopts) == euxls_predicate)
        {

            euxlValue name = euxmCar(euxmCdr(classopts));
            if (!euxmSymbolp(name))
            {
                euxmCompileError("bad name for predicate in defclass", name);
            }

            if (!previous)
            {   // make predicate function
                euxlValue args = euxcCons(euxls_object, euxmNil);
                euxmStackPush(args);

                euxlValue body = euxcCons(classname, euxmNil);
                euxmStackPush(body);
                body = euxcCons(euxls_object, euxmNil);
                body = euxcCons(euxls_claseuxls_of, body);
                body = euxcCons(body, euxmStackPop());

                // (subclass? (class-of object) cl)
                body = euxcCons(euxls_subeuxmClassp, body);

                body = euxcCons(body, euxmNil);
                euxmStackPush(body);

                compileFunction(name, args, body);

                euxmStackDrop(2);
                previous = euxmTrue;
            }

            int lev, off = findVariable(name, &lev);
            if (off)
            {
                compileEVariable(OP_ESET, lev, off);
            }
            else
            {
                compileVariable(OP_GSET, name);
            }
        }
    }
}

///  compileNamedReader
static void compileNamedReader
(
    euxlValue readername,
    euxlValue slotname,
    euxlValue classname
)
{
    putCodeByte(OP_SAVE);
    int nxt1 = putCodeWord(0);
    putCodeByte(OP_SAVE);
    int nxt2 = putCodeWord(0);
    putCodeByte(OP_SAVE);
    int nxt3 = putCodeWord(0);
    putCodeByte(OP_SAVE);
    int nxt4 = putCodeWord(0);

    int lev, off = findVariable(classname, &lev);
    if (off)
    {
        compileEVariable(OP_EREF, lev, off);
    }
    else
    {
        compileVariable(OP_GREF, classname);
    }
    putCodeByte(OP_PUSH);
    compileLiteral(slotname, euxmContNext);
    putCodeByte(OP_PUSH);
    // finds index of slot in class
    compileVariable(OP_GREF, euxls_find_slot_index);
    putCodeByte(OP_CALL);
    putCodeByte(2);
    fixup(nxt4);
    putCodeByte(OP_PUSH);
    compileLiteral(euxls_object, euxmContNext);
    putCodeByte(OP_PUSH);
    compileLiteral(euxls_getivar, euxmContNext);
    putCodeByte(OP_PUSH);
    compileVariable(OP_GREF, euxls_list);       // (%euxmGetIVar object indx)
    putCodeByte(OP_CALL);
    putCodeByte(3);
    fixup(nxt3);
    putCodeByte(OP_PUSH);
    #ifndef NO_CHECK_REF
    putCodeByte(OP_SAVE);
    nxt3 = putCodeWord(0);
    compileLiteral(euxls_object, euxmContNext);
    putCodeByte(OP_PUSH);
    if ((off = findVariable(classname, &lev)))
    {
        compileEVariable(OP_EREF, lev, off);
    }
    else
    {
        compileVariable(OP_GREF, classname);
    }
    putCodeByte(OP_PUSH);
    compileLiteral(euxls_check_ref, euxmContNext);
    putCodeByte(OP_PUSH);
    compileVariable(OP_GREF, euxls_list);       // (check-ref class object)
    putCodeByte(OP_CALL);
    putCodeByte(3);
    fixup(nxt3);
    putCodeByte(OP_PUSH);
    #endif
    putCodeByte(OP_NIL);
    putCodeByte(OP_PUSH);
    compileLiteral(euxls_object, euxmContNext);
    putCodeByte(OP_CONS);
    putCodeByte(OP_PUSH);
    compileLiteral(euxls_lambda, euxmContNext);
    putCodeByte(OP_PUSH);
    compileVariable(OP_GREF, euxls_list);       // (lambda (object)
                                        //   (check-ref class object)
    putCodeByte(OP_CALL);                  // (%euxmGetIVar object indx))
    #ifndef NO_CHECK_REF
    putCodeByte(4);
    #else
    putCodeByte(3);
    #endif
    fixup(nxt2);
    putCodeByte(OP_PUSH);
    compileVariable(OP_GREF, euxls_eval_cm);
    putCodeByte(OP_CALL);
    putCodeByte(1);
    fixup(nxt1);
    if ((off = findVariable(readername, &lev)))
    {
        compileEVariable(OP_ESET, lev, off);
    }
    else
    {
        compileVariable(OP_GSET, readername);
    }
}

///  compileNamedWriter
static void compileNamedWriter
(
    euxlValue writername,
    euxlValue slotname,
    euxlValue classname,
    int setterp
)
{
    putCodeByte(OP_SAVE);
    int nxt1 = putCodeWord(0);
    putCodeByte(OP_SAVE);
    int nxt2 = putCodeWord(0);
    putCodeByte(OP_SAVE);
    int nxt3 = putCodeWord(0);
    compileLiteral(euxls_value, euxmContNext);
    putCodeByte(OP_PUSH);
    putCodeByte(OP_SAVE);
    int nxt4 = putCodeWord(0);

    int lev, off = findVariable(classname, &lev);
    if (off)
    {
        compileEVariable(OP_EREF, lev, off);
    }
    else
    {
        compileVariable(OP_GREF, classname);
    }

    putCodeByte(OP_PUSH);
    compileLiteral(slotname, euxmContNext);
    putCodeByte(OP_PUSH);
    // finds index of slot in class
    compileVariable(OP_GREF, euxls_find_slot_index);
    putCodeByte(OP_CALL);
    putCodeByte(2);
    fixup(nxt4);
    putCodeByte(OP_PUSH);
    compileLiteral(euxls_object, euxmContNext);
    putCodeByte(OP_PUSH);
    compileLiteral(euxls_setivar, euxmContNext);
    putCodeByte(OP_PUSH);
    // (%euxmSetIVar object indx val)
    compileVariable(OP_GREF, euxls_list);
    putCodeByte(OP_CALL);
    putCodeByte(4);
    fixup(nxt3);
    putCodeByte(OP_PUSH);
    #ifndef NO_CHECK_REF
    putCodeByte(OP_SAVE);
    nxt3 = putCodeWord(0);
    compileLiteral(euxls_object, euxmContNext);
    putCodeByte(OP_PUSH);

    if ((off = findVariable(classname, &lev)))
    {
        compileEVariable(OP_EREF, lev, off);
    }
    else
    {
        compileVariable(OP_GREF, classname);
    }

    putCodeByte(OP_PUSH);
    compileLiteral(euxls_check_ref, euxmContNext);
    putCodeByte(OP_PUSH);
    compileVariable(OP_GREF, euxls_list);       // (check-ref class object)
    putCodeByte(OP_CALL);
    putCodeByte(3);
    fixup(nxt3);
    putCodeByte(OP_PUSH);
    #endif
    putCodeByte(OP_NIL);
    putCodeByte(OP_PUSH);
    compileLiteral(euxls_value, euxmContNext);
    putCodeByte(OP_CONS);
    putCodeByte(OP_PUSH);
    compileLiteral(euxls_object, euxmContNext);
    putCodeByte(OP_CONS);
    putCodeByte(OP_PUSH);
    compileLiteral(euxls_lambda, euxmContNext);
    putCodeByte(OP_PUSH);
    compileVariable(OP_GREF, euxls_list);       // (lambda (object)
                                        //   (check-ref classobject))
    putCodeByte(OP_CALL);  // (%euxmSetIVar object indx val))
    #ifndef NO_CHECK_REF
    putCodeByte(4);
    #else
    putCodeByte(3);
    #endif
    fixup(nxt2);
    putCodeByte(OP_PUSH);
    compileVariable(OP_GREF, euxls_eval_cm);
    putCodeByte(OP_CALL);
    putCodeByte(1);
    fixup(nxt1);

    if (setterp)
    {
        putCodeByte(OP_SAVE);
        nxt1 = putCodeWord(0);
        putCodeByte(OP_PUSH);

        if ((off = findVariable(writername, &lev)))
        {
            compileEVariable(OP_EREF, lev, off);
        }
        else
        {
            compileVariable(OP_GREF, writername);
        }

        putCodeByte(OP_PUSH);
        putCodeByte(OP_SAVE);
        nxt2 = putCodeWord(0);
        compileVariable(OP_GREF, euxls_setter);
        putCodeByte(OP_PUSH);
        compileVariable(OP_GREF, euxls_setter);
        putCodeByte(OP_CALL);
        putCodeByte(1);
        fixup(nxt2);
        putCodeByte(OP_CALL);
        putCodeByte(2);
        fixup(nxt1);
    }
    else
    {
        if ((off = findVariable(writername, &lev)))
        {
            compileEVariable(OP_ESET, lev, off);
        }
        else
        {
            compileVariable(OP_GSET, writername);
        }
    }

}

///  compileReaders
static void compileReaders(euxlValue classname, euxlValue slots)
{
    for (; slots; slots = euxmCdr(slots))
    {
        euxlValue slot = euxmCar(slots);
        if (euxmConsp(slot))
        {
            for
            (
                euxlValue slotl = euxmCdr(slot);
                slotl;
                slotl = euxmCdr(euxmCdr(slotl))
            )
            {
                if (euxmCdr(slotl) == euxmNil)
                {
                    euxcCerror
                    (
                        "odd-size slot init list",
                        slot,
                        euxls_telos_error
                    );
                }
                if (euxmCar(slotl) == euxls_reader)
                {
                    compileNamedReader
                    (
                        euxmCar(euxmCdr(slotl)),
                        euxmCar(slot),
                        classname
                    );
                }
            }
        }
    }
}

///  compileWriters
static void compileWriters(euxlValue classname, euxlValue slots)
{
    for (; slots; slots = euxmCdr(slots))
    {
        euxlValue slot = euxmCar(slots);
        if (euxmConsp(slot))
        {
            for
            (
                euxlValue slotl = euxmCdr(slot);
                slotl;
                slotl = euxmCdr(euxmCdr(slotl))
            )
            {
                if (euxmCdr(slotl) == euxmNil)
                {
                    euxcCerror
                    (
                        "odd-size slot init list",
                        slot,
                        euxls_telos_error
                    );
                }
                if (euxmCar(slotl) == euxls_writer)
                {
                    compileNamedWriter
                    (
                        euxmCar(euxmCdr(slotl)),
                        euxmCar(slot),
                        classname,
                        euxmFalse
                    );
                }
            }
        }
    }
}

///  compileClassClass
static void compileClassClass(euxlValue classopts)
{
    euxlValue cls = euxmNil;

    for (; classopts; classopts = euxmCdr(euxmCdr(classopts)))
    {
        if (euxmCar(classopts) == euxls_class)
        {
            if (!euxmSymbolp(euxmCar(euxmCdr(classopts))))
            {
                euxmCompileError
                (
                    "bad name for class in defclass",
                    euxmCar(euxmCdr(classopts))
                );
            }
            cls = euxmCar(euxmCdr(classopts));
            break;
        }
    }

    if (cls == euxmNil)
    {
        cls = euxcEnterModule("<simple-class>", euxcRootModule);
    }

    euxmStackCheckPush(cls);
    compileVariable(OP_GREF, cls);
    putCodeByte(OP_PUSH);
    euxmStackDrop(1);
}

///  compileAccessors
static void compileAccessors(euxlValue classname, euxlValue slots)
{
    for (; slots; slots = euxmCdr(slots))
    {
        euxlValue slot = euxmCar(slots);
        if (euxmConsp(slot))
        {
            for
            (
                euxlValue slotl = euxmCdr(slot);
                slotl;
                slotl = euxmCdr(euxmCdr(slotl))
            )
            {
                if (euxmCdr(slotl) == euxmNil)
                {
                    euxcCerror
                    (
                        "odd-size slot init list",
                        slot, euxls_telos_error
                    );
                }
                if (euxmCar(slotl) == euxls_accessor)
                {
                    compileNamedReader
                    (
                        euxmCar(euxmCdr(slotl)),
                        euxmCar(slot),
                        classname
                    );

                    compileNamedWriter
                    (
                        euxmCar(euxmCdr(slotl)),
                        euxmCar(slot),
                        classname,
                        euxmTrue
                    );
                }
            }
        }
    }
}

///  checkSlotOptions
static void checkSlotOptions(euxlValue slots)
{
    for (; slots; slots = euxmCdr(slots))
    {
        euxlValue slot = euxmCar(slots);

        if (euxmSymbolp(slot))
        {
            continue;
        }

        if (!euxmConsp(slot))
        {
            euxmCompileError("bad slot description in defclass", slot);
        }

        if ((euxcListSize(slot) & 1) == 0)
        {
            euxmCompileError("odd-size slot description in defclass", slot);
        }

        for (slot = euxmCdr(slot); slot; slot = euxmCdr(euxmCdr(slot)))
        {
            if (!euxmConsp(slot))
            {
                euxmCompileError("excess form in slot description", slot);
            }

            euxlValue kwd = euxmCar(slot);
            if
            (
                kwd != euxls_keyword
             && kwd != euxls_default
             && kwd != euxls_reader
             && kwd != euxls_writer
             && kwd != euxls_accessor
             && kwd != euxls_requiredp
            )
            {
                euxmCompileError("unknown keyword in slot description", kwd);
            }
        }
    }
}

///  euxmCheckClassOptions
static void euxmCheckClassOptions(euxlValue classopts)
{
    if ((euxcListSize(classopts) & 1) == 1)
    {
        euxmCompileError("odd-size class option list in defclass", classopts);
    }

    for (; classopts; classopts = euxmCdr(euxmCdr(classopts)))
    {
        if (!euxmConsp(classopts))
        {
            euxmCompileError("excess form in defclass", classopts);
        }

        #if 0
        kwd = euxmCar(classopts);
        if (kwd != euxls_keywords &&
        kwd != euxls_constructor &&
        kwd != euxls_predicate && kwd != euxls_class && kwd != euxls_abstractp)
            euxmCompileError("unknown keyword in class options", kwd);
        #endif // done in initialize-object
    }
}

///  compileDefclass
static void compileDefclass(euxlValue form, int cont)
{
    euxmStackCheckPush(form);

    if (euxmAtom(form))
    {
        euxcFail("missing body in defclass", euxls_syntax_error);
    }

    euxlValue name = euxmCar(form);
    if (!euxmSymbolp(name))
    {
        euxmCompileError("bad class name in defclass", form);
    }

    if (euxmAtom(euxmCdr(form)))
    {
        euxmCompileError("missing superclass in defclass", form);
    }

    euxlValue super = euxmCar(euxmCdr(form));
    if (super != euxmNil && !euxmSymbolp(super))
    {
        euxmCompileError("bad superclass in defclass", form);
    }

    if (euxmAtom(euxmCdr(euxmCdr(form))))
    {
        euxmCompileError("missing slot descriptions in defclass", form);
    }

    euxlValue slots = euxmCar(euxmCdr(euxmCdr(form)));
    if (!euxmListp(slots))
    {
        euxmCompileError("bad slot descriptions in defclass", form);
    }

    euxlValue classopts = euxmCdr(euxmCdr(euxmCdr(form)));

    checkSlotOptions(slots);
    euxmCheckClassOptions(classopts);

    putCodeByte(OP_SAVE);
    int nxt = putCodeWord(0);

    int nargs = 0;
    nargs += compileKeywords(slots, classopts);
    nargs += compileSlots(slots);
    nargs += compileSuperclass(super);
    nargs += compileAbstractp(classopts);

    compileLiteral(name, euxmContNext);
    putCodeByte(OP_PUSH);
    nargs++;

    compileLiteral(euxls_name, euxmContNext);
    putCodeByte(OP_PUSH);
    nargs++;

    compileClassClass(classopts);
    nargs++;

    compileVariable(OP_GREF, euxls_make);
    putCodeByte(OP_CALL);
    putCodeByte(nargs);
    fixup(nxt);

    int lev, off = findVariable(name, &lev);
    if (off)
    {
        compileEVariable(OP_ESET, lev, off);
    }
    else
    {
        compileVariable(OP_GSET, name);
    }

    compileConstructor(name, classopts);
    compilePredicate(name, classopts);
    compileReaders(name, slots);
    compileWriters(name, slots);
    compileAccessors(name, slots);

    compileLiteral(name, cont);

    euxmStackDrop(1);
}

///  compileSupercondition
static int compileSupercondition(euxlValue super)
{
    euxlValue euxls_condition =
        euxcEnterModule("<condition>", euxcGetModule("condition"));

    euxlValue supercond = super;
    if (super == euxmNil)
    {
        supercond = euxls_condition;
    }
    else if
    (
        !euxcSubClassp(euxcClassOf(supercond), euxcClassOf(euxls_condition))
    )
    {
        euxmCompileError
        (
            "bad super-condition in defcondition, should be a <condition>",
            supercond
        );
    }

    putCodeByte(OP_NIL);
    putCodeByte(OP_PUSH);

    int lev, off = findVariable(supercond, &lev);
    if (off)
    {
        compileEVariable(OP_EREF, lev, off);
    }
    else
    {
        compileVariable(OP_GREF, supercond);
    }

    putCodeByte(OP_CONS);
    putCodeByte(OP_PUSH);

    compileLiteral(euxls_superclasses, euxmContNext);
    putCodeByte(OP_PUSH);

    return 2;
}

///  compileDefcondition
static void compileDefcondition(euxlValue form, int cont)
{
    euxmStackCheckPush(form);

    if (euxmAtom(form))
    {
        euxcFail("missing body in defcondition", euxls_syntax_error);
    }

    euxlValue name = euxmCar(form);
    if (euxmAtom(euxmCdr(form)))
    {
        euxmCompileError("missing supercondition in defcondition", name);
    }

    euxlValue super = euxmCar(euxmCdr(form));
    if (!euxmSymbolp(super) && (super != euxmNil))
    {
        euxmCompileError("bad supercondition in defcondition", super);
    }


    if (euxmAtom(euxmCdr(euxmCdr(form))))
    {
        euxmCompileError("missing slot descriptions in defclass", form);
    }

    euxlValue slots = euxmCar(euxmCdr(euxmCdr(form)));
    if (!euxmListp(slots))
    {
        euxmCompileError("bad slot descriptions in defclass", form);
    }

    euxlValue classopts = euxmCdr(euxmCdr(euxmCdr(form)));

    checkSlotOptions(slots);
    euxmCheckClassOptions(classopts);

    putCodeByte(OP_SAVE);
    int nxt = putCodeWord(0);

    int nargs = 0;
    nargs += compileKeywords(slots, classopts);
    nargs += compileSlots(slots);
    nargs += compileSupercondition(super);
    nargs += compileAbstractp(classopts);

    compileLiteral(name, euxmContNext);
    putCodeByte(OP_PUSH);
    nargs++;

    compileLiteral(euxls_name, euxmContNext);
    putCodeByte(OP_PUSH);
    nargs++;

    compileClassClass(classopts);
    nargs++;

    compileVariable(OP_GREF, euxls_make);
    putCodeByte(OP_CALL);
    putCodeByte(nargs);
    fixup(nxt);

    int lev, off = findVariable(name, &lev);
    if (off)
    {
        compileEVariable(OP_ESET, lev, off);
    }
    else
    {
        compileVariable(OP_GSET, name);
    }

    compileConstructor(name, classopts);
    compilePredicate(name, classopts);
    compileReaders(name, slots);
    compileWriters(name, slots);
    compileAccessors(name, slots);

    compileLiteral(name, cont);

    euxmStackDrop(1);
}

///  euxlSyntaxError
euxlValue euxlSyntaxError()
{
    static char *functionName = "raise-syntax-error";

    euxlValue msg = euxmGetArgString();
    euxlValue value = euxmGetArg();
    euxmLastArg();
    euxcCerror(euxmGetString(msg), value, euxls_syntax_error);

    return euxmNil; // not reached
}

///  euxcDecodeProcedure - decode the instructions in a code object
void euxcDecodeProcedure(euxlValue fptr, euxlValue fun)
{
    euxlValue code = euxmGetCode(fun);
    euxlValue env = euxmGetCEnv(fun);
    int len = euxmGetStringLength(euxmGetBCode(code));
    for (int lc = 0; lc < len;)
    {
        lc += euxcDecodeInstruction(fptr, code, lc, env);
    }
}

///  euxcDecodeInstruction - decode a single bytecode instruction
int euxcDecodeInstruction(euxlValue fptr, euxlValue code, int lc, euxlValue env)
{
    // get a pointer to the bytecodes for this instruction
    unsigned char *cp = (unsigned char *)euxmGetString(euxmGetBCode(code)) + lc;

    // show the address and opcode
    char buf[100];
    euxlValue tmp;
    if ((tmp = euxmGetCName(code)) != euxmNil)
    {
        sprintf
        (
            buf,
            "%s:%04x %02x ",
            euxmGetString(euxmGetPName(tmp)),
            lc,
            *cp
        );
    }
    else
    {
        sprintf(buf, euxmAddrFmt, (euxmOffType)code);
        euxcPutString(fptr, buf);
        fflush(euxmGetFile(fptr));
        sprintf(buf, ":%04x %02x ", lc, *cp);
    }
    euxcPutString(fptr, buf);
    fflush(euxmGetFile(fptr));

    // Print the operands
    int i, n = 1;
    for (byteCodeFmtDef *op = otab; op->ot_name; ++op)
    {
        if (*cp == op->ot_code)
        {
            switch (op->ot_fmt)
            {
                case FMT_NONE:
                    sprintf(buf, "      %s\n", op->ot_name);
                    euxcPutString(fptr, buf);
                    fflush(euxmGetFile(fptr));
                    break;
                case FMT_BYTE:
                    sprintf(buf, "%02x    %s %02x\n", cp[1], op->ot_name,
                    cp[1]);
                    euxcPutString(fptr, buf);
                    fflush(euxmGetFile(fptr));
                    n += 1;
                    break;
                case FMT_LOFF:
                    sprintf(buf, "%02x    %s %02x ; ", cp[1], op->ot_name,
                    cp[1]);
                    euxcPutString(fptr, buf);
                    fflush(euxmGetFile(fptr));
                    {
                        euxlValue elt, mod;
                        elt = euxmGetElement(code, cp[1]);
                        euxcPrin1(elt, fptr);
                        fflush(euxmGetFile(fptr));
                        if (euxmSymbolp(elt))
                        {
                            mod = euxmGetModule(elt);
                            if (mod != euxmNil)
                            {
                                euxcPutString(fptr, "@");
                                fflush(euxmGetFile(fptr));
                                euxcPutString
                                (
                                    fptr,
                                    euxmGetString(euxmGetModuleName(mod))
                                );
                                fflush(euxmGetFile(fptr));
                            }
                        }
                        euxcTerpri(fptr);
                    }
                    n += 1;
                    break;
                case FMT_WORD:
                    sprintf(buf, "%02x %02x %s %02x%02x\n", cp[1], cp[2],
                    op->ot_name, cp[1], cp[2]);
                    euxcPutString(fptr, buf);
                    fflush(euxmGetFile(fptr));
                    n += 2;
                    break;
                case FMT_EOFF:
                    if ((i = cp[1]) == 0)
                    {
                        tmp = euxmGetVNames(code);
                    }
                    else
                    {
                        for (tmp = env; i > 1; --i)
                            tmp = euxmCdr(tmp);
                        tmp = euxmGetElement(euxmCar(tmp), 0);
                    }
                    for (i = cp[2]; i > 1 && tmp; --i)
                    {
                        tmp = euxmCdr(tmp);
                    }
                    sprintf(buf, "%02x %02x %s %02x %02x ; ", cp[1], cp[2],
                    op->ot_name, cp[1], cp[2]);
                    euxcPutString(fptr, buf);
                    fflush(euxmGetFile(fptr));
                    if (tmp == euxmNil)
                    {
                        sprintf(buf, "??? offset %d not found", cp[2]);
                        euxcPutString(fptr, buf);
                        fflush(euxmGetFile(fptr));
                    }
                    else
                    {
                        euxcPrin1(euxmCar(tmp), fptr);
                    }
                    fflush(euxmGetFile(fptr));
                    euxcTerpri(fptr);
                    n += 2;
                    break;
                case FMT_LOFFL:
                    sprintf(buf, "%02x %02x %s %02x%02x ; ", cp[1], cp[2],
                    op->ot_name, cp[1], cp[2]);
                    euxcPutString(fptr, buf);
                    fflush(euxmGetFile(fptr));
                    {
                        euxlValue elt, mod;
                        elt = euxmGetElement(code, (cp[1] << 8) | cp[2]);
                        euxcPrin1(elt, fptr);
                        fflush(euxmGetFile(fptr));
                        if (euxmSymbolp(elt))
                        {
                            mod = euxmGetModule(elt);
                            if (mod != euxmNil)
                            {
                                euxcPutString(fptr, "@");
                                fflush(euxmGetFile(fptr));
                                euxcPutString
                                (
                                    fptr,
                                    euxmGetString(euxmGetModuleName(mod))
                                );
                                fflush(euxmGetFile(fptr));
                            }
                        }
                        euxcTerpri(fptr);
                    }
                    n += 2;
                    break;
            }
            return (n);
        }
    }

    // Check for an integrable function
    for (byteCodedFunDef *np = byteCodeFunTab; np->name; ++np)
    {
        if (*cp == np->code)
        {
            sprintf(buf, "      %s\n", np->name);
            euxcPutString(fptr, buf);
            fflush(euxmGetFile(fptr));
            return (n);
        }
    }

    // unknown opcode
    sprintf(buf, "      <UNKNOWN>\n");
    euxcPutString(fptr, buf);
    fflush(euxmGetFile(fptr));
    return (n);
}

typedef struct
{
    char *lType;                 // Lisp type
    char *cType;                 // C type
    char *argGetter;             // Argument getter
    char *valueGetter;           // Value getter
    char *makeLType;             // Value getter
} euxcFFTypeMap;

euxcFFTypeMap euxcFFTypeTab[] =
{
    {"<fpi>", "int",
     "euxmGetArgFPI", "euxmGetFPI", "euxcMakeFPI"},

    {"<character>", "char",
     "euxmGetArgChar", "euxmGetCharCode", "euxcMakeChar"},

    {"<double-float>", "double",
     "euxmGetArgDoubleFloat", "euxmGetDoubleFloat", "euxcMakeDoubleFloat"},

    {"<string>", "char*",
     "euxmGetArgString", "euxmGetString", "euxcMakeString"},

    {"boolean", "bool",
     "euxmGetArgBoolean", "euxmGetBoolean", "euxcMakeBoolean"},

    {"ptr", "void*",
     "euxmGetArgPtr", "euxmGetPtr", "euxcMakePtr"},

    // End of table marker
    {0, "unknown",
     "euxmGetArgUnknown", "euxmGetUnknown", "euxcMakeFPI"}
};

static void processFFI(const char* modname, euxlValue forms)
{
    bool usesFFI = false;

    for (euxlValue rest = forms; rest; rest = euxmCdr(rest))
    {
        euxlValue form = euxmCar(rest);

        if (euxmConsp(form) && euxmCar(form) == euxls_defextern)
        {
            usesFFI = true;
        }
    }

    if (usesFFI)
    {
        FILE* fffile = createModuleFFI(modname);

        euxlValue prev = euxmNil;
        for (euxlValue rest = forms; rest; rest = euxmCdr(rest))
        {
            euxlValue form = euxmCar(rest);

            if (euxmConsp(form) && euxmCar(form) == euxls_defextern)
            {
                processDefextern(fffile, euxmCdr(form));

                // Remove the processed defextern from the module form
                euxmSetCdr(prev, euxmCdr(rest));
            }
            else
            {
                prev = rest;
            }
        }

        closeModuleFFI(fffile);

        compileModuleFFI(modname);

        loadModuleFFI(modname);
    }
}

///  compileDefextern - handle the (defextern ... ) form interactively
static void compileDefextern(euxlValue form, int cont)
{
    euxlValue lname = euxmStackPush
    (
        concat
        (
            euxmGetString(euxmGetModuleName(euxcCurrentModule)),
            "_",
            euxmGetString(euxmGetPName(euxmCar(form))),
            NULL
        )
    );
    const char* name = euxmGetString(lname);

    FILE* fffile = createModuleFFI(name);
    processDefextern(fffile, form);
    closeModuleFFI(fffile);
    compileModuleFFI(name);
    loadModuleFFI(name);
    euxmStackDrop(1);

    putCodeByte(OP_NULL);
    putCodeByte(OP_NULL);
    compileContinuation(cont);
}

///  createModuleFFI
static FILE* createModuleFFI(const char* modname)
{
    euxlValue modFFICname = concat("euxl/", modname, "_ffi.c", NULL);
    FILE* fffile = fopen(euxmGetString(modFFICname), "w");

    fprintf
    (
        fffile,
        "#include \"../euxlisp.h\"\n\n"
    );

    return fffile;
}

///  ffMakeReturn
static const char* ffMakeReturn(euxlValue ret)
{
    const char* cret = euxmGetString(euxmGetPName(ret));

    euxcFFTypeMap *ffmapp = euxcFFTypeTab;
    for (; ffmapp->lType != NULL; ++ffmapp)
    {
        if (!strcmp(cret, ffmapp->lType))
        {
            return ffmapp->makeLType;
        }
    }

    return ffmapp->makeLType;
}

///  ffGetArg
static const char* ffGetArg(euxlValue arg)
{
    const char* carg = euxmGetString(euxmGetPName(arg));

    euxcFFTypeMap *ffmapp = euxcFFTypeTab;
    for (; ffmapp->lType != NULL; ++ffmapp)
    {
        if (!strcmp(carg, ffmapp->lType))
        {
            return ffmapp->argGetter;
        }
    }

    return ffmapp->argGetter;
}

///  ffArgValue
static const char* ffArgValue(euxlValue arg)
{
    const char* carg = euxmGetString(euxmGetPName(arg));

    euxcFFTypeMap *ffmapp = euxcFFTypeTab;
    for (; ffmapp->lType != NULL; ++ffmapp)
    {
        if (!strcmp(carg, ffmapp->lType))
        {
            return ffmapp->valueGetter;
        }
    }

    return ffmapp->valueGetter;
}

///  processDefextern - handle the (defextern ... ) form
static FILE* processDefextern(FILE* fffile, euxlValue form)
{
    if (euxmAtom(form))
    {
        euxmCompileError("expecting function name symbol in defextern", form);
    }

    euxlValue funname = euxmCar(form);
    if (!euxmSymbolp(funname) || euxmKeywordp(funname))
    {
        euxmCompileError
        (
            "expecting a function name symbol in defextern",
            funname
        );
    }

    form = euxmCdr(form);
    if (!euxmConsp(form))
    {
        euxmCompileError("expecting argument list in defextern", funname);
    }

    euxlValue args = euxmCar(form);

    form = euxmCdr(form);
    if (!euxmConsp(form))
    {
        euxmCompileError("expecting return type in defextern", funname);
    }

    euxlValue ret = euxmCar(form);

    form = euxmCdr(form);
    euxlValue cname = euxmNil;
    if (euxmConsp(form))
    {
        cname = euxmCar(form);
        if (!euxmStringp(cname))
        {
            euxmCompileError("expecting c-name string in defextern", funname);
        }
    }

    const char *cfunname = euxmGetString(euxmGetPName(funname));
    char stubname[255];
    sprintf(stubname, "ff_stub_%s",  cname ? euxmGetString(cname) : cfunname);

    fprintf
    (
        fffile,
        "static euxlValue %s()\n"
        "{\n"
        "    static char *functionName = \"%s\";\n",
        stubname,
        cfunname
    );

    // Get arguments
    int nargs = 0;
    if (euxmConsp(args))
    {
        fprintf(fffile, "\n");
        euxlValue restArgs = args;
        euxlValue arg = euxmCar(restArgs);
        do
        {
            // Make sure the argument is a symbol
            if (!euxmSymbolp(arg) || euxmKeywordp(arg))
            {
                euxmCompileError
                (
                    "variable in argument list must be a symbol",
                    arg
                );
            }

            fprintf
            (
                fffile,
                "    euxlValue arg%d = %s();\n",
                nargs++,
                ffGetArg(arg)
            );
        } while
            (
                (restArgs = euxmCdr(restArgs))
                && euxmConsp(restArgs)
                && (arg = euxmCar(restArgs)) != euxmNil
            );

        fprintf
        (
            fffile,
            "    euxmLastArg();\n\n"
        );
    }

    fprintf
    (
        fffile,
        "    return %s(%s(",
        ffMakeReturn(ret),
        cname ? euxmGetString(cname) : cfunname
    );

    // Generate argument list
    if (nargs)
    {
        euxlValue restArgs = args;
        euxlValue arg;
        int argi = 0;
        while (euxmConsp(restArgs) && (arg = euxmCar(restArgs)) != euxmNil)
        {
            fprintf
            (
                fffile,
                "%s(arg%d)%s",
                ffArgValue(arg),
                argi,
                ((argi < nargs-1) ? ", " : "")
            );

            argi++;

            restArgs = euxmCdr(restArgs);
        }
    }

    fprintf(fffile, "));\n}\n\n");

    fprintf
    (
        fffile,
        "void __attribute__ ((constructor)) ff_%s_init(void)\n"
        "{\n"
        "    euxcFun(\"%s\", euxmFun, &%s, 10000);\n"
        "}\n",
        stubname,
        cfunname,
        stubname
    );

    return fffile;
}

///  closeModuleFFI
static void closeModuleFFI(FILE* fffile)
{
    fclose(fffile);
}

///  compileModuleFFI
void compileModuleFFI(const char* modname)
{
    // Push the strings onto the stack to GC-protect
    euxlValue modFFICname = euxmStackPush
    (
        concat("euxl/", modname, "_ffi.c", NULL)
    );

    euxlValue modFFILibName = euxmStackPush
    (
        concat("euxl/", modname, "_ffi.so", NULL)
    );

    euxlValue modFFICompileCmd = euxmStackPush
    (
        concat
        (
            "gcc -pipe -m64 -DWORD_LENGTH=64 -std=gnu99 -O3 -fpic ",
            euxmGetString(modFFICname),
            " platforms/x86_64/*.o -shared -o ",
            euxmGetString(modFFILibName),
            NULL
        )
    );

    system(euxmGetString(modFFICompileCmd));

    euxmStackDrop(3);
}

///  loadModuleFFI
void loadModuleFFI(const char* modname)
{
    euxlValue modFFILibName = concat("euxl/", modname, "_ffi.so", NULL);

    void* functionLibPtr =
        dlopen(euxmGetString(modFFILibName), RTLD_LAZY|RTLD_GLOBAL);

    if (!functionLibPtr)
    {
        fputs(dlerror(), stderr);
        fputs("\n", stderr);
    }
}

///-----------------------------------------------------------------------------
