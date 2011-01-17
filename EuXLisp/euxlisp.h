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
/// Title: EuXLisp definitions
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#ifndef EUXLISP_H
#define EUXLISP_H

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <setjmp.h>
#include <string.h>

extern const char* euxlBanner;

///-----------------------------------------------------------------------------
///  Save signal masks in jmp
///-----------------------------------------------------------------------------
#ifdef _POSIX_SOURCE
#define euxmJmpBuf sigjmp_buf
#define euxmLongJmp siglongjmp
#define euxmSetJmp(env) sigsetjmp(env, 1)
#else
#define euxmJmpBuf jmp_buf
#define euxmLongJmp longjmp
#define euxmSetJmp(env) setjmp(env)
#endif

///-----------------------------------------------------------------------------
///  For segmented addresses on Intel processors
///-----------------------------------------------------------------------------
#ifdef SEGADDR
#define euxmInSegment(n,s)                                                     \
    ((unsigned long)(n) >> 16 == (unsigned long)(s) >> 16)
#endif

///-----------------------------------------------------------------------------
///  Size of each type of memory segment
///-----------------------------------------------------------------------------
#ifndef euxmNsSize
#define euxmNsSize  4000    // number of nodes per node segment
#endif
#ifndef euxmVsSize
#define euxmVsSize  10000   // number of euxlValue's per vector segment
#endif

///-----------------------------------------------------------------------------
///  Reasons for GC
///-----------------------------------------------------------------------------
#define euxmGcNode   0
#define euxmGcVector 1
#define euxmGcUser   2
#define euxmGcSave   3

///-----------------------------------------------------------------------------
/// Primitive types, formatting and limits
///-----------------------------------------------------------------------------
// euxmAddrFmt         printf format for addresses ("%x" or "%lx")
// euxmOffType         number the size of an address (int or long)

// euxmFPIType         data type for fixed point integers (long)
// euxmCstringToFPI    fixed point input conversion function (atol)
// euxmFPIFmt          printf format for fixed point integers ("%ld")

// euxmDoubleFloatType data type for double precision numbers (double)
// euxmDoubleFloatFmt  printf format for double precision numbers (%.15g)

// Default important word-size dependent definitions
#if WORD_LENGTH==32 // 32bit OS
#   define euxmAddrFmt         "%x"
#   define euxmOffType         int
#elif WORD_LENGTH==64 // 64bit OS
#   define euxmAddrFmt         "%lx"
#   define euxmOffType         long
#else
#   error WORD_LENGTH set incorrectly
#endif

#define euxmFPIType         long
#define euxmCstringToFPI(n) atol(n)
#define euxmFPIFmt          "%ld"
#define euxmDoubleFloatType       double
#define euxmDoubleFloatFmt        "%#.15g"
#define euxmFPIMin          -1073741824L
#define euxmFPIMax          1073741823L

#ifndef euxmInSegment
#define euxmInSegment(n,s)                                                     \
    (                                                                          \
        (n) >= &(s)->data[0]                                                   \
     && (n) <  &(s)->data[0] + (s)->size                                       \
    )
#endif
#ifndef euxmVcompare
#define euxmVcompare(f,s,t)                                                    \
    ((f) + (s) <= (t))
#endif

///-----------------------------------------------------------------------------
///  Useful definitions
///-----------------------------------------------------------------------------
#ifndef euxmTrue
#define euxmTrue    1
#endif
#ifndef euxmFalse
#define euxmFalse   0
#endif
#ifndef euxmNil
#define euxmNil     (euxlValue)0
#endif

extern FILE *filein;
#define euxmNoChar -2

///-----------------------------------------------------------------------------
///  Program limits
///-----------------------------------------------------------------------------
#define euxmStringMax          100     // maximum length of a string constant
#define euxmSymbolTableSize           199     // symbol euxcHash table size
#define euxmSampleRate          100     // control character sample rate

///-----------------------------------------------------------------------------
/// Stack manipulation macros
///-----------------------------------------------------------------------------
#define euxmStackCheck(n)                                                      \
    {                                                                          \
        if (euxcStackPtr - (n) < euxcStackBase) euxcStackOverflow();           \
    }

#define euxmStackCheckPush(v)                                                  \
    {                                                                          \
        if (euxcStackPtr > euxcStackBase) euxmStackPush(v);                    \
        else euxcStackOverflow();                                              \
    }

#define euxmStackPush(v)                                                       \
    (*--euxcStackPtr = (v))

#define euxmStackPop()                                                         \
    (*euxcStackPtr++)

#define euxmStackTop()                                                         \
    (*euxcStackPtr)

#define euxmSetStackTop(v)                                                     \
    (*euxcStackPtr = (v))

#define euxmStackDrop(n)                                                       \
    (euxcStackPtr += (n))

// Argument list parsing macros
#define euxmGetArg()                                                           \
    (euxmTestArg(euxmNextArg()))

#define euxmLastArg()                                                          \
    {if (euxcArgC != 0) euxcTooMany(functionName);}

#define euxmStackPopRest()                                                     \
    {euxcStackPtr += euxcArgC;}

#define euxmTestArg(e)                                                         \
    (euxmMoreArgs() ? (e) : euxcTooFew(functionName))

#define euxmTypeArg(tp,n)                                                      \
    (tp(*euxcStackPtr) ? euxmNextArg() : euxcBadType(*euxcStackPtr,n,functionName))

#define euxmNextArg()                                                          \
    (--euxcArgC, *euxcStackPtr++)

#define euxmMoreArgs()                                                         \
    (euxcArgC > 0)

///-----------------------------------------------------------------------------
/// Macros to get arguments of a particular type
///-----------------------------------------------------------------------------
#define euxmGetArgCons()      (euxmTestArg(euxmTypeArg(euxmConsp,"<cons>")))
#define euxmGetArgList()      (euxmTestArg(euxmTypeArg(euxmListp,"<list>")))
#define euxmGetArgSymbol()    (euxmTestArg(euxmTypeArg(euxmSymbolp,"<symbol>")))
#define euxmGetArgString()    (euxmTestArg(euxmTypeArg(euxmStringp,"<string>")))
#define euxmGetArgObject()    (euxmTestArg(euxmTypeArg(euxmObjectp,"<object>")))
#define euxmGetArgFPI()       (euxmTestArg(euxmTypeArg(euxmFPIp,"<integer>")))
#define euxmGetArgDoubleFloat() \
    (euxmTestArg(euxmTypeArg(euxmDoubleFloatp,"<float>")))
#define euxmGetArgNumber()    (euxmTestArg(euxmTypeArg(euxmNumberp,"<number>")))
#define euxmGetArgChar()      (euxmTestArg(euxmTypeArg(euxmCharp,"<char>")))
#define euxmGetArgVector()    (euxmTestArg(euxmTypeArg(euxmVectorp,"<vector>")))
#define euxmGetArgStream()    (euxmTestArg(euxmTypeArg(euxmStreamp,"<stream>")))
#define euxmGetArgIstream()                                                    \
    (euxmTestArg(euxmTypeArg(euxmeuxmIStreamp,"<input-stream>")))
#define euxmGetArgOstream()                                                    \
    (euxmTestArg(euxmTypeArg(euxmeuxmOStreamp,"<output-stream>")))
#define euxmGetArgClosure() \
    (euxmTestArg(euxmTypeArg(euxmClosurep,"<closure>")))
#define euxmGetArgEnv()       (euxmTestArg(euxmTypeArg(euxmEnvp,"<env>")))
#define euxmGetArgModule()    (euxmTestArg(euxmTypeArg(euxmModulep,"<module>")))
#define euxmGetArgGeneric() \
    (euxmTestArg(euxmTypeArg(euxmGenericp,"<generic>")))
#define euxmGetArgMethod()    (euxmTestArg(euxmTypeArg(euxmMethodp,"<method>")))
#define euxmGetArgSlot()      (euxmTestArg(euxmTypeArg(euxmSlotp,"<slot>")))
#define euxmGetArgTable()     (euxmTestArg(euxmTypeArg(euxmTablep,"<table>")))

///-----------------------------------------------------------------------------
/// Node types
///-----------------------------------------------------------------------------
#define euxmFree            0
#define euxmCons            1
#define euxmSymbol          2
#define euxmFPI             3
#define euxmDoubleFloat     4
#define euxmString          5
#define euxmObject          6
#define euxmStream          7
#define euxmVector          8
#define euxmClosure         9
#define euxmCode            11
#define euxmFun             12
#define euxmXFun            13
#define euxmXFunCont        14
#define euxmContinuation    15
#define euxmChar            16
#define euxmPromise         17
#define euxmEnv             18
#define euxmModule          19
#define euxmGeneric         20
#define euxmMethod          21
#define euxmSlot            22
#define euxmTable           23

// Number of node types
#define euxmNTypes          24
#define euxmNullType        euxmNTypes
#define euxmKeyword         (euxmNTypes+1)
#define euxmIStream         (euxmNTypes+2)
#define euxmOStream         (euxmNTypes+3)
#define IeuxmOStream        (euxmNTypes+4)
#define euxmExtraTypes      5

// Node flags
#define euxmMark            1
#define euxmLeft            2

///-----------------------------------------------------------------------------
/// Stream flags
///-----------------------------------------------------------------------------
#define euxmPortFlagInput        1
#define euxmPortFlagOutput       2
#define euxmPortFlagBinary       4

///-----------------------------------------------------------------------------
/// New node access macros
///-----------------------------------------------------------------------------
#define euxmNodeType(x)        ((euxmOffType)(x) & 1 ? euxmFPI : (x)->type)

// Macro to determine if a non-nil value is a pointer
#define euxmIsPointer(x)    (((euxmOffType)(x) & 1) == 0)

// Type predicates
#define euxmAtom(x)                                                            \
    ((x) == euxmNil || euxmNodeType(x) != euxmCons)

#define euxmNull(x)                                                            \
    ((x) == euxmNil)

#define euxmListp(x)                                                           \
    ((x) == euxmNil || euxmNodeType(x) == euxmCons)

#define euxmNumberp(x)                                                         \
    ((x) && (euxmNodeType(x) == euxmFPI || euxmNodeType(x) == euxmDoubleFloat))

#define euxmBoundp(x)                                                          \
    (euxmGetValue(x) != euxls_unbound)

#define euxmeuxmIStreamp(x)                                                    \
    (euxmStreamp(x) && (euxmGetPFlags(x) & euxmPortFlagInput) != 0)

#define euxmeuxmOStreamp(x)                                                    \
    (euxmStreamp(x) && (euxmGetPFlags(x) & euxmPortFlagOutput) != 0)

///-----------------------------------------------------------------------------
/// Basic type predicates
///-----------------------------------------------------------------------------
#define euxmConsp(x)        ((x) && euxmNodeType(x) == euxmCons)
#define euxmStringp(x)      ((x) && euxmNodeType(x) == euxmString)
#define euxmSymbolp(x)      ((x) && euxmNodeType(x) == euxmSymbol)
#define euxmStreamp(x)      ((x) && euxmNodeType(x) == euxmStream)
#define euxmObjectp(x)      ((x) && euxmNodeType(x) == euxmObject)
#define euxmFPIp(x)         ((x) && euxmNodeType(x) == euxmFPI)
#define euxmDoubleFloatp(x)       ((x) && euxmNodeType(x) == euxmDoubleFloat)
#define euxmVectorp(x)      ((x) && euxmNodeType(x) == euxmVector)
#define euxmClosurep(x)     ((x) && euxmNodeType(x) == euxmClosure)
#define euxmContinuationp(x) ((x) && euxmNodeType(x) == euxmContinuation)
#define euxmCodep(x)        ((x) && euxmNodeType(x) == euxmCode)
#define euxmFunp(x)        ((x) && euxmNodeType(x) == euxmFun)
#define euxmXFunContp(x)       ((x) && euxmNodeType(x) == euxmXFunCont)
#define euxmXFunp(x)       ((x) && euxmNodeType(x) == euxmXFun)
#define euxmCharp(x)        ((x) && euxmNodeType(x) == euxmChar)
#define euxmPromisep(x)     ((x) && euxmNodeType(x) == euxmPromise)
#define euxmEnvp(x)         ((x) && euxmNodeType(x) == euxmEnv)
#define euxmBooleanp(x)     ((x) == euxmNil || euxmNodeType(x) == BOOLEAN)
#define euxmModulep(x)      ((x) && euxmNodeType(x) == euxmModule)
#define euxmClassp(x)                                                          \
    (                                                                          \
        euxmObjectp(x)                                                         \
     && ((euxmGetClass(x) == euxlc_simple_class)                               \
     || (euxcSubClassp(euxmGetClass(x), euxlc_class)))                         \
    )
#define euxmGenericp(x)     ((x) && euxmNodeType(x) == euxmGeneric)
#define euxmMethodp(x)      ((x) && euxmNodeType(x) == euxmMethod)
#define euxmKeywordp(x)     (euxmSymbolp(x) && (euxmGetModule(x) == euxmNil))
#define euxmSlotp(x)        ((x) && euxmNodeType(x) == euxmSlot)
#define euxmTablep(x)       ((x) && euxmNodeType(x) == euxmTable)

///-----------------------------------------------------------------------------
/// Vector update macro
///-----------------------------------------------------------------------------
// This is necessary because the memory pointed to by the value.vector.data field
// of a vector object can move during a garbage collection.  This macro
// guarantees that evaluation happens in the right order.
#define euxmVupdate(x,i,v)                                                     \
    {                                                                          \
        euxlValue vutmp=(v);                                                   \
        (x)->value.vector.data[i] = vutmp;                                     \
    }

///-----------------------------------------------------------------------------
/// Access macros
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  Cons access macros
///-----------------------------------------------------------------------------
#define euxmCar(x)          ((x)->value.cons.car)
#define euxmCdr(x)          ((x)->value.cons.cdr)
#define euxmSetCar(x,y)     ((x)->value.cons.car = (y))
#define euxmSetCdr(x,y)     ((x)->value.cons.cdr = (y))

///-----------------------------------------------------------------------------
///  Symbol access macros
///-----------------------------------------------------------------------------
#define euxmGetValue(x)      ((x)->value.vector.data[0])
#define euxmSetValue(x,v)    euxmVupdate(x,0,v)
#define euxmGetPName(x)      ((x)->value.vector.data[1])
#define euxmSetPName(x,v)    euxmVupdate(x,1,v)
#define euxmGetThePList(x)   ((x)->value.vector.data[2])
#define euxmSetThePList(x,v) euxmVupdate(x,2,v)
#define euxmGetPList(x)      euxmCar(euxmGetThePList(x))
#define euxmSetPList(x,v)    euxmSetCar(euxmGetThePList(x),v)
#define euxmSymbolEq(a,b)    (euxmGetPName(a) == euxmGetPName(b))
#define euxmGetModule(x)     ((x)->value.vector.data[3])
#define euxmSetModule(x,v)   euxmVupdate(x,3,v)
#define euxmConstantp(x)     ((x)->value.vector.data[4])
#define euxmSetConstant(x,v) euxmVupdate(x,4,v)
#define euxmGetSyntax(x)     ((x)->value.vector.data[5])
#define euxmPutSyntax(x,v)   euxmVupdate(x,5,v)
#define euxmSymbolSize       6

#define euxmSyntaxOpen     '{'
#define euxmSyntaxClose    '}'

///-----------------------------------------------------------------------------
///  Vector access macros
///-----------------------------------------------------------------------------
#define euxmGetSize(x)      ((x)->value.vector.size)
#define euxmGetElement(x,i) ((x)->value.vector.data[(int)(i)])
#define euxmSetElement(x,i,v) euxmVupdate(x,i,v)

///-----------------------------------------------------------------------------
///  Object access macros
///-----------------------------------------------------------------------------
#define euxmGetClass(x)     ((x)->value.vector.data[0])
#define euxmSetClass(x,v)   euxmVupdate(x,0,v)
#define euxmGetIVar(x,i)    ((x)->value.vector.data[(int)(i)])
#define euxmSetIVar(x,i,v)  euxmVupdate(x,i,v)

///-----------------------------------------------------------------------------
///  Promise access macros
///-----------------------------------------------------------------------------
#define euxmGetPProc(x)     ((x)->value.cons.car)
#define euxmSetPProc(x,v)   ((x)->value.cons.car = (v))
#define euxmGetPValue(x)    ((x)->value.cons.cdr)
#define euxmSetPValue(x,v)  ((x)->value.cons.cdr = (v))

///-----------------------------------------------------------------------------
///  Closure access macros
///-----------------------------------------------------------------------------
#define euxmGetCode(x)      ((x)->value.cons.car)
#define euxmGetCEnv(x)      ((x)->value.cons.cdr)

///-----------------------------------------------------------------------------
///  Code access macros
///-----------------------------------------------------------------------------
#define euxmGetBCode(x)     ((x)->value.vector.data[0])
#define euxmSetBCode(x,v)   euxmVupdate(x,0,v)
#define euxmGetCName(x)     ((x)->value.vector.data[1])
#define euxmSetCName(x,v)   euxmVupdate(x,1,v)
#define euxmGetVNames(x)    ((x)->value.vector.data[2])
#define euxmSetVNames(x,v)  euxmVupdate(x,2,v)
#define euxmFirstLiteral    3

///-----------------------------------------------------------------------------
///  FPI/float/character access macros
///-----------------------------------------------------------------------------
#define euxmGetFPI(x)                                                          \
    ((euxmOffType)(x) & 1 ? euxmGetSmallFPI(x) : (x)->value.fpi)
#define euxmGetDoubleFloat(x)     ((x)->value.euxcDoubleFloat)
#define euxmGetCharCode(x)  ((x)->value.charCode)

///-----------------------------------------------------------------------------
///  Small FPI access macros
///-----------------------------------------------------------------------------
#define euxmMakeSmallFPI(x) ((euxlValue)(((euxmOffType)x << 1) | 1))
#define euxmGetSmallFPI(x)  ((euxmFPIType)((euxmOffType)(x) >> 1))

///-----------------------------------------------------------------------------
///  String access macros
///-----------------------------------------------------------------------------
#define euxmGetString(x)        ((x)->value.vector.sdata)
#define euxmGetStringlength(x)  ((x)->value.vector.size)

///-----------------------------------------------------------------------------
///  Istream/Ostream access macros
///-----------------------------------------------------------------------------
#define euxmGetFile(x)          ((x)->value.filePtr.fp)
#define euxmSetFile(x,v)        ((x)->value.filePtr.fp = (v))
#define euxmGetSaveChar(x)      ((x)->value.filePtr.savech)
#define euxmSetSaveChar(x,v)    ((x)->value.filePtr.savech = (v))
#define euxmGetPFlags(x)        ((x)->value.filePtr.pflags)
#define euxmSetPFlags(x,v)      ((x)->value.filePtr.pflags = (v))

///-----------------------------------------------------------------------------
///  Fun access macros
///-----------------------------------------------------------------------------
#define euxmGetFun(x)           ((x)->value.fun.fun)
#define euxmGetXFun(x)          ((x)->value.fun.xfun)
#define euxmGetFunOffset(x)     ((x)->value.fun.offset)
#define euxmGetFunName(v)                                                      \
    (                                                                          \
        v->type == euxmXFun                                                    \
      ? xFunTab[euxmGetFunOffset(v)].name                                      \
      : funTab[euxmGetFunOffset(v)].name                                       \
    )

///-----------------------------------------------------------------------------
///  Module access macros
///-----------------------------------------------------------------------------
#define euxmGetModuleName(x)        ((x)->value.vector.data[0])
#define euxmSetModuleName(x,v)      euxmVupdate(x,0,v)
#define euxmGetModuleSymbols(x)     ((x)->value.vector.data[1])
#define euxmSetModuleSymbols(x,v)   euxmVupdate(x,1,v)
#define euxmGetModuleExports(x)     ((x)->value.vector.data[2])
#define euxmSetModuleExports(x,v)   euxmVupdate(x,2,v)
#define euxmModuleSize              3

///-----------------------------------------------------------------------------
///  Generic access macros
///-----------------------------------------------------------------------------
#define euxmGetGenericName(x)         ((x)->value.vector.data[0])
#define euxmSetGenericName(x,v)       euxmVupdate(x,0,v)
#define euxmGetGenericArgs(x)         ((x)->value.vector.data[1])
#define euxmSetGenericArgs(x,v)       euxmVupdate(x,1,v)
#define euxmGetGenericOpt(x)          ((x)->value.vector.data[2])
#define euxmSetGenericOpt(x,v)        euxmVupdate(x,2,v)
#define euxmGetGenericMethods(x)      ((x)->value.vector.data[3])
#define euxmSetGenericMethods(x,v)    euxmVupdate(x,3,v)
#define euxmGetGenericCache1(x)       ((x)->value.vector.data[4])
#define euxmSetGenericCache1(x,v)     euxmVupdate(x,4,v)
#define euxmGetGenericCache2(x)       ((x)->value.vector.data[5])
#define euxmSetGenericCache2(x,v)     euxmVupdate(x,5,v)
#define euxmGenericSize               6

///-----------------------------------------------------------------------------
///  Method access macros
///-----------------------------------------------------------------------------
#define euxmGetMethodGenericFun(x)    ((x)->value.vector.data[0])
#define euxmSetMethodGenericFun(x,v)  euxmVupdate(x,0,v)
#define euxmGetMethodFun(x)           ((x)->value.vector.data[1])
#define euxmSetMethodFun(x,v)         euxmVupdate(x,1,v)
#define euxmGetMethodDomain(x)        ((x)->value.vector.data[2])
#define euxmSetMethodDomain(x,v)      euxmVupdate(x,2,v)
#define euxmGetMethodOpt(x)           ((x)->value.vector.data[3])
#define euxmSetMethodOpt(x,v)         euxmVupdate(x,3,v)
#define euxmMethodSize 4

///-----------------------------------------------------------------------------
///  Slot access macros
///-----------------------------------------------------------------------------
#define euxmGetSlotName(x)            ((x)->value.vector.data[0])
#define euxmSetSlotName(x,v)          euxmVupdate(x,0,v)
#define euxmGetSlotKey(x)             ((x)->value.vector.data[1])
#define euxmSetSlotKey(x,v)           euxmVupdate(x,1,v)
#define euxmGetSlotDefault(x)         ((x)->value.vector.data[2])
#define euxmSetSlotDefault(x,v)       euxmVupdate(x,2,v)
#define euxmGetSlotRequiredp(x)       ((x)->value.vector.data[3])
#define euxmSetSlotRequiredp(x,v)     euxmVupdate(x,3,v)
#define euxmSlotSIZE 4

///-----------------------------------------------------------------------------
///  Table access macros
///-----------------------------------------------------------------------------
#define euxmGetTableComp(x)           ((x)->value.vector.data[0])
#define euxmSetTablecomp(x,v)         euxmVupdate(x,0,v)
#define euxmGetTableTable(x)          ((x)->value.vector.data[1])
#define euxmSetTableTable(x,v)        euxmVupdate(x,1,v)
#define euxmGetTableFill(x)           ((x)->value.vector.data[2])
#define euxmSetTableFill(x,v)         euxmVupdate(x,2,v)
#define euxmTableSIZE 3

// Size of a table
#define euxmHashTableSize 31

///-----------------------------------------------------------------------------
/// Node structure
///-----------------------------------------------------------------------------
typedef struct euxcNode euxcNode, *euxlValue;
typedef euxlValue (*euxcFunType)(void);
typedef void (*euxcXFunType)(void);

struct euxcNode
{
    char type;          // Type of node
    char flags;         // Flag bits

    union euxcNodeValue // Value
    {
        // FPI value
        euxmFPIType fpi;

        // Float value
        euxmDoubleFloatType euxcDoubleFloat;

        // Character code
        int charCode;

        // List (cons cell)
        struct
        {
            euxlValue car;      // The car pointer
            euxlValue cdr;      // The cdr pointer
        } cons;

        // File pointer
        struct
        {
            FILE *fp;           // The file pointer
            short savech;       // Lookahead character for input files
            short pflags;       // Stream flags
        } filePtr;

        // Vector
        struct
        {
            euxmFPIType size;   // Vector size
            union
            {
                euxlValue *data;    // Vector data
                char *sdata;        // c-string data
            };
        } vector;

        // Functions
        struct
        {
            union
            {
                euxcFunType fun;    // Function pointer
                euxcXFunType xfun;  // X-function pointer
            };
            int offset;         // Offset into funtab
        } fun;
    } value;
};

///-----------------------------------------------------------------------------
/// Memory allocator definitions
///-----------------------------------------------------------------------------

// Macros to compute the size of a segment
#define euxmNSegSize(n) (sizeof(euxcNodeSegment)+((n)-1)*sizeof(euxcNode))
#define euxmVSegSize(n) (sizeof(euxcVectorSegment)+((n)-1)*sizeof(euxlValue))

// Macro to convert a byte size to a word size
#define euxmByteToWordSize(n)                                                  \
    (((n) + sizeof(euxlValue) - 1)/sizeof(euxlValue))

// Node segment structure
typedef struct euxcNodeSegment euxcNodeSegment;
struct euxcNodeSegment
{
    euxcNodeSegment *next;      // next node segment
    unsigned int size;          // number of nodes in this segment
    euxcNode data[1];           // segment data
};

// Vector segment structure
typedef struct euxcVectorSegment euxcVectorSegment;
struct euxcVectorSegment
{
    euxcVectorSegment *next;    // next vector segment
    euxlValue *free;            // next free location in this segment
    euxlValue *top;             // top of segment (plus one)
    euxlValue data[1];          // segment data
};

// Eval/apply built-in function definition structure
typedef struct
{
    char *name;                  // function name
    euxcXFunType fun;            // function pointer
} euxcXFunDef;

// Built-in unction definition structure
typedef struct
{
    char *name;                  // function name
    euxcFunType fun;             // function pointer
} euxcFunDef;

// External variables
extern euxlValue *euxcStackBase;    // base of value stack
extern euxlValue *euxcStackTop;     // euxmStackTop of value stack
extern euxlValue *euxcStackPtr;     // value stack pointer
extern int euxcArgC;                // argument count for current call
extern euxlValue euxcCurrentModule; // current module
extern euxlValue euxcRootModule;    // the root module
extern euxlValue euxcReinternModule;// module for reinterning symbols
extern euxlValue euxcModuleList;    // all the modules
extern euxlValue euxcKeywordArray;  // all the keywords
extern euxlValue euxcObArray;       // prototype symbols

#define euxmInternAndExport(name)                                                        \
    euxcEnterModule(name, euxcCurrentModule)

///-----------------------------------------------------------------------------
/// Virtual machine registers
///-----------------------------------------------------------------------------
extern euxlValue euxcCurFun;        // current function
extern euxlValue euxcCurEnv;        // current environment
extern euxlValue euxcCurVal;        // value of most recent instruction

///-----------------------------------------------------------------------------
/// External variables
///-----------------------------------------------------------------------------
extern euxmJmpBuf euxmStackTopLevel;
extern euxcXFunDef xFunTab[];
extern euxcXFunDef xContFunTab[];
extern euxcFunDef funTab[];

extern int reading, ctrl_c, quiet, trace;

// Special form table
typedef struct
{
    char *name;
    void (*fun) ();
} specialFormDef;

extern specialFormDef specialFormTab[];

extern int printBreadth, printDepth;
extern FILE *tfp;
extern euxmFPIType nnodes, nfree, gccalls, total;
extern int nscount, vscount;
extern euxmJmpBuf bc_dispatch;
extern char * const *clargv;
extern int clargc;

extern euxcNodeSegment *nsegments;     // list of node segments
extern euxcVectorSegment *vsegments;   // list of vector segments
extern euxlValue *vfree;               // next free location in vector space
extern euxlValue *euxmVecStackTop;     // stack top of vector space

///-----------------------------------------------------------------------------
/// External symbol declarations
///-----------------------------------------------------------------------------
#include "euxlSymbols.h"

///-----------------------------------------------------------------------------
/// External TELOS declarations
///-----------------------------------------------------------------------------
#include "euxlTelos.h"

///-----------------------------------------------------------------------------
/// External function declarations
///-----------------------------------------------------------------------------
#include "euxlProto.h"

///-----------------------------------------------------------------------------
#endif // EUXLISP_H
///-----------------------------------------------------------------------------
