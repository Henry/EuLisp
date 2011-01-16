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
/// Title: Built-in function table
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"

///-----------------------------------------------------------------------------
/// Built-in functions
///-----------------------------------------------------------------------------

///  Functions that call eval or apply
euxcXFunDef xFunTab[] =
{
    {"apply", euxlApply},
    {"call-with-current-continuation", euxlCallCC},
    {"call/cc", euxlCallCC},
    {"%map-list", euxlMapList},
    {"%do-list", euxlDoList},
    {"call-with-input-file", euxlCallWithInput},
    {"call-with-output-file", euxlCallWithOutput},
    {"load", euxlLoad},
    {"load-noisily", euxlLoadNoisily},
    {"force", euxlForce},
    {"initialize-object", euxlInitializeObject},
    {"values", euxlValues},

    {0, 0}      // end of table marker
};

///  Continuations for xFuns
euxcXFunDef xContFunTab[] =
{
    {"%map-list-cont", euxlMapListCont},
    {"%do-list-cont", euxlDoListCont},
    {"%with-file-cont", euxlWithFileCont},
    {"%load-cont", euxlLoadCont},
    {"%force-cont", euxlForceCont},
    {"%initloop-cont", euxlInitLoopCont},

    {0, 0}      // end of table marker
};

///  Other built-in functions
euxcFunDef funTab[] =
{
    // list functions
    {"cons", euxlCons},
    {"car", euxlCar},
    {"cdr", euxlCdr},
    {"caar", euxlCaar},
    {"cadr", euxlCadr},
    {"cdar", euxlCdar},
    {"cddr", euxlCddr},
    {"caaar", euxlCaaar},
    {"caadr", euxlCaadr},
    {"cadar", euxlCadar},
    {"caddr", euxlCaddr},
    {"cdaar", euxlCdaar},
    {"cdadr", euxlCdadr},
    {"cddar", euxlCddar},
    {"cdddr", euxlCdddr},
    {"caaaar", euxlCaaaar},
    {"caaadr", euxlCaaadr},
    {"caadar", euxlCaadar},
    {"caaddr", euxlCaaddr},
    {"cadaar", euxlCadaar},
    {"cadadr", euxlCadadr},
    {"caddar", euxlCaddar},
    {"cadddr", euxlCadddr},
    {"cdaaar", euxlCdaaar},
    {"cdaadr", euxlCdaadr},
    {"cdadar", euxlCdadar},
    {"cdaddr", euxlCdaddr},
    {"cddaar", euxlCddaar},
    {"cddadr", euxlCddadr},
    {"cdddar", euxlCdddar},
    {"cddddr", euxlCddddr},
    {"list", euxlList},
    {"list*", euxlListStar},
    {"append", euxlAppend},
    {"reverse-list", euxlReverseList},
    {"last-pair", euxlLastPair},
    {"list-size", euxlSize},
    {"member-list", euxlMember},
    {"memv", euxlMemv},
    {"memq", euxlMemq},
    {"%assoc", euxlAssoc},
    {"assv", euxlAssv},
    {"assq", euxlAssq},
    {"list-ref", euxlListRef},
    {"list-tail", euxlListTail},

    // destructive list functions
    {"set-car", euxlSetCar},
    {"set-cdr", euxlSetCdr},

    // symbol functions
    {"symbol-exists?", euxlBoundp},
    {"symbol-value", euxlSymValue},
    {"set-symbol-value", euxlSetSymValue},
    {"symbol-plist", euxlSymPlist},
    {"set-symbol-plist", euxlSetSymPlist},
    {"gensym", euxlGensym},
    {"get", euxlGet},
    {"put", euxlPut},
    {"get-syntax", euxlGetSyntax},
    {"put-syntax", euxlPutSyntax},

    // environment functions
    {"the-environment", euxlTheEnvironment},
    {"procedure-environment", euxlProcEnvironment},
    {"environment?", euxlEnvp},
    {"environment-bindings", euxlEnvBindings},
    {"environment-parent", euxlEnvParent},

    // vector functions
    {"vector", euxlVector},
    {"make-vector", euxlMakeVector},
    {"vector-size", euxlVectorSize},
    {"vector-ref", euxlVectorRef},
    {"vector-set", euxlVectorSet},

    // conversion functions
    {"symbol->string", euxlSymbolToString},
    {"string->symbol", euxlStringToSymbol},
    {"vector->list", euxlVectorToList},
    {"list->vector", euxlListToVector},
    {"string->list", euxlStringToList},
    {"list->string", euxlListToString},
    {"char->integer", euxlCharToInt},
    {"integer->char", euxlTntToChar},
    {"string->number", euxlStringToNum},
    {"number->string", euxlNumToString},

    // predicate functions
    {"null?", euxlNullp},
    {"atom?", euxlAtomp},
    {"list?", euxlListp},
    {"number?", euxlNumberp},
    {"boolean?", euxlBooleanp},
    {"cons?", euxlConsp},
    {"symbol?", euxlSymbolp},
    {"keyword?", euxlKeywordp},
    {"complex?", euxlDoubleFloatp},       // (1)
    {"float?", euxlDoubleFloatp},
    {"double-float?", euxlDoubleFloatp},
    {"rational?", euxlIntegerp},    // (1)
    {"integer?", euxlIntegerp},
    {"char?", euxlCharp},
    {"string?", euxlStringp},
    {"vector?", euxlVectorp},
    {"function?", euxlFunctionp},
    {"stream?", euxlStreamp},
    {"input-stream?", euxlInputStreamp},
    {"output-stream?", euxlOutputStreamp},
    {"object?", euxlObjectp},
    {"eof-object?", euxlEOFObjectp},
    {"default-object?", euxlDefaultObjectp},
    {"eq", euxlEq},
    {"eql", euxlEqv},
    {"%equal", euxlEqual},

    // arithmetic functions
    {"%zero?", euxlZerop},
    {"positive?", euxlPositivep},
    {"negative?", euxlNegativep},
    {"odd?", euxlOddp},
    {"even?", euxlEvenp},
    {"exact?", euxlExactp},
    {"inexact?", euxlInexactp},
    {"truncate", euxlTruncate},
    {"floor", euxlFloor},
    {"ceiling", euxlCeiling},
    {"round", euxlRound},
    {"add1", euxlAdd1},
    {"sub1", euxlSub1},
    {"%abs", euxlAbs},
    {"%gcd", euxlGcd},
    {"random", euxlRandom},
    {"%+", euxlAdd},
    {"%-", euxlSub},
    {"%*", euxlMul},
    {"%/", euxlDiv},
    {"%quotient", euxlQuo},
    {"%remainder", euxlRem},
    {"%min", euxlMin},
    {"%max", euxlMax},
    {"sin", euxlSin},
    {"cos", euxlCos},
    {"tan", euxlTan},
    {"asin", euxlAsin},
    {"acos", euxlAcos},
    {"atan", euxlAtan},
    {"exp", euxlXexp},
    {"sqrt", euxlSqrt},
    {"expt", euxlExpt},
    {"log", euxlXlog},

    // bitwise logical functions
    {"logand", euxlLogand},
    {"logior", euxlLogior},
    {"logxor", euxlLogxor},
    {"lognot", euxlLognot},

    // numeric comparison functions
    {"%<", euxlLt},
    {"%<=", euxlLtEq},
    {"%=", euxlEql},
    {"%>=", euxlGtEq},
    {"%>", euxlGt},

    // string functions
    {"string-size", euxlStringLength},
    {"string-null?", euxlStringNullp},
    {"string-append", euxlStringAppend},
    {"string-ref", euxlStringRef},
    {"string-set", euxlStringSet},
    {"substring", euxlSubString},
    {"string<?", euxlStringLt},
    {"string<=?", euxlStringLtEq},
    {"string=?", euxlStringEql},
    {"string>=?", euxlStringGtEq},
    {"string>?", euxlStringGt},
    {"string-ci<?", euxlstringCaInLt},
    {"string-ci<=?", euxlstringCaInLtEq},
    {"string-ci=?", euxlstringCaInEql},
    {"string-ci>=?", euxlstringCaInGtEq},
    {"string-ci>?", euxlstringCaInGt},
    {"make-string", euxlMakeString},

    // character functions
    {"char<?", euxlCharLt},
    {"char<=?", euxlCharLtEq},
    {"char=?", euxlCharEql},
    {"char>=?", euxlCharGtEq},
    {"char>?", euxlCharGt},
    {"char-ci<?", euxlCharCaInLt},
    {"char-ci<=?", euxlCharCaInLteq},
    {"char-ci=?", euxlCharCaInEql},
    {"char-ci>=?", euxlCharCaInGtEq},
    {"char-ci>?", euxlCharCaInGt},

    // i/o functions
    {"read", euxlRead},
    {"read-char", euxlReadChar},
    {"read-byte", euxlReadByte},
    {"read-short", euxlReadShort},
    {"read-long", euxlReadLong},
    {"peek-char", euxlPeekChar},
    {"char-ready?", euxlCharReadyp},
    {"%write", euxlWrite},
    {"write-char", euxlWriteChar},
    {"write-byte", euxlWriteByte},
    {"write-short", euxlWriteShort},
    {"write-long", euxlWriteLong},
    {"%print", euxlPrint},
    {"printnl", euxlPrintnl},
    {"sflush", euxlSFlush},
    {"flush", euxlFlush},

    // print control functions
    {"print-breadth", euxlPrintBreadth},
    {"print-depth", euxlPrintDepth},

    // file i/o functions
    {"open-input-file", euxlOpenInput},
    {"open-output-file", euxlOpenOutput},
    {"open-append-file", euxlOpenAppend},
    {"open-update-file", euxlOpenUpdate},
    {"close-stream", euxlClose},
    {"close-input-stream", euxlCloseInput},
    {"close-output-stream", euxlCloseOutput},
    {"get-file-position", euxlGetFilePosition},
    {"set-file-position", euxlsetFilePosition},
    {"unlink", euxlUnlink},

    // utility functions
    {"transcript-on", euxlTransciptOn},
    {"transcript-off", euxlTranscriptOff},
    {"getarg", euxlGetArg},
    {"exit", euxlExit},
    {"compile", euxlCompile},
    {"decompile", euxlDecompile},
    {"gc", euxlGc},
    {"save", euxlSave},
    {"restore", euxlRestore},
    {"reset", euxlReset},
    {"xserror", euxlError},
    {"default-handler", euxcDefaultHandler},

    // debugging functions
    {"trace-on", euxlTraceOn},
    {"trace-off", euxlTraceOff},

    // internal functions
    {"%car", euxlICar},
    {"%cdr", euxlICdr},
    {"%set-car", euxlIsetCar},
    {"%set-cdr", euxlIsetCdr},
    {"%vector-size", euxlIVectorSize},
    {"%vector-ref", euxlIVectorRef},
    {"%vector-set", euxlIVectorSet},
    {"%keywords", euxlKeywordArray},

    // module functions
    {"module-symbols", euxcModuleSymbols},
    {"module-exports", euxcModuleExports},
    {"symbol-module", euxcSymbolModule},
    {"find-module", euxlFindModule},
    {"current-module", euxcCurrentMod},
    {"module-list", euxcModList},
    {"unintern", euxcUnintern},

    // telos
    {"allocate", euxlAllocate},
    {"find-key", euxlFindKey},
    {"raise-telos-error", euxlTelosError},
    {"initialize-class", euxlInitializeClass},
    {"class-of", euxlClassOf},
    {"describe", euxlDescribe},
    {"class-name", euxlClassName},
    {"class-superclasses", euxlClassSuperclasses},
    {"class-precedence-list", euxlClassCpl},
    {"class-slots", euxlClassSlots},
    {"class-keywords", euxlClassKeywords},
    {"set-class-keywords", euxlSetClassKeywords},
    {"class-subclasses", euxlClassSubclasses},
    {"class-instance-size", euxlClassInstsize},
    {"class-abstract?", euxlClassAbstractp},
    {"class?", euxlClassp},
    {"subclass?", euxlSubClassp},
    {"generic-name", euxlGfName},
    {"generic-args", euxlGfArgs},
    {"set-generic-args", euxlGfSetargs},
    {"generic-optargs?", euxlGfOptargs},
    {"generic-methods", euxlGfMethods},
    {"generic-cache1", euxlGfCache1},
    {"generic-cache2", euxlGfCache2},
    {"make-generic", euxlMakeGeneric},
    {"method-generic", euxlMethodGf},
    {"method-function", euxlMethodFun},
    {"method-domain", euxlMethodDomain},
    {"make-and-add-method", euxlMakeAndAddMethod},
    {"make-method", euxlMakeMethod},
    {"add-method", euxlAddMethod},
    {"slot-name", euxlSlotName},
    {"slot-keyword", euxlSlotKeyword},
    {"slot-default", euxlSlotDefault},
    {"set-slot-default", euxlSetSlotDefault},
    {"slot-required?", euxlSlotRequiredp},
    {"set-slot-required?", euxlSetSlotRequiredp},
    {"find-slot-index", euxlFindSlotIndex},

    {"make-table", euxlMakeTable},
    {"table-ref", euxlTableRef},
    {"table-set", euxlTableSet},
    {"table-comparator", euxlTableComparator},
    {"table-delete", euxlTableDelete},
    {"table-size", euxlTableSize},
    {"table-keys", euxlTableKeys},
    {"table-values", euxlTableValues},

    {"backtrace", euxlBacktrace},

    {"set-module", euxlSetModule},
    {"reintern-module-symbols", euxlReintern},
    {"reintern-syntax", euxlReinternSyntax},
    {"module-directives", euxlModuleDirectives},

    {"table-fill", euxlTableFill},
    {"set-table-fill", euxlTableSetFill},
    {"table-clear", euxlTableClear},

    {"raise-syntax-error", euxlSyntaxError},

    {"setivar", euxlSetIVar},
    {"getivar", euxlGetIVar},

    {"tmpfile", euxlTmpFile},
    {"getenv", euxlGetenv},
    {"putenv", euxlPutenv},
    {"cpu-time", euxlCpuTime},
    {"xsprintf", euxlSprintf},

    // debugging
    {"frame-up", euxlFrameUp},
    {"frame-down", euxlFrameDown},
    {"frame-env", euxlFrameEnv},
    {"frame-fun", euxlFrameFun},

    #ifndef NO_CHECK_REF
    {"check-ref", euxlCheckRef},
    #endif

    #ifdef SOCK
    {"socket-socket", euxcSocketSocket},
    {"socket-connect", euxcSocketConnect},
    {"socket-bind", euxcSocketBind},
    {"socket-listen", euxcSocketListen},
    {"socket-accept", euxcSocketAccept},
    {"socket-block", euxcSocketBlock},
    {"socket-nonblock", euxcSocketNonBlock},
    {"socket-reuse", euxcSocketReuse},
    {"socket-noreuse", euxcSocketNoReuse},
    {"socket-close", euxcSocketClose},
    {"socket-shutdown", euxcSocketShutdown},
    {"socket-peeraddr", euxcSocketPeerAddr},
    {"socket-peerstream", euxcSocketPeerStream},
    {"socket-sockaddr", euxcSocketSockAddr},
    {"socket-sockstream", euxcSocketSockStream},
    {"socket-host-to-ip", euxcSocketHostToIP},
    {"socket-ip-to-host", euxcSocketIPToHost},
    {"socket-convert-to-stream", euxcSocketToStream},
    {"stream-fd", stream_fd,},
    {"stream-unbuffered", stream_unbuffered,},
    {"stream-block-buffered", stream_block_buffered,},
    {"stream-line-buffered", stream_line_buffered,},
    {"socket-fd-zero-read", euxcSocketFdZeroRead},
    {"socket-fd-set-read", euxcSocketFdSetRead},
    {"socket-fd-isset-read", euxcSocketFdIssetRead},
    {"socket-select-read", euxcSocketSelectRead},
    {"socket-fd-zero-write", euxcSocketFdZeroWrite},
    {"socket-fd-set-write", euxcSocketFdSetWrite},
    {"socket-fd-isset-write", euxcSocketFdIssetWrite},
    {"socket-select-write", euxcSocketSelectWrite},
    {"stream-xdr-send-int", euxcStreamXdrSendInt},
    {"stream-xdr-recv-int", euxcStreamXdrRecvInt},
    {"stream-xdr-send-float", euxcStreamXdrSendDoubleFloat},
    {"stream-xdr-recv-float", euxcStreamXdrRecvDoubleFloat},
    {"stream-xdr-send-string", euxcStreamXdrSendString},
    {"stream-xdr-recv-string", euxcStreamXdrRecvString},
    #endif

    // include machine specific table entries
    {"system", euxlSystem},

    {0, 0}      // end of table marker
};

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxlStdin - get the stdin stream
euxlValue euxlStdin()
{
    return (euxmGetValue(euxls_stdin));
}

///  euxlStdout - get the stdout stream
euxlValue euxlStdout()
{
    return (euxmGetValue(euxls_stdout));
}

///  euxcEq - internal 'eq' function
int euxcEq(euxlValue arg1, euxlValue arg2)
{
    if (euxmSymbolp(arg1) && euxmSymbolp(arg2))
    {
        return euxmSymbolEq(arg1, arg2);
    }
    return (arg1 == arg2);
}

///  euxcEqv - internal 'eql' function
int euxcEqv(euxlValue arg1, euxlValue arg2)
{
    // try the euxcEq test first
    if (arg1 == arg2)
    {
        return (euxmTrue);
    }

    // compare FPIs, floats and characters
    if (!euxmNull(arg1))
    {
        switch (euxmNodeType(arg1))
        {
            case euxmSymbol:
                return (euxmSymbolp(arg2) && euxmSymbolEq(arg1, arg2));
            case euxmFPI:
                return (euxmFPIp(arg2) && euxmGetFPI(arg1) == euxmGetFPI(arg2));
            case euxmDoubleFloat:
                return
                (
                    euxmDoubleFloatp(arg2)
                 && euxmGetDoubleFloat(arg1) == euxmGetDoubleFloat(arg2)
                );
            case euxmChar:
                return
                (
                    euxmCharp(arg2)
                 && euxmGetCharCode(arg1) == euxmGetCharCode(arg2)
                );
        }
    }
    return (euxmFalse);
}

///  euxcEqual - internal 'equal' function
int euxcEqual(euxlValue arg1, euxlValue arg2)
{
    // try the euxcEq test first
    if (arg1 == arg2)
    {
        return (euxmTrue);
    }

    // compare FPIs, floats, characters, strings, vectors and conses
    if (!euxmNull(arg1))
    {
        switch (euxmNodeType(arg1))
        {
            case euxmSymbol:
                return (euxmSymbolp(arg2) && euxmSymbolEq(arg1, arg2));
            case euxmFPI:
                return (euxmFPIp(arg2) && euxmGetFPI(arg1) == euxmGetFPI(arg2));
            case euxmDoubleFloat:
                return
                (
                    euxmDoubleFloatp(arg2)
                 && euxmGetDoubleFloat(arg1) == euxmGetDoubleFloat(arg2)
                );
            case euxmChar:
                return
                (
                    euxmCharp(arg2)
                 && euxmGetCharCode(arg1) == euxmGetCharCode(arg2)
                );
            case euxmString:
                return
                (
                    euxmStringp(arg2)
                 && strcmp(euxmGetString(arg1), euxmGetString(arg2)) == 0
                );
            case euxmVector:
                return (euxmVectorp(arg2) && euxcVectorEqual(arg1, arg2));
            case euxmCons:
                return
                (
                    euxmConsp(arg2)
                 && euxcEqual(euxmCar(arg1), euxmCar(arg2))
                 && euxcEqual(euxmCdr(arg1), euxmCdr(arg2))
                );
        }
    }
    return (euxmFalse);
}

///  equality of two numbers
int euxcEquals(euxlValue arg1, euxlValue arg2)
{
    if (euxmFPIp(arg1))
    {
        if (euxmFPIp(arg2))
        {
            return (euxmGetFPI(arg1) == euxmGetFPI(arg2));
        }
        else if (euxmDoubleFloatp(arg2))
        {
            return (euxmGetFPI(arg1) == euxmGetDoubleFloat(arg2));
        }
        euxcCerror("equals called with non-numeric arg", arg2, euxmNil);
    }
    else if (euxmDoubleFloatp(arg1))
    {
        if (euxmFPIp(arg2))
        {
            return (euxmGetDoubleFloat(arg1) == euxmGetFPI(arg2));
        }
        else if (euxmDoubleFloatp(arg2))
        {
            return (euxmGetDoubleFloat(arg1) == euxmGetDoubleFloat(arg2));
        }
        euxcCerror("equals called with non-numeric arg", arg2, euxmNil);
    }
    euxcCerror("equals called with non-numeric arg", arg1, euxmNil);
    return 0;   // not reached
}

///  euxcVectorEqual - compare two vectors
int euxcVectorEqual(euxlValue v1, euxlValue v2)
{
    int len = euxmGetSize(v1);

    // compare the vector sizes
    if (len != euxmGetSize(v2))
    {
        return (euxmFalse);
    }

    // compare the vector elements
    for (int i = 0; i < len; ++i)
    {
        if (!euxcEqual(euxmGetElement(v1, i), euxmGetElement(v2, i)))
        {
            return (euxmFalse);
        }
    }

    return (euxmTrue);
}

///  euxcTooFew - too few arguments to this function
euxlValue euxcTooFew(const char *functionName)
{
    euxlValue name = euxcMakeString(functionName);
    euxcCerror("too few arguments", name, euxmNil);

    return euxmNil; // Never reached
}

///  euxcTooFew - too few arguments to this function
void euxcTooFewInt()
{
    euxcIntError("too few arguments", xlfun, euxmNil);
}

///  euxcTooMany - too many arguments to this function
void euxcTooMany(const char *functionName)
{
    euxlValue name = euxcMakeString(functionName);
    euxcCerror("too many arguments", name, euxmNil);
}

///  euxcTooMany - too many arguments to this function
void euxcTooManyInt()
{
    euxcIntError("too many arguments", xlfun, euxmNil);
}

///  euxcBadType - incorrect argument type
//    cf badargtype in xsint.c
euxlValue euxcBadType(euxlValue val, const char *name, const char *fn)
{
    char buf[256];
    sprintf(buf, "incorrect type in %s", fn);

    euxlValue cond = euxmGetValue(euxls_bad_type_error);
    if (cond != euxls_unbound)
    {
        euxlValue class =
            name[0] == '<'
          ? euxmGetValue(euxcEnterModule(name, euxcRootModule))
          : euxcMakeString(name);
        euxmSetIVar(cond, 3, class);        // cf condition.em
    }

    euxcCerror(buf, val, euxls_bad_type_error);

    return euxmNil;  // Never reached
}


///-----------------------------------------------------------------------------
