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
/// Title: euxlisp built-in functions - part 1 (list, vector, eq/Eql/Equal)
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"

///-----------------------------------------------------------------------------
/// Local variables
///-----------------------------------------------------------------------------
static char gsprefix[euxmStringMax + 1] = { 'G', 0 };  // gensym prefix string
static int gsnumber = 1;                               // gensym number

///-----------------------------------------------------------------------------
/// Macros
///-----------------------------------------------------------------------------
#define isprocedure(x)                                                         \
    (                                                                          \
        euxmClosurep(x)                                                        \
        || euxmContinuationp(x)                                                \
        || euxmFunp(x)                                                         \
        || euxmXFunp(x)                                                        \
        || euxmGenericp(x)                                                     \
    )

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static euxlValue cxr(const char *adstr);
static euxlValue nth(int euxmCarflag);
static euxlValue vRef(euxlValue vector);
static euxlValue vSet(euxlValue vector);
static euxlValue eqEqlEqual(int (*fcn) (euxlValue a, euxlValue b));

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxlCons - construct a new list cell
euxlValue euxlCons()
{
    static const char *functionName = "cons";

    // get the two arguments
    euxlValue euxmCarval = euxmGetArg();
    euxlValue euxmCdrval = euxmGetArg();
    euxmLastArg();

    // construct a new euxcCons node
    return (euxcCons(euxmCarval, euxmCdrval));
}

///  euxlCar - built-in function 'car'
euxlValue euxlCar()
{
    static const char *functionName = "car";

    euxlValue list = euxmGetArgCons();
    euxmLastArg();
    return euxmCar(list);
}

///  euxlICar - built-in function '%car'
euxlValue euxlICar()
{
    static const char *functionName = "%car";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return euxmCar(arg);
}

///  euxlCdr - built-in function 'cdr'
euxlValue euxlCdr()
{
    static const char *functionName = "cdr";

    euxlValue arg = euxmGetArgCons();
    euxmLastArg();
    return euxmCdr(arg);
}

///  euxlICdr - built-in function '%cdr'
euxlValue euxlICdr()
{
    static const char *functionName = "%cdr";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return euxmCdr(arg);
}

///  cxxr functions
euxlValue euxlCaar()
{
    return (cxr("aa"));
}

euxlValue euxlCadr()
{
    return (cxr("da"));
}

euxlValue euxlCdar()
{
    return (cxr("ad"));
}

euxlValue euxlCddr()
{
    return (cxr("dd"));
}

///  cxxxr functions
euxlValue euxlCaaar()
{
    return (cxr("aaa"));
}

euxlValue euxlCaadr()
{
    return (cxr("daa"));
}

euxlValue euxlCadar()
{
    return (cxr("ada"));
}

euxlValue euxlCaddr()
{
    return (cxr("dda"));
}

euxlValue euxlCdaar()
{
    return (cxr("aad"));
}

euxlValue euxlCdadr()
{
    return (cxr("dad"));
}

euxlValue euxlCddar()
{
    return (cxr("add"));
}

euxlValue euxlCdddr()
{
    return (cxr("ddd"));
}

///  cxxxxr functions
euxlValue euxlCaaaar()
{
    return (cxr("aaaa"));
}

euxlValue euxlCaaadr()
{
    return (cxr("daaa"));
}

euxlValue euxlCaadar()
{
    return (cxr("adaa"));
}

euxlValue euxlCaaddr()
{
    return (cxr("ddaa"));
}

euxlValue euxlCadaar()
{
    return (cxr("aada"));
}

euxlValue euxlCadadr()
{
    return (cxr("dada"));
}

euxlValue euxlCaddar()
{
    return (cxr("adda"));
}

euxlValue euxlCadddr()
{
    return (cxr("ddda"));
}

euxlValue euxlCdaaar()
{
    return (cxr("aaad"));
}

euxlValue euxlCdaadr()
{
    return (cxr("daad"));
}

euxlValue euxlCdadar()
{
    return (cxr("adad"));
}

euxlValue euxlCdaddr()
{
    return (cxr("ddad"));
}

euxlValue euxlCddaar()
{
    return (cxr("aadd"));
}

euxlValue euxlCddadr()
{
    return (cxr("dadd"));
}

euxlValue euxlCdddar()
{
    return (cxr("addd"));
}

euxlValue euxlCddddr()
{
    return (cxr("dddd"));
}

///  cxr - common euxmCar/euxmCdr function
static euxlValue cxr(const char *adstr)
{
    static const char *functionName = "c[ad]r";

    // get the list
    euxlValue list = euxmGetArgList();
    euxmLastArg();
    euxlValue lst = list;
    const char *ad = adstr;

    // perform the euxmCar/euxmCdr operations
    while (*adstr && euxmConsp(list))
    {
        list = (*adstr++ == 'a' ? euxmCar(list) : euxmCdr(list));
    }

    // make sure the operation succeeded
    if (*adstr)
    {
        char buf[128];
        sprintf(buf, "c%sr", ad);
        euxcBadType(lst, "a deeper list", buf);
    }

    // return the result
    return (list);
}

///  euxlSetCar - built-in function 'set-car'
euxlValue euxlSetCar()
{
    static const char *functionName = "set-car";

    // get the euxcCons and the new euxmCar
    euxlValue arg = euxmGetArgCons();
    euxlValue neweuxmCar = euxmGetArg();
    euxmLastArg();

    // replace the euxmCar
    euxmSetCar(arg, neweuxmCar);
    return (neweuxmCar);
}

///  euxlIsetCar - built-in function '%set-car'
euxlValue euxlIsetCar()
{
    static const char *functionName = "%set-car";

    // get the euxcCons and the new euxmCar
    euxlValue arg = euxmGetArg();
    euxlValue neweuxmCar = euxmGetArg();
    euxmLastArg();

    // replace the euxmCar
    euxmSetCar(arg, neweuxmCar);
    return (arg);
}

///  euxlSetCdr - built-in function 'set-cdr'
euxlValue euxlSetCdr()
{
    static const char *functionName = "set-cdr";

    // get the euxcCons and the new euxmCdr
    euxlValue arg = euxmGetArgCons();
    euxlValue neweuxmCdr = euxmGetArg();
    euxmLastArg();

    // replace the euxmCdr
    euxmSetCdr(arg, neweuxmCdr);
    return (neweuxmCdr);
}

///  euxlIsetCdr - built-in function '%set-cdr'
euxlValue euxlIsetCdr()
{
    static const char *functionName = "%set-cdr";

    // get the euxcCons and the new euxmCdr
    euxlValue arg = euxmGetArg();
    euxlValue neweuxmCdr = euxmGetArg();
    euxmLastArg();

    // replace the euxmCdr
    euxmSetCdr(arg, neweuxmCdr);
    return (arg);
}

///  euxlList - built-in function 'list'
euxlValue euxlList()
{
    // initialize the list
    euxlValue val = euxmNil;

    // add each argument to the list
    if (euxmMoreArgs())
    {
        euxlValue last = euxcCons(euxmNextArg(), euxmNil);
        val = last;
        while (euxmMoreArgs())
        {
            euxlValue next = euxmNextArg();
            euxmStackPush(val);
            next = euxcCons(next, euxmNil);
            euxmSetCdr(last, next);
            last = next;
            val = euxmStackPop();
        }
    }

    // return the list
    return (val);
}

///  euxlListStar - built-in function 'list*'
euxlValue euxlListStar()
{
    // initialize the list
    euxlValue val = euxmNil;
    euxlValue last = euxmNil;

    // add each argument to the list
    if (euxmMoreArgs())
    {
        for (;;)
        {
            euxlValue next = euxmNextArg();
            if (euxmMoreArgs())
            {
                euxmStackPush(val);
                next = euxcCons(next, euxmNil);
                val = euxmStackPop();
                if (val)
                {
                    euxmSetCdr(last, next);
                }
                else
                {
                    val = next;
                }
                last = next;
            }
            else
            {
                if (val)
                {
                    euxmSetCdr(last, next);
                }
                else
                {
                    val = next;
                }
                break;
            }
        }
    }

    // return the list
    return (val);
}

///  euxlAppend - built-in function 'append'
euxlValue euxlAppend()
{
    static const char *functionName = "append";

    // euxcAppend each argument
    euxlValue last, val;
    for (val = last = euxmNil; euxcArgC > 1;)
    {
        // euxcAppend each element of this list to the result list
        for (euxlValue next = euxmGetArgList(); euxmConsp(next); next = euxmCdr(next))
        {
            euxmStackPush(val);
            euxlValue this = euxcCons(euxmCar(next), euxmNil);
            val = euxmStackPop();
            if (last == euxmNil)
            {
                val = this;
            }
            else
            {
                euxmSetCdr(last, this);
            }
            last = this;
        }
    }

    // tack on the last argument
    if (euxmMoreArgs())
    {
        if (last == euxmNil)
        {
            val = euxmGetArg();
        }
        else
        {
            euxmSetCdr(last, euxmGetArg());
        }
    }

    // return the list
    return (val);
}

euxlValue euxcReverseList(euxlValue next)
{
    euxmStackCheckPush(next);

    // euxcAppend each element of this list to the result list
    euxlValue val;
    for (val = euxmNil; euxmConsp(next); next = euxmCdr(next))
    {
        euxmStackPush(val);
        val = euxcCons(euxmCar(next), euxmStackTop());
        euxmStackDrop(1);
    }

    euxmStackDrop(1);

    // return the list
    return (val);
}

///  euxlReverseList - built-in function 'reverse'
euxlValue euxlReverseList()
{
    static const char *functionName = "reverse-list";

    // get the list to reverse
    euxlValue next = euxmGetArgList();
    euxmLastArg();

    return euxcReverseList(next);
}

///  euxlLastPair - built-in function 'last-pair'
euxlValue euxlLastPair()
{
    static const char *functionName = "last-pair";

    // get the list
    euxlValue list = euxmGetArgList();
    euxmLastArg();

    // find the last euxcCons
    if (euxmConsp(list))
    {
        while (euxmConsp(euxmCdr(list)))
        {
            list = euxmCdr(list);
        }
    }

    // return the last element
    return (list);
}

///  euxlSize - built-in function 'list_size'
euxlValue euxlSize()
{
    static const char *functionName = "list-size";

    // get the argument
    euxlValue arg = euxmGetArgList();
    euxmLastArg();

    // find the euxcListSize
    euxmFPIType n;
    for (n = (euxmFPIType) 0; euxmConsp(arg); ++n)
    {
        arg = euxmCdr(arg);
    }

    // return the euxcListSize
    return (euxcMakeFPI(n));
}

///  euxlMember - built-in function 'member'
euxlValue euxlMember()
{
    return (euxlGeneralMember(euxcEqual));
}

///  euxlMemv - built-in function 'memv'
euxlValue euxlMemv()
{
    return (euxlGeneralMember(euxcEqv));
}

///  euxlMemq - built-in function 'memq'
euxlValue euxlMemq()
{
    return (euxlGeneralMember(euxcEq));
}

///  euxlGeneralMember - common function for member/memv/memq
euxlValue euxcMember(euxlValue x, euxlValue list, int (*fcn) (euxlValue a, euxlValue b))
{
    euxlValue val;

    // look for the expression
    for (val = euxmNil; euxmConsp(list); list = euxmCdr(list))
    {
        if ((*fcn) (x, euxmCar(list)))
        {
            val = list;
            break;
        }
    }

    // return the result
    return (val);
}

euxlValue euxlGeneralMember(int (*fcn) ())
{
    static const char *functionName = "member/memq/memv";

    // get the expression to look for and the list
    euxlValue x = euxmGetArg();
    euxlValue list = euxmGetArgList();
    euxmLastArg();

    return euxcMember(x, list, fcn);
}

///  euxlAssoc - built-in function 'assoc'
euxlValue euxlAssoc()
{
    return (euxcAssoc(euxcEqual));
}

///  euxlAssv - built-in function 'assv'
euxlValue euxlAssv()
{
    return (euxcAssoc(euxcEqv));
}

///  euxlAssq - built-in function 'assq'
euxlValue euxlAssq()
{
    return (euxcAssoc(euxcEq));
}

///  euxcAssoc - common function for assoc/assv/assq
euxlValue euxcAssoc(int (*fcn) ())
{
    static const char *functionName = "assoc/assv/assq";

    // get the expression to look for and the association list
    euxlValue x = euxmGetArg();
    euxlValue alist = euxmGetArgList();
    euxmLastArg();

    // look for the expression
    euxlValue val;
    for (val = euxmNil; euxmConsp(alist); alist = euxmCdr(alist))
    {
        euxlValue pair;
        if ((pair = euxmCar(alist)) != euxmNil && euxmConsp(pair))
        {
            if ((*fcn) (x, euxmCar(pair), fcn))
            {
                val = pair;
                break;
            }
        }
    }

    // return the result
    return (val);
}

///  euxlListRef - built-in function 'list-ref'
euxlValue euxlListRef()
{
    return (nth(euxmTrue));
}

///  euxlListTail - built-in function 'list-tail'
euxlValue euxlListTail()
{
    return (nth(euxmFalse));
}

///  nth - internal nth function
static euxlValue nth(int euxmCarflag)
{
    static const char *functionName = "list-ref/list-tail";

    // get n and the list
    euxlValue list = euxmGetArgList();
    euxlValue arg = euxmGetArgFPI();
    euxmLastArg();

    // range Check the index
    int n;
    if ((n = (int)euxmGetFPI(arg)) < 0 || (euxmCarflag && list == euxmNil))
    {
        euxcCerror("index out of range in list-ref/list-tail", arg, euxmNil);
    }

    // find the nth element
    for (; euxmConsp(list) && n; n--)
    {
        list = euxmCdr(list);
    }

    // make sure the list was long enough
    if (n)
    {
        euxcCerror("index out of range in list-ref/list-tail", arg, euxmNil);
    }

    // return the list beginning at the nth element
    return (euxmCarflag && euxmConsp(list) ? euxmCar(list) : list);
}

///  euxlBoundp - is this a value bound to this symbol?
euxlValue euxlBoundp()
{
    static const char *functionName = "symbol-exists?";

    euxlValue sym = euxmGetArgSymbol();
    euxmLastArg();
    return (euxmBoundp(sym) ? euxl_true : euxmNil);
}

///  euxlSymValue - get the value of a symbol
euxlValue euxlSymValue()
{
    static const char *functionName = "symbol-value";

    euxlValue sym = euxmGetArgSymbol();
    euxmLastArg();
    return (euxmGetValue(sym));
}

///  euxlSetSymValue - set the value of a symbol
euxlValue euxlSetSymValue()
{
    static const char *functionName = "set-symbol-value";

    // get the symbol
    euxlValue sym = euxmGetArgSymbol();
    euxlValue val = euxmGetArg();
    euxmLastArg();

    // set the global value
    euxmSetValue(sym, val);

    // return its value
    return (val);
}

///  euxlSymPlist - get the property list of a symbol
euxlValue euxlSymPlist()
{
    static const char *functionName = "symbol-plist";

    // get the symbol
    euxlValue sym = euxmGetArgSymbol();
    euxmLastArg();

    // return the property list
    return (euxmGetPList(sym));
}

///  euxlSetSymPlist - set the property list of a symbol
euxlValue euxlSetSymPlist()
{
    static const char *functionName = "set-symbol-plist";

    // get the symbol
    euxlValue sym = euxmGetArgSymbol();
    euxlValue val = euxmGetArg();
    euxmLastArg();

    // set the property list
    euxmSetPList(sym, val);
    return (val);
}

///  euxlGet - get the value of a property
euxlValue euxlGet()
{
    static const char *functionName = "get";

    // get the symbol and property
    euxlValue sym = euxmGetArgSymbol();
    euxlValue prp = euxmGetArgSymbol();
    euxmLastArg();

    // retrieve the property value
    return (euxcGetProp(sym, prp));
}

///  euxlPut - set the value of a property
euxlValue euxlPut()
{
    static const char *functionName = "put";

    // get the symbol and property
    euxlValue sym = euxmGetArgSymbol();
    euxlValue prp = euxmGetArgSymbol();
    euxlValue val = euxmGetArg();
    euxmLastArg();

    // set the property value
    euxcPutProp(sym, val, prp);

    // return the value
    return (val);
}

///  euxlGetSyntax - symbol syntax
euxlValue euxlGetSyntax()
{
    static const char *functionName = "get-syntax";

    // get the symbol and property
    euxlValue sym = euxmGetArgSymbol();
    euxlValue prp = euxmGetArgSymbol();
    euxmLastArg();

    // retrieve the syntax value
    return (euxcGetSyntax(sym, prp));
}

///  euxlPut - set symbol syntax
euxlValue euxlPutSyntax()
{
    static const char *functionName = "put-syntax";

    // get the symbol and property
    euxlValue sym = euxmGetArgSymbol();
    euxlValue prp = euxmGetArgSymbol();
    euxlValue val = euxmGetArg();
    euxmLastArg();

    // set the syntax value
    euxcPutSyntax(sym, val, prp);

    // return the value
    return (val);
}

///  euxlTheEnvironment - built-in function 'the-environment'
euxlValue euxlTheEnvironment()
{
    static const char *functionName = "the-environment";
    euxmLastArg();
    return (xlenv);
}

///  euxlProcEnvironment - built-in function 'procedure-environment'
euxlValue euxlProcEnvironment()
{
    static const char *functionName = "procedure-environment";

    euxlValue arg = euxmGetArgClosure();
    euxmLastArg();
    return (euxmGetCEnv(arg));
}

///  euxlEnvp - built-in function 'environment?'
euxlValue euxlEnvp()
{
    static const char *functionName = "environment?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (euxmEnvp(arg) ? euxl_true : euxmNil);
}

///  euxlEnvBindings - built-in function 'environment-bindings'
euxlValue euxlEnvBindings()
{
    static const char *functionName = "environment-bindings";

    // get the environment
    euxlValue env = euxmGetArg();
    euxmLastArg();

    // Check the argument type
    if (euxmClosurep(env))
    {
        env = euxmGetCEnv(env);
    }
    else if (!euxmEnvp(env))
    {
        euxcBadType(env, "<env>", functionName);
    }

    // initialize
    euxlValue frame = euxmCar(env);
    euxlValue names = euxmGetElement(frame, 0);
    int len = euxmGetSize(frame);
    euxmStackCheck(1);

    // build a list of dotted pairs
    euxlValue val, last;
    int i;
    for (val = last = euxmNil, i = 1; i < len; ++i, names = euxmCdr(names))
    {
        euxmStackPush(val);
        euxlValue this = euxcCons(euxcCons(euxmCar(names), euxmGetElement(frame, i)), euxmNil);
        val = euxmStackPop();
        if (last)
        {
            euxmSetCdr(last, this);
        }
        else
        {
            val = this;
        }
        last = this;
    }
    return (val);
}

///  euxlEnvParent - built-in function 'environment-parent'
euxlValue euxlEnvParent()
{
    static const char *functionName = "environment-parent";

    euxlValue env = euxmGetArgEnv();
    euxmLastArg();
    return (euxmCdr(env));
}

///  euxlVector - built-in function 'vector'
euxlValue euxlVector()
{
    static const char *functionName = "vector";

    euxlValue vect = euxcNewVector(euxcArgC);
    for (euxlValue *p = &vect->value.vector.data[0]; euxmMoreArgs();)
    {
        *p++ = euxmGetArg();
    }
    return (vect);
}

///  euxlMakeVector - built-in function 'make-vector'
euxlValue euxlMakeVector()
{
    static const char *functionName = "make-vector";

    // get the vector size
    euxlValue arg = euxmGetArgFPI();
    int len = (int)euxmGetFPI(arg);

    if (len < 0)
    {
        euxcCerror("bad size for make-vector", arg, euxmNil);
    }

    // Check for an initialization value
    euxlValue val;
    if (euxmMoreArgs())
    {
        arg = euxmGetArg();       // get the initializer
        euxmLastArg();            // make sure that's the last argument
        euxmStackCheckPush(arg);             // save the initializer
        val = euxcNewVector(len);   // create the vector
        euxlValue *p = &val->value.vector.data[0];   // initialize the vector
        for (arg = euxmStackPop(); --len >= 0;)
        {
            *p++ = arg;
        }
    }
    else // no initialization value
    {
        val = euxcNewVector(len);   // defaults to initializing to euxmNil
    }

    // return the new vector
    return (val);
}

///  euxlVectorSize - built-in function 'vector-size'
euxlValue euxlVectorSize()
{
    static const char *functionName = "vector-size";

    euxlValue arg = euxmGetArgVector();
    euxmLastArg();
    return (euxcMakeFPI((euxmFPIType) euxmGetSize(arg)));
}

///  euxlIVectorSize - built-in function '%vector-size'
euxlValue euxlIVectorSize()
{
    static const char *functionName = "%vector-size";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (euxcMakeFPI((euxmFPIType) euxmGetSize(arg)));
}

///  euxlVectorRef - built-in function 'vector-ref'
euxlValue euxlVectorRef()
{
    static const char *functionName = "vector-ref";
    return (vRef(euxmGetArgVector()));
}

///  euxlIVectorRef - built-in function '%vector-ref'
euxlValue euxlIVectorRef()
{
    static const char *functionName = "%vector-ref";
    return (vRef(euxmGetArg()));
}

///  vRef - common code for euxlVectorRef and euxlIVectorRef
static euxlValue vRef(euxlValue vector)
{
    static const char *functionName = "vector-ref";

    // get the index
    euxlValue index = euxmGetArgFPI();
    euxmLastArg();

    // range Check the index
    int i;
    if ((i = (int)euxmGetFPI(index)) < 0 || i >= euxmGetSize(vector))
    {
        euxcCerror("index out of range in vector-ref", index, euxmNil);
    }

    // return the vector element
    return (euxmGetElement(vector, i));
}

///  euxlVectorSet - built-in function 'vector-set'
euxlValue euxlVectorSet()
{
    static const char *functionName = "vector-set";
    return (vSet(euxmGetArgVector()));
}

///  euxlIVectorSet - built-in function '%vector-set'
euxlValue euxlIVectorSet()
{
    static const char *functionName = "%vector-set";
    return (vSet(euxmGetArg()));
}

///  vSet - common code for euxlVectorSet and euxlIVectorSet
static euxlValue vSet(euxlValue vector)
{
    static const char *functionName = "vector-set";

    // get the index and the new value
    euxlValue index = euxmGetArgFPI();
    euxlValue val = euxmGetArg();
    euxmLastArg();

    // range Check the index
    int i;
    if ((i = (int)euxmGetFPI(index)) < 0 || i >= euxmGetSize(vector))
    {
        euxcCerror("index out of range in vector-set", index, euxmNil);
    }

    // set the vector element and return the value
    euxmSetElement(vector, i, val);
    return (val);
}

///  euxlVectorToList - built-in function 'vector->list'
euxlValue euxlVectorToList()
{
    static const char *functionName = "vector->list";

    // get the vector
    euxlValue vect = euxmGetArgVector();
    euxmLastArg();

    // make a list from the vector
    euxmStackCheckPush(vect);
    int size = euxmGetSize(vect);
    for (xlval = euxmNil; --size >= 0;)
    {
        xlval = euxcCons(euxmGetElement(vect, size), xlval);
    }
    euxmStackDrop(1);
    return (xlval);
}

///  euxlListToVector - built-in function 'list->vector'
euxlValue euxlListToVector()
{
    static const char *functionName = "list->vector";

    // get the list
    xlval = euxmGetArgList();
    euxmLastArg();

    // make a vector from the list
    int size = euxcListSize(xlval);
    euxlValue vect = euxcNewVector(size);
    for
    (
        euxlValue *p = &vect->value.vector.data[0];
        --size >= 0;
        xlval = euxmCdr(xlval)
    )
    {
        *p++ = euxmCar(xlval);
    }

    return (vect);
}

///  euxlNullp - built-in function 'euxmNull?'
euxlValue euxlNullp()
{
    static const char *functionName = "null?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (euxmNull(arg) ? euxl_true : euxmNil);
}

///  euxlAtomp - built-in function 'euxmAtom?'
euxlValue euxlAtomp()
{
    static const char *functionName = "atom?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (euxmAtom(arg) ? euxl_true : euxmNil);
}

///  euxlListp - built-in function 'list?'
euxlValue euxlListp()
{
    static const char *functionName = "list?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (euxmListp(arg) ? euxl_true : euxmNil);
}

///  euxlNumberp - built-in function 'number?'
euxlValue euxlNumberp()
{
    static const char *functionName = "number?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (euxmNumberp(arg) ? euxl_true : euxmNil);
}

///  euxlBooleanp - built-in function 'boolean?'
euxlValue euxlBooleanp()
{
    static const char *functionName = "boolean?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (arg == euxl_true || arg == euxmNil ? euxl_true : euxmNil);
}

///  euxlConsp - built-in function 'cons?'
euxlValue euxlConsp()
{
    static const char *functionName = "cons?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (euxmConsp(arg) ? euxl_true : euxmNil);
}

///  euxlSymbolp - built-in function 'symbol?'
euxlValue euxlSymbolp()
{
    static const char *functionName = "symbol?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (euxmSymbolp(arg) ? euxl_true : euxmNil);
}

///  euxlKeywordp - built-in function 'keyword?'
euxlValue euxlKeywordp()
{
    static const char *functionName = "keyword?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (euxmKeywordp(arg) ? euxl_true : euxmNil);
}

///  euxlIntegerp - built-in function 'integer?'
euxlValue euxlIntegerp()
{
    static const char *functionName = "integer?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (euxmFPIp(arg) ? euxl_true : euxmNil);
}

///  euxlDoubleFloatp - built-in function 'float?'
euxlValue euxlDoubleFloatp()
{
    static const char *functionName = "float?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (euxmDoubleFloatp(arg) ? euxl_true : euxmNil);
}

///  euxlCharp - built-in function 'char?'
euxlValue euxlCharp()
{
    static const char *functionName = "char?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (euxmCharp(arg) ? euxl_true : euxmNil);
}

///  euxlStringp - built-in function 'string?'
euxlValue euxlStringp()
{
    static const char *functionName = "string?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (euxmStringp(arg) ? euxl_true : euxmNil);
}

///  euxlVectorp - built-in function 'vector?'
euxlValue euxlVectorp()
{
    static const char *functionName = "vector?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (euxmVectorp(arg) ? euxl_true : euxmNil);
}

///  euxlFunctionp - built-in function 'function?'
euxlValue euxlFunctionp()
{
    static const char *functionName = "function?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (isprocedure(arg) ? euxl_true : euxmNil);
}

///  euxlObjectp - built-in function 'object?'
euxlValue euxlObjectp()
{
    static const char *functionName = "object?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (euxmObjectp(arg) ? euxl_true : euxmNil);
}

///  euxlDefaultObjectp - built-in function 'default-object?'
euxlValue euxlDefaultObjectp()
{
    static const char *functionName = "default-object?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (arg == euxl_default_object ? euxl_true : euxmNil);
}

///  euxlEq - built-in function 'eq'
euxlValue euxlEq()
{
    return (eqEqlEqual(euxcEq));
}

///  euxlEqv - built-in function 'eql'
euxlValue euxlEqv()
{
    return (eqEqlEqual(euxcEqv));
}

///  euxlEqual - built-in function 'equal'
euxlValue euxlEqual()
{
    return (eqEqlEqual(euxcEqual));
}

///  eqEqlEqual - common code for eq/eql/equal
static euxlValue eqEqlEqual(int (*fcn) ())
{
    static const char *functionName = "eq/eql/equal";

    euxlValue arg1 = euxmGetArg();
    euxlValue arg2 = euxmGetArg();
    euxmLastArg();
    return ((*fcn) (arg1, arg2) ? euxl_true : euxmNil);
}

///  euxlGensym - generate a symbol
euxlValue euxlGensym()
{
    static const char *functionName = "gensym";

    // get the prefix or number
    if (euxmMoreArgs())
    {
        euxlValue x;
        if ((x = euxmGetArg()) == euxmNil)
        {
            euxcBadType(x, "symbol, string, or integer", functionName);
        }
        else
        {
            switch (euxmNodeType(x))
            {
                case euxmSymbol:
                    x = euxmGetPName(x);
                case euxmString:
                    strncpy(gsprefix, euxmGetString(x), euxmStringMax);
                    gsprefix[euxmStringMax] = '\0';
                    break;
                case euxmFPI:
                    gsnumber = (int)euxmGetFPI(x);
                    break;
                default:
                    euxcBadType(x, "symbol, string, or integer", functionName);
            }
        }
    }
    euxmLastArg();

    // create the pname of the new symbol
    char sym[euxmStringMax + 11];      // enough space for prefix and number
    sprintf(sym, "%s%d", gsprefix, gsnumber++);

    // make a symbol with this print name
    return (euxcMakeSymbol(sym));
}

///  euxlSprintf -- used by format
euxlValue euxlSprintf()
{
    static const char *functionName = "xsprintf";

    euxlValue arg = euxmGetArgString();
    euxlValue ch = euxmGetArgChar();
    euxlValue val = euxmGetArgDoubleFloat();
    euxmLastArg();

    char buf[128], fmt[128];
    sprintf(fmt, "%%%s%c", euxmGetString(arg), euxmGetCharCode(ch));
    sprintf(buf, fmt, euxmGetDoubleFloat(val));

    return euxcMakeString(buf);
}


///-----------------------------------------------------------------------------
