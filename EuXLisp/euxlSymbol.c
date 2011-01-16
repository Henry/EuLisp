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
/// Title: Symbol handling functions
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"

///-----------------------------------------------------------------------------
/// Global variables
///-----------------------------------------------------------------------------
euxlValue euxcObArray;

///-----------------------------------------------------------------------------
/// Macros
///-----------------------------------------------------------------------------
#if 0
#define traceSymbol(a,ss,m) fprintf(stderr,"<%s:%s:%s>", a, ss, m)
#else
#define traceSymbol(a,s,m)
#endif

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static euxlValue findProperty(euxlValue sym, euxlValue prp);
static euxlValue findSyntax(euxlValue sym, euxlValue prp);

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxcXFun - define a builtin eval/apply function
void euxcXFun(const char *sname, int type, euxcXFunType fcn, int offset)
{
    euxlValue sym = euxmEnter(sname);
    euxmSetValue(sym, euxcMakeXFun(type, fcn, offset));
}

///  euxcFun - define a builtin function
void euxcFun(const char *sname, int type, euxcFunType fcn, int offset)
{
    euxlValue sym = euxmEnter(sname);
    euxmSetValue(sym, euxcMakeFun(type, fcn, offset));
}

///  euxcEnterKeyword - enter a keyword
euxlValue euxcEnterKeyword(const char *name)
{
    int i = euxcHash(name, euxmSymbolTableSize);

    // Check if symbol is already in table
    for
    (
        euxlValue sym = euxmGetElement(euxcKeywordArray, i);
        sym;
        sym = euxmCdr(sym)
    )
    {
        if (strcmp(name, euxmGetString(euxmGetPName(euxmCar(sym)))) == 0)
        {
            traceSymbol("O", name, "keyword");
            return (euxmCar(sym));
        }
    }

    // make a new symbol node and link it into the list
    euxlValue symbol = euxcMakeSymbol(name);
    euxmSetModule(symbol, euxmNil);
    euxmSetValue(symbol, symbol);   // self-evaluating

    euxlValue sym = euxcCons(symbol, euxmGetElement(euxcKeywordArray, i));
    euxmSetElement(euxcKeywordArray, i, sym);

    traceSymbol("N", name, "keyword");

    // return the new symbol
    return (symbol);
}

///  euxmEnter - enter a symbol into the euxcObArray of the given module
euxlValue euxcEnterModule(const char *name, euxlValue module)
{
    // see if symbol is marked for reinternment
    int len = strlen(name);
    char buf[euxmStringMax];
    if
    (
        len > 0 && name[0] == euxmSyntaxOpen
     && name[len - 1] == euxmSyntaxClose
    )
    {
        strcpy(buf, name + 1);
        buf[len - 2] = 0;
        return euxcEnterModule(buf, euxcReinternModule);
    }

    // get the current euxcObArray and the euxcHash index for this symbol
    euxlValue array = euxmGetModuleSymbols(module);
    int i = euxcHash(name, euxmSymbolTableSize);

    // Check if symbol is already in table
    for (euxlValue sym = euxmGetElement(array, i); sym; sym = euxmCdr(sym))
    {
        if (strcmp(name, euxmGetString(euxmGetPName(euxmCar(sym)))) == 0)
        {
            traceSymbol
            (
                "O",
                name,
                euxmGetString(euxmGetModuleName(euxmGetModule(euxmCar(sym))))
            );
            return (euxmCar(sym));
        }
    }

    // make a new symbol node
    euxlValue symbol = euxcMakeSymbol(name);

    // ensure correct home module
    euxmSetModule(symbol, module);

    // link it into the list
    euxlValue sym = euxcCons(symbol, euxmGetElement(array, i));
    euxmSetElement(array, i, sym);

    traceSymbol("N", name, euxmGetString(euxmGetModuleName(module)));

    // return the new symbol
    return (symbol);
}

///  euxcGetProp - get the value of a property
euxlValue euxcGetProp(euxlValue sym, euxlValue prp)
{
    euxlValue p;
    return ((p = findProperty(sym, prp)) == euxmNil ? euxmNil : euxmCar(p));
}

///  euxcPutProp - put a property value onto the property list
void euxcPutProp(euxlValue sym, euxlValue val, euxlValue prp)
{
    euxlValue pair;
    if ((pair = findProperty(sym, prp)) != euxmNil)
    {
        euxmSetCar(pair, val);
    }
    else
    {
        euxmSetPList(sym, euxcCons(prp, euxcCons(val, euxmGetPList(sym))));
    }
}

///  findProperty - find a property pair
static euxlValue findProperty(euxlValue sym, euxlValue prp)
{
    for
    (
        euxlValue p = euxmGetPList(sym);
        euxmConsp(p) && euxmConsp(euxmCdr(p));
        p = euxmCdr(euxmCdr(p))
    )
    {
        if (euxmSymbolEq(euxmCar(p), prp))
        {
            return (euxmCdr(p));
        }
    }

    return (euxmNil);
}

///  reinternSymbol - intern symbol in current module
//    can't use symbol name directly as it might move during a GC in euxmEnter
static euxlValue reinternSymbol(euxlValue sym)
{
    if (euxmKeywordp(sym))
    {
        return sym;
    }

    char buf[euxmStringMax + 1];
    strcpy(buf, euxmGetString(euxmGetPName(sym)));
    return euxmEnter(buf);
}

///  euxcGetSyntax - find a syntax property
euxlValue euxcGetSyntax(euxlValue sym, euxlValue prp)
{
    // reintern into current module if necessary
    if (euxmGetModule(sym) == euxcReinternModule)
    {
        sym = reinternSymbol(sym);
        // fprintf(stderr, "<re %s %s %s>", euxmGetString(euxmGetPName(sym)),
        //         euxmGetString(euxmGetModuleName(euxcCurrentModule)),
        //         euxmGetString(euxmGetPName(prp)));
        euxlValue p = findSyntax(sym, prp);
        return
        (
            p == euxmNil
          ? (prp == euxls_rename_flag ? sym : euxmNil)
          : euxmCar(p)
        );
    }

    euxlValue p = findSyntax(sym, prp);
    return (p == euxmNil ? euxmNil : euxmCar(p));
}

///  euxcPutSyntax -
void euxcPutSyntax(euxlValue sym, euxlValue val, euxlValue prp)
{
    euxlValue pair;
    if ((pair = findSyntax(sym, prp)) != euxmNil)
    {
        euxmSetCar(pair, val);
    }
    else
    {
        euxmPutSyntax(sym, euxcCons(prp, euxcCons(val, euxmGetSyntax(sym))));
    }
}

///  findSyntax -
static euxlValue findSyntax(euxlValue sym, euxlValue prp)
{
    for
    (
        euxlValue p = euxmGetSyntax(sym);
        euxmConsp(p) && euxmConsp(euxmCdr(p));
        p = euxmCdr(euxmCdr(p))
    )
    {
        if (euxmCar(p) == prp)      // only ever %macro, %rename or %syntax
        {
            return (euxmCdr(p));
        }
    }

    return (euxmNil);
}

///  euxcHash - euxcHash a symbol name string
int euxcHash(const char *str, int len)
{
    int i;

    for (i = 0; *str;)
    {
        i = (i << 2) ^ *str++;
    }

    i %= len;

    return (i < 0 ? -i : i);
}


///-----------------------------------------------------------------------------
