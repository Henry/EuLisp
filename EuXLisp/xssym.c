/// Copyright 1988 David Michael Betz
/// Copyright 1994 Russell Bradford
/// Copyright 2010 Henry G. Weller
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

#include "xscheme.h"

///-----------------------------------------------------------------------------
/// Local variables
///-----------------------------------------------------------------------------
LVAL obarray;

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static LVAL findprop(LVAL sym, LVAL prp), findsyntax(LVAL sym, LVAL prp);

extern LVAL c_dot, s_rename_flag;

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
// xlsubr - define a builtin function
void xlsubr(char *sname, int type, LVAL(*fcn) (void), int offset)
{
    LVAL sym = xlenter(sname);
    setvalue(sym, cvsubr(type, fcn, offset));
}

#if 0
#define trace_symbol(a,ss,m) fprintf(stderr,"<%s:%s:%s>", a, ss, m)
#else
#define trace_symbol(a,s,m)
#endif

LVAL xlenter_keyword(char *name)
{
    int i = hash(name, HSIZE);

    // check if symbol is already in table
    for (LVAL sym = getelement(keyword_array, i); sym; sym = cdr(sym))
    {
        if (strcmp(name, getstring(getpname(car(sym)))) == 0)
        {
            trace_symbol("O", name, "keyword");
            return (car(sym));
        }
    }

    // make a new symbol node and link it into the list
    LVAL symbol = cvsymbol(name);
    setmodule(symbol, NIL);
    setvalue(symbol, symbol);   // self-evaluating

    LVAL sym = cons(symbol, getelement(keyword_array, i));
    setelement(keyword_array, i, sym);

    trace_symbol("N", name, "keyword");

    // return the new symbol
    return (symbol);
}

// xlenter - enter a symbol into the obarray of the given module
LVAL xlenter_module(char *name, LVAL module)
{
    // see if symbol is marked for reinternment
    int len = strlen(name);
    char buf[STRMAX];
    if (len > 0 && name[0] == SYNTAX_OPEN && name[len - 1] == SYNTAX_CLOSE)
    {
        strcpy(buf, name + 1);
        buf[len - 2] = 0;
        return xlenter_module(buf, reintern_module);
    }

    // get the current obarray and the hash index for this symbol
    LVAL array = getmsymbols(module);
    int i = hash(name, HSIZE);

    // check if symbol is already in table
    for (LVAL sym = getelement(array, i); sym; sym = cdr(sym))
    {
        if (strcmp(name, getstring(getpname(car(sym)))) == 0)
        {
            trace_symbol("O", name, getstring(getmname(getmodule(car(sym)))));
            return (car(sym));
        }
    }

    // make a new symbol node
    LVAL symbol = cvsymbol(name);

    // ensure correct home module
    setmodule(symbol, module);

    // link it into the list
    LVAL sym = cons(symbol, getelement(array, i));
    setelement(array, i, sym);

    trace_symbol("N", name, getstring(getmname(module)));

    // return the new symbol
    return (symbol);
}

// xlgetprop - get the value of a property
LVAL xlgetprop(LVAL sym, LVAL prp)
{
    LVAL p;
    return ((p = findprop(sym, prp)) == NIL ? NIL : car(p));
}

// xlputprop - put a property value onto the property list
void xlputprop(LVAL sym, LVAL val, LVAL prp)
{
    LVAL pair;
    if ((pair = findprop(sym, prp)) != NIL)
    {
        rplaca(pair, val);
    }
    else
    {
        setplist(sym, cons(prp, cons(val, getplist(sym))));
    }
}

// findprop - find a property pair
static LVAL findprop(LVAL sym, LVAL prp)
{
    for (LVAL p = getplist(sym); consp(p) && consp(cdr(p)); p = cdr(cdr(p)))
    {
        if (symboleq(car(p), prp))
        {
            return (cdr(p));
        }
    }

    return (NIL);
}

// intern symbol in current module
// can't use symbol name directly as it might move during a GC in xlenter
static LVAL reintern_symbol(LVAL sym)
{
    if (keywordp(sym))
    {
        return sym;
    }

    char buf[STRMAX + 1];
    strcpy(buf, getstring(getpname(sym)));
    return xlenter(buf);
}

// xlgetsyntax - find a syntax property
LVAL xlgetsyntax(LVAL sym, LVAL prp)
{
    // reintern into current module if necessary
    if (getmodule(sym) == reintern_module)
    {
        sym = reintern_symbol(sym);
        // fprintf(stderr, "<re %s %s %s>", getstring(getpname(sym)),
        //         getstring(getmname(current_module)),
        //         getstring(getpname(prp)));
        LVAL p = findsyntax(sym, prp);
        return (p == NIL ? (prp == s_rename_flag ? sym : NIL) : car(p));
    }

    LVAL p = findsyntax(sym, prp);
    return (p == NIL ? NIL : car(p));
}

void xlputsyntax(LVAL sym, LVAL val, LVAL prp)
{
    LVAL pair;
    if ((pair = findsyntax(sym, prp)) != NIL)
    {
        rplaca(pair, val);
    }
    else
    {
        putsyntax(sym, cons(prp, cons(val, getsyntax(sym))));
    }
}

static LVAL findsyntax(LVAL sym, LVAL prp)
{
    for (LVAL p = getsyntax(sym); consp(p) && consp(cdr(p)); p = cdr(cdr(p)))
    {
        if (car(p) == prp)      // only ever %macro, %rename or %syntax
        {
            return (cdr(p));
        }
    }

    return (NIL);
}

// hash - hash a symbol name string
int hash(char *str, int len)
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
