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
/// Title: euxlisp built-in functions - part 1
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------

#include "euxlisp.h"
#include "euxlsymbols.h"
#include "euxlproto.h"

///-----------------------------------------------------------------------------
/// Gensym variables
///-----------------------------------------------------------------------------
static char gsprefix[STRMAX + 1] = { 'G', 0 };  // gensym prefix string
static int gsnumber = 1;                        // gensym number

///-----------------------------------------------------------------------------
/// External variables
///-----------------------------------------------------------------------------
extern euxlValue xlenv, xlval;

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static euxlValue cxr(char *adstr);
euxlValue member(int (*fcn) (euxlValue a, euxlValue b));
euxlValue assoc(int (*fcn) (euxlValue a, euxlValue b));
static euxlValue nth(int carflag);
static euxlValue vref(euxlValue vector);
static euxlValue vset(euxlValue vector);
static euxlValue eqtest(int (*fcn) (euxlValue a, euxlValue b));

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
// xcons - construct a new list cell
euxlValue xcons()
{
    static char *cfn_name = "cons";

    // get the two arguments
    euxlValue carval = xlgetarg();
    euxlValue cdrval = xlgetarg();
    xllastarg();

    // construct a new cons node
    return (cons(carval, cdrval));
}

// xcar - built-in function 'car'
euxlValue xcar()
{
    static char *cfn_name = "car";

    euxlValue list = xlgacons();
    xllastarg();
    return car(list);
}

// xicar - built-in function '%car'
euxlValue xicar()
{
    static char *cfn_name = "%car";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (car(arg));
}

// xcdr - built-in function 'cdr'
euxlValue xcdr()
{
    static char *cfn_name = "cdr";

    euxlValue arg = xlgacons();
    xllastarg();
    return cdr(arg);
}

// xicdr - built-in function '%cdr'
euxlValue xicdr()
{
    static char *cfn_name = "%cdr";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (cdr(arg));
}

// cxxr functions
euxlValue xcaar()
{
    return (cxr("aa"));
}

euxlValue xcadr()
{
    return (cxr("da"));
}

euxlValue xcdar()
{
    return (cxr("ad"));
}

euxlValue xcddr()
{
    return (cxr("dd"));
}

// cxxxr functions
euxlValue xcaaar()
{
    return (cxr("aaa"));
}

euxlValue xcaadr()
{
    return (cxr("daa"));
}

euxlValue xcadar()
{
    return (cxr("ada"));
}

euxlValue xcaddr()
{
    return (cxr("dda"));
}

euxlValue xcdaar()
{
    return (cxr("aad"));
}

euxlValue xcdadr()
{
    return (cxr("dad"));
}

euxlValue xcddar()
{
    return (cxr("add"));
}

euxlValue xcdddr()
{
    return (cxr("ddd"));
}

// cxxxxr functions
euxlValue xcaaaar()
{
    return (cxr("aaaa"));
}

euxlValue xcaaadr()
{
    return (cxr("daaa"));
}

euxlValue xcaadar()
{
    return (cxr("adaa"));
}

euxlValue xcaaddr()
{
    return (cxr("ddaa"));
}

euxlValue xcadaar()
{
    return (cxr("aada"));
}

euxlValue xcadadr()
{
    return (cxr("dada"));
}

euxlValue xcaddar()
{
    return (cxr("adda"));
}

euxlValue xcadddr()
{
    return (cxr("ddda"));
}

euxlValue xcdaaar()
{
    return (cxr("aaad"));
}

euxlValue xcdaadr()
{
    return (cxr("daad"));
}

euxlValue xcdadar()
{
    return (cxr("adad"));
}

euxlValue xcdaddr()
{
    return (cxr("ddad"));
}

euxlValue xcddaar()
{
    return (cxr("aadd"));
}

euxlValue xcddadr()
{
    return (cxr("dadd"));
}

euxlValue xcdddar()
{
    return (cxr("addd"));
}

euxlValue xcddddr()
{
    return (cxr("dddd"));
}

// cxr - common car/cdr function
static euxlValue cxr(char *adstr)
{
    static char *cfn_name = "c[ad]r";

    // get the list
    euxlValue list = xlgalist();
    xllastarg();
    euxlValue lst = list;
    char *ad = adstr;

    // perform the car/cdr operations
    while (*adstr && consp(list))
    {
        list = (*adstr++ == 'a' ? car(list) : cdr(list));
    }

    // make sure the operation succeeded
    if (*adstr)
    {
        char buf[128];
        sprintf(buf, "c%sr", ad);
        xlbadtype(lst, "a deeper list", buf);
    }

    // return the result
    return (list);
}

// xsetcar - built-in function 'set-car'
euxlValue xsetcar()
{
    static char *cfn_name = "set-car";

    // get the cons and the new car
    euxlValue arg = xlgacons();
    euxlValue newcar = xlgetarg();
    xllastarg();

    // replace the car
    rplaca(arg, newcar);
    return (newcar);
}

// xisetcar - built-in function '%set-car'
euxlValue xisetcar()
{
    static char *cfn_name = "%set-car";

    // get the cons and the new car
    euxlValue arg = xlgetarg();
    euxlValue newcar = xlgetarg();
    xllastarg();

    // replace the car
    rplaca(arg, newcar);
    return (arg);
}

// xsetcdr - built-in function 'set-cdr'
euxlValue xsetcdr()
{
    static char *cfn_name = "set-cdr";

    // get the cons and the new cdr
    euxlValue arg = xlgacons();
    euxlValue newcdr = xlgetarg();
    xllastarg();

    // replace the cdr
    rplacd(arg, newcdr);
    return (newcdr);
}

// xisetcdr - built-in function '%set-cdr'
euxlValue xisetcdr()
{
    static char *cfn_name = "%set-cdr";

    // get the cons and the new cdr
    euxlValue arg = xlgetarg();
    euxlValue newcdr = xlgetarg();
    xllastarg();

    // replace the cdr
    rplacd(arg, newcdr);
    return (arg);
}

// xlist - built-in function 'list'
euxlValue xlist()
{
    // initialize the list
    euxlValue val = NIL;

    // add each argument to the list
    if (moreargs())
    {
        euxlValue last = cons(nextarg(), NIL);
        val = last;
        while (moreargs())
        {
            euxlValue next = nextarg();
            push(val);
            next = cons(next, NIL);
            rplacd(last, next);
            last = next;
            val = pop();
        }
    }

    // return the list
    return (val);
}

// xliststar - built-in function 'list*'
euxlValue xliststar()
{
    // initialize the list
    euxlValue val = NIL;
    euxlValue last = NIL;

    // add each argument to the list
    if (moreargs())
    {
        for (;;)
        {
            euxlValue next = nextarg();
            if (moreargs())
            {
                push(val);
                next = cons(next, NIL);
                val = pop();
                if (val)
                {
                    rplacd(last, next);
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
                    rplacd(last, next);
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

// xappend - built-in function 'append'
euxlValue xappend()
{
    static char *cfn_name = "append";

    // append each argument
    euxlValue last, val;
    for (val = last = NIL; xlargc > 1;)
    {
        // append each element of this list to the result list
        for (euxlValue next = xlgalist(); consp(next); next = cdr(next))
        {
            push(val);
            euxlValue this = cons(car(next), NIL);
            val = pop();
            if (last == NIL)
            {
                val = this;
            }
            else
            {
                rplacd(last, this);
            }
            last = this;
        }
    }

    // tack on the last argument
    if (moreargs())
    {
        if (last == NIL)
        {
            val = xlgetarg();
        }
        else
        {
            rplacd(last, xlgetarg());
        }
    }

    // return the list
    return (val);
}

euxlValue xlreverse(euxlValue next)
{
    cpush(next);

    // append each element of this list to the result list
    euxlValue val;
    for (val = NIL; consp(next); next = cdr(next))
    {
        push(val);
        val = cons(car(next), top());
        drop(1);
    }

    drop(1);

    // return the list
    return (val);
}

// xreverse - built-in function 'reverse'
euxlValue xreverse()
{
    static char *cfn_name = "reverse-list";

    // get the list to reverse
    euxlValue next = xlgalist();
    xllastarg();

    return xlreverse(next);
}

// xlastpair - built-in function 'last-pair'
euxlValue xlastpair()
{
    static char *cfn_name = "last-pair";

    // get the list
    euxlValue list = xlgalist();
    xllastarg();

    // find the last cons
    if (consp(list))
    {
        while (consp(cdr(list)))
        {
            list = cdr(list);
        }
    }

    // return the last element
    return (list);
}

// xsize - built-in function 'list_size'
euxlValue xsize()
{
    static char *cfn_name = "list-size";

    // get the argument
    euxlValue arg = xlgalist();
    xllastarg();

    // find the list_size
    FIXTYPE n;
    for (n = (FIXTYPE) 0; consp(arg); ++n)
    {
        arg = cdr(arg);
    }

    // return the list_size
    return (cvfixnum(n));
}

// xmember - built-in function 'member'
euxlValue xmember()
{
    return (member(equal));
}

// xmemv - built-in function 'memv'
euxlValue xmemv()
{
    return (member(eqv));
}

// xmemq - built-in function 'memq'
euxlValue xmemq()
{
    return (member(eq));
}

// member - common function for member/memv/memq
euxlValue xlmember(euxlValue x, euxlValue list, int (*fcn) (euxlValue a, euxlValue b))
{
    euxlValue val;

    // look for the expression
    for (val = NIL; consp(list); list = cdr(list))
    {
        if ((*fcn) (x, car(list)))
        {
            val = list;
            break;
        }
    }

    // return the result
    return (val);
}

euxlValue member(int (*fcn) ())
{
    static char *cfn_name = "member/memq/memv";

    // get the expression to look for and the list
    euxlValue x = xlgetarg();
    euxlValue list = xlgalist();
    xllastarg();

    return xlmember(x, list, fcn);
}

// xassoc - built-in function 'assoc'
euxlValue xassoc()
{
    return (assoc(equal));
}

// xassv - built-in function 'assv'
euxlValue xassv()
{
    return (assoc(eqv));
}

// xassq - built-in function 'assq'
euxlValue xassq()
{
    return (assoc(eq));
}

// assoc - common function for assoc/assv/assq
euxlValue assoc(int (*fcn) ())
{
    static char *cfn_name = "assoc/assv/assq";

    // get the expression to look for and the association list
    euxlValue x = xlgetarg();
    euxlValue alist = xlgalist();
    xllastarg();

    // look for the expression
    euxlValue val;
    for (val = NIL; consp(alist); alist = cdr(alist))
    {
        euxlValue pair;
        if ((pair = car(alist)) != NIL && consp(pair))
        {
            if ((*fcn) (x, car(pair), fcn))
            {
                val = pair;
                break;
            }
        }
    }

    // return the result
    return (val);
}

// xlistref - built-in function 'list-ref'
euxlValue xlistref()
{
    return (nth(TRUE));
}

// xlisttail - built-in function 'list-tail'
euxlValue xlisttail()
{
    return (nth(FALSE));
}

// nth - internal nth function
static euxlValue nth(int carflag)
{
    static char *cfn_name = "list-ref/list-tail";

    // get n and the list
    euxlValue list = xlgalist();
    euxlValue arg = xlgafixnum();
    xllastarg();

    // range check the index
    int n;
    if ((n = (int)getfixnum(arg)) < 0 || (carflag && list == NIL))
    {
        xlcerror("index out of range in list-ref/list-tail", arg, NIL);
    }

    // find the nth element
    for (; consp(list) && n; n--)
    {
        list = cdr(list);
    }

    // make sure the list was long enough
    if (n)
    {
        xlcerror("index out of range in list-ref/list-tail", arg, NIL);
    }

    // return the list beginning at the nth element
    return (carflag && consp(list) ? car(list) : list);
}

// xboundp - is this a value bound to this symbol?
euxlValue xboundp()
{
    static char *cfn_name = "symbol-exists?";

    euxlValue sym = xlgasymbol();
    xllastarg();
    return (boundp(sym) ? true : NIL);
}

// xsymvalue - get the value of a symbol
euxlValue xsymvalue()
{
    static char *cfn_name = "symbol-value";

    euxlValue sym = xlgasymbol();
    xllastarg();
    return (getvalue(sym));
}

// xsetsymvalue - set the value of a symbol
euxlValue xsetsymvalue()
{
    static char *cfn_name = "set-symbol-value";

    // get the symbol
    euxlValue sym = xlgasymbol();
    euxlValue val = xlgetarg();
    xllastarg();

    // set the global value
    setvalue(sym, val);

    // return its value
    return (val);
}

// xsymplist - get the property list of a symbol
euxlValue xsymplist()
{
    static char *cfn_name = "symbol-plist";

    // get the symbol
    euxlValue sym = xlgasymbol();
    xllastarg();

    // return the property list
    return (getplist(sym));
}

// xsetsymplist - set the property list of a symbol
euxlValue xsetsymplist()
{
    static char *cfn_name = "set-symbol-plist";

    // get the symbol
    euxlValue sym = xlgasymbol();
    euxlValue val = xlgetarg();
    xllastarg();

    // set the property list
    setplist(sym, val);
    return (val);
}

// xget - get the value of a property
euxlValue xget()
{
    static char *cfn_name = "get";

    // get the symbol and property
    euxlValue sym = xlgasymbol();
    euxlValue prp = xlgasymbol();
    xllastarg();

    // retrieve the property value
    return (xlgetprop(sym, prp));
}

// xput - set the value of a property
euxlValue xput()
{
    static char *cfn_name = "put";

    // get the symbol and property
    euxlValue sym = xlgasymbol();
    euxlValue prp = xlgasymbol();
    euxlValue val = xlgetarg();
    xllastarg();

    // set the property value
    xlputprop(sym, val, prp);

    // return the value
    return (val);
}

// xgetsyntax - symbol syntax
euxlValue xgetsyntax()
{
    static char *cfn_name = "get-syntax";

    // get the symbol and property
    euxlValue sym = xlgasymbol();
    euxlValue prp = xlgasymbol();
    xllastarg();

    // retrieve the syntax value
    return (xlgetsyntax(sym, prp));
}

// xput - set symbol syntax
euxlValue xputsyntax()
{
    static char *cfn_name = "put-syntax";

    // get the symbol and property
    euxlValue sym = xlgasymbol();
    euxlValue prp = xlgasymbol();
    euxlValue val = xlgetarg();
    xllastarg();

    // set the syntax value
    xlputsyntax(sym, val, prp);

    // return the value
    return (val);
}

// xtheenvironment - built-in function 'the-environment'
euxlValue xtheenvironment()
{
    static char *cfn_name = "the-environment";
    xllastarg();
    return (xlenv);
}

// xprocenvironment - built-in function 'procedure-environment'
euxlValue xprocenvironment()
{
    static char *cfn_name = "procedure-environment";

    euxlValue arg = xlgaclosure();
    xllastarg();
    return (getcenv(arg));
}

// xenvp - built-in function 'environment?'
euxlValue xenvp()
{
    static char *cfn_name = "environment?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (envp(arg) ? true : NIL);
}

// xenvbindings - built-in function 'environment-bindings'
euxlValue xenvbindings()
{
    static char *cfn_name = "environment-bindings";

    // get the environment
    euxlValue env = xlgetarg();
    xllastarg();

    // check the argument type
    if (closurep(env))
    {
        env = getcenv(env);
    }
    else if (!envp(env))
    {
        xlbadtype(env, "<env>", cfn_name);
    }

    // initialize
    euxlValue frame = car(env);
    euxlValue names = getelement(frame, 0);
    int len = getsize(frame);
    check(1);

    // build a list of dotted pairs
    euxlValue val, last;
    int i;
    for (val = last = NIL, i = 1; i < len; ++i, names = cdr(names))
    {
        push(val);
        euxlValue this = cons(cons(car(names), getelement(frame, i)), NIL);
        val = pop();
        if (last)
        {
            rplacd(last, this);
        }
        else
        {
            val = this;
        }
        last = this;
    }
    return (val);
}

// xenvparent - built-in function 'environment-parent'
euxlValue xenvparent()
{
    static char *cfn_name = "environment-parent";

    euxlValue env = xlgaenv();
    xllastarg();
    return (cdr(env));
}

// xvector - built-in function 'vector'
euxlValue xvector()
{
    static char *cfn_name = "vector";

    euxlValue vect = newvector(xlargc);
    for (euxlValue *p = &vect->n_vdata[0]; moreargs();)
    {
        *p++ = xlgetarg();
    }
    return (vect);
}

// xmakevector - built-in function 'make-vector'
euxlValue xmakevector()
{
    static char *cfn_name = "make-vector";

    // get the vector size
    euxlValue arg = xlgafixnum();
    int len = (int)getfixnum(arg);

    if (len < 0)
    {
        xlcerror("bad size for make-vector", arg, NIL);
    }

    // check for an initialization value
    euxlValue val;
    if (moreargs())
    {
        arg = xlgetarg();       // get the initializer
        xllastarg();            // make sure that's the last argument
        cpush(arg);             // save the initializer
        val = newvector(len);   // create the vector
        euxlValue *p = &val->n_vdata[0];   // initialize the vector
        for (arg = pop(); --len >= 0;)
        {
            *p++ = arg;
        }
    }
    else // no initialization value
    {
        val = newvector(len);   // defaults to initializing to NIL
    }

    // return the new vector
    return (val);
}

// xvsize - built-in function 'vector-size'
euxlValue xvsize()
{
    static char *cfn_name = "vector-size";

    euxlValue arg = xlgavector();
    xllastarg();
    return (cvfixnum((FIXTYPE) getsize(arg)));
}

// xivsize - built-in function '%vector-size'
euxlValue xivsize()
{
    static char *cfn_name = "%vector-size";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (cvfixnum((FIXTYPE) getsize(arg)));
}

// xvref - built-in function 'vector-ref'
euxlValue xvref()
{
    static char *cfn_name = "vector-ref";
    return (vref(xlgavector()));
}

// xivref - built-in function '%vector-ref'
euxlValue xivref()
{
    static char *cfn_name = "%vector-ref";
    return (vref(xlgetarg()));
}

// vref - common code for xvref and xivref
static euxlValue vref(euxlValue vector)
{
    static char *cfn_name = "vector-ref";

    // get the index
    euxlValue index = xlgafixnum();
    xllastarg();

    // range check the index
    int i;
    if ((i = (int)getfixnum(index)) < 0 || i >= getsize(vector))
    {
        xlcerror("index out of range in vector-ref", index, NIL);
    }

    // return the vector element
    return (getelement(vector, i));
}

// xvset - built-in function 'vector-set'
euxlValue xvset()
{
    static char *cfn_name = "vector-set";
    return (vset(xlgavector()));
}

// xivset - built-in function '%vector-set'
euxlValue xivset()
{
    static char *cfn_name = "%vector-set";
    return (vset(xlgetarg()));
}

// vset - common code for xvset and xivset
static euxlValue vset(euxlValue vector)
{
    static char *cfn_name = "vector-set";

    // get the index and the new value
    euxlValue index = xlgafixnum();
    euxlValue val = xlgetarg();
    xllastarg();

    // range check the index
    int i;
    if ((i = (int)getfixnum(index)) < 0 || i >= getsize(vector))
    {
        xlcerror("index out of range in vector-set", index, NIL);
    }

    // set the vector element and return the value
    setelement(vector, i, val);
    return (val);
}

// xvectlist - built-in function 'vector->list'
euxlValue xvectlist()
{
    static char *cfn_name = "vector->list";

    // get the vector
    euxlValue vect = xlgavector();
    xllastarg();

    // make a list from the vector
    cpush(vect);
    int size = getsize(vect);
    for (xlval = NIL; --size >= 0;)
    {
        xlval = cons(getelement(vect, size), xlval);
    }
    drop(1);
    return (xlval);
}

// xlistvect - built-in function 'list->vector'
euxlValue xlistvect()
{
    static char *cfn_name = "list->vector";

    // get the list
    xlval = xlgalist();
    xllastarg();

    // make a vector from the list
    int size = list_size(xlval);
    euxlValue vect = newvector(size);
    for (euxlValue *p = &vect->n_vdata[0]; --size >= 0; xlval = cdr(xlval))
    {
        *p++ = car(xlval);
    }

    return (vect);
}

// xnullp - built-in function 'null?'
euxlValue xnullp()
{
    static char *cfn_name = "null?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (null(arg) ? true : NIL);
}

// xatomp - built-in function 'atom?'
euxlValue xatomp()
{
    static char *cfn_name = "atom?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (atom(arg) ? true : NIL);
}

// xlistp - built-in function 'list?'
euxlValue xlistp()
{
    static char *cfn_name = "list?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (listp(arg) ? true : NIL);
}

// xnumberp - built-in function 'number?'
euxlValue xnumberp()
{
    static char *cfn_name = "number?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (numberp(arg) ? true : NIL);
}

// xbooleanp - built-in function 'boolean?'
euxlValue xbooleanp()
{
    static char *cfn_name = "boolean?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (arg == true || arg == NIL ? true : NIL);
}

// xconsp - built-in function 'cons?'
euxlValue xconsp()
{
    static char *cfn_name = "cons?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (consp(arg) ? true : NIL);
}

// xsymbolp - built-in function 'symbol?'
euxlValue xsymbolp()
{
    static char *cfn_name = "symbol?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (symbolp(arg) ? true : NIL);
}

// xkeywordp - built-in function 'keyword?'
euxlValue xkeywordp()
{
    static char *cfn_name = "keyword?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (keywordp(arg) ? true : NIL);
}

// xintegerp - built-in function 'integer?'
euxlValue xintegerp()
{
    static char *cfn_name = "integer?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (fixp(arg) ? true : NIL);
}

// xfloatp - built-in function 'float?'
euxlValue xfloatp()
{
    static char *cfn_name = "float?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (floatp(arg) ? true : NIL);
}

// xcharp - built-in function 'char?'
euxlValue xcharp()
{
    static char *cfn_name = "char?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (charp(arg) ? true : NIL);
}

// xstringp - built-in function 'string?'
euxlValue xstringp()
{
    static char *cfn_name = "string?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (stringp(arg) ? true : NIL);
}

// xvectorp - built-in function 'vector?'
euxlValue xvectorp()
{
    static char *cfn_name = "vector?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (vectorp(arg) ? true : NIL);
}

#define isprocedure(x)                                                         \
    (closurep(x) || continuationp(x) || subrp(x) || xsubrp(x) || genericp(x))

// xfunctionp - built-in function 'function?'
euxlValue xfunctionp()
{
    static char *cfn_name = "function?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (isprocedure(arg) ? true : NIL);
}

// xobjectp - built-in function 'object?'
euxlValue xobjectp()
{
    static char *cfn_name = "object?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (objectp(arg) ? true : NIL);
}

// xdefaultobjectp - built-in function 'default-object?'
euxlValue xdefaultobjectp()
{
    static char *cfn_name = "default-object?";

    euxlValue arg = xlgetarg();
    xllastarg();
    return (arg == default_object ? true : NIL);
}

// xeq - built-in function 'eq'
euxlValue xeq()
{
    return (eqtest(eq));
}

// xeqv - built-in function 'eql'
euxlValue xeqv()
{
    return (eqtest(eqv));
}

// xequal - built-in function 'equal'
euxlValue xequal()
{
    return (eqtest(equal));
}

// eqtest - common code for eq/eql/equal
static euxlValue eqtest(int (*fcn) ())
{
    static char *cfn_name = "eq/eql/equal";

    euxlValue arg1 = xlgetarg();
    euxlValue arg2 = xlgetarg();
    xllastarg();
    return ((*fcn) (arg1, arg2) ? true : NIL);
}

// xgensym - generate a symbol
euxlValue xgensym()
{
    static char *cfn_name = "gensym";

    // get the prefix or number
    if (moreargs())
    {
        euxlValue x;
        if ((x = xlgetarg()) == NIL)
        {
            xlbadtype(x, "symbol, string, or integer", cfn_name);
        }
        else
        {
            switch (ntype(x))
            {
                case SYMBOL:
                    x = getpname(x);
                case STRING:
                    strncpy(gsprefix, getstring(x), STRMAX);
                    gsprefix[STRMAX] = '\0';
                    break;
                case FIXNUM:
                    gsnumber = (int)getfixnum(x);
                    break;
                default:
                    xlbadtype(x, "symbol, string, or integer", cfn_name);
            }
        }
    }
    xllastarg();

    // create the pname of the new symbol
    char sym[STRMAX + 11];      // enough space for prefix and number
    sprintf(sym, "%s%d", gsprefix, gsnumber++);

    // make a symbol with this print name
    return (cvsymbol(sym));
}

// xsprintf -- used by format
euxlValue xsprintf()
{
    static char *cfn_name = "xsprintf";

    euxlValue arg = xlgastring();
    euxlValue ch = xlgachar();
    euxlValue val = xlgafloat();
    xllastarg();

    char buf[128], fmt[128];
    sprintf(fmt, "%%%s%c", getstring(arg), getchcode(ch));
    sprintf(buf, fmt, getflonum(val));

    return cvstring(buf);
}


///-----------------------------------------------------------------------------
