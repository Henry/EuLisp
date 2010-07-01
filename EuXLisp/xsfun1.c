// xsfun1.c - xscheme built-in functions - part 1
/*     Copyright (c) 1988, by David Michael Betz
       All Rights Reserved */
// Euscheme code Copyright (c) 1994 Russell Bradford

#include "xscheme.h"

// gensym variables
static char gsprefix[STRMAX + 1] = { 'G', 0 };  // gensym prefix string

static int gsnumber = 1;        // gensym number

#include "xssymbols.h"
#include "xsproto.h"

// external variables
extern LVAL xlenv, xlval;

// forward declarations
static LVAL cxr(char *adstr);
LVAL member(int (*fcn) (LVAL a, LVAL b));
LVAL assoc(int (*fcn) (LVAL a, LVAL b));
static LVAL nth(int carflag);
static LVAL vref(LVAL vector);
static LVAL vset(LVAL vector);
static LVAL eqtest(int (*fcn) (LVAL a, LVAL b));

// xcons - construct a new list cell
LVAL xcons()
{
    static char *cfn_name = "cons";
    LVAL carval, cdrval;

    // get the two arguments
    carval = xlgetarg();
    cdrval = xlgetarg();
    xllastarg();

    // construct a new cons node
    return (cons(carval, cdrval));
}

// xcar - built-in function 'car'
LVAL xcar()
{
    static char *cfn_name = "car";
    LVAL list;
    list = xlgacons();
    xllastarg();
    return car(list);
}

// xicar - built-in function '%car'
LVAL xicar()
{
    static char *cfn_name = "%CAR";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (car(arg));
}

// xcdr - built-in function 'cdr'
LVAL xcdr()
{
    static char *cfn_name = "cdr";
    LVAL arg;
    arg = xlgacons();
    xllastarg();
    return cdr(arg);
}

// xicdr - built-in function '%cdr'
LVAL xicdr()
{
    static char *cfn_name = "%CDR";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (cdr(arg));
}

// cxxr functions
LVAL xcaar()
{
    return (cxr("aa"));
}

LVAL xcadr()
{
    return (cxr("da"));
}

LVAL xcdar()
{
    return (cxr("ad"));
}

LVAL xcddr()
{
    return (cxr("dd"));
}

// cxxxr functions
LVAL xcaaar()
{
    return (cxr("aaa"));
}

LVAL xcaadr()
{
    return (cxr("daa"));
}

LVAL xcadar()
{
    return (cxr("ada"));
}

LVAL xcaddr()
{
    return (cxr("dda"));
}

LVAL xcdaar()
{
    return (cxr("aad"));
}

LVAL xcdadr()
{
    return (cxr("dad"));
}

LVAL xcddar()
{
    return (cxr("add"));
}

LVAL xcdddr()
{
    return (cxr("ddd"));
}

// cxxxxr functions
LVAL xcaaaar()
{
    return (cxr("aaaa"));
}

LVAL xcaaadr()
{
    return (cxr("daaa"));
}

LVAL xcaadar()
{
    return (cxr("adaa"));
}

LVAL xcaaddr()
{
    return (cxr("ddaa"));
}

LVAL xcadaar()
{
    return (cxr("aada"));
}

LVAL xcadadr()
{
    return (cxr("dada"));
}

LVAL xcaddar()
{
    return (cxr("adda"));
}

LVAL xcadddr()
{
    return (cxr("ddda"));
}

LVAL xcdaaar()
{
    return (cxr("aaad"));
}

LVAL xcdaadr()
{
    return (cxr("daad"));
}

LVAL xcdadar()
{
    return (cxr("adad"));
}

LVAL xcdaddr()
{
    return (cxr("ddad"));
}

LVAL xcddaar()
{
    return (cxr("aadd"));
}

LVAL xcddadr()
{
    return (cxr("dadd"));
}

LVAL xcdddar()
{
    return (cxr("addd"));
}

LVAL xcddddr()
{
    return (cxr("dddd"));
}

// cxr - common car/cdr routine
static LVAL cxr(char *adstr)
{
    static char *cfn_name = "c[ad]r";
    LVAL list, lst;
    char *ad;

    // get the list
    list = xlgalist();
    xllastarg();
    lst = list;
    ad = adstr;

    // perform the car/cdr operations
    while (*adstr && consp(list))
        list = (*adstr++ == 'a' ? car(list) : cdr(list));

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

// xsetcar - built-in function 'set-car!'
LVAL xsetcar()
{
    static char *cfn_name = "set-car!";
    LVAL arg, newcar;

    // get the cons and the new car
    arg = xlgacons();
    newcar = xlgetarg();
    xllastarg();

    // replace the car
    rplaca(arg, newcar);
    return (arg);
}

// xisetcar - built-in function '%set-car!'
LVAL xisetcar()
{
    static char *cfn_name = "%SET-CAR!";
    LVAL arg, newcar;

    // get the cons and the new car
    arg = xlgetarg();
    newcar = xlgetarg();
    xllastarg();

    // replace the car
    rplaca(arg, newcar);
    return (arg);
}

// xsetcdr - built-in function 'set-cdr!'
LVAL xsetcdr()
{
    static char *cfn_name = "set-cdr!";
    LVAL arg, newcdr;

    // get the cons and the new cdr
    arg = xlgacons();
    newcdr = xlgetarg();
    xllastarg();

    // replace the cdr
    rplacd(arg, newcdr);
    return (arg);
}

// xisetcdr - built-in function '%set-cdr!'
LVAL xisetcdr()
{
    static char *cfn_name = "%SET-CDR!";
    LVAL arg, newcdr;

    // get the cons and the new cdr
    arg = xlgetarg();
    newcdr = xlgetarg();
    xllastarg();

    // replace the cdr
    rplacd(arg, newcdr);
    return (arg);
}

// xlist - built-in function 'list'
LVAL xlist()
{
    LVAL last, next, val;

    // initialize the list
    val = NIL;

    // add each argument to the list
    if (moreargs())
    {
        val = last = cons(nextarg(), NIL);
        while (moreargs())
        {
            next = nextarg();
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
LVAL xliststar()
{
    LVAL last, next, val;

    // initialize the list
    val = last = NIL;

    // add each argument to the list
    if (moreargs())
    {
        for (;;)
        {
            next = nextarg();
            if (moreargs())
            {
                push(val);
                next = cons(next, NIL);
                val = pop();
                if (val)
                    rplacd(last, next);
                else
                    val = next;
                last = next;
            }
            else
            {
                if (val)
                    rplacd(last, next);
                else
                    val = next;
                break;
            }
        }
    }

    // return the list
    return (val);
}

// xappend - built-in function 'append'
LVAL xappend()
{
    static char *cfn_name = "append";
    LVAL next, this, last, val;

    // append each argument
    for (val = last = NIL; xlargc > 1;)

        // append each element of this list to the result list
        for (next = xlgalist(); consp(next); next = cdr(next))
        {
            push(val);
            this = cons(car(next), NIL);
            val = pop();
            if (last == NIL)
                val = this;
            else
                rplacd(last, this);
            last = this;
        }

    // tack on the last argument
    if (moreargs())
    {
        if (last == NIL)
            val = xlgetarg();
        else
            rplacd(last, xlgetarg());
    }

    // return the list
    return (val);
}

LVAL xlreverse(LVAL next)
{
    LVAL val;

    cpush(next);

    // append each element of this list to the result list
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
LVAL xreverse()
{
    static char *cfn_name = "reverse-list";
    LVAL next;

    // get the list to reverse
    next = xlgalist();
    xllastarg();

    return xlreverse(next);
}

// xlastpair - built-in function 'last-pair'
LVAL xlastpair()
{
    static char *cfn_name = "last-pair";
    LVAL list;

    // get the list
    list = xlgalist();
    xllastarg();

    // find the last cons
    if (consp(list))
        while (consp(cdr(list)))
            list = cdr(list);

    // return the last element
    return (list);
}

// xlength - built-in function 'length'
LVAL xlength()
{
    static char *cfn_name = "list-length";
    FIXTYPE n;
    LVAL arg;

    // get the argument
    arg = xlgalist();
    xllastarg();

    // find the length
    for (n = (FIXTYPE) 0; consp(arg); ++n)
        arg = cdr(arg);

    // return the length
    return (cvfixnum(n));
}

// xmember - built-in function 'member'
LVAL xmember()
{
    return (member(equal));
}

// xmemv - built-in function 'memv'
LVAL xmemv()
{
    return (member(eqv));
}

// xmemq - built-in function 'memq'
LVAL xmemq()
{
    return (member(eq));
}

// member - common routine for member/memv/memq
LVAL xlmember(LVAL x, LVAL list, int (*fcn) (LVAL a, LVAL b))
{
    LVAL val;

    // look for the expression
    for (val = NIL; consp(list); list = cdr(list))
        if ((*fcn) (x, car(list)))
        {
            val = list;
            break;
        }

    // return the result
    return (val);
}

LVAL member(int (*fcn) ())
{
    static char *cfn_name = "member/memq/memv";
    LVAL x, list;

    // get the expression to look for and the list
    x = xlgetarg();
    list = xlgalist();
    xllastarg();

    return xlmember(x, list, fcn);
}

// xassoc - built-in function 'assoc'
LVAL xassoc()
{
    return (assoc(equal));
}

// xassv - built-in function 'assv'
LVAL xassv()
{
    return (assoc(eqv));
}

// xassq - built-in function 'assq'
LVAL xassq()
{
    return (assoc(eq));
}

// assoc - common routine for assoc/assv/assq
LVAL assoc(int (*fcn) ())
{
    static char *cfn_name = "assoc/assv/assq";
    LVAL x, alist, pair, val;

    // get the expression to look for and the association list
    x = xlgetarg();
    alist = xlgalist();
    xllastarg();

    // look for the expression
    for (val = NIL; consp(alist); alist = cdr(alist))
        if ((pair = car(alist)) != NIL && consp(pair))
            if ((*fcn) (x, car(pair), fcn))
            {
                val = pair;
                break;
            }

    // return the result
    return (val);
}

// xlistref - built-in function 'list-ref'
LVAL xlistref()
{
    return (nth(TRUE));
}

// xlisttail - built-in function 'list-tail'
LVAL xlisttail()
{
    return (nth(FALSE));
}

// nth - internal nth function
static LVAL nth(int carflag)
{
    static char *cfn_name = "list-ref/list-tail";
    LVAL list, arg;
    int n;

    // get n and the list
    list = xlgalist();
    arg = xlgafixnum();
    xllastarg();

    // range check the index
    if ((n = (int)getfixnum(arg)) < 0 || (carflag && list == NIL))
        xlcerror("index out of range in list-ref/list-tail", arg, NIL);

    // find the nth element
    for (; consp(list) && n; n--)
        list = cdr(list);

    // make sure the list was long enough
    if (n)
        xlcerror("index out of range in list-ref/list-tail", arg, NIL);

    // return the list beginning at the nth element
    return (carflag && consp(list) ? car(list) : list);
}

// xboundp - is this a value bound to this symbol?
LVAL xboundp()
{
    static char *cfn_name = "bound?";
    LVAL sym;
    sym = xlgasymbol();
    xllastarg();
    return (boundp(sym) ? true : NIL);
}

// xsymvalue - get the value of a symbol
LVAL xsymvalue()
{
    static char *cfn_name = "symbol-value";
    LVAL sym;
    sym = xlgasymbol();
    xllastarg();
    return (getvalue(sym));
}

// xsetsymvalue - set the value of a symbol
LVAL xsetsymvalue()
{
    static char *cfn_name = "set-symbol-value!";
    LVAL sym, val;

    // get the symbol
    sym = xlgasymbol();
    val = xlgetarg();
    xllastarg();

    // set the global value
    setvalue(sym, val);

    // return its value
    return (val);
}

// xsymplist - get the property list of a symbol
LVAL xsymplist()
{
    static char *cfn_name = "symbol-plist";
    LVAL sym;

    // get the symbol
    sym = xlgasymbol();
    xllastarg();

    // return the property list
    return (getplist(sym));
}

// xsetsymplist - set the property list of a symbol
LVAL xsetsymplist()
{
    static char *cfn_name = "set-symbol-plist!";
    LVAL sym, val;

    // get the symbol
    sym = xlgasymbol();
    val = xlgetarg();
    xllastarg();

    // set the property list
    setplist(sym, val);
    return (val);
}

// xget - get the value of a property
LVAL xget()
{
    static char *cfn_name = "get";
    LVAL sym, prp;

    // get the symbol and property
    sym = xlgasymbol();
    prp = xlgasymbol();
    xllastarg();

    // retrieve the property value
    return (xlgetprop(sym, prp));
}

// xput - set the value of a property
LVAL xput()
{
    static char *cfn_name = "put";
    LVAL sym, val, prp;

    // get the symbol and property
    sym = xlgasymbol();
    prp = xlgasymbol();
    val = xlgetarg();
    xllastarg();

    // set the property value
    xlputprop(sym, val, prp);

    // return the value
    return (val);
}

// xgetsyntax - symbol macro
LVAL xgetsyntax()
{
    static char *cfn_name = "get-syntax";
    LVAL sym, prp;

    // get the symbol and property
    sym = xlgasymbol();
    prp = xlgasymbol();
    xllastarg();

    // retrieve the syntax value
    return (xlgetsyntax(sym, prp));
}

// xput - set symbol macro
LVAL xputsyntax()
{
    static char *cfn_name = "put-syntax";
    LVAL sym, val, prp;

    // get the symbol and property
    sym = xlgasymbol();
    prp = xlgasymbol();
    val = xlgetarg();
    xllastarg();

    // set the syntax value
    xlputsyntax(sym, val, prp);

    // return the value
    return (val);
}

// xtheenvironment - built-in function 'the-environment'
LVAL xtheenvironment()
{
    static char *cfn_name = "the-environment";
    xllastarg();
    return (xlenv);
}

// xprocenvironment - built-in function 'procedure-environment'
LVAL xprocenvironment()
{
    static char *cfn_name = "procedure-environment";
    LVAL arg;
    arg = xlgaclosure();
    xllastarg();
    return (getcenv(arg));
}

// xenvp - built-in function 'environment?'
LVAL xenvp()
{
    static char *cfn_name = "environment?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (envp(arg) ? true : NIL);
}

// xenvbindings - built-in function 'environment-bindings'
LVAL xenvbindings()
{
    static char *cfn_name = "environment-bindings";
    LVAL env, frame, names, val, this, last;
    int len, i;

    // get the environment
    env = xlgetarg();
    xllastarg();

    // check the argument type
    if (closurep(env))
        env = getcenv(env);
    else if (!envp(env))
        xlbadtype(env, "<env>", cfn_name);

    // initialize
    frame = car(env);
    names = getelement(frame, 0);
    len = getsize(frame);
    check(1);

    // build a list of dotted pairs
    for (val = last = NIL, i = 1; i < len; ++i, names = cdr(names))
    {
        push(val);
        this = cons(cons(car(names), getelement(frame, i)), NIL);
        val = pop();
        if (last)
            rplacd(last, this);
        else
            val = this;
        last = this;
    }
    return (val);
}

// xenvparent - built-in function 'environment-parent'
LVAL xenvparent()
{
    static char *cfn_name = "environment-parent";
    LVAL env;
    env = xlgaenv();
    xllastarg();
    return (cdr(env));
}

// xvector - built-in function 'vector'
LVAL xvector()
{
    static char *cfn_name = "vector";
    LVAL vect, *p;
    vect = newvector(xlargc);
    for (p = &vect->n_vdata[0]; moreargs();)
        *p++ = xlgetarg();
    return (vect);
}

// xmakevector - built-in function 'make-vector'
LVAL xmakevector()
{
    static char *cfn_name = "make-vector";
    LVAL val, *p;

    // get the vector size
    LVAL arg = xlgafixnum();
    int len = (int)getfixnum(arg);

    if (len < 0)
    {
        xlcerror("bad length for make-vector", arg, NIL);
    }

    // check for an initialization value
    if (moreargs())
    {
        arg = xlgetarg();       // get the initializer
        xllastarg();            // make sure that's the last argument
        cpush(arg);             // save the initializer
        val = newvector(len);   // create the vector
        p = &val->n_vdata[0];   // initialize the vector
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

// xvlength - built-in function 'vector-length'
LVAL xvlength()
{
    static char *cfn_name = "vector-length";
    LVAL arg;
    arg = xlgavector();
    xllastarg();
    return (cvfixnum((FIXTYPE) getsize(arg)));
}

// xivlength - built-in function '%vector-length'
LVAL xivlength()
{
    static char *cfn_name = "%VECTOR-LENGTH";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (cvfixnum((FIXTYPE) getsize(arg)));
}

// xvref - built-in function 'vector-ref'
LVAL xvref()
{
    static char *cfn_name = "vector-ref";
    return (vref(xlgavector()));
}

// xivref - built-in function '%vector-ref'
LVAL xivref()
{
    static char *cfn_name = "%VECTOR-REF";
    return (vref(xlgetarg()));
}

// vref - common code for xvref and xivref
static LVAL vref(LVAL vector)
{
    static char *cfn_name = "vector-ref";
    LVAL index;
    int i;

    // get the index
    index = xlgafixnum();
    xllastarg();

    // range check the index
    if ((i = (int)getfixnum(index)) < 0 || i >= getsize(vector))
        xlcerror("index out of range in vector-ref", index, NIL);

    // return the vector element
    return (getelement(vector, i));
}

// xvset - built-in function 'vector-set!'
LVAL xvset()
{
    static char *cfn_name = "vector-set!";
    return (vset(xlgavector()));
}

// xivset - built-in function '%vector-set!'
LVAL xivset()
{
    static char *cfn_name = "%VECTOR-SET!";
    return (vset(xlgetarg()));
}

// vset - common code for xvset and xivset
static LVAL vset(LVAL vector)
{
    static char *cfn_name = "vector-set!";
    LVAL index, val;
    int i;

    // get the index and the new value
    index = xlgafixnum();
    val = xlgetarg();
    xllastarg();

    // range check the index
    if ((i = (int)getfixnum(index)) < 0 || i >= getsize(vector))
        xlcerror("index out of range in vector-set!", index, NIL);

    // set the vector element and return the value
    setelement(vector, i, val);
    return (val);
}

// xvectlist - built-in function 'vector->list'
LVAL xvectlist()
{
    static char *cfn_name = "vector->list";
    LVAL vect;
    int size;

    // get the vector
    vect = xlgavector();
    xllastarg();

    // make a list from the vector
    cpush(vect);
    size = getsize(vect);
    for (xlval = NIL; --size >= 0;)
        xlval = cons(getelement(vect, size), xlval);
    drop(1);
    return (xlval);
}

// xlistvect - built-in function 'list->vector'
LVAL xlistvect()
{
    static char *cfn_name = "list->vector";
    LVAL vect, *p;
    int size;

    // get the list
    xlval = xlgalist();
    xllastarg();

    // make a vector from the list
    size = length(xlval);
    vect = newvector(size);
    for (p = &vect->n_vdata[0]; --size >= 0; xlval = cdr(xlval))
        *p++ = car(xlval);
    return (vect);
}

// xnullp - built-in function 'null?'
LVAL xnullp()
{
    static char *cfn_name = "null?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (null(arg) ? true : NIL);
}

// xatomp - built-in function 'atom?'
LVAL xatomp()
{
    static char *cfn_name = "atom?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (atom(arg) ? true : NIL);
}

// xlistp - built-in function 'list?'
LVAL xlistp()
{
    static char *cfn_name = "list?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (listp(arg) ? true : NIL);
}

// xnumberp - built-in function 'number?'
LVAL xnumberp()
{
    static char *cfn_name = "number?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (numberp(arg) ? true : NIL);
}

// xbooleanp - built-in function 'boolean?'
LVAL xbooleanp()
{
    static char *cfn_name = "boolean?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (arg == true || arg == NIL ? true : NIL);
}

// xpairp - built-in function 'pair?'
LVAL xpairp()
{
    static char *cfn_name = "pair?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (consp(arg) ? true : NIL);
}

// xsymbolp - built-in function 'symbol?'
LVAL xsymbolp()
{
    static char *cfn_name = "symbol?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (symbolp(arg) ? true : NIL);
}

// xkeywordp - built-in function 'keyword?'
LVAL xkeywordp()
{
    static char *cfn_name = "keyword?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (keywordp(arg) ? true : NIL);
}

// xintegerp - built-in function 'integer?'
LVAL xintegerp()
{
    static char *cfn_name = "integer?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (fixp(arg) ? true : NIL);
}

// xrealp - built-in function 'real?'
LVAL xrealp()
{
    static char *cfn_name = "real?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (floatp(arg) ? true : NIL);
}

// xcharp - built-in function 'char?'
LVAL xcharp()
{
    static char *cfn_name = "char?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (charp(arg) ? true : NIL);
}

// xstringp - built-in function 'string?'
LVAL xstringp()
{
    static char *cfn_name = "real?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (stringp(arg) ? true : NIL);
}

// xvectorp - built-in function 'vector?'
LVAL xvectorp()
{
    static char *cfn_name = "vector?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (vectorp(arg) ? true : NIL);
}

#define isprocedure(x)                                                         \
    (closurep(x) || continuationp(x) || subrp(x) || xsubrp(x) || genericp(x))

// xprocedurep - built-in function 'procedure?'
LVAL xprocedurep()
{
    static char *cfn_name = "procedure?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (isprocedure(arg) ? true : NIL);
}

// xobjectp - built-in function 'object?'
LVAL xobjectp()
{
    static char *cfn_name = "object?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (objectp(arg) ? true : NIL);
}

// xdefaultobjectp - built-in function 'default-object?'
LVAL xdefaultobjectp()
{
    static char *cfn_name = "default-object?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (arg == default_object ? true : NIL);
}

// xeq - built-in function 'eq?'
LVAL xeq()
{
    return (eqtest(eq));
}

// xeqv - built-in function 'eqv?'
LVAL xeqv()
{
    return (eqtest(eqv));
}

// xequal - built-in function 'equal?'
LVAL xequal()
{
    return (eqtest(equal));
}

// eqtest - common code for eq?/eqv?/equal?
static LVAL eqtest(int (*fcn) ())
{
    static char *cfn_name = "eq?/eqv?/equal?";
    LVAL arg1, arg2;
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();
    return ((*fcn) (arg1, arg2) ? true : NIL);
}

// xgensym - generate a symbol
LVAL xgensym()
{
    static char *cfn_name = "gensym";
    char sym[STRMAX + 11];      // enough space for prefix and number
    LVAL x;

    // get the prefix or number
    if (moreargs())
    {
        if ((x = xlgetarg()) == NIL)
            xlbadtype(x, "symbol, string, or integer", cfn_name);
        else
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
    xllastarg();

    // create the pname of the new symbol
    sprintf(sym, "%s%d", gsprefix, gsnumber++);

    // make a symbol with this print name
    return (cvsymbol(sym));
}

// xsprintf -- used by format
LVAL xsprintf()
{
    static char *cfn_name = "xsprintf";
    char buf[128], fmt[128];
    LVAL arg, ch, val;

    arg = xlgastring();
    ch = xlgachar();
    val = xlgafloat();
    xllastarg();

    sprintf(fmt, "%%%s%c", getstring(arg), getchcode(ch));
    sprintf(buf, fmt, getflonum(val));
    return cvstring(buf);
}
