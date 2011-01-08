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
/// Title: euxlisp bytecode interpreter
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------

#define CHECK_REF

#include "euxlisp.h"
#include "euxlbcode.h"
#ifdef CHECK_REF
#include "euxlobj.h"
#endif

// sample rate (instructions per sample)
#define SRATE	1000

// macros to get the address of the code string for a code object
#define getcodestr(x) ((unsigned char *)getstring(getbcode(x)))

///-----------------------------------------------------------------------------
/// Globals
///-----------------------------------------------------------------------------
int trace = FALSE;              // trace enable
int xlargc;                     // argument count
JMP_BUF bc_dispatch;            // bytecode dispatcher

#include "euxlsymbols.h"
#include "euxlproto.h"

///-----------------------------------------------------------------------------
/// External variables
///-----------------------------------------------------------------------------
extern LVAL xlfun, xlenv, xlval;

///-----------------------------------------------------------------------------
/// Local variables
///-----------------------------------------------------------------------------
static unsigned char *base, *pc;
static int sample = SRATE;

static LVAL findvar(LVAL env, LVAL var, int *poff);
void xlapply();
void xlreturn();
static void restore_continuation();
static void badfuntype(LVAL arg);
static void badargtype(LVAL arg, char *name, char *fn);
static int generic_call(LVAL, LVAL, LVAL);
void xlstkover();
static void bad_slot_access(char *msg, LVAL index, LVAL object);

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
// xtraceon - built-in function 'trace-on'
LVAL xtraceon()
{
    static char *cfn_name = "trace-on";
    xllastarg()trace = TRUE;
    return (NIL);
}

// xtraceoff - built-in function 'trace-off'
LVAL xtraceoff()
{
    static char *cfn_name = "trace-off";
    xllastarg()trace = FALSE;
    return (NIL);
}

// xlexecute - execute byte codes
void xlexecute(LVAL fun)
{
    extern LVAL s_unbound_error, s_arith_error, s_no_next_md_error;

    // initialize the registers
    xlfun = getcode(fun);
    xlenv = getcenv(fun);
    xlval = NIL;

    // initialize the argument count
    xlargc = 0;

    // set the initial pc
    base = pc = getcodestr(xlfun);

    // setup a target for the error handler
    SETJMP(bc_dispatch);

    register LVAL tmp, tmp2;
    register unsigned int i;
    register int k;

    // execute the code
    for (;;)
    {
        // check for control codes
        if (--sample <= 0)
        {
            sample = SRATE;
            oscheck_int();
        }

        // print the trace information
        if (trace)
        {
            decode_instruction(xstdout(), xlfun, (int)(pc - base), xlenv);
        }

        // execute the next bytecode instruction
        switch (*pc++)
        {
            case OP_BRT:
                i = *pc++ << 8;
                i |= *pc++;
                if (xlval)
                {
                    pc = base + i;
                }
                break;
            case OP_BRF:
                i = *pc++ << 8;
                i |= *pc++;
                if (!xlval)
                {
                    pc = base + i;
                }
                break;
            case OP_BR:
                i = *pc++ << 8;
                i |= *pc++;
                pc = base + i;
                break;
            case OP_LIT:
                xlval = getelement(xlfun, *pc++);
                break;
            case OP_GREF:
                tmp = getelement(xlfun, *pc++);
                xlval = getvalue(tmp);
                if (xlval == s_unbound)
                {
                    char buf[128];
                    sprintf(buf, "variable unbound in module '%s'",
                    getstring(getmname(getmodule(tmp))));
                    xlinterror(buf, tmp, s_unbound_error);
                }
                break;
            case OP_GSET:
                setvalue(getelement(xlfun, *pc++), xlval);
                break;
            case OP_EREF:
                k = *pc++;
                tmp = xlenv;
                while (--k >= 0)
                {
                    tmp = cdr(tmp);
                }
                xlval = getelement(car(tmp), *pc++);
                break;
            case OP_ESET:
                k = *pc++;
                tmp = xlenv;
                while (--k >= 0)
                {
                    tmp = cdr(tmp);
                }
                setelement(car(tmp), *pc++, xlval);
                break;
            case OP_AREF:
                {
                    i = *pc++;
                    tmp = xlval;
                    if (!envp(tmp))
                    {
                        badargtype(tmp, "<env>", "aref");
                    }
                    int off = 0;
                    if ((tmp = findvar(tmp, getelement(xlfun, i), &off)) != NIL)
                    {
                        xlval = getelement(car(tmp), off);
                    }
                    else
                    {
                        xlval = s_unassigned;
                    }
                }
                break;
            case OP_ASET:
                {
                    i = *pc++;
                    tmp = pop();
                    if (!envp(tmp))
                    {
                        badargtype(tmp, "<env>", "aset");
                    }
                    int off = 0;
                    if ((tmp = findvar(tmp, getelement(xlfun, i), &off)) == NIL)
                    {
                        xlinterror
                        (
                            "no binding for variable",
                            getelement(xlfun, i),
                            s_unbound_error
                        );
                    }
                    setelement(car(tmp), off, xlval);
                }
                break;
            case OP_SAVE:      // save a continuation
                i = *pc++ << 8;
                i |= *pc++;
                check(3);
                push(cvsfixnum((FIXTYPE) i));
                push(xlfun);
                push(xlenv);
                break;
            case OP_CALL:      // call a function (or built-in)
                xlargc = *pc++; // get argument count
                xlapply();      // apply the function
                break;
            case OP_RETURN:    // return to the continuation on the stack
                xlreturn();
                break;
            case OP_FRAME:     // create an environment frame
                i = *pc++;      // get the frame size
                xlenv = newframe(xlenv, i);
                setelement(car(xlenv), 0, getvnames(xlfun));
                break;
            case OP_MVARG:     // move required argument to frame slot
                i = *pc++;      // get the slot number
                if (--xlargc < 0)
                {
                    xltoofew_int();
                }
                setelement(car(xlenv), i, pop());
                break;
            case OP_MVOARG:    // move optional argument to frame slot
                i = *pc++;      // get the slot number
                if (xlargc > 0)
                {
                    setelement(car(xlenv), i, pop());
                    --xlargc;
                }
                else
                {
                    setelement(car(xlenv), i, default_object);
                }
                break;
            case OP_MVRARG:    // build rest argument and move to frame slot
                i = *pc++;      // get the slot number
                for (xlval = NIL, k = xlargc; --k >= 0;)
                {
                    xlval = cons(xlsp[k], xlval);
                }
                setelement(car(xlenv), i, xlval);
                drop(xlargc);
                break;
            case OP_ALAST:     // make sure there are no more arguments
                if (xlargc > 0)
                {
                    xltoomany_int();
                }
                break;
            case OP_T:
                xlval = true;
                break;
            case OP_NIL:
                xlval = NIL;
                break;
            case OP_PUSH:
                cpush(xlval);
                break;
            case OP_CLOSE:
                if (!codep(xlval))
                {
                    badargtype(xlval, "<code>", "close");
                }
                xlval = cvclosure(xlval, xlenv);
                break;
            case OP_DELAY:
                if (!codep(xlval))
                {
                    badargtype(xlval, "<code>", "delay");
                }
                xlval = cvpromise(xlval, xlenv);
                break;
            case OP_ATOM:
                xlval = (atom(xlval) ? true : NIL);
                break;
            case OP_EQ:
                tmp = pop();
                if (symbolp(xlval) && symbolp(tmp))
                {
                    xlval = (symboleq(xlval, tmp) ? true : NIL);
                }
                else
                {
                    xlval = (xlval == tmp ? true : NIL);
                }
                break;
            case OP_NULL:
                xlval = (xlval ? NIL : true);
                break;
            case OP_CONS:
                xlval = cons(xlval, pop());
                break;
            case OP_CAR:
                if (!consp(xlval))
                {
                    badargtype(xlval, "<cons>", "car");
                }
                xlval = car(xlval);
                break;
            case OP_CDR:
                if (!consp(xlval))
                {
                    badargtype(xlval, "<cons>", "cdr");
                }
                xlval = cdr(xlval);
                break;
            case OP_SETCAR:
                if (!consp(xlval))
                {
                    badargtype(xlval, "<cons>", "set-car");
                }
                tmp = pop();
                rplaca(xlval, tmp);
                xlval = tmp;
                break;
            case OP_SETCDR:
                if (!consp(xlval))
                {
                    badargtype(xlval, "<cons>", "set-cdr");
                }
                tmp = pop();
                rplacd(xlval, tmp);
                xlval = tmp;
                break;
            case OP_GREFL:
                i = *pc++ << 8;
                i |= *pc++;
                tmp = getelement(xlfun, i);
                xlval = getvalue(tmp);
                if (xlval == s_unbound)
                {
                    char buf[128];
                    sprintf(buf, "variable unbound in module '%s'",
                    getstring(getmname(getmodule(tmp))));
                    xlinterror(buf, tmp, s_unbound_error);
                }
                break;
            case OP_GSETL:
                i = *pc++ << 8;
                i |= *pc++;
                setvalue(getelement(xlfun, i), xlval);
                break;
            case OP_LITL:
                i = *pc++ << 8;
                i |= *pc++;
                xlval = getelement(xlfun, i);
                break;
            case OP_ADD:
                tmp = pop();
                if (fixp(xlval) && fixp(tmp))
                {
                    xlval = cvfixnum(getfixnum(xlval) + getfixnum(tmp));
                }
                else if (fixp(xlval) && floatp(tmp))
                {
                    xlval = cvflonum(getfixnum(xlval) + getflonum(tmp));
                }
                else if (floatp(xlval) && fixp(tmp))
                {
                    xlval = cvflonum(getflonum(xlval) + getfixnum(tmp));
                }
                else if (floatp(xlval) && floatp(tmp))
                {
                    xlval = cvflonum(getflonum(xlval) + getflonum(tmp));
                }
                else if (!generic_call(s_binary_plus, tmp, xlval))
                {
                    push(tmp);
                    push(xlval);
                    xlargc = 2;
                    xlval = xadd();
                }
                break;
            case OP_SUB:
                tmp = pop();
                if (fixp(xlval) && fixp(tmp))
                {
                    xlval = cvfixnum(getfixnum(xlval) - getfixnum(tmp));
                }
                else if (fixp(xlval) && floatp(tmp))
                {
                    xlval = cvflonum(getfixnum(xlval) - getflonum(tmp));
                }
                else if (floatp(xlval) && fixp(tmp))
                {
                    xlval = cvflonum(getflonum(xlval) - getfixnum(tmp));
                }
                else if (floatp(xlval) && floatp(tmp))
                {
                    xlval = cvflonum(getflonum(xlval) - getflonum(tmp));
                }
                else if (!generic_call(s_binary_minus, tmp, xlval))
                {
                    push(tmp);
                    push(xlval);
                    xlargc = 2;
                    xlval = xsub();
                }
                break;
            case OP_MUL:
                tmp = pop();
                if (fixp(xlval) && fixp(tmp))
                {
                    xlval = cvfixnum(getfixnum(xlval) * getfixnum(tmp));
                }
                else if (fixp(xlval) && floatp(tmp))
                {
                    xlval = cvflonum(getfixnum(xlval) * getflonum(tmp));
                }
                else if (floatp(xlval) && fixp(tmp))
                {
                    xlval = cvflonum(getflonum(xlval) * getfixnum(tmp));
                }
                else if (floatp(xlval) && floatp(tmp))
                {
                    xlval = cvflonum(getflonum(xlval) * getflonum(tmp));
                }
                else if (!generic_call(s_binary_times, tmp, xlval))
                {
                    push(tmp);
                    push(xlval);
                    xlargc = 2;
                    xlval = xmul();
                }
                break;
            case OP_DIV:
                tmp = pop();
                if (fixp(xlval) && fixp(tmp))
                {
                    if (getfixnum(tmp) == (FIXTYPE) 0)
                    {
                        xlinterror("division by zero", xlval, s_arith_error);
                    }
                    xlval = cvfixnum(getfixnum(xlval) / getfixnum(tmp));
                }
                else if (fixp(xlval) && floatp(tmp))
                {
                    if (getflonum(tmp) == (FLOTYPE) 0.0)
                    {
                        xlinterror("division by zero", xlval, s_arith_error);
                    }
                    xlval = cvflonum(getfixnum(xlval) / getflonum(tmp));
                }
                else if (floatp(xlval) && fixp(tmp))
                {
                    if (getfixnum(tmp) == (FIXTYPE) 0)
                    {
                        xlinterror("division by zero", xlval, s_arith_error);
                    }
                    xlval = cvflonum(getflonum(xlval) / getfixnum(tmp));
                }
                else if (floatp(xlval) && floatp(tmp))
                {
                    if (getflonum(tmp) == (FLOTYPE) 0.0)
                    {
                        xlinterror("division by zero", xlval, s_arith_error);
                    }
                    xlval = cvflonum(getflonum(xlval) / getflonum(tmp));
                }
                else if (!generic_call(s_binary_divide, tmp, xlval))
                {
                    push(tmp);
                    push(xlval);
                    xlargc = 2;
                    xlval = xdiv();
                }
                break;
            case OP_QUO:
                tmp = pop();
                if (fixp(xlval) && fixp(tmp))
                {
                    FIXTYPE fixtmp;
                    if ((fixtmp = getfixnum(tmp)) == (FIXTYPE) 0)
                    {
                        xlinterror("division by zero", xlval, s_arith_error);
                    }
                    xlval = cvfixnum(getfixnum(xlval) / fixtmp);
                }
                else if (!generic_call(s_quotient, tmp, xlval))
                {
                    push(tmp);
                    push(xlval);
                    xlargc = 2;
                    xlval = xquo();
                }
                break;
            case OP_LSS:
                tmp = pop();
                if (fixp(xlval) && fixp(tmp))
                {
                    xlval = (getfixnum(xlval) < getfixnum(tmp) ? true : NIL);
                }
                else if (fixp(xlval) && floatp(tmp))
                {
                    xlval = (getfixnum(xlval) < getflonum(tmp) ? true : NIL);
                }
                else if (floatp(xlval) && fixp(tmp))
                {
                    xlval = (getflonum(xlval) < getfixnum(tmp) ? true : NIL);
                }
                else if (floatp(xlval) && floatp(tmp))
                {
                    xlval = (getflonum(xlval) < getflonum(tmp) ? true : NIL);
                }
                else if (!generic_call(s_binary_less, tmp, xlval))
                {
                    push(tmp);
                    push(xlval);
                    xlargc = 2;
                    xlval = xlss();
                }
                break;
            case OP_EQL:
                tmp = pop();
                if (fixp(xlval) && fixp(tmp))
                {
                    xlval = (getfixnum(xlval) == getfixnum(tmp) ? true : NIL);
                }
                else if (fixp(xlval) && floatp(tmp))
                {
                    xlval = (getfixnum(xlval) == getflonum(tmp) ? true : NIL);
                }
                else if (floatp(xlval) && fixp(tmp))
                {
                    xlval = (getflonum(xlval) == getfixnum(tmp) ? true : NIL);
                }
                else if (floatp(xlval) && floatp(tmp))
                {
                    xlval = (getflonum(xlval) == getflonum(tmp) ? true : NIL);
                }
                else if (!generic_call(s_binary_equal, tmp, xlval))
                {
                    push(tmp);
                    push(xlval);
                    xlargc = 2;
                    xlval = xeql();
                }
                break;
            case OP_GTR:
                tmp = pop();
                if (fixp(xlval) && fixp(tmp))
                {
                    xlval = (getfixnum(xlval) > getfixnum(tmp) ? true : NIL);
                }
                else if (fixp(xlval) && floatp(tmp))
                {
                    xlval = (getfixnum(xlval) > getflonum(tmp) ? true : NIL);
                }
                else if (floatp(xlval) && fixp(tmp))
                {
                    xlval = (getflonum(xlval) > getfixnum(tmp) ? true : NIL);
                }
                else if (floatp(xlval) && floatp(tmp))
                {
                    xlval = (getflonum(xlval) > getflonum(tmp) ? true : NIL);
                }
                else if (!generic_call(s_binary_less, xlval, tmp))
                {
                    push(tmp);
                    push(xlval);
                    xlargc = 2;
                    xlval = xgtr();
                }
                break;
            case OP_CLASSOF:
                xlval = class_of(xlval);
                break;
            case OP_CNM:       // (apply (car mfl) (cdr mfl) al al)
                if (xlval == NIL)
                {
                    // next method list
                    drop(1);    // arg list
                    xlcerror("no next method in call-next-method", xlfun,
                    s_no_next_md_error);
                }
                {
                    LVAL *p;
                    LVAL mfl, al, args;
                    al = pop();
                    mfl = xlval;
                    xlargc = list_size(al);
                    check(xlargc + 2);
                    args = al;
                    for
                    (
                        xlsp -= xlargc, p = xlsp;
                        consp(args);
                        args = cdr(args)
                    )
                    {
                        *p++ = car(args);
                    }
                    push(al);
                    push(cdr(mfl));
                    xlval = car(mfl);
                    xlargc += 2;
                    xlapply();
                }
                break;
            case OP_GETIVAR:
                tmp = pop();
                #ifdef CHECK_REF
                if
                (
                    !fixp(tmp)
                 || !objectp(xlval)
                 || getfixnum(tmp)
                  > getfixnum(getivar(getclass(xlval), INSTSIZE))
                )
                {
                    bad_slot_access("read", tmp, xlval);
                }
                #endif
                xlval = getivar(xlval, getfixnum(tmp));
                break;
            case OP_SETIVAR:
                {
                    tmp = pop();
                    #ifdef CHECK_REF
                    if
                    (
                        !fixp(tmp)
                     || !objectp(xlval)
                     || getfixnum(tmp)
                      > getfixnum(getivar(getclass(xlval), INSTSIZE))
                    )
                    {
                        bad_slot_access("write", tmp, xlval);
                    }
                    #endif
                    FIXTYPE fixtmp = getfixnum(tmp);
                    tmp = pop();
                    setivar(xlval, fixtmp, tmp);
                    xlval = tmp;
                }
                break;
                // these don't need the bother of frames and are used a lot
            case OP_GET:
                tmp = pop();
                if (!symbolp(xlval))
                {
                    badargtype(xlval, "<symbol>", "get");
                }
                if (!symbolp(tmp))
                {
                    badargtype(tmp, "<symbol>", "get");
                }
                xlval = xlgetprop(xlval, tmp);
                break;
            case OP_PUT:
                tmp = pop();
                tmp2 = pop();
                if (!symbolp(xlval))
                {
                    badargtype(xlval, "<symbol>", "put");
                }
                if (!symbolp(tmp))
                {
                    badargtype(tmp, "<symbol>", "put");
                }
                xlputprop(xlval, tmp2, tmp);
                xlval = tmp2;
                break;
            case OP_CURMOD:
                xlargc = 0;
                xlval = current_mod();
                break;
            case OP_CONSP:
                xlval = consp(xlval) ? true : NIL;
                break;
            case OP_SYMBOLP:
                xlval = symbolp(xlval) ? true : NIL;
                break;
            case OP_VECTORP:
                xlval = vectorp(xlval) ? true : NIL;
                break;
            case OP_APPEND:    // 2 args
                tmp = pop();
                if (!listp(xlval))
                {
                    badargtype(xlval, "<list>", "append");
                }
                if (xlval == NIL)
                {
                    xlval = tmp;
                }
                else
                {
                    LVAL end;
                    push(tmp);
                    end = cons(car(xlval), NIL);
                    cpush(end);
                    for (xlval = cdr(xlval); consp(xlval); xlval = cdr(xlval))
                    {
                        rplacd(end, cons(car(xlval), NIL));
                        end = cdr(end);
                    }
                    rplacd(end, tmp);
                    xlval = pop();
                    drop(1);    // tmp
                }
                break;
            case OP_LIST:      // 2 args
                xlval = cons(xlval, cons(pop(), NIL));
                break;
            case OP_SIZE:
                if (!listp(xlval))
                {
                    badargtype(xlval, "<list>", "list-size");
                }
                cpush(xlval);
                xlargc = 1;
                xlval = xsize();
                break;
            case OP_REVERSE:
                if (!listp(xlval))
                {
                    badargtype(xlval, "<list>", "reverse");
                }
                cpush(xlval);
                xlargc = 1;
                xlval = xreverse();
                break;
            case OP_CAAR:
                if (!consp(xlval))
                {
                    badargtype(xlval, "<cons>", "caar");
                }
                xlval = car(xlval);
                if (!consp(xlval))
                {
                    badargtype(xlval, "<cons>", "caar");
                }
                xlval = car(xlval);
                break;
            case OP_CADR:
                if (!consp(xlval))
                {
                    badargtype(xlval, "<cons>", "cadr");
                }
                xlval = cdr(xlval);
                if (!consp(xlval))
                {
                    badargtype(xlval, "<cons>", "cadr");
                }
                xlval = car(xlval);
                break;
            case OP_CDAR:
                if (!consp(xlval))
                {
                    badargtype(xlval, "<cons>", "cdar");
                }
                xlval = car(xlval);
                if (!consp(xlval))
                {
                    badargtype(xlval, "<cons>", "cdar");
                }
                xlval = cdr(xlval);
                break;
            case OP_CDDR:
                if (!consp(xlval))
                {
                    badargtype(xlval, "<cons>", "cddr");
                }
                xlval = cdr(xlval);
                if (!consp(xlval))
                {
                    badargtype(xlval, "<cons>", "cddr");
                }
                xlval = cdr(xlval);
                break;
            case OP_GETSYNTAX:
                tmp = pop();
                if (!symbolp(xlval))
                {
                    badargtype(xlval, "<symbol>", "get");
                }
                if (!symbolp(tmp))
                {
                    badargtype(tmp, "<symbol>", "get");
                }
                xlval = xlgetsyntax(xlval, tmp);
                break;
            case OP_PUTSYNTAX:
                tmp = pop();
                tmp2 = pop();
                if (!symbolp(xlval))
                {
                    badargtype(xlval, "<symbol>", "put");
                }
                if (!symbolp(tmp))
                {
                    badargtype(tmp, "<symbol>", "put");
                }
                xlputsyntax(xlval, tmp2, tmp);
                xlval = tmp2;
                break;
                #ifndef NO_CHECK_REF
            case OP_CHECKREF:
                tmp = pop();    // the object
                if (!classp(xlval))     // the class
                {
                    xlinterror
                    (
                        "not a class in check-ref",
                        xlval,
                        s_telos_error
                    );
                }
                if (!xlsubclassp(class_of(tmp), xlval))
                {
                    telos_bad_ref_error(tmp, xlval, TRUE);
                }
                xlval = true;
                break;
                #endif
            default:
                xlerror("bad opcode", cvsfixnum((FIXTYPE) * --pc));
                break;
        }
    }
}

// findvar - find a variable in an environment
static LVAL findvar(LVAL env, LVAL var, int *poff)
{
    for (; env != NIL; env = cdr(env))
    {
        LVAL names = getelement(car(env), 0);
        for (int off = 1; names != NIL; ++off, names = cdr(names))
            if (var == car(names))
            {
                *poff = off;
                return (env);
            }
    }
    return (NIL);
}

// xlapply - apply a function to arguments
// The function should be in xlval and the arguments should
// be on the stack.  The number of arguments should be in xlargc.
void xlapply()
{
    static char *cfn_name = "function apply";

    extern LVAL s_no_applic_error;

    // check for null function
    if (null(xlval))
    {
        badfuntype(xlval);
    }

    // dispatch on function type
    switch (ntype(xlval))
    {
        case SUBR:
            xlval = (*getsubr(xlval)) ();
            xlreturn();
            break;
        case XSUBR:
            (*getsubr(xlval)) ();
            break;
        case CLOSURE:
            xlfun = getcode(xlval);
            xlenv = getcenv(xlval);
            base = pc = getcodestr(xlfun);
            break;
        case GENERIC:
            {
                LVAL al, applicable;
                int i;
                al = NIL;       // consing on function call :-(
                for (i = xlargc - 1; i >= 0; i--)
                {
                    al = cons(xlsp[i], al);     // the arg list
                }
                cpush(al);
                applicable = find_and_cache_methods(xlval, al);
                if (applicable == NIL)
                {
                    xlval = cons(getgname(xlval), al);
                    drop(xlargc + 1);   // discard the args and arglist
                    xlcerror("no applicable methods", xlval, s_no_applic_error);
                }
                cpush(cdr(applicable));
                xlval = car(applicable);
                xlargc += 2;
                xlapply();
            }
            break;
        case CONTINUATION:
            {
                LVAL tmp = moreargs()? xlgetarg() : NIL; // zero or one arg allowed
                xllastarg();
                restore_continuation();
                xlval = tmp;
                xlreturn();
            }
            break;
        default:
            badfuntype(xlval);
    }
}

// xlreturn - return to a continuation on the stack
void xlreturn()
{
    // restore the environment and the continuation function
    xlenv = pop();
    LVAL tmp = pop();

    // dispatch on the function type
    switch (ntype(tmp))
    {
        case CODE:
            xlfun = tmp;
            tmp = pop();
            base = getcodestr(xlfun);
            pc = base + (int)getsfixnum(tmp);
            break;
        case CSUBR:
            (*getsubr(tmp)) ();
            break;
        default:
            xlerror("bad continuation", tmp);
    }
}

// save a stack snapshot
// cc is TRUE if return address is needed, e.g., in the interpreter
LVAL current_continuation(int cc)
{
    extern LVAL s_current_thread;

    if (cc)
    {
        check(4);
        push(cvsfixnum((FIXTYPE) (pc - base)));
        push(xlfun);
        push(xlenv);
    }
    else
    {
        check(3);
    }

    // store the thread dynamic state
    // c.f. thread.em get-state
    cpush(getivar(getvalue(s_current_thread), 4));
    #if 0
    xlputstr(xstdout(), "<save ");
    xlprin1(top(), xstdout());
    xlputstr(xstdout(), ">");
    #endif

    // create and initialize a continuation object
    int size = (int)(xlstktop - xlsp);
    LVAL cont = newcontinuation(size);
    LVAL *src, *dst;
    for (src = xlsp, dst = &cont->n_vdata[0]; --size >= 0;)
    {
        *dst++ = *src++;
    }

    drop(1);    // drop the state

    // return the continuation
    return (cont);
}

// restore_continuation - restore a continuation to the stack
//      The continuation should be in xlval.
static void restore_continuation()
{
    int size = getsize(xlval);
    LVAL *src;
    for (src = &xlval->n_vdata[size], xlsp = xlstktop; --size >= 0;)
    {
        *--xlsp = *--src;
    }

    // restore the thread dynamic state
    // c.f. thread.em set-state
    #if 0
    xlputstr(xstdout(), "<restore ");
    xlprin1(top(), xstdout());
    xlputstr(xstdout(), ">");
    #endif

    setivar(getvalue(s_current_thread), 4, pop());
}

// call gf associated with an inlined operator
static int generic_call(LVAL sym, LVAL val1, LVAL val2)
{
    LVAL op = getvalue(sym);
    if (!genericp(op))
    {
        return FALSE;   // generic not defined yet
    }

    // OP_SAVE
    int i = (int)(pc - base);
    check(5);
    push(cvsfixnum((FIXTYPE) i));
    push(xlfun);
    push(xlenv);
    // args and function
    push(val1);
    push(val2);
    xlval = op;
    // OP_CALL
    xlargc = 2;
    xlapply();

    return TRUE;
}

// gc_protect - protect the state of the interpreter from the collector
void gc_protect(void (*protected_fcn) ())
{
    int pcoff = pc - base;
    (*protected_fcn) ();
    if (xlfun)
    {
        base = getcodestr(xlfun);
        pc = base + pcoff;
    }
}

// badfuntype - bad function error
static void badfuntype(LVAL arg)
{
    xlbadtype(arg, "<function>", "function application");
}

// badargtype - bad argument type error
// cf xlbadtype in xsftab.c
static void badargtype(LVAL arg, char *name, char *fn)
{
    extern LVAL s_bad_type_error;

    char buf[256];
    sprintf(buf, "incorrect type in %s", fn);
    LVAL cond = getvalue(s_bad_type_error);
    if (cond != s_unbound)
    {
        LVAL class = name[0] == '<' ?
        getvalue(xlenter_module(name, root_module)) : cvstring(name);
        setivar(cond, 3, class);        // cf condcl.em
    }

    xlinterror(buf, arg, s_bad_type_error);
}

// xlstkover - value stack overflow
void xlstkover()
{
    xlabort("value stack overflow");
}

static void bad_slot_access(char *msg, LVAL index, LVAL object)
{
    cpush(index);
    char buf[20];
    sprintf(buf, "bad slot %s", msg);
    object = cons(object, NIL);
    object = cons(xlenter("object:"), object);
    object = cons(index, object);
    object = cons(xlenter("slot:"), object);
    drop(1);
    xlcerror(buf, object, NIL);
}


///-----------------------------------------------------------------------------
