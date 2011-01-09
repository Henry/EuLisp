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
/// Title: euxlisp built-in arithmetic functions
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------

#include "euxlisp.h"
#include <math.h>

///-----------------------------------------------------------------------------
/// External variables
///-----------------------------------------------------------------------------
extern euxlValue true, s_arith_error;

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static euxlValue binary(int fcn);
static euxlValue unary(int fcn);
static euxlValue predicate(int fcn);
static euxlValue compare(int fcn);
static FLOTYPE toflotype(euxlValue val);
static void checkizero(FIXTYPE iarg, FIXTYPE num);
static void checkineg(FIXTYPE iarg);
static void checkfzero(FLOTYPE farg, FLOTYPE num);
static void checkfneg(FLOTYPE farg);
static void badiop();
static void badfop();

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
// xexactp - built-in function 'exact?'
// *** THIS IS REALLY JUST A STUB FOR NOW ***
euxlValue xexactp()
{
    static char *cfn_name = "exact?";
    (void)xlganumber();
    xllastarg();
    return (NIL);
}

// xinexactp - built-in function 'inexact?'
// *** THIS IS REALLY JUST A STUB FOR NOW ***
euxlValue xinexactp()
{
    static char *cfn_name = "inexact?";
    (void)xlganumber();
    xllastarg();
    return (true);
}

// xatan - built-in function 'atan'
euxlValue xatan()
{
    static char *cfn_name = "atan";

    // get the first argument
    euxlValue arg = xlganumber();

    FLOTYPE val;
    if (moreargs())    // handle two argument (atan y x)
    {
        euxlValue arg2 = xlganumber();
        xllastarg();
        val = atan2(toflotype(arg), toflotype(arg2));
    }
    else    // handle one argument (atan x)
    {
        val = atan(toflotype(arg));
    }

    // return the resulting flonum
    return (cvflonum(val));
}

// xfloor - built-in function 'floor'
euxlValue xfloor()
{
    static char *cfn_name = "floor";

    // get the argument
    euxlValue arg = xlgetarg();
    xllastarg();

    // check its type
    if (fixp(arg))
    {
        return (arg);
    }
    else if (floatp(arg))
    {
        return (cvfixnum((FIXTYPE) floor(getflonum(arg))));
    }

    xlbadtype(arg, "<number>", cfn_name);

    return (NIL);       // never reached
}

// xceiling - built-in function 'ceiling'
euxlValue xceiling()
{
    static char *cfn_name = "ceiling";

    // get the argument
    euxlValue arg = xlgetarg();
    xllastarg();

    // check its type
    if (fixp(arg))
    {
        return (arg);
    }
    else if (floatp(arg))
    {
        return (cvfixnum((FIXTYPE) ceil(getflonum(arg))));
    }

    xlbadtype(arg, "<number>", cfn_name);

    return (NIL);       // never reached
}

// xround - built-in function 'round'
euxlValue xround()
{
    static char *cfn_name = "round";

    // get the argument
    euxlValue arg = xlgetarg();
    xllastarg();

    // check its type
    if (fixp(arg))
    {
        return (arg);
    }
    else if (floatp(arg))
    {
        FLOTYPE x = getflonum(arg);
        FLOTYPE y = floor(x);
        FLOTYPE z = x - y;

        if (z == 0.5)
        {
            if (((FIXTYPE) y & 1) == 1)
            {
                y += 1.0;
            }
            return (cvfixnum((FIXTYPE) y));
        }
        else if (z < 0.5)
        {
            return (cvfixnum((FIXTYPE) y));
        }
        else
        {
            return (cvfixnum((FIXTYPE) (y + 1.0)));
        }
    }

    xlbadtype(arg, "<number>", cfn_name);

    return (NIL);       // never reached
}

// xtruncate - built-in function 'truncate'
euxlValue xtruncate()
{
    static char *cfn_name = "truncate";

    // get the argument
    euxlValue arg = xlgetarg();
    xllastarg();

    // check its type
    if (fixp(arg))
    {
        return (arg);
    }
    else if (floatp(arg))
    {
        return (cvfixnum((FIXTYPE) (getflonum(arg))));
    }

    xlbadtype(arg, "<number>", cfn_name);

    return (NIL);       // never reached
}

///-----------------------------------------------------------------------------
/// binary functions
///-----------------------------------------------------------------------------

euxlValue xadd() // +
{
    if (!moreargs())
    {
        return (cvfixnum((FIXTYPE) 0));
    }
    return (binary('+'));
}

euxlValue xmul() // *
{
    if (!moreargs())
    {
        return (cvfixnum((FIXTYPE) 1));
    }
    return (binary('*'));
}

euxlValue xsub()  // -
{
    return (binary('-'));
}

euxlValue xdiv()  // /
{
    return (binary('/'));
}

euxlValue xquo()  // quotient
{
    return (binary('Q'));
}

euxlValue xrem()  // remainder
{
    return (binary('R'));
}

euxlValue xmin()
{
    return (binary('m'));
}

euxlValue xmax()
{
    return (binary('M'));
}

euxlValue xexpt()
{
    return (binary('E'));
}

euxlValue xlogand()
{
    return (binary('&'));
}

euxlValue xlogior()
{
    return (binary('|'));
}

euxlValue xlogxor()
{
    return (binary('^'));
}

// binary - handle binary operations
static euxlValue binary(int fcn)
{

    static char *cfn_name = "binary arith op";

    // get the first argument
    euxlValue arg = xlgetarg();

    // set the type of the first argument
    int mode = 0;
    FIXTYPE ival = 0;
    FLOTYPE fval = 0;
    if (fixp(arg))
    {
        ival = getfixnum(arg);
        mode = 'I';
    }
    else if (floatp(arg))
    {
        fval = getflonum(arg);
        mode = 'F';
    }
    else
    {
        xlbadtype(arg, "<number>", cfn_name);
    }

    // treat a single argument as a special case
    if (!moreargs())
    {
        switch (fcn)
        {
            case '-':
                switch (mode)
                {
                    case 'I':
                        ival = -ival;
                        break;
                    case 'F':
                        fval = -fval;
                        break;
                }
                break;
            case '/':
                switch (mode)
                {
                    case 'I':
                        checkizero(ival, (FIXTYPE) 1);
                        ival = 1 / ival;
                        break;
                    case 'F':
                        checkfzero(fval, (FLOTYPE) 1.0);
                        fval = 1.0 / fval;
                        break;
                }
        }
    }

    FIXTYPE iarg = 0;
    FLOTYPE farg = 0;

    // handle each remaining argument
    while (moreargs())
    {
        // get the next argument
        arg = xlgetarg();

        // check its type
        if (fixp(arg))
        {
            switch (mode)
            {
                case 'I':
                    iarg = getfixnum(arg);
                    break;
                case 'F':
                    farg = (FLOTYPE) getfixnum(arg);
                    break;
            }
        }
        else if (floatp(arg))
        {
            switch (mode)
            {
                case 'I':
                    fval = (FLOTYPE) ival;
                    farg = getflonum(arg);
                    mode = 'F';
                    break;
                case 'F':
                    farg = getflonum(arg);
                    break;
            }
        }
        else
        {
            xlbadtype(arg, "<number>", cfn_name);
        }

        // accumulate the result value
        switch (mode)
        {
            case 'I':
                switch (fcn)
                {
                    case '+':
                        ival += iarg;
                        break;
                    case '-':
                        ival -= iarg;
                        break;
                    case '*':
                        ival *= iarg;
                        break;
                    case '/':
                        checkizero(iarg, ival);
                        ival /= iarg;
                        break;
                    case 'Q':
                        checkizero(iarg, ival);
                        ival /= iarg;
                        break;
                    case 'R':
                        checkizero(iarg, ival);
                        ival %= iarg;
                        break;
                    case 'M':
                        if (iarg > ival)
                            ival = iarg;
                        break;
                    case 'm':
                        if (iarg < ival)
                            ival = iarg;
                        break;
                    case 'E':
                        return (cvflonum((FLOTYPE) pow((FLOTYPE) ival,
                        (FLOTYPE) iarg)));
                    case '&':
                        ival &= iarg;
                        break;
                    case '|':
                        ival |= iarg;
                        break;
                    case '^':
                        ival ^= iarg;
                        break;
                    default:
                        badiop();
                }
                break;
            case 'F':
                switch (fcn)
                {
                    case '+':
                        fval += farg;
                        break;
                    case '-':
                        fval -= farg;
                        break;
                    case '*':
                        fval *= farg;
                        break;
                    case '/':
                        checkfzero(farg, fval);
                        fval /= farg;
                        break;
                    case 'M':
                        if (farg > fval)
                            fval = farg;
                        break;
                    case 'm':
                        if (farg < fval)
                            fval = farg;
                        break;
                    case 'E':
                        fval = pow(fval, farg);
                        break;
                    case 'R':
                        fval = fmod(fval, farg);
                        break;
                    default:
                        badfop();
                }
                break;
        }
    }

    // return the result
    switch (mode)
    {
        case 'I':
            return (cvfixnum(ival));
        case 'F':
            return (cvflonum(fval));
    }

    return (NIL);       // never reached
}

///-----------------------------------------------------------------------------
/// Unary functions
///-----------------------------------------------------------------------------
euxlValue xlognot()
{
    return (unary('~'));
}

euxlValue xabs()
{
    return (unary('A'));
}

euxlValue xadd1()
{
    return (unary('+'));
}

euxlValue xsub1()
{
    return (unary('-'));
}

euxlValue xsin()
{
    return (unary('S'));
}

euxlValue xcos()
{
    return (unary('C'));
}

euxlValue xtan()
{
    return (unary('T'));
}

euxlValue xasin()
{
    return (unary('s'));
}

euxlValue xacos()
{
    return (unary('c'));
}

euxlValue xxexp()
{
    return (unary('E'));
}

euxlValue xsqrt()
{
    return (unary('R'));
}

euxlValue xxlog()
{
    return (unary('L'));
}

euxlValue xrandom()
{
    return (unary('?'));
}

static char *unary_to_name(int fcn)
{
    switch (fcn)
    {
        case '~':
            return "lognot";
        case 'A':
            return "abs";
        case '+':
            return "+";
        case '-':
            return "-";
        case 'S':
            return "sin";
        case 'C':
            return "cos";
        case 'T':
            return "tan";
        case 's':
            return "asin";
        case 'c':
            return "acos";
        case 'E':
            return "exp";
        case 'R':
            return "sqrt";
        case 'L':
            return "log";
        case '?':
            return "random";
    }

    return "unknown unary op";
}

// unary - handle unary operations
static euxlValue unary(int fcn)
{
    euxlValue arg = NULL;

    // get the argument
    if (moreargs())
    {
        // arg = xlgetarg();
        arg = nextarg();
    }
    else
    {
        xltoofew(unary_to_name(fcn));
    }

    if (xlargc != 0)
    {   // xllastarg();
        xltoomany(unary_to_name(fcn));
    }

    // check its type
    if (fixp(arg))
    {
        FIXTYPE ival = getfixnum(arg);
        switch (fcn)
        {
            case '~':
                ival = ~ival;
                break;
            case 'A':
                ival = (ival < 0 ? -ival : ival);
                break;
            case '+':
                ival++;
                break;
            case '-':
                ival--;
                break;
            case 'S':
                return (cvflonum((FLOTYPE) sin((FLOTYPE) ival)));
            case 'C':
                return (cvflonum((FLOTYPE) cos((FLOTYPE) ival)));
            case 'T':
                return (cvflonum((FLOTYPE) tan((FLOTYPE) ival)));
            case 's':
                return (cvflonum((FLOTYPE) asin((FLOTYPE) ival)));
            case 'c':
                return (cvflonum((FLOTYPE) acos((FLOTYPE) ival)));
            case 't':
                return (cvflonum((FLOTYPE) atan((FLOTYPE) ival)));
            case 'E':
                return (cvflonum((FLOTYPE) exp((FLOTYPE) ival)));
            case 'L':
                return (cvflonum((FLOTYPE) log((FLOTYPE) ival)));
            case 'R':
                checkineg(ival);
                return (cvflonum((FLOTYPE) sqrt((FLOTYPE) ival)));
            case '?':
                ival = (FIXTYPE) osrand((int)ival);
                break;
            default:
                badiop();
        }
        return (cvfixnum(ival));
    }
    else if (floatp(arg))
    {
        FLOTYPE fval = getflonum(arg);
        switch (fcn)
        {
            case 'A':
                fval = (fval < 0.0 ? -fval : fval);
                break;
            case '+':
                fval += 1.0;
                break;
            case '-':
                fval -= 1.0;
                break;
            case 'S':
                fval = sin(fval);
                break;
            case 'C':
                fval = cos(fval);
                break;
            case 'T':
                fval = tan(fval);
                break;
            case 's':
                fval = asin(fval);
                break;
            case 'c':
                fval = acos(fval);
                break;
            case 't':
                fval = atan(fval);
                break;
            case 'E':
                fval = exp(fval);
                break;
            case 'L':
                fval = log(fval);
                break;
            case 'R':
                checkfneg(fval);
                fval = sqrt(fval);
                break;
            default:
                badfop();
        }
        return (cvflonum(fval));
    }

    xlbadtype(arg, "<number>", unary_to_name(fcn));
    return (NIL);       // never reached
}

// xgcd - greatest common divisor
euxlValue xgcd()
{
    static char *cfn_name = "gcd";

    if (!moreargs())    // check for identity case
    {
        return (cvfixnum((FIXTYPE) 0));
    }

    euxlValue arg = xlgafixnum();
    FIXTYPE n = getfixnum(arg);

    if (n < (FIXTYPE) 0)
    {
        n = -n; // absolute value
    }

    while (moreargs())
    {
        arg = xlgafixnum();
        FIXTYPE m = getfixnum(arg);

        if (m < (FIXTYPE) 0)
        {
            m = -m;     // absolute value
        }

        if (n > 0)
        {
            for (;;)
            {
                // euclid's algorithm
                FIXTYPE r = m % n;
                if (r == (FIXTYPE) 0)
                {
                    break;
                }
                m = n;
                n = r;
            }
        }
        else
        {
            n = m;      // (gcd 0 m)
        }
    }
    return (cvfixnum(n));
}

///-----------------------------------------------------------------------------
/// unary predicates
///-----------------------------------------------------------------------------
euxlValue xnegativep()  // negative?
{
    return (predicate('-'));
}

euxlValue xzerop()  // zero?
{
    return (predicate('Z'));
}

euxlValue xpositivep()  // positive?
{
    return (predicate('+'));
}

euxlValue xevenp()  // even?
{
    return (predicate('E'));
}

euxlValue xoddp()  // odd?
{
    return (predicate('O'));
}

// predicate - handle a predicate function
static euxlValue predicate(int fcn)
{
    static char *cfn_name = "arith predicate";

    // get the argument
    euxlValue arg = xlgetarg();
    xllastarg();

    FIXTYPE ival = 0;

    // check the argument type
    if (fixp(arg))
    {
        ival = getfixnum(arg);
        switch (fcn)
        {
            case '-':
                ival = (ival < 0);
                break;
            case 'Z':
                ival = (ival == 0);
                break;
            case '+':
                ival = (ival > 0);
                break;
            case 'E':
                ival = ((ival & 1) == 0);
                break;
            case 'O':
                ival = ((ival & 1) != 0);
                break;
            default:
                badiop();
        }
    }
    else if (floatp(arg))
    {
        FLOTYPE fval = getflonum(arg);
        switch (fcn)
        {
            case '-':
                ival = (fval < 0);
                break;
            case 'Z':
                ival = (fval == 0);
                break;
            case '+':
                ival = (fval > 0);
                break;
            default:
                badfop();
        }
    }
    else
    {
        xlbadtype(arg, "<number>", cfn_name);
    }

    // return the result value
    return (ival ? true : NIL);
}

///-----------------------------------------------------------------------------
/// comparison functions
///-----------------------------------------------------------------------------
euxlValue xlss()  // <
{
    return (compare('<'));
}

euxlValue xleq()  // <=
{
    return (compare('L'));
}

euxlValue xeql()  // =
{
    return (compare('='));
}

euxlValue xgeq()  // >=
{
    return (compare('G'));
}

euxlValue xgtr()  // >
{
    return (compare('>'));
}

// compare - common compare function
static euxlValue compare(int fcn)
{
    static char *cfn_name = "arith compare op";

    // get the first argument
    euxlValue arg = xlgetarg();

    // set the type of the first argument
    int mode = 0;
    FIXTYPE ival = 0;
    FLOTYPE fval = 0;
    if (fixp(arg))
    {
        ival = getfixnum(arg);
        mode = 'I';
    }
    else if (floatp(arg))
    {
        fval = getflonum(arg);
        mode = 'F';
    }
    else
    {
        xlbadtype(arg, "<number>", cfn_name);
    }

    // handle each remaining argument
    FIXTYPE icmp, iarg = 0;
    FLOTYPE fcmp, farg = 0;
    for (icmp = TRUE; icmp && moreargs();)
    {
        // get the next argument
        arg = xlgetarg();

        // check its type
        if (fixp(arg))
        {
            switch (mode)
            {
                case 'I':
                    iarg = getfixnum(arg);
                    break;
                case 'F':
                    farg = (FLOTYPE) getfixnum(arg);
                    break;
            }
        }
        else if (floatp(arg))
        {
            switch (mode)
            {
                case 'I':
                    fval = (FLOTYPE) ival;
                    farg = getflonum(arg);
                    mode = 'F';
                    break;
                case 'F':
                    farg = getflonum(arg);
                    break;
            }
        }
        else
        {
            xlbadtype(arg, "<number>", cfn_name);
        }

        // compute result of the compare
        switch (mode)
        {
            case 'I':
                icmp = ival - iarg;
                switch (fcn)
                {
                    case '<':
                        icmp = (icmp < 0);
                        break;
                    case 'L':
                        icmp = (icmp <= 0);
                        break;
                    case '=':
                        icmp = (icmp == 0);
                        break;
                    case 'G':
                        icmp = (icmp >= 0);
                        break;
                    case '>':
                        icmp = (icmp > 0);
                        break;
                }
                break;
            case 'F':
                fcmp = fval - farg;
                switch (fcn)
                {
                    case '<':
                        icmp = (fcmp < 0.0);
                        break;
                    case 'L':
                        icmp = (fcmp <= 0.0);
                        break;
                    case '=':
                        icmp = (fcmp == 0.0);
                        break;
                    case 'G':
                        icmp = (fcmp >= 0.0);
                        break;
                    case '>':
                        icmp = (fcmp > 0.0);
                        break;
                }
                break;
        }

        // update the values
        ival = iarg;
        fval = farg;
    }

    // get rid of extra arguments
    if (moreargs())
    {
        xlpoprest();
    }

    // return the result
    return (icmp ? true : NIL);
}

// toflotype - convert a lisp value to a floating point number
static FLOTYPE toflotype(euxlValue val)
{
    // must be a number for this to work
    switch (ntype(val))
    {
        case FIXNUM:
            return ((FLOTYPE) getfixnum(val));
        case FLONUM:
            return (getflonum(val));
    }
    return ((FLOTYPE) 0);       // never reached
}

// checkizero - check for integer division by zero
static void checkizero(FIXTYPE iarg, FIXTYPE num)
{
    if (iarg == 0)
    {
        xlcerror("division by zero", cvfixnum(num), s_arith_error);
    }
}

// checkineg - check for square root of a negative number
static void checkineg(FIXTYPE iarg)
{
    if (iarg < 0)
    {
        xlcerror
        (
            "square root of a negative number",
            cvfixnum(iarg),
            s_arith_error
        );
    }
}

// checkfzero - check for floating point division by zero
static void checkfzero(FLOTYPE farg, FLOTYPE num)
{
    if (farg == 0.0)
    {
        xlcerror("division by zero", cvflonum(num), s_arith_error);
    }
}

// checkfneg - check for square root of a negative number
static void checkfneg(FLOTYPE farg)
{
    if (farg < 0.0)
    {
        xlcerror
        (
            "square root of a negative number",
            cvflonum(farg),
            s_arith_error
        );
    }
}

// badiop - bad integer operation
static void badiop()
{
    xlfail("bad integer operation", s_arith_error);
}

// badfop - bad floating point operation
static void badfop()
{
    xlfail("bad floating point operation", s_arith_error);
}


///-----------------------------------------------------------------------------
