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
/// Forward declarations
///-----------------------------------------------------------------------------
static euxlValue binary(int fcn);
static euxlValue unary(int fcn);
static euxlValue predicate(int fcn);
static euxlValue compare(int fcn);
static euxmDoubleFloatType convertToDoubleFloat(euxlValue val);
static void checkFPIZero(euxmFPIType iarg, euxmFPIType num);
static void checkFPINegative(euxmFPIType iarg);
static void checkDoubleFloatZero
(
    euxmDoubleFloatType farg,
    euxmDoubleFloatType num
);
static void checkDoubleFloatNegative(euxmDoubleFloatType farg);
static void badFPIOp();
static void badDoubleFloadOp();

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxlExactp - built-in function 'exact?'
//    *** THIS IS REALLY JUST A STUB FOR NOW ***
euxlValue euxlExactp()
{
    static char *functionName = "exact?";
    (void)euxmGetArgNumber();
    euxmLastArg();
    return (euxmNil);
}

///  euxlInexactp - built-in function 'inexact?'
//    *** THIS IS REALLY JUST A STUB FOR NOW ***
euxlValue euxlInexactp()
{
    static char *functionName = "inexact?";
    (void)euxmGetArgNumber();
    euxmLastArg();
    return (euxl_true);
}

///  euxlAtan - built-in function 'atan'
euxlValue euxlAtan()
{
    static char *functionName = "atan";

    // get the first argument
    euxlValue arg = euxmGetArgNumber();

    euxmDoubleFloatType val;
    if (euxmMoreArgs())    // handle two argument (atan y x)
    {
        euxlValue arg2 = euxmGetArgNumber();
        euxmLastArg();
        val = atan2(convertToDoubleFloat(arg), convertToDoubleFloat(arg2));
    }
    else    // handle one argument (atan x)
    {
        val = atan(convertToDoubleFloat(arg));
    }

    // return the resulting float
    return (euxcMakeDoubleFloat(val));
}

///  euxlFloor - built-in function 'floor'
euxlValue euxlFloor()
{
    static char *functionName = "floor";

    // get the argument
    euxlValue arg = euxmGetArg();
    euxmLastArg();

    // check its type
    if (euxmFPIp(arg))
    {
        return (arg);
    }
    else if (euxmDoubleFloatp(arg))
    {
        return (euxcMakeFPI((euxmFPIType) floor(euxmGetDoubleFloat(arg))));
    }

    euxcBadType(arg, "<number>", functionName);

    return (euxmNil);       // never reached
}

///  euxlCeiling - built-in function 'ceiling'
euxlValue euxlCeiling()
{
    static char *functionName = "ceiling";

    // get the argument
    euxlValue arg = euxmGetArg();
    euxmLastArg();

    // check its type
    if (euxmFPIp(arg))
    {
        return (arg);
    }
    else if (euxmDoubleFloatp(arg))
    {
        return (euxcMakeFPI((euxmFPIType) ceil(euxmGetDoubleFloat(arg))));
    }

    euxcBadType(arg, "<number>", functionName);

    return (euxmNil);       // never reached
}

///  euxlRound - built-in function 'round'
euxlValue euxlRound()
{
    static char *functionName = "round";

    // get the argument
    euxlValue arg = euxmGetArg();
    euxmLastArg();

    // check its type
    if (euxmFPIp(arg))
    {
        return (arg);
    }
    else if (euxmDoubleFloatp(arg))
    {
        euxmDoubleFloatType x = euxmGetDoubleFloat(arg);
        euxmDoubleFloatType y = floor(x);
        euxmDoubleFloatType z = x - y;

        if (z == 0.5)
        {
            if (((euxmFPIType) y & 1) == 1)
            {
                y += 1.0;
            }
            return (euxcMakeFPI((euxmFPIType) y));
        }
        else if (z < 0.5)
        {
            return (euxcMakeFPI((euxmFPIType) y));
        }
        else
        {
            return (euxcMakeFPI((euxmFPIType) (y + 1.0)));
        }
    }

    euxcBadType(arg, "<number>", functionName);

    return (euxmNil);       // never reached
}

///  euxlTruncate - built-in function 'truncate'
euxlValue euxlTruncate()
{
    static char *functionName = "truncate";

    // get the argument
    euxlValue arg = euxmGetArg();
    euxmLastArg();

    // check its type
    if (euxmFPIp(arg))
    {
        return (arg);
    }
    else if (euxmDoubleFloatp(arg))
    {
        return (euxcMakeFPI((euxmFPIType) (euxmGetDoubleFloat(arg))));
    }

    euxcBadType(arg, "<number>", functionName);

    return (euxmNil);       // never reached
}

///-----------------------------------------------------------------------------
/// binary functions
///-----------------------------------------------------------------------------

/// euxlAdd - +
euxlValue euxlAdd()
{
    if (!euxmMoreArgs())
    {
        return (euxcMakeFPI((euxmFPIType) 0));
    }
    return (binary('+'));
}

///  euxlMul - *
euxlValue euxlMul()
{
    if (!euxmMoreArgs())
    {
        return (euxcMakeFPI((euxmFPIType) 1));
    }
    return (binary('*'));
}

///  euxlSub - -
euxlValue euxlSub()
{
    return (binary('-'));
}

///  euxlDiv - /
euxlValue euxlDiv()
{
    return (binary('/'));
}

///  euxlQuo - quotient
euxlValue euxlQuo()
{
    return (binary('Q'));
}

///  euxlRem - remainder
euxlValue euxlRem()
{
    return (binary('R'));
}

///  euxlMin
euxlValue euxlMin()
{
    return (binary('m'));
}

///  euxlMax
euxlValue euxlMax()
{
    return (binary('M'));
}

///  euxlExpt
euxlValue euxlExpt()
{
    return (binary('E'));
}

///  euxlLogand
euxlValue euxlLogand()
{
    return (binary('&'));
}

///  euxlLogior
euxlValue euxlLogior()
{
    return (binary('|'));
}

///  euxlLogxor
euxlValue euxlLogxor()
{
    return (binary('^'));
}

///  binary - handle binary operations
static euxlValue binary(int fcn)
{

    static char *functionName = "binary arith op";

    // get the first argument
    euxlValue arg = euxmGetArg();

    // set the type of the first argument
    int mode = 0;
    euxmFPIType ival = 0;
    euxmDoubleFloatType fval = 0;
    if (euxmFPIp(arg))
    {
        ival = euxmGetFPI(arg);
        mode = 'I';
    }
    else if (euxmDoubleFloatp(arg))
    {
        fval = euxmGetDoubleFloat(arg);
        mode = 'F';
    }
    else
    {
        euxcBadType(arg, "<number>", functionName);
    }

    // treat a single argument as a special case
    if (!euxmMoreArgs())
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
                        checkFPIZero(ival, (euxmFPIType) 1);
                        ival = 1 / ival;
                        break;
                    case 'F':
                        checkDoubleFloatZero(fval, (euxmDoubleFloatType) 1.0);
                        fval = 1.0 / fval;
                        break;
                }
        }
    }

    euxmFPIType iarg = 0;
    euxmDoubleFloatType farg = 0;

    // handle each remaining argument
    while (euxmMoreArgs())
    {
        // get the next argument
        arg = euxmGetArg();

        // check its type
        if (euxmFPIp(arg))
        {
            switch (mode)
            {
                case 'I':
                    iarg = euxmGetFPI(arg);
                    break;
                case 'F':
                    farg = (euxmDoubleFloatType) euxmGetFPI(arg);
                    break;
            }
        }
        else if (euxmDoubleFloatp(arg))
        {
            switch (mode)
            {
                case 'I':
                    fval = (euxmDoubleFloatType) ival;
                    farg = euxmGetDoubleFloat(arg);
                    mode = 'F';
                    break;
                case 'F':
                    farg = euxmGetDoubleFloat(arg);
                    break;
            }
        }
        else
        {
            euxcBadType(arg, "<number>", functionName);
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
                        checkFPIZero(iarg, ival);
                        ival /= iarg;
                        break;
                    case 'Q':
                        checkFPIZero(iarg, ival);
                        ival /= iarg;
                        break;
                    case 'R':
                        checkFPIZero(iarg, ival);
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
                        return
                        (
                            euxcMakeDoubleFloat
                            (
                                (euxmDoubleFloatType) pow
                                (
                                    (euxmDoubleFloatType) ival,
                                    (euxmDoubleFloatType) iarg
                                )
                            )
                        );
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
                        badFPIOp();
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
                        checkDoubleFloatZero(farg, fval);
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
                        badDoubleFloadOp();
                }
                break;
        }
    }

    // return the result
    switch (mode)
    {
        case 'I':
            return (euxcMakeFPI(ival));
        case 'F':
            return (euxcMakeDoubleFloat(fval));
    }

    return (euxmNil);       // never reached
}

///-----------------------------------------------------------------------------
/// Unary functions
///-----------------------------------------------------------------------------
///  euxlLognot
euxlValue euxlLognot()
{
    return (unary('~'));
}

///  euxlAbs
euxlValue euxlAbs()
{
    return (unary('A'));
}

///  euxlAdd1
euxlValue euxlAdd1()
{
    return (unary('+'));
}

///  euxlSub1
euxlValue euxlSub1()
{
    return (unary('-'));
}

///  euxlSin
euxlValue euxlSin()
{
    return (unary('S'));
}

///  euxlCos
euxlValue euxlCos()
{
    return (unary('C'));
}

///  euxlTan
euxlValue euxlTan()
{
    return (unary('T'));
}

///  euxlAsin
euxlValue euxlAsin()
{
    return (unary('s'));
}

///  euxlAcos
euxlValue euxlAcos()
{
    return (unary('c'));
}

///  euxlXexp
euxlValue euxlXexp()
{
    return (unary('E'));
}

///  euxlSqrt
euxlValue euxlSqrt()
{
    return (unary('R'));
}

///  euxlXlog
euxlValue euxlXlog()
{
    return (unary('L'));
}

///  euxlRandom
euxlValue euxlRandom()
{
    return (unary('?'));
}

///  unaryToName
static char *unaryToName(int fcn)
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

///  unary - handle unary operations
static euxlValue unary(int fcn)
{
    euxlValue arg = NULL;

    // get the argument
    if (euxmMoreArgs())
    {
        // arg = euxmGetArg();
        arg = euxmNextArg();
    }
    else
    {
        euxcTooFew(unaryToName(fcn));
    }

    if (euxcArgC != 0)
    {   // euxmLastArg();
        euxcTooMany(unaryToName(fcn));
    }

    // check its type
    if (euxmFPIp(arg))
    {
        euxmFPIType ival = euxmGetFPI(arg);
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
                return
                (
                    euxcMakeDoubleFloat
                    (
                        (euxmDoubleFloatType) sin((euxmDoubleFloatType) ival)
                    )
                );
            case 'C':
                return
                (
                    euxcMakeDoubleFloat
                    (
                        (euxmDoubleFloatType) cos((euxmDoubleFloatType) ival)
                    )
                );
            case 'T':
                return
                (
                    euxcMakeDoubleFloat
                    (
                        (euxmDoubleFloatType) tan((euxmDoubleFloatType) ival)
                    )
                );
            case 's':
                return
                (
                    euxcMakeDoubleFloat
                    (
                        (euxmDoubleFloatType) asin((euxmDoubleFloatType) ival)
                    )
                );
            case 'c':
                return
                (
                    euxcMakeDoubleFloat
                    (
                        (euxmDoubleFloatType) acos((euxmDoubleFloatType) ival)
                    )
                );
            case 't':
                return
                (
                    euxcMakeDoubleFloat
                    (
                        (euxmDoubleFloatType) atan((euxmDoubleFloatType) ival)
                    )
                );
            case 'E':
                return
                (
                    euxcMakeDoubleFloat
                    (
                        (euxmDoubleFloatType) exp((euxmDoubleFloatType) ival)
                    )
                );
            case 'L':
                return
                (
                    euxcMakeDoubleFloat
                    (
                        (euxmDoubleFloatType) log((euxmDoubleFloatType) ival)));
            case 'R':
                checkFPINegative(ival);
                return
                (
                    euxcMakeDoubleFloat
                    (
                        (euxmDoubleFloatType) sqrt((euxmDoubleFloatType) ival)
                    )
                );
            case '?':
                ival = (euxmFPIType) euxcOSRand((int)ival);
                break;
            default:
                badFPIOp();
        }
        return (euxcMakeFPI(ival));
    }
    else if (euxmDoubleFloatp(arg))
    {
        euxmDoubleFloatType fval = euxmGetDoubleFloat(arg);
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
                checkDoubleFloatNegative(fval);
                fval = sqrt(fval);
                break;
            default:
                badDoubleFloadOp();
        }
        return (euxcMakeDoubleFloat(fval));
    }

    euxcBadType(arg, "<number>", unaryToName(fcn));
    return (euxmNil);       // never reached
}

///  euxlGcd - greatest common divisor
euxlValue euxlGcd()
{
    static char *functionName = "gcd";

    if (!euxmMoreArgs())    // check for identity case
    {
        return (euxcMakeFPI((euxmFPIType) 0));
    }

    euxlValue arg = euxmGetArgFPI();
    euxmFPIType n = euxmGetFPI(arg);

    if (n < (euxmFPIType) 0)
    {
        n = -n; // absolute value
    }

    while (euxmMoreArgs())
    {
        arg = euxmGetArgFPI();
        euxmFPIType m = euxmGetFPI(arg);

        if (m < (euxmFPIType) 0)
        {
            m = -m;     // absolute value
        }

        if (n > 0)
        {
            for (;;)
            {
                // euclid's algorithm
                euxmFPIType r = m % n;
                if (r == (euxmFPIType) 0)
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
    return (euxcMakeFPI(n));
}

///-----------------------------------------------------------------------------
/// unary predicates
///-----------------------------------------------------------------------------
///  euxlNegativep - negative?
euxlValue euxlNegativep()
{
    return (predicate('-'));
}

///  euxlZerop - zero?
euxlValue euxlZerop()
{
    return (predicate('Z'));
}

///  euxlPositivep - positive?
euxlValue euxlPositivep()
{
    return (predicate('+'));
}

///  euxlEvenp - even?
euxlValue euxlEvenp()
{
    return (predicate('E'));
}

///  euxlOddp - odd?
euxlValue euxlOddp()
{
    return (predicate('O'));
}

///  predicate - handle a predicate function
static euxlValue predicate(int fcn)
{
    static char *functionName = "arith predicate";

    // get the argument
    euxlValue arg = euxmGetArg();
    euxmLastArg();

    euxmFPIType ival = 0;

    // check the argument type
    if (euxmFPIp(arg))
    {
        ival = euxmGetFPI(arg);
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
                badFPIOp();
        }
    }
    else if (euxmDoubleFloatp(arg))
    {
        euxmDoubleFloatType fval = euxmGetDoubleFloat(arg);
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
                badDoubleFloadOp();
        }
    }
    else
    {
        euxcBadType(arg, "<number>", functionName);
    }

    // return the result value
    return (ival ? euxl_true : euxmNil);
}

///-----------------------------------------------------------------------------
/// comparison functions
///-----------------------------------------------------------------------------
///  euxlLt - <
euxlValue euxlLt()
{
    return (compare('<'));
}

///  euxlLtEq - <=
euxlValue euxlLtEq()
{
    return (compare('L'));
}

///  euxlEql - =
euxlValue euxlEql()
{
    return (compare('='));
}

///  euxlGtEq - >=
euxlValue euxlGtEq()
{
    return (compare('G'));
}

///  euxlGt - >
euxlValue euxlGt()
{
    return (compare('>'));
}

///  compare - common compare function
static euxlValue compare(int fcn)
{
    static char *functionName = "arith compare op";

    // get the first argument
    euxlValue arg = euxmGetArg();

    // set the type of the first argument
    int mode = 0;
    euxmFPIType ival = 0;
    euxmDoubleFloatType fval = 0;
    if (euxmFPIp(arg))
    {
        ival = euxmGetFPI(arg);
        mode = 'I';
    }
    else if (euxmDoubleFloatp(arg))
    {
        fval = euxmGetDoubleFloat(arg);
        mode = 'F';
    }
    else
    {
        euxcBadType(arg, "<number>", functionName);
    }

    // handle each remaining argument
    euxmFPIType icmp, iarg = 0;
    euxmDoubleFloatType fcmp, farg = 0;
    for (icmp = euxmTrue; icmp && euxmMoreArgs();)
    {
        // get the next argument
        arg = euxmGetArg();

        // check its type
        if (euxmFPIp(arg))
        {
            switch (mode)
            {
                case 'I':
                    iarg = euxmGetFPI(arg);
                    break;
                case 'F':
                    farg = (euxmDoubleFloatType) euxmGetFPI(arg);
                    break;
            }
        }
        else if (euxmDoubleFloatp(arg))
        {
            switch (mode)
            {
                case 'I':
                    fval = (euxmDoubleFloatType) ival;
                    farg = euxmGetDoubleFloat(arg);
                    mode = 'F';
                    break;
                case 'F':
                    farg = euxmGetDoubleFloat(arg);
                    break;
            }
        }
        else
        {
            euxcBadType(arg, "<number>", functionName);
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
    if (euxmMoreArgs())
    {
        xleuxmStackPopRest();
    }

    // return the result
    return (icmp ? euxl_true : euxmNil);
}

///  convertToDoubleFloat - convert a lisp value to a floating point number
static euxmDoubleFloatType convertToDoubleFloat(euxlValue val)
{
    // must be a number for this to work
    switch (euxmNodeType(val))
    {
        case euxmFPI:
            return ((euxmDoubleFloatType) euxmGetFPI(val));
        case euxmDoubleFloat:
            return (euxmGetDoubleFloat(val));
    }
    return ((euxmDoubleFloatType) 0);       // never reached
}

///  checkFPIZero - check for integer division by zero
static void checkFPIZero(euxmFPIType iarg, euxmFPIType num)
{
    if (iarg == 0)
    {
        euxcCerror("division by zero", euxcMakeFPI(num), euxls_arith_error);
    }
}

///  checkFPINegative - check for square root of a negative number
static void checkFPINegative(euxmFPIType iarg)
{
    if (iarg < 0)
    {
        euxcCerror
        (
            "square root of a negative number",
            euxcMakeFPI(iarg),
            euxls_arith_error
        );
    }
}

///  checkDoubleFloatZero - check for floating point division by zero
static void checkDoubleFloatZero
(
    euxmDoubleFloatType farg,
    euxmDoubleFloatType num
)
{
    if (farg == 0.0)
    {
        euxcCerror
        (
            "division by zero",
            euxcMakeDoubleFloat(num),
            euxls_arith_error
        );
    }
}

///  checkDoubleFloatNegative - check for square root of a negative number
static void checkDoubleFloatNegative(euxmDoubleFloatType farg)
{
    if (farg < 0.0)
    {
        euxcCerror
        (
            "square root of a negative number",
            euxcMakeDoubleFloat(farg),
            euxls_arith_error
        );
    }
}

///  badFPIOp - bad integer operation
static void badFPIOp()
{
    euxcFail("bad integer operation", euxls_arith_error);
}

///  badDoubleFloadOp - bad floating point operation
static void badDoubleFloadOp()
{
    euxcFail("bad floating point operation", euxls_arith_error);
}


///-----------------------------------------------------------------------------
