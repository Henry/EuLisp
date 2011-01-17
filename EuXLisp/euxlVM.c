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
/// Title: Byte-code virtual machine
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"
#include "euxlBCodes.h"

///-----------------------------------------------------------------------------
/// Macros
///-----------------------------------------------------------------------------
///  Check references
#define CHECK_REF

///  Sample rate (instructions per sample)
#define SRATE	1000

///  Get the address of the code string for a code object
#define euxmGetCodestr(x) ((unsigned char *)euxmGetString(euxmGetBCode(x)))

///-----------------------------------------------------------------------------
/// Global variables
///-----------------------------------------------------------------------------
int trace = euxmFalse;             // trace enable
int euxcArgC;                      // argument count
euxmJmpBuf bc_dispatch;            // bytecode dispatcher

///-----------------------------------------------------------------------------
/// Local variables
///-----------------------------------------------------------------------------
static unsigned char *base, *pc;
static int sample = SRATE;

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static euxlValue findVar(euxlValue env, euxlValue var, int *poff);
static void restoreContinuation();
static void badFunctionNodeType(euxlValue arg);
static void badArgType(euxlValue arg, const char *name, const char *fn);
static int genericCall(euxlValue, euxlValue, euxlValue);
static void badSlotAccess(const char *msg, euxlValue index, euxlValue object);

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxlTraceOn - built-in function 'trace-on'
euxlValue euxlTraceOn()
{
    static char *functionName = "trace-on";

    euxmLastArg()trace = euxmTrue;
    return (euxmNil);
}

///  euxlTraceOff - built-in function 'trace-off'
euxlValue euxlTraceOff()
{
    static char *functionName = "trace-off";

    euxmLastArg()trace = euxmFalse;
    return (euxmNil);
}

///  euxcExecute - execute byte codes
void euxcExecute(euxlValue fun)
{
    // initialize the registers
    euxcCurFun = euxmGetCode(fun);
    euxcCurEnv = euxmGetCEnv(fun);
    euxcCurVal = euxmNil;

    // initialize the argument count
    euxcArgC = 0;

    // set the initial pc
    base = pc = euxmGetCodestr(euxcCurFun);

    // setup a target for the error handler
    euxmSetJmp(bc_dispatch);

    register euxlValue tmp, tmp2;
    register unsigned int i;
    register int k;

    // execute the code
    for (;;)
    {
        // Check for control codes
        if (--sample <= 0)
        {
            sample = SRATE;
            euxcOSCheckInt();
        }

        // print the trace information
        if (trace)
        {
            euxcDecodeInstruction
            (
                euxlStdout(),
                euxcCurFun,
                (int)(pc - base),
                euxcCurEnv
            );
        }

        // execute the next bytecode instruction
        switch (*pc++)
        {
            case OP_BRT:
                i = *pc++ << 8;
                i |= *pc++;
                if (euxcCurVal)
                {
                    pc = base + i;
                }
                break;
            case OP_BRF:
                i = *pc++ << 8;
                i |= *pc++;
                if (!euxcCurVal)
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
                euxcCurVal = euxmGetElement(euxcCurFun, *pc++);
                break;
            case OP_GREF:
                tmp = euxmGetElement(euxcCurFun, *pc++);
                euxcCurVal = euxmGetValue(tmp);
                if (euxcCurVal == euxls_unbound)
                {
                    char buf[128];
                    sprintf(buf, "variable unbound in module '%s'",
                    euxmGetString(euxmGetModuleName(euxmGetModule(tmp))));
                    euxcIntError(buf, tmp, euxls_unbound_error);
                }
                break;
            case OP_GSET:
                euxmSetValue(euxmGetElement(euxcCurFun, *pc++), euxcCurVal);
                break;
            case OP_EREF:
                k = *pc++;
                tmp = euxcCurEnv;
                while (--k >= 0)
                {
                    tmp = euxmCdr(tmp);
                }
                euxcCurVal = euxmGetElement(euxmCar(tmp), *pc++);
                break;
            case OP_ESET:
                k = *pc++;
                tmp = euxcCurEnv;
                while (--k >= 0)
                {
                    tmp = euxmCdr(tmp);
                }
                euxmSetElement(euxmCar(tmp), *pc++, euxcCurVal);
                break;
            case OP_AREF:
                {
                    i = *pc++;
                    tmp = euxcCurVal;
                    if (!euxmEnvp(tmp))
                    {
                        badArgType(tmp, "<env>", "aref");
                    }
                    int off = 0;
                    if
                    (
                        (
                            tmp = findVar
                            (
                                tmp,
                                euxmGetElement(euxcCurFun, i),
                                &off
                            )
                        )
                     != euxmNil
                    )
                    {
                        euxcCurVal = euxmGetElement(euxmCar(tmp), off);
                    }
                    else
                    {
                        euxcCurVal = euxls_unassigned;
                    }
                }
                break;
            case OP_ASET:
                {
                    i = *pc++;
                    tmp = euxmStackPop();
                    if (!euxmEnvp(tmp))
                    {
                        badArgType(tmp, "<env>", "aset");
                    }
                    int off = 0;
                    if
                    (
                        (tmp = findVar
                        (
                            tmp,
                            euxmGetElement(euxcCurFun, i),
                            &off
                        )
                        )
                     == euxmNil
                    )
                    {
                        euxcIntError
                        (
                            "no binding for variable",
                            euxmGetElement(euxcCurFun, i),
                            euxls_unbound_error
                        );
                    }
                    euxmSetElement(euxmCar(tmp), off, euxcCurVal);
                }
                break;
            case OP_SAVE:      // save a continuation
                i = *pc++ << 8;
                i |= *pc++;
                euxmStackCheck(3);
                euxmStackPush(euxmMakeSmallFPI((euxmFPIType) i));
                euxmStackPush(euxcCurFun);
                euxmStackPush(euxcCurEnv);
                break;
            case OP_CALL:      // call a function (or built-in)
                euxcArgC = *pc++; // get argument count
                euxcApply();      // apply the function
                break;
            case OP_RETURN:    // return to the continuation on the stack
                euxcReturn();
                break;
            case OP_FRAME:     // create an environment frame
                i = *pc++;      // get the frame size
                euxcCurEnv = euxcNewFrame(euxcCurEnv, i);
                euxmSetElement
                (
                    euxmCar(euxcCurEnv),
                    0,
                    euxmGetVNames(euxcCurFun)
                );
                break;
            case OP_MVARG:     // move required argument to frame slot
                i = *pc++;      // get the slot number
                if (--euxcArgC < 0)
                {
                    euxcTooFewInt();
                }
                euxmSetElement(euxmCar(euxcCurEnv), i, euxmStackPop());
                break;
            case OP_MVOARG:    // move optional argument to frame slot
                i = *pc++;      // get the slot number
                if (euxcArgC > 0)
                {
                    euxmSetElement(euxmCar(euxcCurEnv), i, euxmStackPop());
                    --euxcArgC;
                }
                else
                {
                    euxmSetElement(euxmCar(euxcCurEnv), i, euxs_default);
                }
                break;
            case OP_MVRARG:    // build rest argument and move to frame slot
                i = *pc++;      // get the slot number
                for (euxcCurVal = euxmNil, k = euxcArgC; --k >= 0;)
                {
                    euxcCurVal = euxcCons(euxcStackPtr[k], euxcCurVal);
                }
                euxmSetElement(euxmCar(euxcCurEnv), i, euxcCurVal);
                euxmStackDrop(euxcArgC);
                break;
            case OP_ALAST:     // make sure there are no more arguments
                if (euxcArgC > 0)
                {
                    euxcTooManyInt();
                }
                break;
            case OP_T:
                euxcCurVal = euxs_t;
                break;
            case OP_NIL:
                euxcCurVal = euxmNil;
                break;
            case OP_PUSH:
                euxmStackCheckPush(euxcCurVal);
                break;
            case OP_CLOSE:
                if (!euxmCodep(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<code>", "close");
                }
                euxcCurVal = euxcMakeClosure(euxcCurVal, euxcCurEnv);
                break;
            case OP_DELAY:
                if (!euxmCodep(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<code>", "delay");
                }
                euxcCurVal = euxcMakePromise(euxcCurVal, euxcCurEnv);
                break;
            case OP_ATOM:
                euxcCurVal = (euxmAtom(euxcCurVal) ? euxs_t : euxmNil);
                break;
            case OP_EQ:
                tmp = euxmStackPop();
                if (euxmSymbolp(euxcCurVal) && euxmSymbolp(tmp))
                {
                    euxcCurVal =
                    (
                        euxmSymbolEq(euxcCurVal, tmp)
                      ? euxs_t
                      : euxmNil
                    );
                }
                else
                {
                    euxcCurVal = (euxcCurVal == tmp ? euxs_t : euxmNil);
                }
                break;
            case OP_NULL:
                euxcCurVal = (euxcCurVal ? euxmNil : euxs_t);
                break;
            case OP_CONS:
                euxcCurVal = euxcCons(euxcCurVal, euxmStackPop());
                break;
            case OP_CAR:
                if (!euxmConsp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<cons>", "car");
                }
                euxcCurVal = euxmCar(euxcCurVal);
                break;
            case OP_CDR:
                if (!euxmConsp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<cons>", "cdr");
                }
                euxcCurVal = euxmCdr(euxcCurVal);
                break;
            case OP_SETCAR:
                if (!euxmConsp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<cons>", "set-car");
                }
                tmp = euxmStackPop();
                euxmSetCar(euxcCurVal, tmp);
                euxcCurVal = tmp;
                break;
            case OP_SETCDR:
                if (!euxmConsp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<cons>", "set-cdr");
                }
                tmp = euxmStackPop();
                euxmSetCdr(euxcCurVal, tmp);
                euxcCurVal = tmp;
                break;
            case OP_GREFL:
                i = *pc++ << 8;
                i |= *pc++;
                tmp = euxmGetElement(euxcCurFun, i);
                euxcCurVal = euxmGetValue(tmp);
                if (euxcCurVal == euxls_unbound)
                {
                    char buf[128];
                    sprintf(buf, "variable unbound in module '%s'",
                    euxmGetString(euxmGetModuleName(euxmGetModule(tmp))));
                    euxcIntError(buf, tmp, euxls_unbound_error);
                }
                break;
            case OP_GSETL:
                i = *pc++ << 8;
                i |= *pc++;
                euxmSetValue(euxmGetElement(euxcCurFun, i), euxcCurVal);
                break;
            case OP_LITL:
                i = *pc++ << 8;
                i |= *pc++;
                euxcCurVal = euxmGetElement(euxcCurFun, i);
                break;
            case OP_ADD:
                tmp = euxmStackPop();
                if (euxmFPIp(euxcCurVal) && euxmFPIp(tmp))
                {
                    euxcCurVal = euxcMakeFPI
                    (
                        euxmGetFPI(euxcCurVal) + euxmGetFPI(tmp)
                    );
                }
                else if (euxmFPIp(euxcCurVal) && euxmDoubleFloatp(tmp))
                {
                    euxcCurVal = euxcMakeDoubleFloat
                    (
                        euxmGetFPI(euxcCurVal) + euxmGetDoubleFloat(tmp)
                    );
                }
                else if (euxmDoubleFloatp(euxcCurVal) && euxmFPIp(tmp))
                {
                    euxcCurVal = euxcMakeDoubleFloat
                    (
                        euxmGetDoubleFloat(euxcCurVal) + euxmGetFPI(tmp)
                    );
                }
                else if (euxmDoubleFloatp(euxcCurVal) && euxmDoubleFloatp(tmp))
                {
                    euxcCurVal = euxcMakeDoubleFloat
                    (
                        euxmGetDoubleFloat(euxcCurVal) + euxmGetDoubleFloat(tmp)
                    );
                }
                else if (!genericCall(euxls_binary_plus, tmp, euxcCurVal))
                {
                    euxmStackPush(tmp);
                    euxmStackPush(euxcCurVal);
                    euxcArgC = 2;
                    euxcCurVal = euxlAdd();
                }
                break;
            case OP_SUB:
                tmp = euxmStackPop();
                if (euxmFPIp(euxcCurVal) && euxmFPIp(tmp))
                {
                    euxcCurVal = euxcMakeFPI
                    (
                        euxmGetFPI(euxcCurVal) - euxmGetFPI(tmp)
                    );
                }
                else if (euxmFPIp(euxcCurVal) && euxmDoubleFloatp(tmp))
                {
                    euxcCurVal = euxcMakeDoubleFloat
                    (
                        euxmGetFPI(euxcCurVal) - euxmGetDoubleFloat(tmp)
                    );
                }
                else if (euxmDoubleFloatp(euxcCurVal) && euxmFPIp(tmp))
                {
                    euxcCurVal = euxcMakeDoubleFloat
                    (
                        euxmGetDoubleFloat(euxcCurVal) - euxmGetFPI(tmp)
                    );
                }
                else if (euxmDoubleFloatp(euxcCurVal) && euxmDoubleFloatp(tmp))
                {
                    euxcCurVal = euxcMakeDoubleFloat
                    (
                        euxmGetDoubleFloat(euxcCurVal) - euxmGetDoubleFloat(tmp)
                    );
                }
                else if (!genericCall(euxls_binary_minus, tmp, euxcCurVal))
                {
                    euxmStackPush(tmp);
                    euxmStackPush(euxcCurVal);
                    euxcArgC = 2;
                    euxcCurVal = euxlSub();
                }
                break;
            case OP_MUL:
                tmp = euxmStackPop();
                if (euxmFPIp(euxcCurVal) && euxmFPIp(tmp))
                {
                    euxcCurVal = euxcMakeFPI
                    (
                        euxmGetFPI(euxcCurVal) * euxmGetFPI(tmp)
                    );
                }
                else if (euxmFPIp(euxcCurVal) && euxmDoubleFloatp(tmp))
                {
                    euxcCurVal = euxcMakeDoubleFloat
                    (
                        euxmGetFPI(euxcCurVal) * euxmGetDoubleFloat(tmp)
                    );
                }
                else if (euxmDoubleFloatp(euxcCurVal) && euxmFPIp(tmp))
                {
                    euxcCurVal = euxcMakeDoubleFloat
                    (
                        euxmGetDoubleFloat(euxcCurVal) * euxmGetFPI(tmp)
                    );
                }
                else if (euxmDoubleFloatp(euxcCurVal) && euxmDoubleFloatp(tmp))
                {
                    euxcCurVal = euxcMakeDoubleFloat
                    (
                        euxmGetDoubleFloat(euxcCurVal) * euxmGetDoubleFloat(tmp)
                    );
                }
                else if (!genericCall(euxls_binary_times, tmp, euxcCurVal))
                {
                    euxmStackPush(tmp);
                    euxmStackPush(euxcCurVal);
                    euxcArgC = 2;
                    euxcCurVal = euxlMul();
                }
                break;
            case OP_DIV:
                tmp = euxmStackPop();
                if (euxmFPIp(euxcCurVal) && euxmFPIp(tmp))
                {
                    if (euxmGetFPI(tmp) == (euxmFPIType) 0)
                    {
                        euxcIntError
                        (
                            "division by zero",
                            euxcCurVal,
                            euxls_arith_error
                        );
                    }
                    euxcCurVal = euxcMakeFPI
                    (
                        euxmGetFPI(euxcCurVal) / euxmGetFPI(tmp)
                    );
                }
                else if (euxmFPIp(euxcCurVal) && euxmDoubleFloatp(tmp))
                {
                    if (euxmGetDoubleFloat(tmp) == (euxmDoubleFloatType) 0.0)
                    {
                        euxcIntError
                        (
                            "division by zero",
                            euxcCurVal,
                            euxls_arith_error
                        );
                    }
                    euxcCurVal = euxcMakeDoubleFloat
                    (
                        euxmGetFPI(euxcCurVal) / euxmGetDoubleFloat(tmp)
                    );
                }
                else if (euxmDoubleFloatp(euxcCurVal) && euxmFPIp(tmp))
                {
                    if (euxmGetFPI(tmp) == (euxmFPIType) 0)
                    {
                        euxcIntError
                        (
                            "division by zero",
                            euxcCurVal,
                            euxls_arith_error
                        );
                    }
                    euxcCurVal = euxcMakeDoubleFloat
                    (
                        euxmGetDoubleFloat(euxcCurVal) / euxmGetFPI(tmp)
                    );
                }
                else if (euxmDoubleFloatp(euxcCurVal) && euxmDoubleFloatp(tmp))
                {
                    if (euxmGetDoubleFloat(tmp) == (euxmDoubleFloatType) 0.0)
                    {
                        euxcIntError
                        (
                            "division by zero",
                            euxcCurVal,
                            euxls_arith_error
                        );
                    }
                    euxcCurVal = euxcMakeDoubleFloat
                    (
                        euxmGetDoubleFloat(euxcCurVal) / euxmGetDoubleFloat(tmp)
                    );
                }
                else if (!genericCall(euxls_binary_divide, tmp, euxcCurVal))
                {
                    euxmStackPush(tmp);
                    euxmStackPush(euxcCurVal);
                    euxcArgC = 2;
                    euxcCurVal = euxlDiv();
                }
                break;
            case OP_QUO:
                tmp = euxmStackPop();
                if (euxmFPIp(euxcCurVal) && euxmFPIp(tmp))
                {
                    euxmFPIType fixtmp;
                    if ((fixtmp = euxmGetFPI(tmp)) == (euxmFPIType) 0)
                    {
                        euxcIntError
                        (
                            "division by zero",
                            euxcCurVal,
                            euxls_arith_error
                        );
                    }
                    euxcCurVal = euxcMakeFPI(euxmGetFPI(euxcCurVal) / fixtmp);
                }
                else if (!genericCall(euxls_quotient, tmp, euxcCurVal))
                {
                    euxmStackPush(tmp);
                    euxmStackPush(euxcCurVal);
                    euxcArgC = 2;
                    euxcCurVal = euxlQuo();
                }
                break;
            case OP_LSS:
                tmp = euxmStackPop();
                if (euxmFPIp(euxcCurVal) && euxmFPIp(tmp))
                {
                    euxcCurVal =
                    (
                        euxmGetFPI(euxcCurVal) < euxmGetFPI(tmp)
                      ? euxs_t
                      : euxmNil
                    );
                }
                else if (euxmFPIp(euxcCurVal) && euxmDoubleFloatp(tmp))
                {
                    euxcCurVal =
                    (
                        euxmGetFPI(euxcCurVal) < euxmGetDoubleFloat(tmp)
                      ? euxs_t
                      : euxmNil
                    );
                }
                else if (euxmDoubleFloatp(euxcCurVal) && euxmFPIp(tmp))
                {
                    euxcCurVal =
                    (
                        euxmGetDoubleFloat(euxcCurVal) < euxmGetFPI(tmp)
                      ? euxs_t
                      : euxmNil
                    );
                }
                else if (euxmDoubleFloatp(euxcCurVal) && euxmDoubleFloatp(tmp))
                {
                    euxcCurVal =
                    (
                        euxmGetDoubleFloat(euxcCurVal) < euxmGetDoubleFloat(tmp)
                      ? euxs_t
                      : euxmNil
                    );
                }
                else if (!genericCall(euxls_binary_less, tmp, euxcCurVal))
                {
                    euxmStackPush(tmp);
                    euxmStackPush(euxcCurVal);
                    euxcArgC = 2;
                    euxcCurVal = euxlLt();
                }
                break;
            case OP_EQL:
                tmp = euxmStackPop();
                if (euxmFPIp(euxcCurVal) && euxmFPIp(tmp))
                {
                    euxcCurVal =
                    (
                        euxmGetFPI(euxcCurVal) == euxmGetFPI(tmp)
                      ? euxs_t
                      : euxmNil
                    );
                }
                else if (euxmFPIp(euxcCurVal) && euxmDoubleFloatp(tmp))
                {
                    euxcCurVal =
                    (
                        euxmGetFPI(euxcCurVal) == euxmGetDoubleFloat(tmp)
                      ? euxs_t
                      : euxmNil
                    );
                }
                else if (euxmDoubleFloatp(euxcCurVal) && euxmFPIp(tmp))
                {
                    euxcCurVal =
                    (
                        euxmGetDoubleFloat(euxcCurVal) == euxmGetFPI(tmp)
                      ? euxs_t
                      : euxmNil
                    );
                }
                else if (euxmDoubleFloatp(euxcCurVal) && euxmDoubleFloatp(tmp))
                {
                    euxcCurVal =
                    (
                        euxmGetDoubleFloat(euxcCurVal) ==
                            euxmGetDoubleFloat(tmp)
                          ? euxs_t
                          : euxmNil
                    );
                }
                else if (!genericCall(euxls_binary_equal, tmp, euxcCurVal))
                {
                    euxmStackPush(tmp);
                    euxmStackPush(euxcCurVal);
                    euxcArgC = 2;
                    euxcCurVal = euxlEql();
                }
                break;
            case OP_GTR:
                tmp = euxmStackPop();
                if (euxmFPIp(euxcCurVal) && euxmFPIp(tmp))
                {
                    euxcCurVal =
                    (
                        euxmGetFPI(euxcCurVal) > euxmGetFPI(tmp)
                      ? euxs_t
                      : euxmNil
                    );
                }
                else if (euxmFPIp(euxcCurVal) && euxmDoubleFloatp(tmp))
                {
                    euxcCurVal =
                    (
                        euxmGetFPI(euxcCurVal) > euxmGetDoubleFloat(tmp)
                      ? euxs_t
                      : euxmNil
                    );
                }
                else if (euxmDoubleFloatp(euxcCurVal) && euxmFPIp(tmp))
                {
                    euxcCurVal =
                    (
                        euxmGetDoubleFloat(euxcCurVal) > euxmGetFPI(tmp)
                      ? euxs_t
                      : euxmNil
                    );
                }
                else if (euxmDoubleFloatp(euxcCurVal) && euxmDoubleFloatp(tmp))
                {
                    euxcCurVal =
                    (
                        euxmGetDoubleFloat(euxcCurVal) > euxmGetDoubleFloat(tmp)
                      ? euxs_t
                      : euxmNil
                    );
                }
                else if (!genericCall(euxls_binary_less, euxcCurVal, tmp))
                {
                    euxmStackPush(tmp);
                    euxmStackPush(euxcCurVal);
                    euxcArgC = 2;
                    euxcCurVal = euxlGt();
                }
                break;
            case OP_CLASSOF:
                euxcCurVal = euxcClassOf(euxcCurVal);
                break;
            case OP_CNM:       // (apply (euxmCar mfl) (euxmCdr mfl) al al)
                if (euxcCurVal == euxmNil)
                {
                    // next method list
                    euxmStackDrop(1);    // arg list
                    euxcCerror("no next method in call-next-method", euxcCurFun,
                    euxls_no_next_md_error);
                }
                {
                    euxlValue *p;
                    euxlValue mfl, al, args;
                    al = euxmStackPop();
                    mfl = euxcCurVal;
                    euxcArgC = euxcListSize(al);
                    euxmStackCheck(euxcArgC + 2);
                    args = al;
                    for
                    (
                        euxcStackPtr -= euxcArgC, p = euxcStackPtr;
                        euxmConsp(args);
                        args = euxmCdr(args)
                    )
                    {
                        *p++ = euxmCar(args);
                    }
                    euxmStackPush(al);
                    euxmStackPush(euxmCdr(mfl));
                    euxcCurVal = euxmCar(mfl);
                    euxcArgC += 2;
                    euxcApply();
                }
                break;
            case OP_GETIVAR:
                tmp = euxmStackPop();
                #ifdef CHECK_REF
                if
                (
                    !euxmFPIp(tmp)
                 || !euxmObjectp(euxcCurVal)
                 || euxmGetFPI(tmp)
                  > euxmGetFPI
                    (
                        euxmGetIVar(euxmGetClass(euxcCurVal),
                        euxmInstanceSizeId)
                    )
                )
                {
                    badSlotAccess("read", tmp, euxcCurVal);
                }
                #endif
                euxcCurVal = euxmGetIVar(euxcCurVal, euxmGetFPI(tmp));
                break;
            case OP_SETIVAR:
                {
                    tmp = euxmStackPop();
                    #ifdef CHECK_REF
                    if
                    (
                        !euxmFPIp(tmp)
                     || !euxmObjectp(euxcCurVal)
                     || euxmGetFPI(tmp)
                      > euxmGetFPI
                        (
                            euxmGetIVar
                            (
                                euxmGetClass(euxcCurVal),
                                euxmInstanceSizeId
                            )
                        )
                    )
                    {
                        badSlotAccess("write", tmp, euxcCurVal);
                    }
                    #endif
                    euxmFPIType fixtmp = euxmGetFPI(tmp);
                    tmp = euxmStackPop();
                    euxmSetIVar(euxcCurVal, fixtmp, tmp);
                    euxcCurVal = tmp;
                }
                break;
                // these don't need the bother of frames and are used a lot
            case OP_GET:
                tmp = euxmStackPop();
                if (!euxmSymbolp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<symbol>", "get");
                }
                if (!euxmSymbolp(tmp))
                {
                    badArgType(tmp, "<symbol>", "get");
                }
                euxcCurVal = euxcGetProp(euxcCurVal, tmp);
                break;
            case OP_PUT:
                tmp = euxmStackPop();
                tmp2 = euxmStackPop();
                if (!euxmSymbolp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<symbol>", "put");
                }
                if (!euxmSymbolp(tmp))
                {
                    badArgType(tmp, "<symbol>", "put");
                }
                euxcPutProp(euxcCurVal, tmp2, tmp);
                euxcCurVal = tmp2;
                break;
            case OP_CURMOD:
                euxcArgC = 0;
                euxcCurVal = euxcCurrentMod();
                break;
            case OP_CONSP:
                euxcCurVal = euxmConsp(euxcCurVal) ? euxs_t : euxmNil;
                break;
            case OP_SYMBOLP:
                euxcCurVal = euxmSymbolp(euxcCurVal) ? euxs_t : euxmNil;
                break;
            case OP_VECTORP:
                euxcCurVal = euxmVectorp(euxcCurVal) ? euxs_t : euxmNil;
                break;
            case OP_APPEND:    // 2 args
                tmp = euxmStackPop();
                if (!euxmListp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<list>", "append");
                }
                if (euxcCurVal == euxmNil)
                {
                    euxcCurVal = tmp;
                }
                else
                {
                    euxlValue end;
                    euxmStackPush(tmp);
                    end = euxcCons(euxmCar(euxcCurVal), euxmNil);
                    euxmStackCheckPush(end);
                    for
                    (
                        euxcCurVal = euxmCdr(euxcCurVal);
                        euxmConsp(euxcCurVal);
                        euxcCurVal = euxmCdr(euxcCurVal)
                    )
                    {
                        euxmSetCdr(end, euxcCons(euxmCar(euxcCurVal), euxmNil));
                        end = euxmCdr(end);
                    }
                    euxmSetCdr(end, tmp);
                    euxcCurVal = euxmStackPop();
                    euxmStackDrop(1);    // tmp
                }
                break;
            case OP_LIST:      // 2 args
                euxcCurVal = euxcCons
                (
                    euxcCurVal,
                    euxcCons(euxmStackPop(), euxmNil)
                );
                break;
            case OP_SIZE:
                if (!euxmListp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<list>", "list-size");
                }
                euxmStackCheckPush(euxcCurVal);
                euxcArgC = 1;
                euxcCurVal = euxlSize();
                break;
            case OP_REVERSE:
                if (!euxmListp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<list>", "reverse");
                }
                euxmStackCheckPush(euxcCurVal);
                euxcArgC = 1;
                euxcCurVal = euxlReverseList();
                break;
            case OP_CAAR:
                if (!euxmConsp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<cons>", "caar");
                }
                euxcCurVal = euxmCar(euxcCurVal);
                if (!euxmConsp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<cons>", "caar");
                }
                euxcCurVal = euxmCar(euxcCurVal);
                break;
            case OP_CADR:
                if (!euxmConsp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<cons>", "cadr");
                }
                euxcCurVal = euxmCdr(euxcCurVal);
                if (!euxmConsp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<cons>", "cadr");
                }
                euxcCurVal = euxmCar(euxcCurVal);
                break;
            case OP_CDAR:
                if (!euxmConsp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<cons>", "cdar");
                }
                euxcCurVal = euxmCar(euxcCurVal);
                if (!euxmConsp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<cons>", "cdar");
                }
                euxcCurVal = euxmCdr(euxcCurVal);
                break;
            case OP_CDDR:
                if (!euxmConsp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<cons>", "cddr");
                }
                euxcCurVal = euxmCdr(euxcCurVal);
                if (!euxmConsp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<cons>", "cddr");
                }
                euxcCurVal = euxmCdr(euxcCurVal);
                break;
            case OP_GETSYNTAX:
                tmp = euxmStackPop();
                if (!euxmSymbolp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<symbol>", "get");
                }
                if (!euxmSymbolp(tmp))
                {
                    badArgType(tmp, "<symbol>", "get");
                }
                euxcCurVal = euxcGetSyntax(euxcCurVal, tmp);
                break;
            case OP_PUTSYNTAX:
                tmp = euxmStackPop();
                tmp2 = euxmStackPop();
                if (!euxmSymbolp(euxcCurVal))
                {
                    badArgType(euxcCurVal, "<symbol>", "put");
                }
                if (!euxmSymbolp(tmp))
                {
                    badArgType(tmp, "<symbol>", "put");
                }
                euxcPutSyntax(euxcCurVal, tmp2, tmp);
                euxcCurVal = tmp2;
                break;
                #ifndef NO_CHECK_REF
            case OP_CHECKREF:
                tmp = euxmStackPop();    // the object
                if (!euxmClassp(euxcCurVal))     // the class
                {
                    euxcIntError
                    (
                        "not a class in check-ref",
                        euxcCurVal,
                        euxls_telos_error
                    );
                }
                if (!euxcSubClassp(euxcClassOf(tmp), euxcCurVal))
                {
                    euxcTelosBadRefError(tmp, euxcCurVal, euxmTrue);
                }
                euxcCurVal = euxs_t;
                break;
                #endif
            default:
                euxcError("bad opcode", euxmMakeSmallFPI((euxmFPIType) * --pc));
                break;
        }
    }
}

///  findVar - find a variable in an environment
static euxlValue findVar(euxlValue env, euxlValue var, int *poff)
{
    for (; env != euxmNil; env = euxmCdr(env))
    {
        euxlValue names = euxmGetElement(euxmCar(env), 0);
        for (int off = 1; names != euxmNil; ++off, names = euxmCdr(names))
        {
            if (var == euxmCar(names))
            {
                *poff = off;
                return (env);
            }
        }
    }
    return (euxmNil);
}

///  euxcApply - apply a function to arguments
//    The function should be in euxcCurVal and the arguments should
//    be on the stack.  The number of arguments should be in euxcArgC.
void euxcApply()
{
    static char *functionName = "function apply";

    // Check for euxmNull function
    if (euxmNull(euxcCurVal))
    {
        badFunctionNodeType(euxcCurVal);
    }

    // dispatch on function type
    switch (euxmNodeType(euxcCurVal))
    {
        case euxmFun:
            euxcCurVal = (*euxmGetFun(euxcCurVal)) ();
            euxcReturn();
            break;
        case euxmXFun:
            (*euxmGetXFun(euxcCurVal)) ();
            break;
        case euxmClosure:
            euxcCurFun = euxmGetCode(euxcCurVal);
            euxcCurEnv = euxmGetCEnv(euxcCurVal);
            base = pc = euxmGetCodestr(euxcCurFun);
            break;
        case euxmGeneric:
            {
                euxlValue al, applicable;
                int i;
                al = euxmNil;       // consing on function call :-(
                for (i = euxcArgC - 1; i >= 0; i--)
                {
                    al = euxcCons(euxcStackPtr[i], al);     // the arg list
                }
                euxmStackCheckPush(al);
                applicable = euxcFindAndCacheMethods(euxcCurVal, al);
                if (applicable == euxmNil)
                {
                    euxcCurVal = euxcCons(euxmGetGenericName(euxcCurVal), al);
                    // discard the args and arglist
                    euxmStackDrop(euxcArgC + 1);
                    euxcCerror
                    (
                        "no applicable methods",
                        euxcCurVal,
                        euxls_no_applic_error
                    );
                }
                euxmStackCheckPush(euxmCdr(applicable));
                euxcCurVal = euxmCar(applicable);
                euxcArgC += 2;
                euxcApply();
            }
            break;
        case euxmContinuation:
            {
                // zero or one arg allowed
                euxlValue tmp = euxmMoreArgs()? euxmGetArg() : euxmNil;
                euxmLastArg();
                restoreContinuation();
                euxcCurVal = tmp;
                euxcReturn();
            }
            break;
        default:
            badFunctionNodeType(euxcCurVal);
    }
}

///  euxcReturn - return to a continuation on the stack
void euxcReturn()
{
    // restore the environment and the continuation function
    euxcCurEnv = euxmStackPop();
    euxlValue tmp = euxmStackPop();

    // dispatch on the function type
    switch (euxmNodeType(tmp))
    {
        case euxmCode:
            euxcCurFun = tmp;
            tmp = euxmStackPop();
            base = euxmGetCodestr(euxcCurFun);
            pc = base + (int)euxmGetSmallFPI(tmp);
            break;
        case euxmXFunCont:
            (*euxmGetXFun(tmp)) ();
            break;
        default:
            euxcError("bad continuation", tmp);
    }
}

///  euxcCurrentContinuation - save a stack snapshot
//    cc is euxs_t if return address is needed, e.g., in the interpreter
euxlValue euxcCurrentContinuation(int cc)
{
    if (cc)
    {
        euxmStackCheck(4);
        euxmStackPush(euxmMakeSmallFPI((euxmFPIType) (pc - base)));
        euxmStackPush(euxcCurFun);
        euxmStackPush(euxcCurEnv);
    }
    else
    {
        euxmStackCheck(3);
    }

    // store the thread dynamic state
    // c.f. thread.em get-state
    euxmStackCheckPush(euxmGetIVar(euxmGetValue(euxls_current_thread), 4));
    #if 0
    euxcPutString(euxlStdout(), "<save ");
    euxcPrin1(euxmStackTop(), euxlStdout());
    euxcPutString(euxlStdout(), ">");
    #endif

    // create and initialize a continuation object
    int size = (int)(euxcStackTop - euxcStackPtr);
    euxlValue cont = euxcNewContinuation(size);
    euxlValue *src, *dst;
    for (src = euxcStackPtr, dst = &cont->value.vector.data[0]; --size >= 0;)
    {
        *dst++ = *src++;
    }

    euxmStackDrop(1);    // euxmStackDrop the state

    // return the continuation
    return (cont);
}

///  restoreContinuation - restore a continuation to the stack
///       The continuation should be in euxcCurVal.
static void restoreContinuation()
{
    int size = euxmGetSize(euxcCurVal);
    euxlValue *src;
    for
    (
        src = &euxcCurVal->value.vector.data[size], euxcStackPtr = euxcStackTop;
        --size >= 0;
    )
    {
        *--euxcStackPtr = *--src;
    }

    // restore the thread dynamic state
    // c.f. thread.em set-state
    #if 0
    euxcPutString(euxlStdout(), "<restore ");
    euxcPrin1(euxmStackTop(), euxlStdout());
    euxcPutString(euxlStdout(), ">");
    #endif

    euxmSetIVar(euxmGetValue(euxls_current_thread), 4, euxmStackPop());
}

///  genericCall - call gf associated with an inlined operator
static int genericCall(euxlValue sym, euxlValue val1, euxlValue val2)
{
    euxlValue op = euxmGetValue(sym);
    if (!euxmGenericp(op))
    {
        return euxmFalse;   // generic not defined yet
    }

    // OP_SAVE
    int i = (int)(pc - base);
    euxmStackCheck(5);
    euxmStackPush(euxmMakeSmallFPI((euxmFPIType) i));
    euxmStackPush(euxcCurFun);
    euxmStackPush(euxcCurEnv);
    // args and function
    euxmStackPush(val1);
    euxmStackPush(val2);
    euxcCurVal = op;
    // OP_CALL
    euxcArgC = 2;
    euxcApply();

    return euxmTrue;
}

///  euxcGcProtect - protect the state of the interpreter from the collector
void euxcGcProtect(void (*protected_fcn) ())
{
    int pcoff = pc - base;
    (*protected_fcn) ();
    if (euxcCurFun)
    {
        base = euxmGetCodestr(euxcCurFun);
        pc = base + pcoff;
    }
}

///  badFunctionNodeType - bad function error
static void badFunctionNodeType(euxlValue arg)
{
    euxcBadType(arg, "<function>", "function application");
}

///  badArgType - bad argument type error
///  cf euxcBadType in xsftab.c
static void badArgType(euxlValue arg, const char *name, const char *fn)
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

    euxcIntError(buf, arg, euxls_bad_type_error);
}

///  euxcStackOverflow - value stack overflow
void euxcStackOverflow()
{
    euxcAbort("value stack overflow");
}

///  badSlotAccess -
static void badSlotAccess(const char *msg, euxlValue index, euxlValue object)
{
    euxmStackCheckPush(index);
    char buf[20];
    sprintf(buf, "bad slot %s", msg);
    object = euxcCons(object, euxmNil);
    object = euxcCons(euxmInternAndExport("object:"), object);
    object = euxcCons(index, object);
    object = euxcCons(euxmInternAndExport("slot:"), object);
    euxmStackDrop(1);
    euxcCerror(buf, object, euxmNil);
}


///-----------------------------------------------------------------------------
