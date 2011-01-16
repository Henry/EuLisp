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
    xlfun = euxmGetCode(fun);
    xlenv = euxmGetCEnv(fun);
    xlval = euxmNil;

    // initialize the argument count
    euxcArgC = 0;

    // set the initial pc
    base = pc = euxmGetCodestr(xlfun);

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
            euxcDecodeInstruction(euxlStdout(), xlfun, (int)(pc - base), xlenv);
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
                xlval = euxmGetElement(xlfun, *pc++);
                break;
            case OP_GREF:
                tmp = euxmGetElement(xlfun, *pc++);
                xlval = euxmGetValue(tmp);
                if (xlval == euxls_unbound)
                {
                    char buf[128];
                    sprintf(buf, "variable unbound in module '%s'",
                    euxmGetString(euxmGetModuleName(euxmGetModule(tmp))));
                    euxcIntError(buf, tmp, euxls_unbound_error);
                }
                break;
            case OP_GSET:
                euxmSetValue(euxmGetElement(xlfun, *pc++), xlval);
                break;
            case OP_EREF:
                k = *pc++;
                tmp = xlenv;
                while (--k >= 0)
                {
                    tmp = euxmCdr(tmp);
                }
                xlval = euxmGetElement(euxmCar(tmp), *pc++);
                break;
            case OP_ESET:
                k = *pc++;
                tmp = xlenv;
                while (--k >= 0)
                {
                    tmp = euxmCdr(tmp);
                }
                euxmSetElement(euxmCar(tmp), *pc++, xlval);
                break;
            case OP_AREF:
                {
                    i = *pc++;
                    tmp = xlval;
                    if (!euxmEnvp(tmp))
                    {
                        badArgType(tmp, "<env>", "aref");
                    }
                    int off = 0;
                    if
                    (
                        (tmp = findVar(tmp, euxmGetElement(xlfun, i), &off))
                     != euxmNil
                    )
                    {
                        xlval = euxmGetElement(euxmCar(tmp), off);
                    }
                    else
                    {
                        xlval = euxls_unassigned;
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
                        (tmp = findVar(tmp, euxmGetElement(xlfun, i), &off))
                     == euxmNil
                    )
                    {
                        euxcIntError
                        (
                            "no binding for variable",
                            euxmGetElement(xlfun, i),
                            euxls_unbound_error
                        );
                    }
                    euxmSetElement(euxmCar(tmp), off, xlval);
                }
                break;
            case OP_SAVE:      // save a continuation
                i = *pc++ << 8;
                i |= *pc++;
                euxmStackCheck(3);
                euxmStackPush(euxmMakeSmallFPI((euxmFPIType) i));
                euxmStackPush(xlfun);
                euxmStackPush(xlenv);
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
                xlenv = euxcNewFrame(xlenv, i);
                euxmSetElement(euxmCar(xlenv), 0, euxmGetVNames(xlfun));
                break;
            case OP_MVARG:     // move required argument to frame slot
                i = *pc++;      // get the slot number
                if (--euxcArgC < 0)
                {
                    euxcTooFewInt();
                }
                euxmSetElement(euxmCar(xlenv), i, euxmStackPop());
                break;
            case OP_MVOARG:    // move optional argument to frame slot
                i = *pc++;      // get the slot number
                if (euxcArgC > 0)
                {
                    euxmSetElement(euxmCar(xlenv), i, euxmStackPop());
                    --euxcArgC;
                }
                else
                {
                    euxmSetElement(euxmCar(xlenv), i, euxl_default_object);
                }
                break;
            case OP_MVRARG:    // build rest argument and move to frame slot
                i = *pc++;      // get the slot number
                for (xlval = euxmNil, k = euxcArgC; --k >= 0;)
                {
                    xlval = euxcCons(euxcStackPtr[k], xlval);
                }
                euxmSetElement(euxmCar(xlenv), i, xlval);
                euxmStackDrop(euxcArgC);
                break;
            case OP_ALAST:     // make sure there are no more arguments
                if (euxcArgC > 0)
                {
                    euxcTooManyInt();
                }
                break;
            case OP_T:
                xlval = euxl_true;
                break;
            case OP_NIL:
                xlval = euxmNil;
                break;
            case OP_PUSH:
                euxmStackCheckPush(xlval);
                break;
            case OP_CLOSE:
                if (!euxmCodep(xlval))
                {
                    badArgType(xlval, "<code>", "close");
                }
                xlval = euxcMakeClosure(xlval, xlenv);
                break;
            case OP_DELAY:
                if (!euxmCodep(xlval))
                {
                    badArgType(xlval, "<code>", "delay");
                }
                xlval = euxcMakePromise(xlval, xlenv);
                break;
            case OP_ATOM:
                xlval = (euxmAtom(xlval) ? euxl_true : euxmNil);
                break;
            case OP_EQ:
                tmp = euxmStackPop();
                if (euxmSymbolp(xlval) && euxmSymbolp(tmp))
                {
                    xlval = (euxmSymbolEq(xlval, tmp) ? euxl_true : euxmNil);
                }
                else
                {
                    xlval = (xlval == tmp ? euxl_true : euxmNil);
                }
                break;
            case OP_NULL:
                xlval = (xlval ? euxmNil : euxl_true);
                break;
            case OP_CONS:
                xlval = euxcCons(xlval, euxmStackPop());
                break;
            case OP_CAR:
                if (!euxmConsp(xlval))
                {
                    badArgType(xlval, "<cons>", "car");
                }
                xlval = euxmCar(xlval);
                break;
            case OP_CDR:
                if (!euxmConsp(xlval))
                {
                    badArgType(xlval, "<cons>", "cdr");
                }
                xlval = euxmCdr(xlval);
                break;
            case OP_SETCAR:
                if (!euxmConsp(xlval))
                {
                    badArgType(xlval, "<cons>", "set-car");
                }
                tmp = euxmStackPop();
                euxmSetCar(xlval, tmp);
                xlval = tmp;
                break;
            case OP_SETCDR:
                if (!euxmConsp(xlval))
                {
                    badArgType(xlval, "<cons>", "set-cdr");
                }
                tmp = euxmStackPop();
                euxmSetCdr(xlval, tmp);
                xlval = tmp;
                break;
            case OP_GREFL:
                i = *pc++ << 8;
                i |= *pc++;
                tmp = euxmGetElement(xlfun, i);
                xlval = euxmGetValue(tmp);
                if (xlval == euxls_unbound)
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
                euxmSetValue(euxmGetElement(xlfun, i), xlval);
                break;
            case OP_LITL:
                i = *pc++ << 8;
                i |= *pc++;
                xlval = euxmGetElement(xlfun, i);
                break;
            case OP_ADD:
                tmp = euxmStackPop();
                if (euxmFPIp(xlval) && euxmFPIp(tmp))
                {
                    xlval = euxcMakeFPI(euxmGetFPI(xlval) + euxmGetFPI(tmp));
                }
                else if (euxmFPIp(xlval) && euxmDoubleFloatp(tmp))
                {
                    xlval = euxcMakeDoubleFloat
                    (
                        euxmGetFPI(xlval) + euxmGetDoubleFloat(tmp)
                    );
                }
                else if (euxmDoubleFloatp(xlval) && euxmFPIp(tmp))
                {
                    xlval = euxcMakeDoubleFloat
                    (
                        euxmGetDoubleFloat(xlval) + euxmGetFPI(tmp)
                    );
                }
                else if (euxmDoubleFloatp(xlval) && euxmDoubleFloatp(tmp))
                {
                    xlval = euxcMakeDoubleFloat
                    (
                        euxmGetDoubleFloat(xlval) + euxmGetDoubleFloat(tmp)
                    );
                }
                else if (!genericCall(euxls_binary_plus, tmp, xlval))
                {
                    euxmStackPush(tmp);
                    euxmStackPush(xlval);
                    euxcArgC = 2;
                    xlval = euxlAdd();
                }
                break;
            case OP_SUB:
                tmp = euxmStackPop();
                if (euxmFPIp(xlval) && euxmFPIp(tmp))
                {
                    xlval = euxcMakeFPI(euxmGetFPI(xlval) - euxmGetFPI(tmp));
                }
                else if (euxmFPIp(xlval) && euxmDoubleFloatp(tmp))
                {
                    xlval = euxcMakeDoubleFloat
                    (
                        euxmGetFPI(xlval) - euxmGetDoubleFloat(tmp)
                    );
                }
                else if (euxmDoubleFloatp(xlval) && euxmFPIp(tmp))
                {
                    xlval = euxcMakeDoubleFloat
                    (
                        euxmGetDoubleFloat(xlval) - euxmGetFPI(tmp)
                    );
                }
                else if (euxmDoubleFloatp(xlval) && euxmDoubleFloatp(tmp))
                {
                    xlval = euxcMakeDoubleFloat
                    (
                        euxmGetDoubleFloat(xlval) - euxmGetDoubleFloat(tmp)
                    );
                }
                else if (!genericCall(euxls_binary_minus, tmp, xlval))
                {
                    euxmStackPush(tmp);
                    euxmStackPush(xlval);
                    euxcArgC = 2;
                    xlval = euxlSub();
                }
                break;
            case OP_MUL:
                tmp = euxmStackPop();
                if (euxmFPIp(xlval) && euxmFPIp(tmp))
                {
                    xlval = euxcMakeFPI(euxmGetFPI(xlval) * euxmGetFPI(tmp));
                }
                else if (euxmFPIp(xlval) && euxmDoubleFloatp(tmp))
                {
                    xlval = euxcMakeDoubleFloat
                    (
                        euxmGetFPI(xlval) * euxmGetDoubleFloat(tmp)
                    );
                }
                else if (euxmDoubleFloatp(xlval) && euxmFPIp(tmp))
                {
                    xlval = euxcMakeDoubleFloat
                    (
                        euxmGetDoubleFloat(xlval) * euxmGetFPI(tmp)
                    );
                }
                else if (euxmDoubleFloatp(xlval) && euxmDoubleFloatp(tmp))
                {
                    xlval = euxcMakeDoubleFloat
                    (
                        euxmGetDoubleFloat(xlval) * euxmGetDoubleFloat(tmp)
                    );
                }
                else if (!genericCall(euxls_binary_times, tmp, xlval))
                {
                    euxmStackPush(tmp);
                    euxmStackPush(xlval);
                    euxcArgC = 2;
                    xlval = euxlMul();
                }
                break;
            case OP_DIV:
                tmp = euxmStackPop();
                if (euxmFPIp(xlval) && euxmFPIp(tmp))
                {
                    if (euxmGetFPI(tmp) == (euxmFPIType) 0)
                    {
                        euxcIntError
                        (
                            "division by zero",
                            xlval,
                            euxls_arith_error
                        );
                    }
                    xlval = euxcMakeFPI(euxmGetFPI(xlval) / euxmGetFPI(tmp));
                }
                else if (euxmFPIp(xlval) && euxmDoubleFloatp(tmp))
                {
                    if (euxmGetDoubleFloat(tmp) == (euxmDoubleFloatType) 0.0)
                    {
                        euxcIntError
                        (
                            "division by zero",
                            xlval,
                            euxls_arith_error
                        );
                    }
                    xlval = euxcMakeDoubleFloat
                    (
                        euxmGetFPI(xlval) / euxmGetDoubleFloat(tmp)
                    );
                }
                else if (euxmDoubleFloatp(xlval) && euxmFPIp(tmp))
                {
                    if (euxmGetFPI(tmp) == (euxmFPIType) 0)
                    {
                        euxcIntError
                        (
                            "division by zero",
                            xlval,
                            euxls_arith_error
                        );
                    }
                    xlval = euxcMakeDoubleFloat
                    (
                        euxmGetDoubleFloat(xlval) / euxmGetFPI(tmp)
                    );
                }
                else if (euxmDoubleFloatp(xlval) && euxmDoubleFloatp(tmp))
                {
                    if (euxmGetDoubleFloat(tmp) == (euxmDoubleFloatType) 0.0)
                    {
                        euxcIntError
                        (
                            "division by zero",
                            xlval,
                            euxls_arith_error
                        );
                    }
                    xlval = euxcMakeDoubleFloat
                    (
                        euxmGetDoubleFloat(xlval) / euxmGetDoubleFloat(tmp)
                    );
                }
                else if (!genericCall(euxls_binary_divide, tmp, xlval))
                {
                    euxmStackPush(tmp);
                    euxmStackPush(xlval);
                    euxcArgC = 2;
                    xlval = euxlDiv();
                }
                break;
            case OP_QUO:
                tmp = euxmStackPop();
                if (euxmFPIp(xlval) && euxmFPIp(tmp))
                {
                    euxmFPIType fixtmp;
                    if ((fixtmp = euxmGetFPI(tmp)) == (euxmFPIType) 0)
                    {
                        euxcIntError
                        (
                            "division by zero",
                            xlval,
                            euxls_arith_error
                        );
                    }
                    xlval = euxcMakeFPI(euxmGetFPI(xlval) / fixtmp);
                }
                else if (!genericCall(euxls_quotient, tmp, xlval))
                {
                    euxmStackPush(tmp);
                    euxmStackPush(xlval);
                    euxcArgC = 2;
                    xlval = euxlQuo();
                }
                break;
            case OP_LSS:
                tmp = euxmStackPop();
                if (euxmFPIp(xlval) && euxmFPIp(tmp))
                {
                    xlval =
                    (
                        euxmGetFPI(xlval) < euxmGetFPI(tmp)
                      ? euxl_true
                      : euxmNil
                    );
                }
                else if (euxmFPIp(xlval) && euxmDoubleFloatp(tmp))
                {
                    xlval =
                    (
                        euxmGetFPI(xlval) < euxmGetDoubleFloat(tmp)
                      ? euxl_true
                      : euxmNil
                    );
                }
                else if (euxmDoubleFloatp(xlval) && euxmFPIp(tmp))
                {
                    xlval =
                    (
                        euxmGetDoubleFloat(xlval) < euxmGetFPI(tmp)
                      ? euxl_true
                      : euxmNil
                    );
                }
                else if (euxmDoubleFloatp(xlval) && euxmDoubleFloatp(tmp))
                {
                    xlval =
                    (
                        euxmGetDoubleFloat(xlval) < euxmGetDoubleFloat(tmp)
                      ? euxl_true
                      : euxmNil
                    );
                }
                else if (!genericCall(euxls_binary_less, tmp, xlval))
                {
                    euxmStackPush(tmp);
                    euxmStackPush(xlval);
                    euxcArgC = 2;
                    xlval = euxlLt();
                }
                break;
            case OP_EQL:
                tmp = euxmStackPop();
                if (euxmFPIp(xlval) && euxmFPIp(tmp))
                {
                    xlval =
                    (
                        euxmGetFPI(xlval) == euxmGetFPI(tmp)
                      ? euxl_true
                      : euxmNil
                    );
                }
                else if (euxmFPIp(xlval) && euxmDoubleFloatp(tmp))
                {
                    xlval =
                    (
                        euxmGetFPI(xlval) == euxmGetDoubleFloat(tmp)
                      ? euxl_true
                      : euxmNil
                    );
                }
                else if (euxmDoubleFloatp(xlval) && euxmFPIp(tmp))
                {
                    xlval =
                    (
                        euxmGetDoubleFloat(xlval) == euxmGetFPI(tmp)
                      ? euxl_true
                      : euxmNil
                    );
                }
                else if (euxmDoubleFloatp(xlval) && euxmDoubleFloatp(tmp))
                {
                    xlval =
                    (
                        euxmGetDoubleFloat(xlval) == euxmGetDoubleFloat(tmp)
                      ? euxl_true
                      : euxmNil
                    );
                }
                else if (!genericCall(euxls_binary_equal, tmp, xlval))
                {
                    euxmStackPush(tmp);
                    euxmStackPush(xlval);
                    euxcArgC = 2;
                    xlval = euxlEql();
                }
                break;
            case OP_GTR:
                tmp = euxmStackPop();
                if (euxmFPIp(xlval) && euxmFPIp(tmp))
                {
                    xlval =
                    (
                        euxmGetFPI(xlval) > euxmGetFPI(tmp)
                      ? euxl_true
                      : euxmNil
                    );
                }
                else if (euxmFPIp(xlval) && euxmDoubleFloatp(tmp))
                {
                    xlval =
                    (
                        euxmGetFPI(xlval) > euxmGetDoubleFloat(tmp)
                      ? euxl_true
                      : euxmNil
                    );
                }
                else if (euxmDoubleFloatp(xlval) && euxmFPIp(tmp))
                {
                    xlval =
                    (
                        euxmGetDoubleFloat(xlval) > euxmGetFPI(tmp)
                      ? euxl_true
                      : euxmNil
                    );
                }
                else if (euxmDoubleFloatp(xlval) && euxmDoubleFloatp(tmp))
                {
                    xlval =
                    (
                        euxmGetDoubleFloat(xlval) > euxmGetDoubleFloat(tmp)
                      ? euxl_true
                      : euxmNil
                    );
                }
                else if (!genericCall(euxls_binary_less, xlval, tmp))
                {
                    euxmStackPush(tmp);
                    euxmStackPush(xlval);
                    euxcArgC = 2;
                    xlval = euxlGt();
                }
                break;
            case OP_CLASSOF:
                xlval = euxcClassOf(xlval);
                break;
            case OP_CNM:       // (apply (euxmCar mfl) (euxmCdr mfl) al al)
                if (xlval == euxmNil)
                {
                    // next method list
                    euxmStackDrop(1);    // arg list
                    euxcCerror("no next method in call-next-method", xlfun,
                    euxls_no_next_md_error);
                }
                {
                    euxlValue *p;
                    euxlValue mfl, al, args;
                    al = euxmStackPop();
                    mfl = xlval;
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
                    xlval = euxmCar(mfl);
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
                 || !euxmObjectp(xlval)
                 || euxmGetFPI(tmp)
                  > euxmGetFPI
                    (
                        euxmGetIVar(euxmGetClass(xlval),
                        euxmInstanceSizeId)
                    )
                )
                {
                    badSlotAccess("read", tmp, xlval);
                }
                #endif
                xlval = euxmGetIVar(xlval, euxmGetFPI(tmp));
                break;
            case OP_SETIVAR:
                {
                    tmp = euxmStackPop();
                    #ifdef CHECK_REF
                    if
                    (
                        !euxmFPIp(tmp)
                     || !euxmObjectp(xlval)
                     || euxmGetFPI(tmp)
                      > euxmGetFPI
                        (
                            euxmGetIVar(euxmGetClass(xlval), euxmInstanceSizeId)
                        )
                    )
                    {
                        badSlotAccess("write", tmp, xlval);
                    }
                    #endif
                    euxmFPIType fixtmp = euxmGetFPI(tmp);
                    tmp = euxmStackPop();
                    euxmSetIVar(xlval, fixtmp, tmp);
                    xlval = tmp;
                }
                break;
                // these don't need the bother of frames and are used a lot
            case OP_GET:
                tmp = euxmStackPop();
                if (!euxmSymbolp(xlval))
                {
                    badArgType(xlval, "<symbol>", "get");
                }
                if (!euxmSymbolp(tmp))
                {
                    badArgType(tmp, "<symbol>", "get");
                }
                xlval = euxcGetProp(xlval, tmp);
                break;
            case OP_PUT:
                tmp = euxmStackPop();
                tmp2 = euxmStackPop();
                if (!euxmSymbolp(xlval))
                {
                    badArgType(xlval, "<symbol>", "put");
                }
                if (!euxmSymbolp(tmp))
                {
                    badArgType(tmp, "<symbol>", "put");
                }
                euxcPutProp(xlval, tmp2, tmp);
                xlval = tmp2;
                break;
            case OP_CURMOD:
                euxcArgC = 0;
                xlval = euxcCurrentMod();
                break;
            case OP_CONSP:
                xlval = euxmConsp(xlval) ? euxl_true : euxmNil;
                break;
            case OP_SYMBOLP:
                xlval = euxmSymbolp(xlval) ? euxl_true : euxmNil;
                break;
            case OP_VECTORP:
                xlval = euxmVectorp(xlval) ? euxl_true : euxmNil;
                break;
            case OP_APPEND:    // 2 args
                tmp = euxmStackPop();
                if (!euxmListp(xlval))
                {
                    badArgType(xlval, "<list>", "append");
                }
                if (xlval == euxmNil)
                {
                    xlval = tmp;
                }
                else
                {
                    euxlValue end;
                    euxmStackPush(tmp);
                    end = euxcCons(euxmCar(xlval), euxmNil);
                    euxmStackCheckPush(end);
                    for
                    (
                        xlval = euxmCdr(xlval);
                        euxmConsp(xlval);
                        xlval = euxmCdr(xlval)
                    )
                    {
                        euxmSetCdr(end, euxcCons(euxmCar(xlval), euxmNil));
                        end = euxmCdr(end);
                    }
                    euxmSetCdr(end, tmp);
                    xlval = euxmStackPop();
                    euxmStackDrop(1);    // tmp
                }
                break;
            case OP_LIST:      // 2 args
                xlval = euxcCons(xlval, euxcCons(euxmStackPop(), euxmNil));
                break;
            case OP_SIZE:
                if (!euxmListp(xlval))
                {
                    badArgType(xlval, "<list>", "list-size");
                }
                euxmStackCheckPush(xlval);
                euxcArgC = 1;
                xlval = euxlSize();
                break;
            case OP_REVERSE:
                if (!euxmListp(xlval))
                {
                    badArgType(xlval, "<list>", "reverse");
                }
                euxmStackCheckPush(xlval);
                euxcArgC = 1;
                xlval = euxlReverseList();
                break;
            case OP_CAAR:
                if (!euxmConsp(xlval))
                {
                    badArgType(xlval, "<cons>", "caar");
                }
                xlval = euxmCar(xlval);
                if (!euxmConsp(xlval))
                {
                    badArgType(xlval, "<cons>", "caar");
                }
                xlval = euxmCar(xlval);
                break;
            case OP_CADR:
                if (!euxmConsp(xlval))
                {
                    badArgType(xlval, "<cons>", "cadr");
                }
                xlval = euxmCdr(xlval);
                if (!euxmConsp(xlval))
                {
                    badArgType(xlval, "<cons>", "cadr");
                }
                xlval = euxmCar(xlval);
                break;
            case OP_CDAR:
                if (!euxmConsp(xlval))
                {
                    badArgType(xlval, "<cons>", "cdar");
                }
                xlval = euxmCar(xlval);
                if (!euxmConsp(xlval))
                {
                    badArgType(xlval, "<cons>", "cdar");
                }
                xlval = euxmCdr(xlval);
                break;
            case OP_CDDR:
                if (!euxmConsp(xlval))
                {
                    badArgType(xlval, "<cons>", "cddr");
                }
                xlval = euxmCdr(xlval);
                if (!euxmConsp(xlval))
                {
                    badArgType(xlval, "<cons>", "cddr");
                }
                xlval = euxmCdr(xlval);
                break;
            case OP_GETSYNTAX:
                tmp = euxmStackPop();
                if (!euxmSymbolp(xlval))
                {
                    badArgType(xlval, "<symbol>", "get");
                }
                if (!euxmSymbolp(tmp))
                {
                    badArgType(tmp, "<symbol>", "get");
                }
                xlval = euxcGetSyntax(xlval, tmp);
                break;
            case OP_PUTSYNTAX:
                tmp = euxmStackPop();
                tmp2 = euxmStackPop();
                if (!euxmSymbolp(xlval))
                {
                    badArgType(xlval, "<symbol>", "put");
                }
                if (!euxmSymbolp(tmp))
                {
                    badArgType(tmp, "<symbol>", "put");
                }
                euxcPutSyntax(xlval, tmp2, tmp);
                xlval = tmp2;
                break;
                #ifndef NO_CHECK_REF
            case OP_CHECKREF:
                tmp = euxmStackPop();    // the object
                if (!euxmClassp(xlval))     // the class
                {
                    euxcIntError
                    (
                        "not a class in check-ref",
                        xlval,
                        euxls_telos_error
                    );
                }
                if (!euxcSubClassp(euxcClassOf(tmp), xlval))
                {
                    euxcTelosBadRefError(tmp, xlval, euxmTrue);
                }
                xlval = euxl_true;
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
//    The function should be in xlval and the arguments should
//    be on the stack.  The number of arguments should be in euxcArgC.
void euxcApply()
{
    static char *functionName = "function apply";

    // Check for euxmNull function
    if (euxmNull(xlval))
    {
        badFunctionNodeType(xlval);
    }

    // dispatch on function type
    switch (euxmNodeType(xlval))
    {
        case euxmFun:
            xlval = (*euxmGetFun(xlval)) ();
            euxcReturn();
            break;
        case euxmXFun:
            (*euxmGetXFun(xlval)) ();
            break;
        case euxmClosure:
            xlfun = euxmGetCode(xlval);
            xlenv = euxmGetCEnv(xlval);
            base = pc = euxmGetCodestr(xlfun);
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
                applicable = euxcFindAndCacheMethods(xlval, al);
                if (applicable == euxmNil)
                {
                    xlval = euxcCons(euxmGetGenericName(xlval), al);
                    // discard the args and arglist
                    euxmStackDrop(euxcArgC + 1);
                    euxcCerror
                    (
                        "no applicable methods",
                        xlval,
                        euxls_no_applic_error
                    );
                }
                euxmStackCheckPush(euxmCdr(applicable));
                xlval = euxmCar(applicable);
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
                xlval = tmp;
                euxcReturn();
            }
            break;
        default:
            badFunctionNodeType(xlval);
    }
}

///  euxcReturn - return to a continuation on the stack
void euxcReturn()
{
    // restore the environment and the continuation function
    xlenv = euxmStackPop();
    euxlValue tmp = euxmStackPop();

    // dispatch on the function type
    switch (euxmNodeType(tmp))
    {
        case euxmCode:
            xlfun = tmp;
            tmp = euxmStackPop();
            base = euxmGetCodestr(xlfun);
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
//    cc is euxl_true if return address is needed, e.g., in the interpreter
euxlValue euxcCurrentContinuation(int cc)
{
    if (cc)
    {
        euxmStackCheck(4);
        euxmStackPush(euxmMakeSmallFPI((euxmFPIType) (pc - base)));
        euxmStackPush(xlfun);
        euxmStackPush(xlenv);
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
///       The continuation should be in xlval.
static void restoreContinuation()
{
    int size = euxmGetSize(xlval);
    euxlValue *src;
    for
    (
        src = &xlval->value.vector.data[size], euxcStackPtr = euxcStackTop;
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
    euxmStackPush(xlfun);
    euxmStackPush(xlenv);
    // args and function
    euxmStackPush(val1);
    euxmStackPush(val2);
    xlval = op;
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
    if (xlfun)
    {
        base = euxmGetCodestr(xlfun);
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
    object = euxcCons(euxmEnter("object:"), object);
    object = euxcCons(index, object);
    object = euxcCons(euxmEnter("slot:"), object);
    euxmStackDrop(1);
    euxcCerror(buf, object, euxmNil);
}


///-----------------------------------------------------------------------------
