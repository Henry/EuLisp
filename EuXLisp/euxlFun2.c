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
/// Title: euxlisp built-in functions - part 2
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static void mapLoop(euxlValue last);
static void forLoop();
static void withFile(int flags, const char *mode);
static void load(euxlValue print);
static void loadLoop(euxlValue print);
static euxlValue setPrint(int *pvar);
static euxlValue openFile(int flags, const char *mode);
static euxlValue stringCompare(int fcn, int icase);
static euxlValue charCompare(int fcn, int icase);

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
///  euxlApply - built-in function 'apply'
//    (apply list 1 2 '(3 4)) -> (list 1 2 3 4)
void euxlApply()
{
    static char *functionName = "apply";

    // get the function
    euxcCurVal = euxmGetArg();

    euxlValue arglist = euxcStackPtr[euxcArgC - 1];
    if (!euxmListp(arglist))
    {
        euxcBadType(arglist, "<list>", functionName);
    }

    int nargs = euxcListSize(arglist);

    // Check for room for extra args
    euxmStackCheck(nargs - 1);
    euxcArgC--;   // number of explicit args

    // shift up (or down) explicit args
    if (nargs == 0)
    {
        euxlValue *from, *to;
        int i;
        for
        (
            from = euxcStackPtr + euxcArgC - 1,
            to = euxcStackPtr + euxcArgC, i = 0;
            i < euxcArgC;
            i++
        )
        {
            *to-- = *from--;
        }
        euxcStackPtr++;
    }
    else
    {
        euxcStackPtr -= nargs - 1;
        euxlValue *from, *to;
        int i;
        for (from = euxcStackPtr + nargs - 1,
             to = euxcStackPtr, i = 0; i < euxcArgC; i++)
        {
            *to++ = *from++;
        }
    }

    // copy the list arguments onto the stack
    for
    (
        euxlValue *to = euxcStackPtr + euxcArgC;
        euxmConsp(arglist);
        arglist = euxmCdr(arglist))
    {
        *to++ = euxmCar(arglist);
    }

    euxcArgC += nargs;

    // apply the function to the arguments
    euxcApply();
}

///  values -
void euxlValues()
{
    static char *functionName = "values";

    // get the function
    euxcCurVal = euxmGetArg();

    euxlValue arglist = euxcStackPtr[euxcArgC - 1];
    if (!euxmListp(arglist))
    {
        euxcBadType(arglist, "<list>", functionName);
    }

    int nargs = euxcListSize(arglist);

    // Check for room for extra args
    euxmStackCheck(nargs - 1);
    euxcArgC--;   // number of explicit args

    // shift up (or down) explicit args
    if (nargs == 0)
    {
        euxlValue *from, *to;
        int i;
        for
        (
            from = euxcStackPtr + euxcArgC - 1,
            to = euxcStackPtr + euxcArgC, i = 0;
            i < euxcArgC;
            i++
        )
        {
            *to-- = *from--;
        }
        euxcStackPtr++;
    }
    else
    {
        euxcStackPtr -= nargs - 1;
        euxlValue *from, *to;
        int i;
        for (from = euxcStackPtr + nargs - 1,
             to = euxcStackPtr, i = 0; i < euxcArgC; i++)
        {
            *to++ = *from++;
        }
    }

    // copy the list arguments onto the stack
    for
    (
        euxlValue *to = euxcStackPtr + euxcArgC;
        euxmConsp(arglist);
        arglist = euxmCdr(arglist)
    )
    {
        *to++ = euxmCar(arglist);
    }

    euxcArgC += nargs;

    //restore_continuation();
    //xlreturn();
    //euxmStackPop();
    //euxmStackPop();
    //euxmStackPop();
    //xlreturn();
    //xlapply();
}

///  euxlCallCC - built-in function 'call-with-current-continuation'
void euxlCallCC()
{
    static char *functionName = "call/cc";

    // get the function to call
    euxcCurVal = euxmGetArg();
    euxmLastArg();

    // create a continuation object
    euxlValue cont = euxcCurrentContinuation(euxmFalse);

    // setup the argument list
    euxmStackCheckPush(cont);
    euxcArgC = 1;

    // apply the function
    euxcApply();
}

///  euxlMapList - built-in function 'map-list'
void euxlMapList()
{
    static char *functionName = "%map-list";

    if (euxcArgC < 2)
    {
        euxcTooFew(functionName);
    }

    euxcCurVal = euxmNil;
    mapLoop(euxmNil);
}

///  mapLoop - setup for the next application
static void mapLoop(euxlValue last)
{
    // get a pointer to the end of the argument list
    euxlValue *p = &euxcStackPtr[euxcArgC];
    euxlValue *oldsp = euxcStackPtr;

    // save a continuation
    if (euxcCurVal)
    {
        euxmStackCheck(5);
        euxmStackPush(euxcCurVal);
        euxmStackPush(last);
    }
    else
    {
        euxmStackCheck(4);
        euxmStackPush(euxmNil);
    }
    euxmStackPush(euxcMakeFPI((euxmFPIType) euxcArgC));
    euxmStackPush(euxls_map_list_cont);
    euxmStackPush(euxcCurEnv);

    // build the argument list for the next application
    for (int cnt = euxcArgC; --cnt >= 1;)
    {
        euxlValue x = *--p;
        if (euxmConsp(x))
        {
            euxmStackCheckPush(euxmCar(x));
            *p = euxmCdr(x);
        }
        else
        {
            euxcStackPtr = oldsp;
            euxmStackDrop(euxcArgC);
            euxcReturn();
            return;
        }
    }
    euxcCurVal = *--p;       // get the function to apply
    euxcArgC -= 1;        // count shouldn't include the function itself
    euxcApply();  // apply the function
}

///  euxlMapListCont - continuation for euxlMapList
void euxlMapListCont()
{
    // get the argument count
    euxlValue tmp = euxmStackPop();

    // get the tail of the value list
    euxlValue last;
    if ((last = euxmStackPop()) != euxmNil)
    {
        // add the new value to the tail
        euxmSetCdr(last, euxcCons(euxcCurVal, euxmNil));
        last = euxmCdr(last);    // remember the new tail
        euxcCurVal = euxmStackPop();  // restore the head of the list
    }
    else
    {
        // build the initial value list
        euxcCurVal = last = euxcCons(euxcCurVal, euxmNil);
    }

    // convert the argument count and loop
    euxcArgC = (int)euxmGetFPI(tmp);
    mapLoop(last);
}

///  euxlDoList - built-in function '%do-list'
void euxlDoList()
{
    static char *functionName = "%do-list";

    if (euxcArgC < 2)
    {
        euxcTooFew(functionName);
    }

    forLoop();
}

///  forLoop - setup for the next application
static void forLoop()
{
    // get a pointer to the end of the argument list
    euxlValue *p = &euxcStackPtr[euxcArgC];
    euxlValue *oldsp = euxcStackPtr;

    // save a continuation
    euxmStackCheck(3);
    euxmStackPush(euxcMakeFPI((euxmFPIType) euxcArgC));
    euxmStackPush(euxls_do_list_cont);
    euxmStackPush(euxcCurEnv);

    // build the argument list for the next application
    for (int cnt = euxcArgC; --cnt >= 1;)
    {
        euxlValue x = *--p;
        if (euxmConsp(x))
        {
            euxmStackCheckPush(euxmCar(x));
            *p = euxmCdr(x);
        }
        else
        {
            euxcStackPtr = oldsp;
            euxmStackDrop(euxcArgC);
            euxcCurVal = euxmNil;
            euxcReturn();
            return;
        }
    }

    euxcCurVal = *--p;       // get the function to apply
    euxcArgC -= 1;        // count shouldn't include the function itself
    euxcApply();  // apply the function
}

///  euxlDoListCont - continuation for euxlDoList
void euxlDoListCont()
{
    // get the argument count
    euxlValue tmp = euxmStackPop();

    // convert the argument count and loop
    euxcArgC = (int)euxmGetFPI(tmp);
    forLoop();
}

///  euxlCallWithInput - built-in function 'call-with-input-file'
void euxlCallWithInput()
{
    withFile(euxmPortFlagInput, "r");
}

///  euxlCallWithOutput - built-in function 'call-with-output-file'
void euxlCallWithOutput()
{
    withFile(euxmPortFlagOutput, "w");
}

///  withFile - handle the 'call-with-xxx-file' functions
static void withFile(int flags, const char *mode)
{
    static char *functionName = "call-with-input/output-file";

    // get the function to call
    euxlValue name = euxmGetArgString();
    euxcCurVal = euxmGetArg();
    euxmLastArg();

    // create a file object
    euxlValue file = euxcMakeStream(NULL, flags);
    FILE *fp;
    if ((fp = euxcOSAOpen(euxmGetString(name), mode)) == NULL)
    {
        euxcCerror("can't open file", name, euxmNil);
    }
    euxmSetFile(file, fp);

    // save a continuation
    euxmStackCheck(3);
    euxmStackPush(file);
    euxmStackPush(euxls_with_file_cont);
    euxmStackPush(euxcCurEnv);

    // setup the argument list
    euxmStackCheckPush(file);
    euxcArgC = 1;

    // apply the function
    euxcApply();
}

///  euxlWithFileCont - continuation for
//    euxlCallWithInput and euxlCallWithOutput
void euxlWithFileCont()
{
    euxcOSClose(euxmGetFile(euxmStackTop()));
    euxmSetFile(euxmStackPop(), NULL);
    euxcReturn();
}

///  euxlLoad - built-in function 'load'
void euxlLoad()
{
    load(euxmNil);
}

///  euxlLoadNoisily - built-in function 'load-noisily'
void euxlLoadNoisily()
{
    load(euxs_t);
}

///  load - open the file and setup the load loop
static void load(euxlValue print)
{
    static char *functionName = "load";

    // get the function to call
    euxcCurVal = euxmGetArgString();
    euxmLastArg();

    // create a file object
    euxlValue file = euxcMakeStream(NULL, euxmPortFlagInput);
    FILE *fp;
    if ((fp = euxcOSAOpen(euxmGetString(euxcCurVal), "r")) == NULL)
    {
        euxcCurVal = euxmNil;
        euxcReturn();
        return;
    }
    euxmSetFile(file, fp);
    euxcCurVal = file;

    // do the first read
    loadLoop(print);
}

///  loadLoop - read the next expression and setup to evaluate it
static void loadLoop(euxlValue print)
{
    // try to read the next expression from the file
    euxlValue expr;
    if (euxcRead(euxcCurVal, &expr))
    {
        // save a continuation
        euxmStackCheck(4);
        euxmStackPush(euxcCurVal);
        euxmStackPush(print);
        euxmStackPush(euxls_load_cont);
        euxmStackPush(euxcCurEnv);

        // setup the argument list
        euxcCurVal = euxmGetValue(euxls_eval_cm);
        euxmStackCheckPush(expr);
        euxcArgC = 1;

        // apply the function
        euxcApply();
    }
    else
    {
        euxcOSClose(euxmGetFile(euxcCurVal));
        euxmSetFile(euxcCurVal, NULL);
        euxcCurVal = euxs_t;
        euxcReturn();
    }
}

///  euxlLoadCont - continuation for euxlLoad
void euxlLoadCont()
{
    // print the value if the print variable is set
    euxlValue print;
    if ((print = euxmStackPop()) != euxmNil)
    {
        euxcPrin1(euxcCurVal, euxlStdout());
        euxcTerpri(euxlStdout());
    }
    euxcCurVal = euxmStackPop();

    // setup for the next read
    loadLoop(print);
}

///  euxlForce - built-in function 'force'
void euxlForce()
{
    static char *functionName = "force";

    // get the promise
    euxcCurVal = euxmGetArg();
    euxmLastArg();

    // Check for a promise
    if (euxmPromisep(euxcCurVal))
    {

        // force the promise the first time
        if ((euxcCurFun = euxmGetPProc(euxcCurVal)) != euxmNil)
        {
            euxmStackCheck(3);
            euxmStackPush(euxcCurVal);
            euxmStackPush(euxls_force_cont);
            euxmStackPush(euxcCurEnv);
            euxcCurVal = euxcCurFun;
            euxcArgC = 0;
            euxcApply();
        }

        // return the saved value if the promise has already been forced
        else
        {
            euxcCurVal = euxmGetPValue(euxcCurVal);
            euxcReturn();
        }

    }
    // otherwise, just return the argument
    else
    {
        euxcReturn();
    }
}

///  euxlForceCont - continuation for euxlForce
void euxlForceCont()
{
    euxlValue promise = euxmStackPop();
    euxmSetPValue(promise, euxcCurVal);
    euxmSetPProc(promise, euxmNil);
    euxcReturn();
}

///  euxlSymbolToString - built-in function 'symbol->string'
euxlValue euxlSymbolToString()
{
    static char *functionName = "symbol->string";

    euxcCurVal = euxmGetArgSymbol();
    euxmLastArg();
    return (euxmGetPName(euxcCurVal));
}

///  euxlStringToSymbol - built-in function 'string->symbol'
euxlValue euxlStringToSymbol()
{
    static char *functionName = "string->symbol";

    euxcCurVal = euxmGetArgString();
    euxmLastArg();
    return (euxmInternAndExport(euxmGetString(euxcCurVal)));
}

///  euxlRead - built-in function 'read'
euxlValue euxlRead()
{
    static char *functionName = "read";

    // get file pointer and eof value
    euxlValue fptr = (euxmMoreArgs()? euxmGetArgIstream() : euxlStdin());
    euxmLastArg();

    // read an expression
    euxlValue val;
    if (!euxcRead(fptr, &val))
    {
        val = euxs_eof;
    }

    // return the expression
    return (val);
}

///  euxlReadChar - built-in function 'read-char'
euxlValue euxlReadChar()
{
    static char *functionName = "read-char";

    euxlValue fptr = (euxmMoreArgs()? euxmGetArgIstream() : euxlStdin());
    euxmLastArg();
    int ch;
    return ((ch = euxcGetc(fptr)) == EOF ? euxs_eof : euxcMakeChar(ch));
}

///  euxlReadByte - built-in function 'read-byte'
euxlValue euxlReadByte()
{
    static char *functionName = "read-byte";

    euxlValue fptr = (euxmMoreArgs()? euxmGetArgIstream() : euxlStdin());
    euxmLastArg();

    int ch;
    return
    (
        (ch = euxcGetc(fptr)) == EOF
      ? euxs_eof
      : euxcMakeFPI((euxmFPIType) ch)
    );
}

///  euxlReadShort - built-in function 'read-short'
euxlValue euxlReadShort()
{
    static char *functionName = "read-short";

    unsigned char *p;
    short int val = 0;
    int n;
    euxlValue fptr = (euxmMoreArgs()? euxmGetArgIstream() : euxlStdin());
    euxmLastArg();
    for (n = sizeof(short int), p = (unsigned char *)&val; --n >= 0;)
    {
        int ch;
        if ((ch = euxcGetc(fptr)) == EOF)
        {
            return (euxs_eof);
        }
        *p++ = ch;
    }

    return (euxcMakeFPI((euxmFPIType) val));
}

///  euxlReadLong - built-in function 'read-long'
euxlValue euxlReadLong()
{
    static char *functionName = "read-long";

    unsigned char *p;
    long int val = 0;
    int n;
    euxlValue fptr = (euxmMoreArgs()? euxmGetArgIstream() : euxlStdin());
    euxmLastArg();
    for (n = sizeof(long int), p = (unsigned char *)&val; --n >= 0;)
    {
        int ch;
        if ((ch = euxcGetc(fptr)) == EOF)
        {
            return (euxs_eof);
        }
        *p++ = ch;
    }

    return (euxcMakeFPI((euxmFPIType) val));
}

///  peek-char
euxlValue euxlPeekChar()
{
    static char *functionName = "peek-char";

    euxlValue fptr = (euxmMoreArgs()? euxmGetArgIstream() : euxlStdin());
    euxmLastArg();

    int ch = euxcPeekChar(fptr);
    if (ch == euxmNoChar)
    {
        return euxmNil;
    }
    else if (ch == EOF)
    {
        return euxs_eof;
    }
    else
    {
        return euxcMakeChar(ch);
    }
}

///  char-ready?
euxlValue euxlCharReadyp()
{
    static char *functionName = "char-ready?";

    euxlValue fptr = (euxmMoreArgs()? euxmGetArgIstream() : euxlStdin());
    euxmLastArg();

    if (euxcPeekChar(fptr) == euxmNoChar)
    {
        return euxmNil;
    }
    else
    {
        return euxs_t;
    }
}

///  euxlEOFObjectp - built-in function 'eof-object?'
euxlValue euxlEOFObjectp()
{
    static char *functionName = "eof-object?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();
    return (arg == euxs_eof ? euxs_t : euxmNil);
}

///  euxlWrite - built-in function 'write'
euxlValue euxlWrite()
{
    static char *functionName = "write";

    // get expression to print and file pointer
    euxlValue val = euxmGetArg();
    euxlValue fptr = (euxmMoreArgs()? euxmGetArgOstream() : euxlStdout());
    euxmLastArg();

    // print the value
    euxcPrin1(val, fptr);

    return (euxs_t);
}

///  euxlPrintnl - built-in function 'printnl'
euxlValue euxlPrintnl()
{
    static char *functionName = "printnl";

    // get expression to print and file pointer
    euxlValue val = euxmGetArg();
    euxlValue fptr = (euxmMoreArgs()? euxmGetArgOstream() : euxlStdout());
    euxmLastArg();

    // print the value
    euxcPrin1(val, fptr);
    euxcTerpri(fptr);

    return (euxs_t);
}

///  euxlWriteChar - built-in function 'write-char'
euxlValue euxlWriteChar()
{
    static char *functionName = "write-char";

    euxlValue ch = euxmGetArgChar();
    euxlValue fptr = (euxmMoreArgs()? euxmGetArgOstream() : euxlStdout());
    euxmLastArg();
    euxcPutc(fptr, (int)euxmGetCharCode(ch));

    return (euxs_t);
}

///  euxlWriteByte - built-in function 'write-byte'
euxlValue euxlWriteByte()
{
    static char *functionName = "write-byte";

    euxlValue ch = euxmGetArgFPI();
    euxlValue fptr = (euxmMoreArgs()? euxmGetArgOstream() : euxlStdout());
    euxmLastArg();
    euxcPutc(fptr, (int)euxmGetFPI(ch));

    return (euxs_t);
}

///  euxlWriteShort - built-in function 'write-short'
euxlValue euxlWriteShort()
{
    static char *functionName = "write-short";

    euxlValue v = euxmGetArgFPI();
    short int val = (short int)euxmGetFPI(v);
    euxlValue fptr = (euxmMoreArgs()? euxmGetArgOstream() : euxlStdout());
    euxmLastArg();
    unsigned char *p;
    int n;
    for (n = sizeof(short int), p = (unsigned char *)&val; --n >= 0;)
    {
        euxcPutc(fptr, *p++);
    }

    return (euxs_t);
}

///  euxlWriteLong - built-in function 'write-long'
euxlValue euxlWriteLong()
{
    static char *functionName = "write-long";

    euxlValue v = euxmGetArgFPI();
    long int val = (long int)euxmGetFPI(v);
    euxlValue fptr = (euxmMoreArgs()? euxmGetArgOstream() : euxlStdout());
    euxmLastArg();
    unsigned char *p;
    int n;
    for (n = sizeof(long int), p = (unsigned char *)&val; --n >= 0;)
    {
        euxcPutc(fptr, *p++);
    }

    return (euxs_t);
}

///  euxlPrint - built-in function 'print'
euxlValue euxlPrint()
{
    static char *functionName = "print";

    // get expression to print and file pointer
    euxlValue val = euxmGetArg();
    euxlValue fptr = (euxmMoreArgs()? euxmGetArgOstream() : euxlStdout());
    euxmLastArg();

    // print the value
    euxcPrint(val, fptr);

    return (euxs_t);
}

///  euxlSFlush - flush stream
euxlValue euxlSFlush()
{
    static char *functionName = "sflush";

    // get file pointer
    euxlValue fptr = euxmGetArgOstream();
    euxmLastArg();

    // flush and return the stream
    fflush(euxmGetFile(fptr));

    return (fptr);
}

///  euxlFlush - flush stdout
euxlValue euxlFlush()
{
    // get file pointer
    euxlValue fptr = euxlStdout();

    // flush and return the stream
    fflush(euxmGetFile(fptr));

    return (fptr);
}

///  euxlPrintBreadth - set the maximum number of elements to be printed
euxlValue euxlPrintBreadth()
{
    return (setPrint(&printBreadth));
}

///  euxlPrintDepth - set the maximum depth of nested lists to be printed
euxlValue euxlPrintDepth()
{
    return (setPrint(&printDepth));
}

///  setPrint - common function for printBreadth/printDepth
static euxlValue setPrint(int *pvar)
{
    static char *functionName = "printBreadth/printDepth";

    // get the optional argument
    if (euxmMoreArgs())
    {
        euxlValue arg = euxmGetArg();
        euxmLastArg();
        *pvar = (euxmFPIp(arg) ? (int)euxmGetFPI(arg) : -1);
    }

    // return the value of the variable
    return (*pvar >= 0 ? euxcMakeFPI((euxmFPIType) * pvar) : euxmNil);
}

///  euxlOpenInput - built-in function 'open-input-file'
euxlValue euxlOpenInput()
{
    return (openFile(euxmPortFlagInput, "r"));
}

///  euxlOpenOutput - built-in function 'open-output-file'
euxlValue euxlOpenOutput()
{
    return (openFile(euxmPortFlagOutput, "w"));
}

///  euxlOpenAppend - built-in function 'open-append-file'
euxlValue euxlOpenAppend()
{
    return (openFile(euxmPortFlagOutput, "a"));
}

///  euxlOpenUpdate - built-in function 'open-update-file'
euxlValue euxlOpenUpdate()
{
    return (openFile(euxmPortFlagInput | euxmPortFlagOutput, "r+"));
}

///  openFile - open an ascii or binary file
static euxlValue openFile(int flags, const char *mode)
{
    static char *functionName = "open";
    euxlValue euxcEnterKeyword();

    // get the file name and direction
    char * name = euxmGetString(euxmGetArgString());
    euxlValue modekey = (euxmMoreArgs()? euxmGetArgSymbol() : euxmNil);
    euxmLastArg();

    // Check for binary mode
    if (modekey != euxmNil)
    {
        if (modekey == euxcEnterKeyword("binary:"))
        {
            flags |= euxmPortFlagBinary;
        }
        else if (modekey != euxcEnterKeyword("text:"))
        {
            euxcCerror("unrecognized open mode", modekey, euxmNil);
        }
    }

    // try to open the file
    euxlValue file = euxcMakeStream(NULL, flags);
    FILE *fp =
    (
        (flags & euxmPortFlagBinary) == 0
      ? euxcOSAOpen(name, mode)
      : euxcOSBOpen(name, mode)
    );

    if (fp == NULL)
    {
        return (euxmNil);
    }
    euxmSetFile(file, fp);

    return (file);
}

///  euxlClose - built-in function 'close-stream'
euxlValue euxlClose()
{
    static char *functionName = "close-stream";

    euxlValue fptr = euxmGetArgStream();
    euxmLastArg();
    if (euxmGetFile(fptr))
    {
        euxcOSClose(euxmGetFile(fptr));
    }
    euxmSetFile(fptr, NULL);

    return (euxmNil);
}

///  euxlCloseInput - built-in function 'close-input-stream'
euxlValue euxlCloseInput()
{
    static char *functionName = "close-input-stream";

    euxlValue fptr = euxmGetArgIstream();
    euxmLastArg();
    if (euxmGetFile(fptr))
    {
        euxcOSClose(euxmGetFile(fptr));
    }
    euxmSetFile(fptr, NULL);

    return (euxmNil);
}

///  euxlCloseOutput - built-in function 'close-output-stream'
euxlValue euxlCloseOutput()
{
    static char *functionName = "close-output-stream";

    euxlValue fptr = euxmGetArgOstream();
    euxmLastArg();
    if (euxmGetFile(fptr))
    {
        euxcOSClose(euxmGetFile(fptr));
    }
    euxmSetFile(fptr, NULL);

    return (euxmNil);
}

///  euxlGetFilePosition - built-in function 'get-file-position'
euxlValue euxlGetFilePosition()
{
    static char *functionName = "get-file-position";

    euxlValue fptr = euxmGetArgStream();
    euxmLastArg();

    return (euxcMakeFPI(euxcOSTell(euxmGetFile(fptr))));
}

///  euxlsetFilePosition - built-in function 'set-file-position'
euxlValue euxlsetFilePosition()
{
    static char *functionName = "set-file-position";

    euxlValue fptr = euxmGetArgStream();
    euxlValue val = euxmGetArgFPI();
    long position = euxmGetFPI(val);
    val = euxmGetArgFPI();
    int whence = (int)euxmGetFPI(val);
    euxmLastArg();

    return
    (
        euxcOSSeek(euxmGetFile(fptr), position, whence) == 0
      ? euxs_t
      : euxmNil
    );
}

///  euxlUnlink -  built-in function 'unlink'
euxlValue euxlUnlink()
{
    static char *functionName = "unlink";

    euxlValue path = euxmGetArgString();
    euxmLastArg();

    return (euxcOSUnlink(euxmGetString(path)) == 0 ? euxs_t : euxmNil);
}

///  euxlStreamp - built-in function 'stream?'
euxlValue euxlStreamp()
{
    static char *functionName = "stream?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();

    return (euxmStreamp(arg) ? euxs_t : euxmNil);
}

///  euxlInputStreamp - built-in function 'input-stream?'
euxlValue euxlInputStreamp()
{
    static char *functionName = "input-stream?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();

    return (euxmeuxmIStreamp(arg) ? euxs_t : euxmNil);
}

///  euxlOutputStreamp - built-in function 'output-stream?'
euxlValue euxlOutputStreamp()
{
    static char *functionName = "output-stream?";

    euxlValue arg = euxmGetArg();
    euxmLastArg();

    return (euxmeuxmOStreamp(arg) ? euxs_t : euxmNil);
}

///  euxlTransciptOn - built-in function 'transcript-on'
euxlValue euxlTransciptOn()
{
    static char *functionName = "transcript-on";

    // get the file name and direction
    char *name = euxmGetString(euxmGetArgString());
    euxmLastArg();

    // close any currently open transcript file
    if (tfp)
    {
        euxcOSClose(tfp);
        tfp = NULL;
    }

    // try to open the file
    return ((tfp = euxcOSAOpen(name, "w")) == NULL ? euxmNil : euxs_t);
}

///  euxlTranscriptOff - built-in function 'transcript-off'
euxlValue euxlTranscriptOff()
{
    static char *functionName = "transcript-off";

    // make sure there aren't any arguments
    euxmLastArg();

    // make sure the transcript is open
    if (tfp == NULL)
    {
        return (euxmNil);
    }

    // close the transcript and return successfully
    euxcOSClose(tfp);
    tfp = NULL;

    return (euxs_t);
}

///  euxlMakeString - built-in function 'make-string'
euxlValue euxlMakeString()
{
    static char *functionName = "make-string";

    // get the string size
    euxlValue arg = euxmGetArgFPI();
    int len = (int)euxmGetFPI(arg);

    if (len < 0)
    {
        euxcCerror("bad size for make-string", arg, euxmNil);
    }

    // Check for an initialization value
    char ch;
    if (euxmMoreArgs())
    {
        arg = euxmGetArgChar();       // get the initializer
        euxmLastArg();    // make sure that's the last argument
        ch = euxmGetCharCode(arg);
    }
    else
    {
        ch = ' ';       // no initialization value, default to space
    }

    euxlValue val = euxcNewString(len + 1);
    char *p = euxmGetString(val); // initialize the string
    p[len] = 0;
    for (; --len >= 0;)
    {
        *p++ = ch;
    }

    // return the new vector
    return (val);
}

///  euxlStringLength - built-in function 'string-size'
euxlValue euxlStringLength()
{
    static char *functionName = "string-size";

    euxlValue str = euxmGetArgString();
    euxmLastArg();

    return (euxcMakeFPI((euxmFPIType) (euxmGetStringlength(str) - 1)));
}

///  euxlStringNullp - built-in function 'string-null?'
euxlValue euxlStringNullp()
{
    static char *functionName = "string-null?";

    euxlValue str = euxmGetArgString();
    euxmLastArg();

    return (euxmGetStringlength(str) == 1 ? euxs_t : euxmNil);
}

///  euxlStringAppend - built-in function 'string-append'
euxlValue euxlStringAppend()
{
    static char *functionName = "string-append?";

    // save the argument list
    int saveargc = euxcArgC;
    euxlValue *savesp = euxcStackPtr;

    // find the size of the new string
    int len;
    for (len = 0; euxmMoreArgs();)
    {
        euxlValue tmp = euxmGetArgString();
        len += (int)euxmGetStringlength(tmp) - 1;
    }

    // restore the argument list
    euxcArgC = saveargc;
    euxcStackPtr = savesp;

    // create the result string
    euxlValue val = euxcNewString(len + 1);
    char *str = euxmGetString(val);

    // combine the strings
    for (*str = '\0'; euxmMoreArgs();)
    {
        euxlValue tmp = euxmNextArg();
        strcat(str, euxmGetString(tmp));
    }

    // return the new string
    return (val);
}

///  euxlStringRef - built-in function 'string-ref'
euxlValue euxlStringRef()
{
    static char *functionName = "string-ref";

    // get the string and the index
    euxlValue str = euxmGetArgString();
    euxlValue num = euxmGetArgFPI();
    euxmLastArg();

    // range Check the index
    int n;
    if ((n = (int)euxmGetFPI(num)) < 0 || n >= euxmGetStringlength(str) - 1)
    {
        euxcCerror("index out of range in string-ref", num, euxmNil);
    }

    // return the character
    return (euxcMakeChar(euxmGetString(str)[n]));
}

///  euxlStringSet - built-in function 'string-set'
euxlValue euxlStringSet()
{
    static char *functionName = "string-set";

    // get the string, the index and the value
    euxlValue str = euxmGetArgString();
    euxlValue num = euxmGetArgFPI();
    euxlValue val = euxmGetArgChar();
    euxmLastArg();

    // range Check the index
    int n;
    if ((n = (int)euxmGetFPI(num)) < 0 || n >= euxmGetStringlength(str) - 1)
    {
        euxcCerror("index out of range in string-set", num, euxmNil);
    }

    // insert the character
    euxmGetString(str)[n] = euxmGetCharCode(val);

    return val;
}

///  euxlSubString - built-in function 'substring'
euxlValue euxlSubString()
{
    static char *functionName = "substring";

    // get string and starting and ending positions
    euxlValue src = euxmGetArgString();

    // get the starting position
    euxlValue dst = euxmGetArgFPI();
    int start = (int)euxmGetFPI(dst);
    if (start < 0 || start > euxmGetStringlength(src) - 1)
    {
        euxcCerror("index out of range in substring", dst, euxmNil);
    }

    // get the ending position
    int end;
    if (euxmMoreArgs())
    {
        dst = euxmGetArgFPI();
        end = (int)euxmGetFPI(dst);
        if (end < 0 || end > euxmGetStringlength(src) - 1)
        {
            euxcCerror("index out of range in substring", dst, euxmNil);
        }
    }
    else
    {
        end = euxmGetStringlength(src) - 1;
    }

    euxmLastArg();

    // setup the source pointer
    char *srcp = euxmGetString(src) + start;
    int len = end - start;

    // make a destination string and setup the pointer
    dst = euxcNewString(len + 1);
    char *dstp = euxmGetString(dst);

    // copy the source to the destination
    while (--len >= 0)
    {
        *dstp++ = *srcp++;
    }
    *dstp = '\0';

    // return the substring
    return (dst);
}

///  euxlStringToList - built-in function 'string->list'
euxlValue euxlStringToList()
{
    static char *functionName = "string->list";

    // get the vector
    euxlValue str = euxmGetArgString();
    euxmLastArg();

    // make a list from the vector
    euxmStackCheckPush(str);
    int size = euxmGetStringlength(str) - 1;
    char *p;
    for (euxcCurVal = euxmNil, p = &euxmGetString(str)[size]; --size >= 0;)
    {
        euxcCurVal = euxcCons(euxcMakeChar(*--p), euxcCurVal);
    }
    euxmStackDrop(1);

    return (euxcCurVal);
}

///  euxlListToString - built-in function 'list->string'
euxlValue euxlListToString()
{
    static char *functionName = "list->string";

    // get the list
    euxcCurVal = euxmGetArgList();
    euxmLastArg();

    // make a vector from the list
    int size = euxcListSize(euxcCurVal);
    euxlValue str = euxcNewString(size + 1);
    char *p;
    for (p = euxmGetString(str); --size >= 0; euxcCurVal = euxmCdr(euxcCurVal))
    {
        if (euxmCharp(euxmCar(euxcCurVal)))
        {
            *p++ = euxmGetCharCode(euxmCar(euxcCurVal));
        }
        else
        {
            euxcBadType(euxmCar(euxcCurVal), "<char>", functionName);
        }
    }

    *p = '\0';

    return (str);
}

///-----------------------------------------------------------------------------
/// String comparision functions
///-----------------------------------------------------------------------------
///  string<?
euxlValue euxlStringLt()
{
    return (stringCompare('<', euxmFalse));
}

///  string<=?
euxlValue euxlStringLtEq()
{
    return (stringCompare('L', euxmFalse));
}

///  string=?
euxlValue euxlStringEql()
{
    return (stringCompare('=', euxmFalse));
}

///  string>=?
euxlValue euxlStringGtEq()
{
    return (stringCompare('G', euxmFalse));
}

///  string>?
euxlValue euxlStringGt()
{
    return (stringCompare('>', euxmFalse));
}

///  string comparison functions (case insensitive)
///  string-ci<?
euxlValue euxlstringCaInLt()
{
    return (stringCompare('<', euxmTrue));
}

///  string-ci<=?
euxlValue euxlstringCaInLtEq()
{
    return (stringCompare('L', euxmTrue));
}

///  string-ci=?
euxlValue euxlstringCaInEql()
{
    return (stringCompare('=', euxmTrue));
}

///  string-ci>=?
euxlValue euxlstringCaInGtEq()
{
    return (stringCompare('G', euxmTrue));
}

///  string-ci>?
euxlValue euxlstringCaInGt()
{
    return (stringCompare('>', euxmTrue));
}

///  stringCompare - compare strings
static euxlValue stringCompare(int fcn, int icase)
{
    static char *functionName = "string compare";

    // get the strings
    euxlValue str1 = euxmGetArgString();
    euxlValue str2 = euxmGetArgString();
    euxmLastArg();

    // setup the string pointers
    char *p1 = euxmGetString(str1);
    int start1 = 0;
    int end1 = euxmGetStringlength(str1);
    char *p2 = euxmGetString(str2);
    int start2 = 0;
    int end2 = euxmGetStringlength(str2);

    // compare the strings
    for (; start1 < end1 && start2 < end2; ++start1, ++start2)
    {
        int ch1 = *p1++;
        int ch2 = *p2++;
        if (icase)
        {
            if (isupper(ch1))
            {
                ch1 = tolower(ch1);
            }
            if (isupper(ch2))
            {
                ch2 = tolower(ch2);
            }
        }
        if (ch1 != ch2)
        {
            switch (fcn)
            {
                case '<':
                    return (ch1 < ch2 ? euxs_t : euxmNil);
                case 'L':
                    return (ch1 <= ch2 ? euxs_t : euxmNil);
                case '=':
                    return (euxmNil);
                case 'G':
                    return (ch1 >= ch2 ? euxs_t : euxmNil);
                case '>':
                    return (ch1 > ch2 ? euxs_t : euxmNil);
            }
        }
    }

    // Check the termination condition
    switch (fcn)
    {
        case '<':
            return (start1 >= end1 && start2 < end2 ? euxs_t : euxmNil);
        case 'L':
            return (start1 >= end1 ? euxs_t : euxmNil);
        case '=':
            return (start1 >= end1 && start2 >= end2 ? euxs_t : euxmNil);
        case 'G':
            return (start2 >= end2 ? euxs_t : euxmNil);
        case '>':
            return (start2 >= end2 && start1 < end1 ? euxs_t : euxmNil);
    }

    return (euxmNil);       // never reached
}

///  euxlCharToInt - built-in function 'char->integer'
euxlValue euxlCharToInt()
{
    static char *functionName = "char->integer";

    euxlValue arg = euxmGetArgChar();
    euxmLastArg();

    return (euxcMakeFPI((euxmFPIType) euxmGetCharCode(arg)));
}

///  euxlTntToChar - built-in function 'integer->char'
euxlValue euxlTntToChar()
{
    static char *functionName = "integer->char";

    euxlValue arg = euxmGetArgFPI();
    euxmLastArg();

    return (euxcMakeChar((int)euxmGetFPI(arg)));
}

static int radix_int(char *str, euxlValue arg)
{
    int ch = (int)*str;

    if (ch == 0)
    {
        euxcCerror("malformed number in string->number", arg, euxmNil);
    }

    if (ch == 'x')
    {
        ch = (int)*++str;
        if (!isascii(ch) || !isxdigit(ch))
        {
            euxcCerror("malformed hex number in string->number", arg, euxmNil);
        }
        return (int)strtol(str, NULL, 16);
    }
    else if (ch == 'o')
    {
        ch = (int)*++str;
        if
        (
            ch != '0'
         && ch != '1'
         && ch != '2'
         && ch != '3'
         && ch != '4'
         && ch != '5'
         && ch != '6'
         && ch != '7'
        )
        {
            euxcCerror
            (
                "malformed octal number in string->number",
                arg,
                euxmNil
            );
        }

        return (int)strtol(str, NULL, 8);
    }
    else if (ch == 'b')
    {
        ch = (int)*++str;
        if (ch != '0' && ch != '1')
        {
            euxcCerror
            (
                "malformed binary number in string->number",
                arg,
                euxmNil
            );
        }

        return (int)strtol(str, NULL, 2);
    }

    if (!isascii(ch) || !isdigit(ch))
    {
        euxcCerror("malformed number in string->number", arg, euxmNil);
    }

    char *p;
    int base = (int)strtol(str, &p, 10);
    if (base < 2 || base > 36)
    {
        euxcCerror("bad base in string->number", arg, euxmNil);
    }

    if ((*p != 'r' && *p != 'R') || !isascii((int)p[1]) || !isalnum((int)p[1]))
    {
        euxcCerror("malformed number in string->number", arg, euxmNil);
    }

    return (int)strtol(p + 1, NULL, base);
}

///  built-in function 'string->number'
euxlValue euxlStringToNum()
{
    static char *functionName = "string->number";

    euxlValue arg = euxmGetArgString();
    euxmLastArg();

    char *str = euxmGetString(arg);

    if (*str == 0)
    {
        euxcCerror("malformed number in string->number", arg, euxmNil);
    }

    char *p = str;
    if (*p == '+' || *p == '-')
    {
        p++;
    }

    int ival;
    if (*p == '#')
    {
        ival = radix_int(p + 1, arg);
        if (*str == '-')
        {
            ival = -ival;
        }

        return euxcMakeFPI(ival);
    }

    for (; *p; p++)
    {
        if (*p == '.' || *p == 'E' || *p == 'e' || *p == 'D' || *p == 'd')
        {
            euxmDoubleFloatType fval;
            sscanf(str, "%lf", &fval);
            return euxcMakeDoubleFloat(fval);
        }
    }

    ival = atoi(str);

    return euxcMakeFPI(ival);
}

///  built-in function 'number->string'
euxlValue euxlNumToString()
{
    static char *functionName = "number->string";

    euxlValue arg = euxmGetArg();
    euxmLastArg();

    if (!euxmNumberp(arg))
    {
        euxcBadType(arg, "<number>", functionName);
    }

    char buf[128];
    if (euxmFPIp(arg))
    {
        sprintf(buf, "%ld", euxmGetFPI(arg));
    }
    else
    {
        sprintf(buf, euxmDoubleFloatFmt, euxmGetDoubleFloat(arg));
    }

    return euxcMakeString(buf);
}

///-----------------------------------------------------------------------------
/// Character comparision functions
///-----------------------------------------------------------------------------
///  char<?
euxlValue euxlCharLt()
{
    return (charCompare('<', euxmFalse));
}

///  char<=?
euxlValue euxlCharLtEq()
{
    return (charCompare('L', euxmFalse));
}

///  char=?
euxlValue euxlCharEql()
{
    return (charCompare('=', euxmFalse));
}

///  char>=?
euxlValue euxlCharGtEq()
{
    return (charCompare('G', euxmFalse));
}

///  char>?
euxlValue euxlCharGt()
{
    return (charCompare('>', euxmFalse));
}

///-----------------------------------------------------------------------------
/// Character comparision functions (case insensitive)
///-----------------------------------------------------------------------------
///  char-ci<?
euxlValue euxlCharCaInLt()
{
    return (charCompare('<', euxmTrue));
}

///  char-ci<=?
euxlValue euxlCharCaInLteq()
{
    return (charCompare('L', euxmTrue));
}

///  char-ci=?
euxlValue euxlCharCaInEql()
{
    return (charCompare('=', euxmTrue));
}

///  char-ci>=?
euxlValue euxlCharCaInGtEq()
{
    return (charCompare('G', euxmTrue));
}

///  char-ci>?
euxlValue euxlCharCaInGt()
{
    return (charCompare('>', euxmTrue));
}

///  charCompare - compare characters
static euxlValue charCompare(int fcn, int icase)
{
    static char *functionName = "char compare";

    // get the characters
    euxlValue arg = euxmGetArgChar();
    int ch1 = euxmGetCharCode(arg);
    arg = euxmGetArgChar();
    int ch2 = euxmGetCharCode(arg);
    euxmLastArg();

    // convert to lowercase if case insensitive
    if (icase)
    {
        if (isupper(ch1))
        {
            ch1 = tolower(ch1);
        }
        if (isupper(ch2))
        {
            ch2 = tolower(ch2);
        }
    }

    // compare the characters
    switch (fcn)
    {
        case '<':
            return (ch1 < ch2 ? euxs_t : euxmNil);
        case 'L':
            return (ch1 <= ch2 ? euxs_t : euxmNil);
        case '=':
            return (ch1 == ch2 ? euxs_t : euxmNil);
        case 'G':
            return (ch1 >= ch2 ? euxs_t : euxmNil);
        case '>':
            return (ch1 > ch2 ? euxs_t : euxmNil);
    }

    return (euxmNil);       // never reached
}

///  euxlCompile - built-in function 'compile'
euxlValue euxlCompile()
{
    static char *functionName = "compile";

    // get the expression to compile and the environment
    euxcCurVal = euxmGetArg();
    euxlValue env = (euxmMoreArgs()? euxmGetArgEnv() : euxmNil);
    euxmLastArg();

    // build the closure
    euxmStackCheckPush(env);
    euxcCurVal = euxcCompile(euxcCurVal, env);
    euxcCurVal = euxcMakeClosure(euxcCurVal, env);
    euxmStackDrop(1);

    return (euxcCurVal);
}

///  euxlEvalCm - built-in function 'eval/cm'
void euxlEvalCm()
{
    static char *functionName = "eval/cm";

    //euxlCompile();
    // Get the expression to compile
    euxcCurVal = euxmGetArg();
    euxlValue env = euxmNil;
    euxmLastArg();

    // Build the closure
    euxmStackCheckPush(env);
    euxcCurVal = euxcCompile(euxcCurVal, env);
    euxcCurVal = euxcMakeClosure(euxcCurVal, env);
    euxmStackDrop(1);

    euxcArgC = 1;
    euxcApply();
    //euxcApply();
    //euxcApply();
}

///  euxlDecompile - built-in function 'decompile'
euxlValue euxlDecompile()
{
    static char *functionName = "decompile";

    // get the closure (or code) and file pointer
    euxlValue fun = euxmGetArg();
    euxlValue fptr = (euxmMoreArgs()? euxmGetArgOstream() : euxlStdout());
    euxmLastArg();

    // make sure we got either a closure or a code object
    if (!euxmClosurep(fun))
    {
        euxcBadType(fun, "<closure>", functionName);
    }

    // decompile (disassemble) the procedure
    euxcDecodeProcedure(fptr, fun);

    return (euxmNil);
}

///  euxlSave - save the memory image
euxlValue euxlSave()
{
    static char *functionName = "save";

    // get the file name, verbose flag and print flag
    char *name = euxmGetString(euxmGetArgString());
    euxmLastArg();

    // save the memory image
    return (euxlSaveImage(name) ? euxs_t : euxmNil);
}

///  euxlRestore - restore a saved memory image
euxlValue euxlRestore()
{
    static char *functionName = "restore";

    // get the file name, verbose flag and print flag
    char *name = euxmGetString(euxmGetArgString());
    euxmLastArg();

    // restore the saved memory image
    if (!euxlRestoreImage(name))
    {
        return (euxmNil);
    }

    // return directly to the top level
    euxcStdPutString("[ returning to the top level ]\n");
    euxmLongJmp(euxmStackTopLevel, 1);

    return (euxmNil);       // never reached
}

///  euxlGc - function to force garbage collection
euxlValue euxlGc()
{
    static char *functionName = "gc";

    // Check the argument list and call the garbage collector
    if (euxmMoreArgs())
    {
        euxlValue arg = euxmGetArgFPI();
        int arg1 = (int)euxmGetFPI(arg);
        arg = euxmGetArgFPI();
        int arg2 = (int)euxmGetFPI(arg);
        euxmLastArg();
        while (--arg1 >= 0)
        {
            euxcNExpand(euxmNsSize);
        }
        while (--arg2 >= 0)
        {
            euxcVexpand(euxmVsSize);
        }
    }
    else
    {
        gc(euxmGcUser);
    }

    // return (gccalls nnodes nfree nscount vscount total)
    euxcCurVal = euxcCons(euxcMakeFPI(total), euxmNil);
    euxcCurVal = euxcCons(euxcMakeFPI((euxmFPIType) vscount), euxcCurVal);
    euxcCurVal = euxcCons(euxcMakeFPI((euxmFPIType) nscount), euxcCurVal);
    euxcCurVal = euxcCons(euxcMakeFPI(nfree), euxcCurVal);
    euxcCurVal = euxcCons(euxcMakeFPI(nnodes), euxcCurVal);
    euxcCurVal = euxcCons(euxcMakeFPI(gccalls), euxcCurVal);

    return (euxcCurVal);
}

///  euxlError - built-in function 'error'
euxlValue euxlError()
{
    static char *functionName = "error";

    // print the error message
    euxlValue msg = euxmGetArgString();
    euxcErrorPutString("error: ");
    euxcErrorPutString(euxmGetString(msg));
    euxcErrorPutString("\n");

    // print each of the remaining arguments on separate lines
    while (euxmMoreArgs())
    {
        euxcErrorPutString("  ");
        euxcErrorPrint(euxmGetArg());
    }

    // print the function where the error occurred
    euxcErrorPutString("happened in: ");
    euxcErrorPrint(euxcCurFun);

    // call the handler
    euxcCallErrorHandler();

    return (euxmNil);       // never reached
}

///  default-handler
euxlValue euxcDefaultHandler()
{
    static char *functionName = "default error handler";

    euxlValue condition = euxmGetArgObject();
    euxlValue cc = euxmGetArg();
    euxmLastArg();
    euxlValue fptr = euxmGetValue(euxls_stderr);
    euxlValue cls = euxcClassOf(condition);
    euxlValue sds = euxmGetIVar(cls, euxmSlotsId);

    if (cc == NULL)
    {
        euxcPutString(fptr, "Non-c");
    }
    else
    {
        euxcPutString(fptr, "C");
    }

    euxcPutString(fptr, "ontinuable error---calling default handler:\n");
    euxcPutString(fptr, "Condition class is ");
    euxcPrin1(cls, fptr);
    euxcTerpri(fptr);
    for (int i = 1; sds; i++, sds = euxmCdr(sds))
    {
        if (euxmGetIVar(condition, i) == euxls_unbound)
        {
            continue;
        }
        int len = 15
          - strlen(euxmGetString(euxmGetPName(euxmGetSlotName(euxmCar(sds)))));

        euxcPrin1(euxmGetSlotName(euxmCar(sds)), fptr);
        euxcPutc(fptr, ':');
        while (len-- > 0)
        {
            euxcPutc(fptr, ' ');
        }
        euxcPrin1(euxmGetIVar(condition, i), fptr);
        euxcTerpri(fptr);
    }

    euxcOSCheck();
    euxcSetFrame(1);
    euxlValue thread_module = euxcGetModule("thread");
    euxcCurVal = euxmGetValue(euxcEnterModule("debug", thread_module));
    euxmStackCheckPush(condition);
    euxmStackCheckPush(cc);
    euxcArgC = 2;
    euxcApply();
    euxmLongJmp(bc_dispatch, 1);

    // never reached
    euxmLongJmp(euxmStackTopLevel, 1);

    return (euxmNil);
}

///  euxlReset - built-in function 'reset'
euxlValue euxlReset()
{
    static char *functionName = "reset";

    euxmLastArg();
    euxmLongJmp(euxmStackTopLevel, 1);

    return (euxmNil);       // never reached
}

///  euxlGetArg - return a command line argument
euxlValue euxlGetArg()
{
    static char *functionName = "getarg";

    euxlValue arg = euxmGetArgFPI();
    int n = (int)euxmGetFPI(arg);
    euxmLastArg();

    return (n >= 0 && n < clargc ? euxcMakeString(clargv[n]) : euxmNil);
}

///  euxlExit - exit to the operating system
euxlValue euxlExit()
{
    static char *functionName = "exit";

    int retval = 0;

    if (euxmMoreArgs())
    {
        euxlValue val = euxmGetArgFPI();
        retval = euxmGetFPI(val);
        euxmLastArg();
    }

    euxcWrapup(retval);

    return (euxmNil);       // never reached
}


///-----------------------------------------------------------------------------
