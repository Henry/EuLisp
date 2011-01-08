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
/// Title: euxlisp built-in functions - part 2
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------

#include "euxlisp.h"
#include "euxlobj.h"
#include "euxlsymbols.h"
#include "euxlproto.h"

///-----------------------------------------------------------------------------
/// External variables
///-----------------------------------------------------------------------------
extern LVAL xlfun, xlenv, xlval;
extern int prbreadth, prdepth;
extern FILE *tfp;

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static void do_maploop(LVAL last);
static void do_forloop();
static void do_withfile(int flags, char *mode);
static void do_load(LVAL print);
static void do_loadloop(LVAL print);
static LVAL setit(int *pvar);
static LVAL openfile(int flags, char *mode);
static LVAL strcompare(int fcn, int icase);
static LVAL chrcompare(int fcn, int icase);

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
// xapply - built-in function 'apply'
// (apply list 1 2 '(3 4)) -> (list 1 2 3 4)
void xapply()
{
    static char *cfn_name = "apply";

    // get the function
    xlval = xlgetarg();

    LVAL arglist = xlsp[xlargc - 1];
    if (!listp(arglist))
    {
        xlbadtype(arglist, "<list>", cfn_name);
    }

    int nargs = list_size(arglist);

    // check for room for extra args
    check(nargs - 1);
    xlargc--;   // number of explicit args

    // shift up (or down) explicit args
    if (nargs == 0)
    {
        LVAL *from, *to;
        int i;
        for
        (
            from = xlsp + xlargc - 1, to = xlsp + xlargc, i = 0;
            i < xlargc;
            i++
        )
        {
            *to-- = *from--;
        }
        xlsp++;
    }
    else
    {
        xlsp -= nargs - 1;
        LVAL *from, *to;
        int i;
        for (from = xlsp + nargs - 1, to = xlsp, i = 0; i < xlargc; i++)
        {
            *to++ = *from++;
        }
    }

    // copy the list arguments onto the stack
    for (LVAL *to = xlsp + xlargc; consp(arglist); arglist = cdr(arglist))
    {
        *to++ = car(arglist);
    }

    xlargc += nargs;

    // apply the function to the arguments
    xlapply();
}

// values -
void xvalues()
{
    static char *cfn_name = "values";

    // get the function
    xlval = xlgetarg();

    LVAL arglist = xlsp[xlargc - 1];
    if (!listp(arglist))
    {
        xlbadtype(arglist, "<list>", cfn_name);
    }

    int nargs = list_size(arglist);

    // check for room for extra args
    check(nargs - 1);
    xlargc--;   // number of explicit args

    // shift up (or down) explicit args
    if (nargs == 0)
    {
        LVAL *from, *to;
        int i;
        for
        (
            from = xlsp + xlargc - 1, to = xlsp + xlargc, i = 0;
            i < xlargc;
            i++
        )
        {
            *to-- = *from--;
        }
        xlsp++;
    }
    else
    {
        xlsp -= nargs - 1;
        LVAL *from, *to;
        int i;
        for (from = xlsp + nargs - 1, to = xlsp, i = 0; i < xlargc; i++)
        {
            *to++ = *from++;
        }
    }

    // copy the list arguments onto the stack
    for (LVAL *to = xlsp + xlargc; consp(arglist); arglist = cdr(arglist))
    {
        *to++ = car(arglist);
    }

    xlargc += nargs;

    //restore_continuation();
    //xlreturn();
    pop();
    pop();
    pop();
    //xlreturn();
    //xlapply();
}

// xcallcc - built-in function 'call-with-current-continuation'
void xcallcc()
{
    static char *cfn_name = "call/cc";
    extern LVAL current_continuation();

    // get the function to call
    xlval = xlgetarg();
    xllastarg();

    // create a continuation object
    LVAL cont = current_continuation(FALSE);

    // setup the argument list
    cpush(cont);
    xlargc = 1;

    // apply the function
    xlapply();
}

// xmap - built-in function 'map'
void xmap()
{
    static char *cfn_name = "map-list";

    if (xlargc < 2)
    {
        xltoofew(cfn_name);
    }

    xlval = NIL;
    do_maploop(NIL);
}

// do_maploop - setup for the next application
static void do_maploop(LVAL last)
{
    extern LVAL cs_map1;

    // get a pointer to the end of the argument list
    LVAL *p = &xlsp[xlargc];
    LVAL *oldsp = xlsp;

    // save a continuation
    if (xlval)
    {
        check(5);
        push(xlval);
        push(last);
    }
    else
    {
        check(4);
        push(NIL);
    }
    push(cvfixnum((FIXTYPE) xlargc));
    push(cs_map1);
    push(xlenv);

    // build the argument list for the next application
    for (int cnt = xlargc; --cnt >= 1;)
    {
        LVAL x = *--p;
        if (consp(x))
        {
            cpush(car(x));
            *p = cdr(x);
        }
        else
        {
            xlsp = oldsp;
            drop(xlargc);
            xlreturn();
            return;
        }
    }
    xlval = *--p;       // get the function to apply
    xlargc -= 1;        // count shouldn't include the function itself
    xlapply();  // apply the function
}

// xmap1 - continuation for xmap
void xmap1()
{
    // get the argument count
    LVAL tmp = pop();

    // get the tail of the value list
    LVAL last;
    if ((last = pop()) != NIL)
    {
        rplacd(last, cons(xlval, NIL)); // add the new value to the tail
        last = cdr(last);       // remember the new tail
        xlval = pop();  // restore the head of the list
    }
    else
    {
        xlval = last = cons(xlval, NIL);        // build the initial value list
    }

    // convert the argument count and loop
    xlargc = (int)getfixnum(tmp);
    do_maploop(last);
}

// xforeach - built-in function 'for-each'
void xforeach()
{
    static char *cfn_name = "for-each";

    if (xlargc < 2)
    {
        xltoofew(cfn_name);
    }

    do_forloop();
}

// do_forloop - setup for the next application
static void do_forloop()
{
    extern LVAL cs_foreach1;

    // get a pointer to the end of the argument list
    LVAL *p = &xlsp[xlargc];
    LVAL *oldsp = xlsp;

    // save a continuation
    check(3);
    push(cvfixnum((FIXTYPE) xlargc));
    push(cs_foreach1);
    push(xlenv);

    // build the argument list for the next application
    for (int cnt = xlargc; --cnt >= 1;)
    {
        LVAL x = *--p;
        if (consp(x))
        {
            cpush(car(x));
            *p = cdr(x);
        }
        else
        {
            xlsp = oldsp;
            drop(xlargc);
            xlval = NIL;
            xlreturn();
            return;
        }
    }

    xlval = *--p;       // get the function to apply
    xlargc -= 1;        // count shouldn't include the function itself
    xlapply();  // apply the function
}

// xforeach1 - continuation for xforeach
void xforeach1()
{
    // get the argument count
    LVAL tmp = pop();

    // convert the argument count and loop
    xlargc = (int)getfixnum(tmp);
    do_forloop();
}

// xcallwi - built-in function 'call-with-input-file'
void xcallwi()
{
    do_withfile(PF_INPUT, "r");
}

// xcallwo - built-in function 'call-with-output-file'
void xcallwo()
{
    do_withfile(PF_OUTPUT, "w");
}

// do_withfile - handle the 'call-with-xxx-file' functions
static void do_withfile(int flags, char *mode)
{
    static char *cfn_name = "call-with-input/output-file";
    extern LVAL cs_withfile1;
    extern FILE *osaopen();

    // get the function to call
    LVAL name = xlgastring();
    xlval = xlgetarg();
    xllastarg();

    // create a file object
    LVAL file = cvstream(NULL, flags);
    FILE *fp;
    if ((fp = osaopen(getstring(name), mode)) == NULL)
    {
        xlcerror("can't open file", name, NIL);
    }
    setfile(file, fp);

    // save a continuation
    check(3);
    push(file);
    push(cs_withfile1);
    push(xlenv);

    // setup the argument list
    cpush(file);
    xlargc = 1;

    // apply the function
    xlapply();
}

// xwithfile1 - continuation for xcallwi and xcallwo
void xwithfile1()
{
    osclose(getfile(top()));
    setfile(pop(), NULL);
    xlreturn();
}

// xload - built-in function 'load'
void xload()
{
    do_load(NIL);
}

// xloadnoisily - built-in function 'load-noisily'
void xloadnoisily()
{
    do_load(true);
}

// do_load - open the file and setup the load loop
static void do_load(LVAL print)
{
    static char *cfn_name = "load";
    extern FILE *osaopen();

    // get the function to call
    xlval = xlgastring();
    xllastarg();

    // create a file object
    LVAL file = cvstream(NULL, PF_INPUT);
    FILE *fp;
    if ((fp = osaopen(getstring(xlval), "r")) == NULL)
    {
        xlval = NIL;
        xlreturn();
        return;
    }
    setfile(file, fp);
    xlval = file;

    // do the first read
    do_loadloop(print);
}

// do_loadloop - read the next expression and setup to evaluate it
static void do_loadloop(LVAL print)
{
    extern LVAL cs_load1, s_eval_cm;

    // try to read the next expression from the file
    LVAL expr;
    if (xlread(xlval, &expr))
    {
        // save a continuation
        check(4);
        push(xlval);
        push(print);
        push(cs_load1);
        push(xlenv);

        // setup the argument list
        xlval = getvalue(s_eval_cm);
        cpush(expr);
        xlargc = 1;

        // apply the function
        xlapply();
    }
    else
    {
        osclose(getfile(xlval));
        setfile(xlval, NULL);
        xlval = true;
        xlreturn();
    }
}

// xload1 - continuation for xload
void xload1()
{
    // print the value if the print variable is set
    LVAL print;
    if ((print = pop()) != NIL)
    {
        xlprin1(xlval, xstdout());
        xlterpri(xstdout());
    }
    xlval = pop();

    // setup for the next read
    do_loadloop(print);
}

// xforce - built-in function 'force'
void xforce()
{
    static char *cfn_name = "force";
    extern LVAL cs_force1;

    // get the promise
    xlval = xlgetarg();
    xllastarg();

    // check for a promise
    if (promisep(xlval))
    {

        // force the promise the first time
        if ((xlfun = getpproc(xlval)) != NIL)
        {
            check(3);
            push(xlval);
            push(cs_force1);
            push(xlenv);
            xlval = xlfun;
            xlargc = 0;
            xlapply();
        }

        // return the saved value if the promise has already been forced
        else
        {
            xlval = getpvalue(xlval);
            xlreturn();
        }

    }
    // otherwise, just return the argument
    else
    {
        xlreturn();
    }
}

// xforce1 - continuation for xforce
void xforce1()
{
    LVAL promise = pop();
    setpvalue(promise, xlval);
    setpproc(promise, NIL);
    xlreturn();
}

// xsymstr - built-in function 'symbol->string'
LVAL xsymstr()
{
    static char *cfn_name = "symbol->string";

    xlval = xlgasymbol();
    xllastarg();
    return (getpname(xlval));
}

// xstrsym - built-in function 'string->symbol'
LVAL xstrsym()
{
    static char *cfn_name = "string->symbol";

    xlval = xlgastring();
    xllastarg();
    return (xlenter(getstring(xlval)));
}

// xread - built-in function 'read'
LVAL xread()
{
    static char *cfn_name = "read";

    // get file pointer and eof value
    LVAL fptr = (moreargs()? xlgaistream() : xstdin());
    xllastarg();

    // read an expression
    LVAL val;
    if (!xlread(fptr, &val))
    {
        val = eof_object;
    }

    // return the expression
    return (val);
}

// xrdchar - built-in function 'read-char'
LVAL xrdchar()
{
    static char *cfn_name = "read-char";

    LVAL fptr = (moreargs()? xlgaistream() : xstdin());
    xllastarg();
    int ch;
    return ((ch = xlgetc(fptr)) == EOF ? eof_object : cvchar(ch));
}

// xrdbyte - built-in function 'read-byte'
LVAL xrdbyte()
{
    static char *cfn_name = "read-byte";

    LVAL fptr = (moreargs()? xlgaistream() : xstdin());
    xllastarg();

    int ch;
    return ((ch = xlgetc(fptr)) == EOF ? eof_object : cvfixnum((FIXTYPE) ch));
}

// xrdshort - built-in function 'read-short'
LVAL xrdshort()
{
    static char *cfn_name = "read-short";

    unsigned char *p;
    short int val = 0;
    int n;
    LVAL fptr = (moreargs()? xlgaistream() : xstdin());
    xllastarg();
    for (n = sizeof(short int), p = (unsigned char *)&val; --n >= 0;)
    {
        int ch;
        if ((ch = xlgetc(fptr)) == EOF)
        {
            return (eof_object);
        }
        *p++ = ch;
    }

    return (cvfixnum((FIXTYPE) val));
}

// xrdlong - built-in function 'read-long'
LVAL xrdlong()
{
    static char *cfn_name = "read-long";

    unsigned char *p;
    long int val = 0;
    int n;
    LVAL fptr = (moreargs()? xlgaistream() : xstdin());
    xllastarg();
    for (n = sizeof(long int), p = (unsigned char *)&val; --n >= 0;)
    {
        int ch;
        if ((ch = xlgetc(fptr)) == EOF)
        {
            return (eof_object);
        }
        *p++ = ch;
    }

    return (cvfixnum((FIXTYPE) val));
}

// peek-char
LVAL xpeek_char()
{
    static char *cfn_name = "peek-char";

    LVAL fptr = (moreargs()? xlgaistream() : xstdin());
    xllastarg();

    int ch = xlpeekchar(fptr);
    if (ch == NOCHAR)
    {
        return NIL;
    }
    else if (ch == EOF)
    {
        return eof_object;
    }
    else
    {
        return cvchar(ch);
    }
}

// char-ready?
LVAL xchar_readyp()
{
    static char *cfn_name = "char-ready?";

    LVAL fptr = (moreargs()? xlgaistream() : xstdin());
    xllastarg();

    if (xlpeekchar(fptr) == NOCHAR)
    {
        return NIL;
    }
    else
    {
        return true;
    }
}

// xeofobjectp - built-in function 'eof-object?'
LVAL xeofobjectp()
{
    static char *cfn_name = "eof-object?";

    LVAL arg = xlgetarg();
    xllastarg();
    return (arg == eof_object ? true : NIL);
}

// xwrite - built-in function 'write'
LVAL xwrite()
{
    static char *cfn_name = "write";

    // get expression to print and file pointer
    LVAL val = xlgetarg();
    LVAL fptr = (moreargs()? xlgaostream() : xstdout());
    xllastarg();

    // print the value
    xlprin1(val, fptr);

    return (true);
}

// xprintnl - built-in function 'printnl'
LVAL xprintnl()
{
    static char *cfn_name = "printnl";

    // get expression to print and file pointer
    LVAL val = xlgetarg();
    LVAL fptr = (moreargs()? xlgaostream() : xstdout());
    xllastarg();

    // print the value
    xlprin1(val, fptr);
    xlterpri(fptr);

    return (true);
}

// xwrchar - built-in function 'write-char'
LVAL xwrchar()
{
    static char *cfn_name = "write-char";

    LVAL ch = xlgachar();
    LVAL fptr = (moreargs()? xlgaostream() : xstdout());
    xllastarg();
    xlputc(fptr, (int)getchcode(ch));

    return (true);
}

// xwrbyte - built-in function 'write-byte'
LVAL xwrbyte()
{
    static char *cfn_name = "write-byte";

    LVAL ch = xlgafixnum();
    LVAL fptr = (moreargs()? xlgaostream() : xstdout());
    xllastarg();
    xlputc(fptr, (int)getfixnum(ch));

    return (true);
}

// xwrshort - built-in function 'write-short'
LVAL xwrshort()
{
    static char *cfn_name = "write-short";

    LVAL v = xlgafixnum();
    short int val = (short int)getfixnum(v);
    LVAL fptr = (moreargs()? xlgaostream() : xstdout());
    xllastarg();
    unsigned char *p;
    int n;
    for (n = sizeof(short int), p = (unsigned char *)&val; --n >= 0;)
    {
        xlputc(fptr, *p++);
    }

    return (true);
}

// xwrlong - built-in function 'write-long'
LVAL xwrlong()
{
    static char *cfn_name = "write-long";

    LVAL v = xlgafixnum();
    long int val = (long int)getfixnum(v);
    LVAL fptr = (moreargs()? xlgaostream() : xstdout());
    xllastarg();
    unsigned char *p;
    int n;
    for (n = sizeof(long int), p = (unsigned char *)&val; --n >= 0;)
    {
        xlputc(fptr, *p++);
    }

    return (true);
}

// xprint - built-in function 'print'
LVAL xprint()
{
    static char *cfn_name = "print";

    // get expression to print and file pointer
    LVAL val = xlgetarg();
    LVAL fptr = (moreargs()? xlgaostream() : xstdout());
    xllastarg();

    // print the value
    xlprint(val, fptr);

    return (true);
}

// xsflush - flush stream
LVAL xsflush()
{
    static char *cfn_name = "sflush";

    // get file pointer
    LVAL fptr = xlgaostream();
    xllastarg();

    // flush and return the stream
    fflush(getfile(fptr));

    return (fptr);
}

// xflush - flush stdout
LVAL xflush()
{
    // get file pointer
    LVAL fptr = xstdout();

    // flush and return the stream
    fflush(getfile(fptr));

    return (fptr);
}

// xprbreadth - set the maximum number of elements to be printed
LVAL xprbreadth()
{
    return (setit(&prbreadth));
}

// xprdepth - set the maximum depth of nested lists to be printed
LVAL xprdepth()
{
    return (setit(&prdepth));
}

// setit - common function for prbreadth/prdepth
static LVAL setit(int *pvar)
{
    static char *cfn_name = "prbreadth/prdepth";

    // get the optional argument
    if (moreargs())
    {
        LVAL arg = xlgetarg();
        xllastarg();
        *pvar = (fixp(arg) ? (int)getfixnum(arg) : -1);
    }

    // return the value of the variable
    return (*pvar >= 0 ? cvfixnum((FIXTYPE) * pvar) : NIL);
}

// xopeni - built-in function 'open-input-file'
LVAL xopeni()
{
    return (openfile(PF_INPUT, "r"));
}

// xopeno - built-in function 'open-output-file'
LVAL xopeno()
{
    return (openfile(PF_OUTPUT, "w"));
}

// xopena - built-in function 'open-append-file'
LVAL xopena()
{
    return (openfile(PF_OUTPUT, "a"));
}

// xopenu - built-in function 'open-update-file'
LVAL xopenu()
{
    return (openfile(PF_INPUT | PF_OUTPUT, "r+"));
}

// openfile - open an ascii or binary file
static LVAL openfile(int flags, char *mode)
{
    static char *cfn_name = "open";
    extern FILE *osaopen(), *osbopen();
    LVAL xlenter_keyword();

    // get the file name and direction
    char * name = getstring(xlgastring());
    LVAL modekey = (moreargs()? xlgasymbol() : NIL);
    xllastarg();

    // check for binary mode
    if (modekey != NIL)
    {
        if (modekey == xlenter_keyword("binary:"))
        {
            flags |= PF_BINARY;
        }
        else if (modekey != xlenter_keyword("text:"))
        {
            xlcerror("unrecognized open mode", modekey, NIL);
        }
    }

    // try to open the file
    LVAL file = cvstream(NULL, flags);
    FILE *fp =
        ((flags & PF_BINARY) == 0 ? osaopen(name, mode) : osbopen(name, mode));
    if (fp == NULL)
    {
        return (NIL);
    }
    setfile(file, fp);

    return (file);
}

// xclose - built-in function 'close-stream'
LVAL xclose()
{
    static char *cfn_name = "close-stream";

    LVAL fptr = xlgastream();
    xllastarg();
    if (getfile(fptr))
    {
        osclose(getfile(fptr));
    }
    setfile(fptr, NULL);

    return (NIL);
}

// xclosei - built-in function 'close-input-stream'
LVAL xclosei()
{
    static char *cfn_name = "close-input-stream";

    LVAL fptr = xlgaistream();
    xllastarg();
    if (getfile(fptr))
    {
        osclose(getfile(fptr));
    }
    setfile(fptr, NULL);

    return (NIL);
}

// xcloseo - built-in function 'close-output-stream'
LVAL xcloseo()
{
    static char *cfn_name = "close-output-stream";

    LVAL fptr = xlgaostream();
    xllastarg();
    if (getfile(fptr))
    {
        osclose(getfile(fptr));
    }
    setfile(fptr, NULL);

    return (NIL);
}

// xgetfposition - built-in function 'get-file-position'
LVAL xgetfposition()
{
    static char *cfn_name = "get-file-position";
    extern long ostell();

    LVAL fptr = xlgastream();
    xllastarg();

    return (cvfixnum(ostell(getfile(fptr))));
}

// xsetfposition - built-in function 'set-file-position'
LVAL xsetfposition()
{
    static char *cfn_name = "set-file-position";

    LVAL fptr = xlgastream();
    LVAL val = xlgafixnum();
    long position = getfixnum(val);
    val = xlgafixnum();
    int whence = (int)getfixnum(val);
    xllastarg();

    return (osseek(getfile(fptr), position, whence) == 0 ? true : NIL);
}

// xunlink -  built-in function 'unlink'
LVAL xunlink()
{
    static char *cfn_name = "unlink";

    extern int osunlink();
    extern void check_if_disabled();

    check_if_disabled(cfn_name);

    LVAL path = xlgastring();
    xllastarg();

    return (osunlink(getstring(path)) == 0 ? true : NIL);
}

// xstreamp - built-in function 'stream?'
LVAL xstreamp()
{
    static char *cfn_name = "stream?";

    LVAL arg = xlgetarg();
    xllastarg();

    return (streamp(arg) ? true : NIL);
}

// xinputstreamp - built-in function 'input-stream?'
LVAL xinputstreamp()
{
    static char *cfn_name = "input-stream?";

    LVAL arg = xlgetarg();
    xllastarg();

    return (istreamp(arg) ? true : NIL);
}

// xoutputstreamp - built-in function 'output-stream?'
LVAL xoutputstreamp()
{
    static char *cfn_name = "output-stream?";

    LVAL arg = xlgetarg();
    xllastarg();

    return (ostreamp(arg) ? true : NIL);
}

// xtranson - built-in function 'transcript-on'
LVAL xtranson()
{
    static char *cfn_name = "transcript-on";
    extern FILE *osaopen();

    check_if_disabled(cfn_name);

    // get the file name and direction
    char *name = getstring(xlgastring());
    xllastarg();

    // close any currently open transcript file
    if (tfp)
    {
        osclose(tfp);
        tfp = NULL;
    }

    // try to open the file
    return ((tfp = osaopen(name, "w")) == NULL ? NIL : true);
}

// xtransoff - built-in function 'transcript-off'
LVAL xtransoff()
{
    static char *cfn_name = "transcript-off";

    check_if_disabled(cfn_name);

    // make sure there aren't any arguments
    xllastarg();

    // make sure the transcript is open
    if (tfp == NULL)
    {
        return (NIL);
    }

    // close the transcript and return successfully
    osclose(tfp);
    tfp = NULL;

    return (true);
}

// xmakestring - built-in function 'make-string'
LVAL xmakestring()
{
    static char *cfn_name = "make-string";

    // get the string size
    LVAL arg = xlgafixnum();
    int len = (int)getfixnum(arg);

    if (len < 0)
    {
        xlcerror("bad size for make-string", arg, NIL);
    }

    // check for an initialization value
    char ch;
    if (moreargs())
    {
        arg = xlgachar();       // get the initializer
        xllastarg();    // make sure that's the last argument
        ch = getchcode(arg);
    }
    else
    {
        ch = ' ';       // no initialization value, default to space
    }

    LVAL val = newstring(len + 1);
    char *p = getstring(val); // initialize the string
    p[len] = 0;
    for (; --len >= 0;)
    {
        *p++ = ch;
    }

    // return the new vector
    return (val);
}

// xstrlen - built-in function 'string-size'
LVAL xstrlen()
{
    static char *cfn_name = "string-size";

    LVAL str = xlgastring();
    xllastarg();

    return (cvfixnum((FIXTYPE) (getslength(str) - 1)));
}

// xstrnullp - built-in function 'string-null?'
LVAL xstrnullp()
{
    static char *cfn_name = "string-null?";

    LVAL str = xlgastring();
    xllastarg();

    return (getslength(str) == 1 ? true : NIL);
}

// xstrappend - built-in function 'string-append'
LVAL xstrappend()
{
    static char *cfn_name = "string-append?";

    // save the argument list
    int saveargc = xlargc;
    LVAL *savesp = xlsp;

    // find the size of the new string
    int len;
    for (len = 0; moreargs();)
    {
        LVAL tmp = xlgastring();
        len += (int)getslength(tmp) - 1;
    }

    // restore the argument list
    xlargc = saveargc;
    xlsp = savesp;

    // create the result string
    LVAL val = newstring(len + 1);
    char *str = getstring(val);

    // combine the strings
    for (*str = '\0'; moreargs();)
    {
        LVAL tmp = nextarg();
        strcat(str, getstring(tmp));
    }

    // return the new string
    return (val);
}

// xstrref - built-in function 'string-ref'
LVAL xstrref()
{
    static char *cfn_name = "string-ref";

    // get the string and the index
    LVAL str = xlgastring();
    LVAL num = xlgafixnum();
    xllastarg();

    // range check the index
    int n;
    if ((n = (int)getfixnum(num)) < 0 || n >= getslength(str) - 1)
    {
        xlcerror("index out of range in string-ref", num, NIL);
    }

    // return the character
    return (cvchar(getstring(str)[n]));
}

// xstrset - built-in function 'string-set'
LVAL xstrset()
{
    static char *cfn_name = "string-set";

    // get the string, the index and the value
    LVAL str = xlgastring();
    LVAL num = xlgafixnum();
    LVAL val = xlgachar();
    xllastarg();

    // range check the index
    int n;
    if ((n = (int)getfixnum(num)) < 0 || n >= getslength(str) - 1)
    {
        xlcerror("index out of range in string-set", num, NIL);
    }

    // insert the character
    getstring(str)[n] = getchcode(val);

    return val;
}

// xsubstring - built-in function 'substring'
LVAL xsubstring()
{
    static char *cfn_name = "substring";

    // get string and starting and ending positions
    LVAL src = xlgastring();

    // get the starting position
    LVAL dst = xlgafixnum();
    int start = (int)getfixnum(dst);
    if (start < 0 || start > getslength(src) - 1)
    {
        xlcerror("index out of range in substring", dst, NIL);
    }

    // get the ending position
    int end;
    if (moreargs())
    {
        dst = xlgafixnum();
        end = (int)getfixnum(dst);
        if (end < 0 || end > getslength(src) - 1)
        {
            xlcerror("index out of range in substring", dst, NIL);
        }
    }
    else
    {
        end = getslength(src) - 1;
    }

    xllastarg();

    // setup the source pointer
    char *srcp = getstring(src) + start;
    int len = end - start;

    // make a destination string and setup the pointer
    dst = newstring(len + 1);
    char *dstp = getstring(dst);

    // copy the source to the destination
    while (--len >= 0)
    {
        *dstp++ = *srcp++;
    }
    *dstp = '\0';

    // return the substring
    return (dst);
}

// xstrlist - built-in function 'string->list'
LVAL xstrlist()
{
    static char *cfn_name = "string->list";

    // get the vector
    LVAL str = xlgastring();
    xllastarg();

    // make a list from the vector
    cpush(str);
    int size = getslength(str) - 1;
    char *p;
    for (xlval = NIL, p = &getstring(str)[size]; --size >= 0;)
    {
        xlval = cons(cvchar(*--p), xlval);
    }
    drop(1);

    return (xlval);
}

// xliststring - built-in function 'list->string'
LVAL xliststring()
{
    static char *cfn_name = "list->string";

    // get the list
    xlval = xlgalist();
    xllastarg();

    // make a vector from the list
    int size = list_size(xlval);
    LVAL str = newstring(size + 1);
    char *p;
    for (p = getstring(str); --size >= 0; xlval = cdr(xlval))
    {
        if (charp(car(xlval)))
        {
            *p++ = getchcode(car(xlval));
        }
        else
        {
            xlbadtype(car(xlval), "<char>", cfn_name);
        }
    }

    *p = '\0';

    return (str);
}

///-----------------------------------------------------------------------------
/// String comparision functions
///-----------------------------------------------------------------------------
// string<?
LVAL xstrlss()
{
    return (strcompare('<', FALSE));
}

// string<=?
LVAL xstrleq()
{
    return (strcompare('L', FALSE));
}

// string=?
LVAL xstreql()
{
    return (strcompare('=', FALSE));
}

// string>=?
LVAL xstrgeq()
{
    return (strcompare('G', FALSE));
}

// string>?
LVAL xstrgtr()
{
    return (strcompare('>', FALSE));
}

// string comparison functions (case insensitive)
// string-ci<?
LVAL xstrilss()
{
    return (strcompare('<', TRUE));
}

// string-ci<=?
LVAL xstrileq()
{
    return (strcompare('L', TRUE));
}

// string-ci=?
LVAL xstrieql()
{
    return (strcompare('=', TRUE));
}

// string-ci>=?
LVAL xstrigeq()
{
    return (strcompare('G', TRUE));
}

// string-ci>?
LVAL xstrigtr()
{
    return (strcompare('>', TRUE));
}

// strcompare - compare strings
static LVAL strcompare(int fcn, int icase)
{
    static char *cfn_name = "string compare";

    // get the strings
    LVAL str1 = xlgastring();
    LVAL str2 = xlgastring();
    xllastarg();

    // setup the string pointers
    char *p1 = getstring(str1);
    int start1 = 0;
    int end1 = getslength(str1);
    char *p2 = getstring(str2);
    int start2 = 0;
    int end2 = getslength(str2);

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
                    return (ch1 < ch2 ? true : NIL);
                case 'L':
                    return (ch1 <= ch2 ? true : NIL);
                case '=':
                    return (NIL);
                case 'G':
                    return (ch1 >= ch2 ? true : NIL);
                case '>':
                    return (ch1 > ch2 ? true : NIL);
            }
        }
    }

    // check the termination condition
    switch (fcn)
    {
        case '<':
            return (start1 >= end1 && start2 < end2 ? true : NIL);
        case 'L':
            return (start1 >= end1 ? true : NIL);
        case '=':
            return (start1 >= end1 && start2 >= end2 ? true : NIL);
        case 'G':
            return (start2 >= end2 ? true : NIL);
        case '>':
            return (start2 >= end2 && start1 < end1 ? true : NIL);
    }

    return (NIL);       // never reached
}

// xcharint - built-in function 'char->integer'
LVAL xcharint()
{
    static char *cfn_name = "char->integer";

    LVAL arg = xlgachar();
    xllastarg();

    return (cvfixnum((FIXTYPE) getchcode(arg)));
}

// xintchar - built-in function 'integer->char'
LVAL xintchar()
{
    static char *cfn_name = "integer->char";

    LVAL arg = xlgafixnum();
    xllastarg();

    return (cvchar((int)getfixnum(arg)));
}

static int radix_int(char *str, LVAL arg)
{
    int ch = (int)*str;

    if (ch == 0)
    {
        xlcerror("malformed number in string->number", arg, NIL);
    }

    if (ch == 'x')
    {
        ch = (int)*++str;
        if (!isascii(ch) || !isxdigit(ch))
        {
            xlcerror("malformed hex number in string->number", arg, NIL);
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
            xlcerror("malformed octal number in string->number", arg, NIL);
        }

        return (int)strtol(str, NULL, 8);
    }
    else if (ch == 'b')
    {
        ch = (int)*++str;
        if (ch != '0' && ch != '1')
        {
            xlcerror("malformed binary number in string->number", arg, NIL);
        }

        return (int)strtol(str, NULL, 2);
    }

    if (!isascii(ch) || !isdigit(ch))
    {
        xlcerror("malformed number in string->number", arg, NIL);
    }

    char *p;
    int base = (int)strtol(str, &p, 10);
    if (base < 2 || base > 36)
    {
        xlcerror("bad base in string->number", arg, NIL);
    }

    if ((*p != 'r' && *p != 'R') || !isascii((int)p[1]) || !isalnum((int)p[1]))
    {
        xlcerror("malformed number in string->number", arg, NIL);
    }

    return (int)strtol(p + 1, NULL, base);
}

// built-in function 'string->number'
LVAL xstringnum()
{
    static char *cfn_name = "string->number";

    LVAL arg = xlgastring();
    xllastarg();

    char *str = getstring(arg);

    if (*str == 0)
    {
        xlcerror("malformed number in string->number", arg, NIL);
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

        return cvfixnum(ival);
    }

    for (; *p; p++)
    {
        if (*p == '.' || *p == 'E' || *p == 'e' || *p == 'D' || *p == 'd')
        {
            FLOTYPE fval;
            sscanf(str, "%lf", &fval);
            return cvflonum(fval);
        }
    }

    ival = atoi(str);

    return cvfixnum(ival);
}

// built-in function 'number->string'
LVAL xnumstring()
{
    static char *cfn_name = "number->string";

    LVAL arg = xlgetarg();
    xllastarg();

    if (!numberp(arg))
    {
        xlbadtype(arg, "<number>", cfn_name);
    }

    char buf[128];
    if (fixp(arg))
    {
        sprintf(buf, "%ld", getfixnum(arg));
    }
    else
    {
        sprintf(buf, FFMT, getflonum(arg));
    }

    return cvstring(buf);
}

///-----------------------------------------------------------------------------
/// Character comparision functions
///-----------------------------------------------------------------------------
// char<?
LVAL xchrlss()
{
    return (chrcompare('<', FALSE));
}

// char<=?
LVAL xchrleq()
{
    return (chrcompare('L', FALSE));
}

// char=?
LVAL xchreql()
{
    return (chrcompare('=', FALSE));
}

// char>=?
LVAL xchrgeq()
{
    return (chrcompare('G', FALSE));
}

// char>?
LVAL xchrgtr()
{
    return (chrcompare('>', FALSE));
}

///-----------------------------------------------------------------------------
/// Character comparision functions (case insensitive)
///-----------------------------------------------------------------------------
// char-ci<?
LVAL xchrilss()
{
    return (chrcompare('<', TRUE));
}

// char-ci<=?
LVAL xchrileq()
{
    return (chrcompare('L', TRUE));
}

// char-ci=?
LVAL xchrieql()
{
    return (chrcompare('=', TRUE));
}

// char-ci>=?
LVAL xchrigeq()
{
    return (chrcompare('G', TRUE));
}

// char-ci>?
LVAL xchrigtr()
{
    return (chrcompare('>', TRUE));
}

// chrcompare - compare characters
static LVAL chrcompare(int fcn, int icase)
{
    static char *cfn_name = "char compare";

    // get the characters
    LVAL arg = xlgachar();
    int ch1 = getchcode(arg);
    arg = xlgachar();
    int ch2 = getchcode(arg);
    xllastarg();

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
            return (ch1 < ch2 ? true : NIL);
        case 'L':
            return (ch1 <= ch2 ? true : NIL);
        case '=':
            return (ch1 == ch2 ? true : NIL);
        case 'G':
            return (ch1 >= ch2 ? true : NIL);
        case '>':
            return (ch1 > ch2 ? true : NIL);
    }

    return (NIL);       // never reached
}

// xcompile - built-in function 'compile'
LVAL xcompile()
{
    static char *cfn_name = "compile";
    extern LVAL xlcompile();

    // get the expression to compile and the environment
    xlval = xlgetarg();
    LVAL env = (moreargs()? xlgaenv() : NIL);
    xllastarg();

    // build the closure
    cpush(env);
    xlval = xlcompile(xlval, env);
    xlval = cvclosure(xlval, env);
    drop(1);

    return (xlval);
}

// xdecompile - built-in function 'decompile'
LVAL xdecompile()
{
    static char *cfn_name = "decompile";

    // get the closure (or code) and file pointer
    LVAL fun = xlgetarg();
    LVAL fptr = (moreargs()? xlgaostream() : xstdout());
    xllastarg();

    // make sure we got either a closure or a code object
    if (!closurep(fun))
    {
        xlbadtype(fun, "<closure>", cfn_name);
    }

    // decompile (disassemble) the procedure
    decode_procedure(fptr, fun);

    return (NIL);
}

// xsave - save the memory image
LVAL xsave()
{
    static char *cfn_name = "save";

    // get the file name, verbose flag and print flag
    char *name = getstring(xlgastring());
    xllastarg();

    // save the memory image
    return (xlisave(name) ? true : NIL);
}

// xrestore - restore a saved memory image
LVAL xrestore()
{
    static char *cfn_name = "restore";
    extern JMP_BUF top_level;

    // get the file name, verbose flag and print flag
    char *name = getstring(xlgastring());
    xllastarg();

    // restore the saved memory image
    if (!xlirestore(name))
    {
        return (NIL);
    }

    // return directly to the top level
    stdputstr("[ returning to the top level ]\n");
    LONGJMP(top_level, 1);

    return (NIL);       // never reached
}

// xgc - function to force garbage collection
LVAL xgc()
{
    static char *cfn_name = "gc";
    extern FIXTYPE nnodes, nfree, gccalls, total;
    extern int nscount, vscount;

    // check the argument list and call the garbage collector
    if (moreargs())
    {
        LVAL arg = xlgafixnum();
        int arg1 = (int)getfixnum(arg);
        arg = xlgafixnum();
        int arg2 = (int)getfixnum(arg);
        xllastarg();
        while (--arg1 >= 0)
        {
            nexpand(NSSIZE);
        }
        while (--arg2 >= 0)
        {
            vexpand(VSSIZE);
        }
    }
    else
    {
        gc(GC_USER);
    }

    // return (gccalls nnodes nfree nscount vscount total)
    xlval = cons(cvfixnum(total), NIL);
    xlval = cons(cvfixnum((FIXTYPE) vscount), xlval);
    xlval = cons(cvfixnum((FIXTYPE) nscount), xlval);
    xlval = cons(cvfixnum(nfree), xlval);
    xlval = cons(cvfixnum(nnodes), xlval);
    xlval = cons(cvfixnum(gccalls), xlval);

    return (xlval);
}

// xerror - built-in function 'error'
LVAL xerror()
{
    static char *cfn_name = "error";

    // print the error message
    LVAL msg = xlgastring();
    errputstr("error: ");
    errputstr(getstring(msg));
    errputstr("\n");

    // print each of the remaining arguments on separate lines
    while (moreargs())
    {
        errputstr("  ");
        errprint(xlgetarg());
    }

    // print the function where the error occurred
    errputstr("happened in: ");
    errprint(xlfun);

    // call the handler
    callerrorhandler();

    return (NIL);       // never reached
}

// default-handler
LVAL default_handler()
{
    static char *cfn_name = "default error handler";

    extern JMP_BUF top_level, bc_dispatch;
    extern LVAL s_stderr, s_unbound, xlreverse(), get_module();
    // extern LVAL s_backtracep;
    extern void do_backtrace(), set_xlframe();

    LVAL condition = xlgaobject();
    LVAL cc = xlgetarg();
    xllastarg();
    LVAL fptr = getvalue(s_stderr);
    LVAL cls = class_of(condition);
    LVAL sds = getivar(cls, SLOTS);

    if (cc == NULL)
    {
        xlputstr(fptr, "Non-c");
    }
    else
    {
        xlputstr(fptr, "C");
    }

    xlputstr(fptr, "ontinuable error---calling default handler:\n");
    xlputstr(fptr, "Condition class is ");
    xlprin1(cls, fptr);
    xlterpri(fptr);
    for (int i = 1; sds; i++, sds = cdr(sds))
    {
        if (getivar(condition, i) == s_unbound)
        {
            continue;
        }
        int len = 15 - strlen(getstring(getpname(getslotname(car(sds)))));
        xlprin1(getslotname(car(sds)), fptr);
        xlputc(fptr, ':');
        while (len-- > 0)
        {
            xlputc(fptr, ' ');
        }
        xlprin1(getivar(condition, i), fptr);
        xlterpri(fptr);
    }

    oscheck();
    set_xlframe(1);
    LVAL thread_module = get_module("thread");
    xlval = getvalue(xlenter_module("debug", thread_module));
    cpush(condition);
    cpush(cc);
    xlargc = 2;
    xlapply();
    LONGJMP(bc_dispatch, 1);

    // never reached
    LONGJMP(top_level, 1);

    return (NIL);
}

// xreset - built-in function 'reset'
LVAL xreset()
{
    static char *cfn_name = "reset";
    extern JMP_BUF top_level;

    xllastarg();
    LONGJMP(top_level, 1);

    return (NIL);       // never reached
}

// xgetarg - return a command line argument
LVAL xgetarg()
{
    static char *cfn_name = "getarg";
    extern char **clargv;
    extern int clargc;

    LVAL arg = xlgafixnum();
    int n = (int)getfixnum(arg);
    xllastarg();

    return (n >= 0 && n < clargc ? cvstring(clargv[n]) : NIL);
}

// xexit - exit to the operating system
LVAL xexit()
{
    static char *cfn_name = "exit";

    int retval = 0;

    if (moreargs())
    {
        LVAL val = xlgafixnum();
        retval = getfixnum(val);
        xllastarg();
    }

    xlwrapup(retval);
    return (NIL);       // never reached
}


///-----------------------------------------------------------------------------
