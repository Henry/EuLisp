// xsfun2.c - xscheme built-in functions - part 2
/*     Copyright (c) 1988, by David Michael Betz
       All Rights Reserved */
// Euscheme code Copyright (c) 1994 Russell Bradford

#include "xscheme.h"
#include "xsobj.h"
#include "xssymbols.h"
#include "xsproto.h"

// external variables
extern JMP_BUF top_level;
extern LVAL xlfun, xlenv, xlval;
extern int prbreadth, prdepth;
extern FILE *tfp;

// forward declarations
static void do_maploop(LVAL last);
static void do_forloop();
static void do_withfile(int flags, char *mode);
static void do_load(LVAL print);
static void do_loadloop(LVAL print);
static LVAL setit(int *pvar);
static LVAL openfile(int flags, char *mode);
static LVAL strcompare(int fcn, int icase);
static LVAL chrcompare(int fcn, int icase);

// xapply - built-in function 'apply'
// (apply list 1 2 '(3 4)) -> (list 1 2 3 4)
void xapply()
{
    static char *cfn_name = "apply";
    LVAL arglist, *from, *to;
    int nargs, i;

    // get the function
    xlval = xlgetarg();

    arglist = xlsp[xlargc - 1];
    if (!listp(arglist))
        xlbadtype(arglist, "<list>", cfn_name);

    nargs = length(arglist);

    // check for room for extra args
    check(nargs - 1);
    xlargc--;   // number of explicit args

    // shift up (or down) explicit args
    if (nargs == 0)
    {
        for (from = xlsp + xlargc - 1, to = xlsp + xlargc, i = 0;
             i < xlargc; i++)
            *to-- = *from--;
        xlsp++;
    }
    else
    {
        xlsp -= nargs - 1;
        for (from = xlsp + nargs - 1, to = xlsp, i = 0; i < xlargc; i++)
            *to++ = *from++;
    }

    // copy the list arguments onto the stack
    for (to = xlsp + xlargc; consp(arglist); arglist = cdr(arglist))
        *to++ = car(arglist);

    xlargc += nargs;

    // apply the function to the arguments
    xlapply();
}

// xcallcc - built-in function 'call-with-current-continuation'
void xcallcc()
{
    static char *cfn_name = "call/cc";
    LVAL cont;
    extern LVAL current_continuation();

    // get the function to call
    xlval = xlgetarg();
    xllastarg();

    // create a continuation object
    cont = current_continuation(FALSE);

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
        xltoofew(cfn_name);
    xlval = NIL;
    do_maploop(NIL);
}

// do_maploop - setup for the next application
static void do_maploop(LVAL last)
{
    extern LVAL cs_map1;
    LVAL *oldsp, *p, x;
    int cnt;

    // get a pointer to the end of the argument list
    p = &xlsp[xlargc];
    oldsp = xlsp;

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
    for (cnt = xlargc; --cnt >= 1;)
    {
        x = *--p;
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
    LVAL last, tmp;

    // get the argument count
    tmp = pop();

    // get the tail of the value list
    if ((last = pop()) != NIL)
    {
        rplacd(last, cons(xlval, NIL)); // add the new value to the tail
        last = cdr(last);       // remember the new tail
        xlval = pop();  // restore the head of the list
    }
    else
        xlval = last = cons(xlval, NIL);        /* build the initial value list
                                                 */

    // convert the argument count and loop
    xlargc = (int)getfixnum(tmp);
    do_maploop(last);
}

// xforeach - built-in function 'for-each'
void xforeach()
{
    static char *cfn_name = "for-each";
    if (xlargc < 2)
        xltoofew(cfn_name);
    do_forloop();
}

// do_forloop - setup for the next application
static void do_forloop()
{
    extern LVAL cs_foreach1;
    LVAL *oldsp, *p, x;
    int cnt;

    // get a pointer to the end of the argument list
    p = &xlsp[xlargc];
    oldsp = xlsp;

    // save a continuation
    check(3);
    push(cvfixnum((FIXTYPE) xlargc));
    push(cs_foreach1);
    push(xlenv);

    // build the argument list for the next application
    for (cnt = xlargc; --cnt >= 1;)
    {
        x = *--p;
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
    LVAL tmp;

    // get the argument count
    tmp = pop();

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
    LVAL name, file;
    FILE *fp;

    // get the function to call
    name = xlgastring();
    xlval = xlgetarg();
    xllastarg();

    // create a file object
    file = cvport(NULL, flags);
    if ((fp = osaopen(getstring(name), mode)) == NULL)
        xlcerror("can't open file", name, NIL);
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
    LVAL file;
    FILE *fp;

    // get the function to call
    xlval = xlgastring();
    xllastarg();

    // create a file object
    file = cvport(NULL, PF_INPUT);
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
    extern LVAL cs_load1, s_eval;
    LVAL expr;

    // try to read the next expression from the file
    if (xlread(xlval, &expr))
    {

        // save a continuation
        check(4);
        push(xlval);
        push(print);
        push(cs_load1);
        push(xlenv);

        // setup the argument list
        xlval = getvalue(s_eval);
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
    LVAL print;

    // print the value if the print variable is set
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
        xlreturn();
}

// xforce1 - continuation for xforce
void xforce1()
{
    LVAL promise;
    promise = pop();
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
    LVAL fptr, val;

    // get file pointer and eof value
    fptr = (moreargs()? xlgaiport() : xstdin());
    xllastarg();

    // read an expression
    if (!xlread(fptr, &val))
        val = eof_object;

    // return the expression
    return (val);
}

// xrdchar - built-in function 'read-char'
LVAL xrdchar()
{
    static char *cfn_name = "read-char";
    LVAL fptr;
    int ch;
    fptr = (moreargs()? xlgaiport() : xstdin());
    xllastarg();
    return ((ch = xlgetc(fptr)) == EOF ? eof_object : cvchar(ch));
}

// xrdbyte - built-in function 'read-byte'
LVAL xrdbyte()
{
    static char *cfn_name = "read-byte";
    LVAL fptr;
    int ch;
    fptr = (moreargs()? xlgaiport() : xstdin());
    xllastarg();
    return ((ch = xlgetc(fptr)) == EOF ? eof_object : cvfixnum((FIXTYPE) ch));
}

// xrdshort - built-in function 'read-short'
LVAL xrdshort()
{
    static char *cfn_name = "read-short";
    unsigned char *p;
    short int val = 0;
    LVAL fptr;
    int ch, n;
    fptr = (moreargs()? xlgaiport() : xstdin());
    xllastarg();
    for (n = sizeof(short int), p = (unsigned char *)&val; --n >= 0;)
    {
        if ((ch = xlgetc(fptr)) == EOF)
            return (eof_object);
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
    LVAL fptr;
    int ch, n;
    fptr = (moreargs()? xlgaiport() : xstdin());
    xllastarg();
    for (n = sizeof(long int), p = (unsigned char *)&val; --n >= 0;)
    {
        if ((ch = xlgetc(fptr)) == EOF)
            return (eof_object);
        *p++ = ch;
    }
    return (cvfixnum((FIXTYPE) val));
}

// peek-char
LVAL xpeek_char()
{
    static char *cfn_name = "peek-char";
    LVAL fptr;
    int ch;

    fptr = (moreargs()? xlgaiport() : xstdin());
    xllastarg();

    ch = xlpeekchar(fptr);
    if (ch == NOCHAR)
        return NIL;
    else if (ch == EOF)
        return eof_object;
    else
        return cvchar(ch);
}

// char-ready?
LVAL xchar_readyp()
{
    static char *cfn_name = "char-ready?";
    LVAL fptr;

    fptr = (moreargs()? xlgaiport() : xstdin());
    xllastarg();

    if (xlpeekchar(fptr) == NOCHAR)
        return NIL;
    else
        return true;
}

// xeofobjectp - built-in function 'eof-object?'
LVAL xeofobjectp()
{
    static char *cfn_name = "eof-object?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (arg == eof_object ? true : NIL);
}

// xwrite - built-in function 'write'
LVAL xwrite()
{
    static char *cfn_name = "write";
    LVAL fptr, val;

    // get expression to print and file pointer
    val = xlgetarg();
    fptr = (moreargs()? xlgaoport() : xstdout());
    xllastarg();

    // print the value
    xlprin1(val, fptr);
    return (true);
}

// xprint - built-in function 'print'
LVAL xprint()
{
    static char *cfn_name = "print";
    LVAL fptr, val;

    // get expression to print and file pointer
    val = xlgetarg();
    fptr = (moreargs()? xlgaoport() : xstdout());
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
    LVAL fptr, ch;
    ch = xlgachar();
    fptr = (moreargs()? xlgaoport() : xstdout());
    xllastarg();
    xlputc(fptr, (int)getchcode(ch));
    return (true);
}

// xwrbyte - built-in function 'write-byte'
LVAL xwrbyte()
{
    static char *cfn_name = "write-byte";
    LVAL fptr, ch;
    ch = xlgafixnum();
    fptr = (moreargs()? xlgaoport() : xstdout());
    xllastarg();
    xlputc(fptr, (int)getfixnum(ch));
    return (true);
}

// xwrshort - built-in function 'write-short'
LVAL xwrshort()
{
    static char *cfn_name = "write-short";
    unsigned char *p;
    short int val;
    LVAL fptr, v;
    int n;
    v = xlgafixnum();
    val = (short int)getfixnum(v);
    fptr = (moreargs()? xlgaoport() : xstdout());
    xllastarg();
    for (n = sizeof(short int), p = (unsigned char *)&val; --n >= 0;)
        xlputc(fptr, *p++);
    return (true);
}

// xwrlong - built-in function 'write-long'
LVAL xwrlong()
{
    static char *cfn_name = "write-long";
    unsigned char *p;
    long int val;
    LVAL fptr, v;
    int n;
    v = xlgafixnum();
    val = (long int)getfixnum(v);
    fptr = (moreargs()? xlgaoport() : xstdout());
    xllastarg();
    for (n = sizeof(long int), p = (unsigned char *)&val; --n >= 0;)
        xlputc(fptr, *p++);
    return (true);
}

// xdisplay - built-in function 'display'
LVAL xdisplay()
{
    static char *cfn_name = "display";
    LVAL fptr, val;

    // get expression to print and file pointer
    val = xlgetarg();
    fptr = (moreargs()? xlgaoport() : xstdout());
    xllastarg();

    // print the value
    xlprinc(val, fptr);
    return (true);
}

// xnewline - terminate the current print line
LVAL xnewline()
{
    static char *cfn_name = "newline";
    LVAL fptr;

    // get file pointer
    fptr = (moreargs()? xlgaoport() : xstdout());
    xllastarg();

    // terminate the print line and return nil
    xlterpri(fptr);

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

// setit - common routine for prbreadth/prdepth
static LVAL setit(int *pvar)
{
    static char *cfn_name = "prbreadth/prdepth";
    LVAL arg;

    // get the optional argument
    if (moreargs())
    {
        arg = xlgetarg();
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
    LVAL file, modekey, xlenter_keyword();
    char *name;
    FILE *fp;

    // get the file name and direction
    name = getstring(xlgastring());
    modekey = (moreargs()? xlgasymbol() : NIL);
    xllastarg();

    // check for binary mode
    if (modekey != NIL)
    {
        if (modekey == xlenter_keyword("binary:"))
            flags |= PF_BINARY;
        else if (modekey != xlenter_keyword("text:"))
            xlcerror("unrecognized open mode", modekey, NIL);
    }

    // try to open the file
    file = cvport(NULL, flags);
    fp = ((flags & PF_BINARY) == 0 ? osaopen(name, mode) : osbopen(name, mode));
    if (fp == NULL)
        return (NIL);
    setfile(file, fp);
    return (file);
}

// xclose - built-in function 'close-stream'
LVAL xclose()
{
    static char *cfn_name = "close-stream";
    LVAL fptr;
    fptr = xlgaport();
    xllastarg();
    if (getfile(fptr))
        osclose(getfile(fptr));
    setfile(fptr, NULL);
    return (NIL);
}

// xclosei - built-in function 'close-input-stream'
LVAL xclosei()
{
    static char *cfn_name = "close-input-stream";
    LVAL fptr;
    fptr = xlgaiport();
    xllastarg();
    if (getfile(fptr))
        osclose(getfile(fptr));
    setfile(fptr, NULL);
    return (NIL);
}

// xcloseo - built-in function 'close-output-stream'
LVAL xcloseo()
{
    static char *cfn_name = "close-output-stream";
    LVAL fptr;
    fptr = xlgaoport();
    xllastarg();
    if (getfile(fptr))
        osclose(getfile(fptr));
    setfile(fptr, NULL);
    return (NIL);
}

// xgetfposition - built-in function 'get-file-position'
LVAL xgetfposition()
{
    static char *cfn_name = "get-file-position";
    extern long ostell();
    LVAL fptr;
    fptr = xlgaport();
    xllastarg();
    return (cvfixnum(ostell(getfile(fptr))));
}

// xsetfposition - built-in function 'set-file-position!'
LVAL xsetfposition()
{
    static char *cfn_name = "set-file-position!";
    LVAL fptr, val;
    long position;
    int whence;
    fptr = xlgaport();
    val = xlgafixnum();
    position = getfixnum(val);
    val = xlgafixnum();
    whence = (int)getfixnum(val);
    xllastarg();
    return (osseek(getfile(fptr), position, whence) == 0 ? true : NIL);
}

// xunlink -  built-in function 'unlink'
LVAL xunlink()
{
    static char *cfn_name = "unlink";
    LVAL path;
    extern int osunlink();
    extern void check_if_disabled();

    check_if_disabled(cfn_name);

    path = xlgastring();
    xllastarg();
    return (osunlink(getstring(path)) == 0 ? true : NIL);
}

// xportp - built-in function 'stream?'
LVAL xportp()
{
    static char *cfn_name = "stream?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (portp(arg) ? true : NIL);
}

// xinputportp - built-in function 'input-stream?'
LVAL xinputportp()
{
    static char *cfn_name = "input-stream?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (iportp(arg) ? true : NIL);
}

// xoutputportp - built-in function 'output-stream?'
LVAL xoutputportp()
{
    static char *cfn_name = "output-stream?";
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (oportp(arg) ? true : NIL);
}

// xtranson - built-in function 'transcript-on'
LVAL xtranson()
{
    static char *cfn_name = "transcript-on";
    extern FILE *osaopen();
    char *name;

    check_if_disabled(cfn_name);

    // get the file name and direction
    name = getstring(xlgastring());
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
        return (NIL);

    // close the transcript and return successfully
    osclose(tfp);
    tfp = NULL;
    return (true);
}

// xmakestring - built-in function 'make-string'
LVAL xmakestring()
{
    static char *cfn_name = "make-string";
    LVAL arg, val;
    char ch, *p;
    int len;

    // get the string size
    arg = xlgafixnum();
    len = (int)getfixnum(arg);

    if (len < 0)
        xlcerror("bad length for make-string", arg, NIL);

    // check for an initialization value
    if (moreargs())
    {
        arg = xlgachar();       // get the initializer
        xllastarg();    // make sure that's the last argument
        ch = getchcode(arg);
    }
    else
        ch = ' ';       // no initialization value, default to space

    val = newstring(len + 1);
    p = getstring(val); // initialize the string
    p[len] = 0;
    for (; --len >= 0;)
        *p++ = ch;

    // return the new vector
    return (val);
}

// xstrlen - built-in function 'string-length'
LVAL xstrlen()
{
    static char *cfn_name = "string-length";
    LVAL str;
    str = xlgastring();
    xllastarg();
    return (cvfixnum((FIXTYPE) (getslength(str) - 1)));
}

// xstrnullp - built-in function 'string-null?'
LVAL xstrnullp()
{
    static char *cfn_name = "string-null?";
    LVAL str;
    str = xlgastring();
    xllastarg();
    return (getslength(str) == 1 ? true : NIL);
}

// xstrappend - built-in function 'string-append'
LVAL xstrappend()
{
    static char *cfn_name = "string-append?";
    LVAL *savesp, tmp, val;
    char *str;
    int saveargc, len;

    // save the argument list
    saveargc = xlargc;
    savesp = xlsp;

    // find the length of the new string
    for (len = 0; moreargs();)
    {
        tmp = xlgastring();
        len += (int)getslength(tmp) - 1;
    }

    // restore the argument list
    xlargc = saveargc;
    xlsp = savesp;

    // create the result string
    val = newstring(len + 1);
    str = getstring(val);

    // combine the strings
    for (*str = '\0'; moreargs();)
    {
        tmp = nextarg();
        strcat(str, getstring(tmp));
    }

    // return the new string
    return (val);
}

// xstrref - built-in function 'string-ref'
LVAL xstrref()
{
    static char *cfn_name = "string-ref";
    LVAL str, num;
    int n;

    // get the string and the index
    str = xlgastring();
    num = xlgafixnum();
    xllastarg();

    // range check the index
    if ((n = (int)getfixnum(num)) < 0 || n >= getslength(str) - 1)
        xlcerror("index out of range in string-ref", num, NIL);

    // return the character
    return (cvchar(getstring(str)[n]));
}

// xstrset - built-in function 'string-set!'
LVAL xstrset()
{
    static char *cfn_name = "string-set!";
    LVAL str, num, val;
    int n;

    // get the string, the index and the value
    str = xlgastring();
    num = xlgafixnum();
    val = xlgachar();
    xllastarg();

    // range check the index
    if ((n = (int)getfixnum(num)) < 0 || n >= getslength(str) - 1)
        xlcerror("index out of range in string-set!", num, NIL);

    // insert the character
    getstring(str)[n] = getchcode(val);

    return val;
}

// xsubstring - built-in function 'substring'
LVAL xsubstring()
{
    static char *cfn_name = "substring";
    char *srcp, *dstp;
    int start, end, len;
    LVAL src, dst;

    // get string and starting and ending positions
    src = xlgastring();

    // get the starting position
    dst = xlgafixnum();
    start = (int)getfixnum(dst);
    if (start < 0 || start > getslength(src) - 1)
        xlcerror("index out of range in substring", dst, NIL);

    // get the ending position
    if (moreargs())
    {
        dst = xlgafixnum();
        end = (int)getfixnum(dst);
        if (end < 0 || end > getslength(src) - 1)
            xlcerror("index out of range in substring", dst, NIL);
    }
    else
        end = getslength(src) - 1;
    xllastarg();

    // setup the source pointer
    srcp = getstring(src) + start;
    len = end - start;

    // make a destination string and setup the pointer
    dst = newstring(len + 1);
    dstp = getstring(dst);

    // copy the source to the destination
    while (--len >= 0)
        *dstp++ = *srcp++;
    *dstp = '\0';

    // return the substring
    return (dst);
}

// xstrlist - built-in function 'string->list'
LVAL xstrlist()
{
    static char *cfn_name = "string->list";
    char *p;
    LVAL str;
    int size;

    // get the vector
    str = xlgastring();
    xllastarg();

    // make a list from the vector
    cpush(str);
    size = getslength(str) - 1;
    for (xlval = NIL, p = &getstring(str)[size]; --size >= 0;)
        xlval = cons(cvchar(*--p), xlval);
    drop(1);
    return (xlval);
}

// xliststring - built-in function 'list->string'
LVAL xliststring()
{
    static char *cfn_name = "list->string";
    char *p;
    LVAL str;
    int size;

    // get the list
    xlval = xlgalist();
    xllastarg();

    // make a vector from the list
    size = length(xlval);
    str = newstring(size + 1);
    for (p = getstring(str); --size >= 0; xlval = cdr(xlval))
        if (charp(car(xlval)))
            *p++ = getchcode(car(xlval));
        else
            xlbadtype(car(xlval), "<char>", cfn_name);
    *p = '\0';
    return (str);
}

// string comparision functions
LVAL xstrlss()
{
    return (strcompare('<', FALSE));
}  // string<?

LVAL xstrleq()
{
    return (strcompare('L', FALSE));
}  // string<=?

LVAL xstreql()
{
    return (strcompare('=', FALSE));
}  // string=?

LVAL xstrgeq()
{
    return (strcompare('G', FALSE));
}  // string>=?

LVAL xstrgtr()
{
    return (strcompare('>', FALSE));
}  // string>?

// string comparison functions (case insensitive)
LVAL xstrilss()
{
    return (strcompare('<', TRUE));
}  // string-ci<?

LVAL xstrileq()
{
    return (strcompare('L', TRUE));
}  // string-ci<=?

LVAL xstrieql()
{
    return (strcompare('=', TRUE));
}  // string-ci=?

LVAL xstrigeq()
{
    return (strcompare('G', TRUE));
}  // string-ci>=?

LVAL xstrigtr()
{
    return (strcompare('>', TRUE));
}  // string-ci>?

// strcompare - compare strings
static LVAL strcompare(int fcn, int icase)
{
    static char *cfn_name = "string compare";
    int start1, end1, start2, end2, ch1, ch2;
    char *p1, *p2;
    LVAL str1, str2;

    // get the strings
    str1 = xlgastring();
    str2 = xlgastring();
    xllastarg();

    // setup the string pointers
    p1 = getstring(str1);
    start1 = 0;
    end1 = getslength(str1);
    p2 = getstring(str2);
    start2 = 0;
    end2 = getslength(str2);

    // compare the strings
    for (; start1 < end1 && start2 < end2; ++start1, ++start2)
    {
        ch1 = *p1++;
        ch2 = *p2++;
        if (icase)
        {
            if (isupper(ch1))
                ch1 = tolower(ch1);
            if (isupper(ch2))
                ch2 = tolower(ch2);
        }
        if (ch1 != ch2)
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
    LVAL arg;
    arg = xlgachar();
    xllastarg();
    return (cvfixnum((FIXTYPE) getchcode(arg)));
}

// xintchar - built-in function 'integer->char'
LVAL xintchar()
{
    static char *cfn_name = "integer->char";
    LVAL arg;
    arg = xlgafixnum();
    xllastarg();
    return (cvchar((int)getfixnum(arg)));
}

static int radix_int(char *str, LVAL arg)
{
    char *p;
    int base, ch;

    ch = (int)*str;

    if (ch == 0)
        xlcerror("malformed number in string->number", arg, NIL);

    if (ch == 'x')
    {
        ch = (int)*++str;
        if (!isascii(ch) || !isxdigit(ch))
            xlcerror("malformed hex number in string->number", arg, NIL);
        return (int)strtol(str, NULL, 16);
    }
    else if (ch == 'o')
    {
        ch = (int)*++str;
        if (ch != '0' && ch != '1' && ch != '2' && ch != '3' &&
        ch != '4' && ch != '5' && ch != '6' && ch != '7')
            xlcerror("malformed octal number in string->number", arg, NIL);
        return (int)strtol(str, NULL, 8);
    }
    else if (ch == 'b')
    {
        ch = (int)*++str;
        if (ch != '0' && ch != '1')
            xlcerror("malformed binary number in string->number", arg, NIL);
        return (int)strtol(str, NULL, 2);
    }

    if (!isascii(ch) || !isdigit(ch))
        xlcerror("malformed number in string->number", arg, NIL);

    base = (int)strtol(str, &p, 10);
    if (base < 2 || base > 36)
        xlcerror("bad base in string->number", arg, NIL);

    if ((*p != 'r' && *p != 'R') || !isascii((int)p[1]) || !isalnum((int)p[1]))
        xlcerror("malformed number in string->number", arg, NIL);

    return (int)strtol(p + 1, NULL, base);
}

// built-in function 'string->number'
LVAL xstringnum()
{
    static char *cfn_name = "string->number";
    LVAL arg;
    char *str, *p;
    int ival;
    FLOTYPE fval;

    arg = xlgastring();
    xllastarg();

    str = getstring(arg);

    if (*str == 0)
        xlcerror("malformed number in string->number", arg, NIL);

    p = str;
    if (*p == '+' || *p == '-')
        p++;

    if (*p == '#')
    {
        ival = radix_int(p + 1, arg);
        if (*str == '-')
            ival = -ival;
        return cvfixnum(ival);
    }

    for (; *p; p++)
    {
        if (*p == '.' || *p == 'E' || *p == 'e' || *p == 'D' || *p == 'd')
        {
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
    LVAL arg;
    char buf[128];

    arg = xlgetarg();
    xllastarg();

    if (!numberp(arg))
        xlbadtype(arg, "<number>", cfn_name);

    if (fixp(arg))
        sprintf(buf, "%ld", getfixnum(arg));
    else
        sprintf(buf, FFMT, getflonum(arg));

    return cvstring(buf);
}

// character comparision functions
LVAL xchrlss()
{
    return (chrcompare('<', FALSE));
}  // char<?

LVAL xchrleq()
{
    return (chrcompare('L', FALSE));
}  // char<=?

LVAL xchreql()
{
    return (chrcompare('=', FALSE));
}  // char=?

LVAL xchrgeq()
{
    return (chrcompare('G', FALSE));
}  // char>=?

LVAL xchrgtr()
{
    return (chrcompare('>', FALSE));
}  // char>?

// character comparision functions (case insensitive)
LVAL xchrilss()
{
    return (chrcompare('<', TRUE));
}  // char-ci<?

LVAL xchrileq()
{
    return (chrcompare('L', TRUE));
}  // char-ci<=?

LVAL xchrieql()
{
    return (chrcompare('=', TRUE));
}  // char-ci=?

LVAL xchrigeq()
{
    return (chrcompare('G', TRUE));
}  // char-ci>=?

LVAL xchrigtr()
{
    return (chrcompare('>', TRUE));
}  // char-ci>?

// chrcompare - compare characters
static LVAL chrcompare(int fcn, int icase)
{
    static char *cfn_name = "char compare";
    int ch1, ch2;
    LVAL arg;

    // get the characters
    arg = xlgachar();
    ch1 = getchcode(arg);
    arg = xlgachar();
    ch2 = getchcode(arg);
    xllastarg();

    // convert to lowercase if case insensitive
    if (icase)
    {
        if (isupper(ch1))
            ch1 = tolower(ch1);
        if (isupper(ch2))
            ch2 = tolower(ch2);
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
    LVAL env;

    // get the expression to compile and the environment
    xlval = xlgetarg();
    env = (moreargs()? xlgaenv() : NIL);
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
    LVAL fun, fptr;

    // get the closure (or code) and file pointer
    fun = xlgetarg();
    fptr = (moreargs()? xlgaoport() : xstdout());
    xllastarg();

    // make sure we got either a closure or a code object
    if (!closurep(fun))
        xlbadtype(fun, "<closure>", cfn_name);

    // decompile (disassemble) the procedure
    decode_procedure(fptr, fun);
    return (NIL);
}

// xsave - save the memory image
LVAL xsave()
{
    static char *cfn_name = "save";
    char *name;

    // get the file name, verbose flag and print flag
    name = getstring(xlgastring());
    xllastarg();

    // save the memory image
    return (xlisave(name) ? true : NIL);
}

// xrestore - restore a saved memory image
LVAL xrestore()
{
    static char *cfn_name = "restore";
    extern JMP_BUF top_level;
    char *name;

    // get the file name, verbose flag and print flag
    name = getstring(xlgastring());
    xllastarg();

    // restore the saved memory image
    if (!xlirestore(name))
        return (NIL);

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
    int arg1, arg2;
    LVAL arg;

    // check the argument list and call the garbage collector
    if (moreargs())
    {
        arg = xlgafixnum();
        arg1 = (int)getfixnum(arg);
        arg = xlgafixnum();
        arg2 = (int)getfixnum(arg);
        xllastarg();
        while (--arg1 >= 0)
            nexpand(NSSIZE);
        while (--arg2 >= 0)
            vexpand(VSSIZE);
    }
    else
        gc(GC_USER);

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
    // extern JMP_BUF top_level;
    LVAL msg;

    // display the error message
    msg = xlgastring();
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
    LVAL condition, fptr, cls, sds, cc;
    int i, len;
    extern JMP_BUF top_level, bc_dispatch;
    extern LVAL s_stderr, s_unbound, xlreverse(), get_module();
    // extern LVAL s_backtracep;
    extern void do_backtrace(), set_xlframe();
    LVAL thread_module;

    condition = xlgaobject();
    cc = xlgetarg();
    xllastarg();
    fptr = getvalue(s_stderr);
    cls = class_of(condition);
    sds = getivar(cls, SLOTS);

    if (cc == NULL)
        xlputstr(fptr, "Non-c");
    else
        xlputstr(fptr, "C");
    xlputstr(fptr, "ontinuable error---calling default handler:\n");
    xlputstr(fptr, "Condition class is ");
    xlprin1(cls, fptr);
    xlterpri(fptr);
    for (i = 1; sds; i++, sds = cdr(sds))
    {
        if (getivar(condition, i) == s_unbound)
            continue;
        len = 15 - strlen(getstring(getpname(getslotname(car(sds)))));
        xlprin1(getslotname(car(sds)), fptr);
        xlputc(fptr, ':');
        while (len-- > 0)
            xlputc(fptr, ' ');
        xlprin1(getivar(condition, i), fptr);
        xlterpri(fptr);
    }

    oscheck();
    set_xlframe(1);
    thread_module = get_module("thread");
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
    LVAL arg;
    int n;
    arg = xlgafixnum();
    n = (int)getfixnum(arg);
    xllastarg();
    return (n >= 0 && n < clargc ? cvstring(clargv[n]) : NIL);
}

// xexit - exit to the operating system
LVAL xexit()
{
    LVAL val;
    int retval;

    static char *cfn_name = "exit";

    retval = 0;

    if (moreargs())
    {
        val = xlgafixnum();
        retval = getfixnum(val);
        xllastarg();
    }

    xlwrapup(retval);
    return (NIL);       // never reached
}
