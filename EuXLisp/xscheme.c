// xscheme.c - xscheme main routine
/*     Copyright (c) 1990, by David Michael Betz
       All Rights Reserved */
// Euscheme code Copyright (c) 1994 Russell Bradford

#include "xscheme.h"
#include "xsobj.h"
#include "xsbanner.h"

#define is_cont(sp)                                                            \
    (envp(sp[0]) && (codep(sp[1]) || csubrp(sp[1])) && !ispointer(sp[2]))

// global variables
JMP_BUF top_level;
int clargc;                     // command line argument count
char **clargv;                  // array of command line arguments
void do_backtrace();

// trace file pointer
FILE *tfp = NULL;

// external variables
extern LVAL xlfun, xlenv, xlval;

#include "xssymbols.h"
#include "xsproto.h"

extern int trace;
int quiet, no_system;
FILE *filein;

static void usage();
void xltoplevel();
void xlerror(char *msg, LVAL arg);
void callerrorhandler();
void xlwrapup(int);
void do_xlerror(char *msg, LVAL arg, LVAL errname, int cc);

#ifdef SIGNAL
#define sigset signal
#endif

#ifdef UNIX
#include <signal.h>
#define CAST (void (*)(int))

int ctrl_c = 0;
extern int reading, quiet;

void sig_int()
{
    ctrl_c = 1;

    // various signal behaviours
    #ifdef RESTORE_SIGNAL
    // one-shot signal
    sigset(SIGINT, (void (*)(int))sig_int);
    #endif
    #ifdef SIGNAL
    // restarting syscalls
    if (reading)
    {
        ctrl_c = 0;
        reading = 0;
        osflush();
        ostputc('\n');
        xltoplevel();
    }
    #endif
}

#ifdef SOCK
int broken_pipe = 0;

void sig_pipe()
{
    broken_pipe = 1;

    // various signal behaviours
    #ifdef RESTORE_SIGNAL
    // one-shot signal
    sigset(SIGPIPE, (void (*)(int))sig_pipe);
    #endif
    #ifdef SIGNAL
    // restarting syscalls
    if (reading)
    {
        broken_pipe = 0;
        reading = 0;
        osflush();
        ostputc('\n');
        xltoplevel();
    }
    #endif
}
#endif

#endif

// xlmain - the main routine
void xlmain(int argc, char **argv)
{
    int src, dst, ch;
    LVAL code;
    char *p;
    int image = TRUE;

    quiet = FALSE;
    filein = stdin;
    no_system = FALSE;

    // process the arguments
    for (src = dst = 1, clargv = argv, clargc = 1; src < argc; ++src)
    {
        // handle options
        if (argv[src][0] == '-' && strlen(argv[src]) > 1)
        {
            // handle options with long names
            if (argv[src][1] == '-')
            {
                if (strcmp(argv[src], "--script") == 0)
                {
                    src++;
                    if (src >= argc)
                    {
                        fprintf
                        (
                            stderr,
                            "euxlisp: missing input file\n"
                        );
                        exit(5);
                    }
                    filein = osaopen(argv[src], "r");
                    if (filein == NULL)
                    {
                        fprintf
                        (
                            stderr,
                            "euxlisp: can't open input '%s'\n",
                            argv[src]
                        );
                        exit(6);
                    }
                    do
                    {
                        // skip #! line
                        ch = osagetc(filein);
                    } while (ch != '\n' && ch != EOF);
                    argv[0] = argv[src];
                    quiet = TRUE;   // no mesages
                }
            }
            else // handle options with single character names
            {
                for (p = &argv[src][1]; *p != '\0';)
                {
                    switch (*p++)
                    {
                        case 't':  // root directory
                            trace = TRUE;
                            break;
                        case 'n':
                            image = FALSE;  // no image
                            break;
                        case 'q':
                            quiet = TRUE;   // no mesages
                            break;
                        case 's':
                            no_system = TRUE;       // don't allow a system call
                            break;
                        default:
                            usage();
                    }
                }
            }
        }

        // handle a filename
        else
        {
            argv[dst++] = argv[src];
            ++clargc;
        }
    }

    // setup an initialization error handler
    if (SETJMP(top_level))
    {
        fprintf(stderr, "Error in initialisation\n");
        exit(2);
    }

    // initialize
    osinit(BANNER);

    // restore the default workspace, otherwise create a new one
    if (!image || !xlirestore(IMAGE))   // image.wks
    {
        xlinitws(5000);
    }

    // do the initialization code first
    code = xlenter_module("*INITIALIZE*", root_module);
    code = (boundp(code) ? getvalue(code) : NIL);

    // trap errors
    if (SETJMP(top_level))
    {
        code = xlenter_module("*TOPLEVEL*", root_module);
        code = (boundp(code) ? getvalue(code) : NIL);
        xlfun = xlenv = xlval = NIL;
        xlsp = xlstktop;
    }

    #ifdef UNIX
    sigset(SIGINT, CAST sig_int);
    #ifdef SOCK
    sigset(SIGPIPE, CAST sig_pipe);
    #endif
    #endif

    // execute the main loop
    if (code != NIL)
    {
        xlexecute(code);
    }
    xlwrapup(1);
}

static void usage()
{
    fprintf(stderr, "usage: euscheme [-tqns] [--script] [file] [arg ...]\n");
    exit(1);
}

void xlload()
{
}

void xlcontinue()
{
}

void xlcleanup()
{
}

// xltoplevel - return to the top level
static void do_xltoplevel(int cc)
{
    if (getvalue(s_general_error) == s_unbound)
    {   // no conditions yet
        stdputstr("[ back to top level ]\n");
        LONGJMP(top_level, 1);
    }

    do_xlerror("user interrupt", NIL, s_user_intr, cc);
}

void xltoplevel()
{
    do_xltoplevel(FALSE);
}

void xltoplevel_int()
{
    do_xltoplevel(TRUE);
}

// xlfail - report an error
void xlfail(char *msg, LVAL err)
{
    xlcerror(msg, s_unbound, err);
}

// xlerror - report an error
void xlerror(char *msg, LVAL arg)
{
    // display the error message
    errputstr("Error: ");
    errputstr(msg);
    errputstr("\n");

    // print the argument on a separate line
    if (arg != s_unbound)
    {
        errputstr("  ");
        errprint(arg);
    }

    // print the function where the error occurred
    errputstr("happened in: ");
    errprint(xlfun);

    // call the handler
    callerrorhandler();
}

void set_xlframe(int n)
{
    LVAL *ptr;
    extern LVAL s_xlframe;

    ptr = xlsp + n;
    while(!is_cont(ptr) && ptr < xlstktop)
    {
        ptr++;
    }

    if (ptr == xlstktop)
    {
        ptr = xlsp;
    }

    setvalue(s_xlframe, cvsfixnum((FIXTYPE) (xlstktop - ptr)));
}

// cc is TRUE if return address is needed, e.g., in interpreter
void do_xlerror(char *msg, LVAL arg, LVAL errname, int cc)
{
    LVAL cond, condcl, cont;
    extern LVAL current_continuation();
    extern JMP_BUF bc_dispatch;

    #ifdef NOERROR
    xlerror(msg, arg);
    #endif

    if (errname == NIL)
    {
        cond = getvalue(s_general_error);
    }
    else
    {
        cond = getvalue(errname);
    }

    xlval = getvalue(s_signal);

    if (cond == s_unbound || xlval == s_unbound)
    {   // no conditions yet
        errputstr("Run-Time ");
        xlerror(msg, arg);
    }
    else
    {
        set_xlframe(1);
        setivar(cond, 1, cvstring(msg));
        condcl = getclass(cond);
        if (getsfixnum(getivar(condcl, INSTSIZE)) > 1)
            setivar(cond, 2, arg);
        oscheck();
        cont = current_continuation(cc);
        check(2);
        push(cont);     // resume continuation
        push(cond);     // condition
        xlargc = 2;
        xlapply();
        LONGJMP(bc_dispatch, 1);
    }
}

// xlinterror - report run-time error in the interpreter
void xlinterror(char *msg, LVAL arg, LVAL errname)
{
    do_xlerror(msg, arg, errname, TRUE);
}

// xlcerror - report run-time error in C code
void xlcerror(char *msg, LVAL arg, LVAL errname)
{
    // discard everything until the continuation
    while (!is_cont(xlsp))
    {
        drop(1);
    }

    do_xlerror(msg, arg, errname, FALSE);
}

// callerrorhandler - call the error handler
void callerrorhandler()
{
    if (getvalue(s_backtracep) != NIL)
    {
        do_backtrace(xlsp);
    }

    // no handler, just reset back to the top level
    // this probably leaves the thread state in a state
    LONGJMP(top_level, 1);
}

// xlabort - print an error message and abort
void xlabort(char *msg)
{
    // display the error message
    errputstr("Abort: ");
    errputstr(msg);
    errputstr("\n");

    // print the function where the error occurred
    errputstr("happened in: ");
    errprint(xlfun);

    // reset back to the top level
    oscheck();  // an opportunity to break out
    LONGJMP(top_level, 1);
}

// xlfatal - print a fatal error message and exit
void xlfatal(char *msg)
{
    oserror(msg);
    exit(4);
}

// xlwrapup - clean up and exit to the operating system
void xlwrapup(int n)
{
    if (tfp)
        osclose(tfp);
    osfinish();
    exit(n);
}

#define errprin(x) xlprin1(x, errout)
#define errstr(x)  xlputstr(errout, x)
static LVAL errout;

static void indent(int n)
{
    while (n++ < 15)
    {
        errstr(" ");
    }
}

static void trace_function(LVAL fun, LVAL env)
{
    extern FUNDEF funtab[];
    LVAL frame, vars;
    int i;

    errout = getvalue(s_stderr);

    if (codep(fun))
    {
        errstr("function ");
        if (getcname(fun) == NIL)
        {
            errstr("<anon>");
        }
        else
        {
            errprin(getcname(fun));
        }
        errstr(" ");
        errprin(getvnames(fun));
    }
    else if (csubrp(fun))
    {
        errstr("csubr ");
        errstr(funtab[getoffset(fun)].fd_name);
    }
    else
    {
        errstr("???");
    }
    xlterpri(errout);

    if (env != NIL)
    {
        if (envp(env))
        {
            frame = car(env);
            if (vectorp(frame))
            {
                vars = getelement(frame, 0);
                for (i = 1; vars; vars = cdr(vars), i++)
                {
                    errprin(car(vars));
                    errstr(": ");
                    indent(strlen(getstring(getpname(car(vars)))));
                    errprin(getelement(frame, i));
                    xlterpri(errout);
                }
            }
            else
            {
                errstr("mangled frame: ");
                errprin(frame);
                xlterpri(errout);
            }
        }
        else
        {
            errstr("mangled environment list: ");
            errprin(env);
            xlterpri(errout);
        }
    }

    xlterpri(errout);

}

// print a backtrace
void do_backtrace(LVAL * from)
{
    LVAL *sp;

    errout = getvalue(s_stderr);

    errstr("\nStack backtrace:\n\n");

    for (sp = from; sp < xlstktop; sp++)
    {
        if (is_cont(sp))
        {
            trace_function(sp[1], sp[0]);
            sp += 2;
        }
    }
}

// lisp backtrace
LVAL xbacktrace()
{
    static char *cfn_name = "backtrace";
    extern LVAL true;
    LVAL frameptr;
    LVAL *ptr;

    if (moreargs())
    {
        frameptr = xlgafixnum();
        ptr = xlstktop - getfixnum(frameptr);
        xllastarg();
    }
    else
    {
        ptr = xlsp;
    }

    do_backtrace(ptr);
    return true;
}

// debugging -- move up a frame
LVAL xframe_up()
{
    static char *cfn_name = "frame-up";
    LVAL frameptr, arg;
    LVAL *ptr, *old;
    int n;

    frameptr = xlgafixnum();
    arg = xlgetarg();   // cc
    arg = xlgetarg();   // condition
    if (moreargs())
    {
        arg = xlgafixnum();
        xllastarg();
        n = getfixnum(arg);
    }
    else
    {
        n = 1;
    }

    ptr = xlstktop - getfixnum(frameptr);

    for (; n > 0; n--)
    {
        old = ptr;
        for (ptr++; ptr < xlstktop; ptr++)
        {
            if (is_cont(ptr))
            {
                break;
            }
        }

        if (ptr >= xlstktop)
        {
            errstr("(no more frames)\n");
            ptr = old;
            break;
        }
    }

    trace_function(ptr[1], ptr[0]);
    return cvfixnum((FIXTYPE) (xlstktop - ptr));
}

// debugging -- move down a frame
LVAL xframe_down()
{
    static char *cfn_name = "frame-down";
    LVAL frameptr, arg;
    LVAL *ptr, *old;
    int n;

    frameptr = xlgafixnum();
    arg = xlgetarg();   // cc
    arg = xlgetarg();   // condition
    if (moreargs())
    {
        arg = xlgafixnum();
        xllastarg();
        n = getfixnum(arg);
    }
    else
    {
        n = 1;
    }

    ptr = xlstktop - getfixnum(frameptr);

    for (; n > 0; n--)
    {
        old = ptr;
        for (ptr--; ptr >= xlsp; ptr--)
        {
            if (is_cont(ptr))
            {
                break;
            }
        }

        if (ptr < xlsp)
        {
            errstr("(no more frames)\n");
            ptr = old;
            break;
        }
    }

    trace_function(ptr[1], ptr[0]);
    return cvfixnum((FIXTYPE) (xlstktop - ptr));
}

// debugging -- get current env
LVAL xframe_env()
{
    static char *cfn_name = "frame-env";
    LVAL frameptr;
    LVAL *ptr;

    frameptr = xlgafixnum();
    xllastarg();
    ptr = xlstktop - getfixnum(frameptr);

    return *ptr;
}

// debugging -- get current fun
LVAL xframe_fun()
{
    static char *cfn_name = "frame-fun";
    LVAL frameptr;
    LVAL *ptr;

    frameptr = xlgafixnum();
    xllastarg();
    ptr = xlstktop - getfixnum(frameptr);

    return ptr[1];
}
