/// Copyright 1990 David Michael Betz
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
/// Title: xscheme main function
///-----------------------------------------------------------------------------

#include "xscheme.h"
#include "xsobj.h"
#include "xsbanner.h"
#include <getopt.h>

#define is_cont(sp)                                                            \
    (envp(sp[0]) && (codep(sp[1]) || csubrp(sp[1])) && !ispointer(sp[2]))

///-----------------------------------------------------------------------------
/// Global variable
///-----------------------------------------------------------------------------

const char* program_name;       // The name of this program (euxlisp).
int clargc;                     // command line argument count
char **clargv;                  // array of command line arguments

JMP_BUF top_level;
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

static void print_usage(FILE* stream, int exit_code);

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

///-----------------------------------------------------------------------------
/// Signal handling
///-----------------------------------------------------------------------------

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

///-----------------------------------------------------------------------------
/// xlmain - the main function
///-----------------------------------------------------------------------------

void xlmain(int argc, char **argv)
{
    int image = TRUE;
    quiet = FALSE;
    filein = stdin;
    no_system = FALSE;
    char* image_name = IMAGE;

    // Remember the name of the program, to incorporate in messages.
    program_name = argv[0];
    clargv = argv;
    clargc = argc;

    // A string listing valid short options letters.
    const char* const short_options = "hqnNs:m:i:t";

    // An array describing valid long options.
    const struct option long_options[] =
    {
        { "help",         0, NULL, 'h' },
        { "quiet",        0, NULL, 'q' },
        { "no-image",     0, NULL, 'n' },
        { "no-sys-calls", 0, NULL, 'N' },
        { "script",       1, NULL, 's' },
        { "module",       1, NULL, 'm' },
        { "image",        1, NULL, 'i' },
        { "trace",        0, NULL, 't' },
        { NULL,           0, NULL, 0   }   // Required at end of array.
    };

    int next_option;

    do
    {
        next_option = getopt_long (argc, argv, short_options,
        long_options, NULL);

        switch (next_option)
        {
            case 'h':   // -h or --help
                // User has requested usage information.  Print it to standard
                // output, and exit with exit code zero (normal termination).
                print_usage(stdout, 0);

            case 'q':   // -q or --quiet
                quiet = TRUE;   // no messages
                break;

            case 'n':   // -n or --no-image
                image = FALSE; // no image
                break;

            case 'N':   // -N or --no-sys-calls
                no_system = TRUE;       // don't allow a system call
                break;

            case 's':   // -s file or --script file
                filein = osaopen(optarg, "r");
                if (filein == NULL)
                {
                    fprintf
                    (
                        stderr,
                        "%s: can't open script file '%s'\n",
                        program_name,
                        optarg
                    );
                    print_usage(stderr, 6);
                }

                int ch;
                do
                {
                    // skip #! line
                    ch = osagetc(filein);
                } while (ch != '\n' && ch != EOF);

                quiet = TRUE;   // no mesages
                break;

            case 'm':   // -m file or --module file
                // Reset the image_name
                // if it hasn't already reset by the --image option
                if (strcmp(image_name, IMAGE) == 0)
                {
                    image_name = IMAGE_MOD;
                }
                filein = osaopen(optarg, "r");
                if (filein == NULL)
                {
                    fprintf
                    (
                        stderr,
                        "%s: can't open module file '%s'\n",
                        program_name,
                        optarg
                    );
                    print_usage(stderr, 6);
                }

                quiet = TRUE;   // no messages
                break;

            case 'i':   // -i file or --image file
                // Set the name of the image file to the argument
                image_name = optarg;
                break;

            case 't':   // -t or --trace
                trace = TRUE;
                break;

            case '?':   // The user specified an invalid option.
                // Print usage information to standard error, and exit with exit
                // code one (indicating abnormal termination).
                print_usage(stderr, 1);

            case -1:    // Done with options.
                break;

            default:    // Something else: unexpected.
                abort();
        }
    } while(next_option != -1);


    // setup an initialization error handler
    if (SETJMP(top_level))
    {
        fprintf(stderr, "Error in initialisation\n");
        exit(2);
    }

    // initialize
    osinit(BANNER);

    // restore the default workspace, otherwise create a new one
    if (!image || !xlirestore(image_name))
    {
        if (image)
        {
            fprintf
            (
                stderr,
                "\nError: Could not find image file %s in path "
                IMAGE_SEARCH_PATH
                "\n",
                image_name
            );
            exit(3);
        }

        xlinitws(5000);
    }

    // do the initialization code first
    LVAL code = xlenter_module("*INITIALIZE*", root_module);
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

///-----------------------------------------------------------------------------
/// print_usage helper function
///-----------------------------------------------------------------------------

static void print_usage(FILE* stream, int exit_code)
{
    fprintf(stream, "Usage: %s options [ files ... ]\n", program_name);
    fprintf
    (
        stream,
        "  -h  --help             Display this usage information.\n"
        "  -q  --quiet            Print no messages, prompts or values.\n"
        "  -n  --no-image         Do not read in the initial Lisp image.\n"
        "  -N  --no-sys-calls     Disable system calls.\n"
        "  -s  --script file      Read and execute script from file.\n"
        "  -m  --module file      Read and execute module from file.\n"
        "  -i  --image file       Read the given image file rather than the default.\n"
        "  -t  --trace            Switch on byte-code level tracing.\n"
    );
    exit(exit_code);
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
    extern LVAL s_xlframe;

    LVAL *ptr = xlsp + n;
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
    extern LVAL current_continuation();
    extern JMP_BUF bc_dispatch;

    #ifdef NOERROR
    xlerror(msg, arg);
    #endif

    LVAL cond;
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
    {
        // no conditions yet
        errputstr("Run-Time ");
        xlerror(msg, arg);
    }
    else
    {
        set_xlframe(1);
        setivar(cond, 1, cvstring(msg));
        LVAL condcl = getclass(cond);
        if (getsfixnum(getivar(condcl, INSTSIZE)) > 1)
        {
            setivar(cond, 2, arg);
        }
        oscheck();
        LVAL cont = current_continuation(cc);
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
    {
        osclose(tfp);
    }
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
            LVAL frame = car(env);
            if (vectorp(frame))
            {
                LVAL vars = getelement(frame, 0);
                for (int i = 1; vars; vars = cdr(vars), i++)
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
    errout = getvalue(s_stderr);

    errstr("\nStack backtrace:\n\n");

    for (LVAL *sp = from; sp < xlstktop; sp++)
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

    LVAL *ptr;
    if (moreargs())
    {
        LVAL frameptr = xlgafixnum();
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

    LVAL frameptr = xlgafixnum();
    LVAL arg = xlgetarg();   // cc
    arg = xlgetarg();        // condition
    int n;
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

    LVAL *ptr = xlstktop - getfixnum(frameptr);

    for (; n > 0; n--)
    {
        LVAL *old = ptr;
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

    LVAL frameptr = xlgafixnum();
    LVAL arg = xlgetarg();   // cc
    arg = xlgetarg();   // condition
    int n;
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

    LVAL *ptr = xlstktop - getfixnum(frameptr);

    for (; n > 0; n--)
    {
        LVAL *old = ptr;
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

    LVAL frameptr = xlgafixnum();
    xllastarg();
    LVAL *ptr = xlstktop - getfixnum(frameptr);

    return *ptr;
}

// debugging -- get current fun
LVAL xframe_fun()
{
    static char *cfn_name = "frame-fun";

    LVAL frameptr = xlgafixnum();
    xllastarg();
    LVAL *ptr = xlstktop - getfixnum(frameptr);

    return ptr[1];
}


///-----------------------------------------------------------------------------
