/// Copyright 1990 David Michael Betz
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
/// Title: EuXLisp main function
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "euxlisp.h"
#include <getopt.h>
#include <signal.h>

///-----------------------------------------------------------------------------
/// Macros
///-----------------------------------------------------------------------------
#define ieuxls_cont(sp)                                                        \
    (                                                                          \
        euxmEnvp(sp[0])                                                        \
     && (euxmCodep(sp[1]) || euxmXFunContp(sp[1])) && !euxmIsPointer(sp[2])    \
    )

///-----------------------------------------------------------------------------
/// Global variables
///-----------------------------------------------------------------------------
///  Banner
const char* euxlBanner =
"EuLisp System EuXLisp (formally Euscheme) - Version 0.991";

///  Command-line arguments
int clargc;            // command line argument count
char * const *clargv;  // array of command line arguments

///  Stack top
euxmJmpBuf euxmStackTopLevel;

///  Trace file pointer
FILE *tfp = NULL;

///  Don't print prompt in script-mode
int quiet;

///  Current input file
FILE *filein;

///  Has a ^C been signalled
int ctrl_c = 0;

///-----------------------------------------------------------------------------
/// Local variables
///-----------------------------------------------------------------------------
///  The name of this program (euxlisp).
static const char* program_name;

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static void print_usage(FILE* stream, int exit_code);

///  Signal handling
static void eux_signal_handler_int(int signo)
{
    ctrl_c = 1;

    // restarting syscalls
    if (reading)
    {
        ctrl_c = 0;
        reading = 0;
        euxcOSFlush();
        euxcOSTPutc('\n');
        euxcToplevel();
    }
}

static void eux_signal_handler_quit(int signo)
{
    fprintf(stderr,"\nReceived SIGQUIT\n");
    exit(0);
}

#ifdef SOCK
int broken_pipe = 0;

static void eux_signal_handler_pipe(int signo)
{
    broken_pipe = 1;

    // restarting syscalls
    if (reading)
    {
        broken_pipe = 0;
        reading = 0;
        euxcOSFlush();
        euxcOSTPutc('\n');
        euxcToplevel();
    }
}
#endif

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------

///  euxcMain - the main function
void euxcMain(int argc, char * const *argv)
{
    int image = euxmTrue;
    quiet = euxmFalse;
    filein = stdin;
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
        next_option = getopt_long
        (
            argc,
            argv,
            short_options,
            long_options,
            NULL
        );

        switch (next_option)
        {
            case 'h':   // -h or --help
                // User has requested usage information.  Print it to standard
                // output, and exit with exit code zero (normal termination).
                print_usage(stdout, 0);

            case 'q':   // -q or --quiet
                quiet = euxmTrue;   // no messages
                break;

            case 'n':   // -n or --no-image
                image = euxmFalse; // no image
                break;

            case 's':   // -s file or --script file
                filein = euxcOSAOpen(optarg, "r");
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
                    ch = euxcOSAGetc(filein);
                } while (ch != '\n' && ch != EOF);

                quiet = euxmTrue;   // no mesages
                break;

            case 'm':   // -m file or --module file
                // Reset the image_name
                // if it hasn't already reset by the --image option
                if (strcmp(image_name, IMAGE) == 0)
                {
                    image_name = IMAGE_MOD;
                }
                // Add the .em extension to the module name provided
                char module_file_name[256];
                strcpy(module_file_name ,optarg);
                strcat(module_file_name, ".em");
                filein = euxcOSAOpen(module_file_name, "r");
                if (filein == NULL)
                {
                    fprintf
                    (
                        stderr,
                        "%s: can't open module file '%s'\n",
                        program_name,
                        module_file_name
                    );
                    print_usage(stderr, 6);
                }

                quiet = euxmTrue;   // no messages
                break;

            case 'i':   // -i file or --image file
                // Set the name of the image file to the argument
                image_name = optarg;
                break;

            case 't':   // -t or --trace
                trace = euxmTrue;
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
    if (euxmSetJmp(euxmStackTopLevel))
    {
        fprintf(stderr, "Error in initialisation\n");
        exit(2);
    }

    // initialize
    euxcOSInit(euxlBanner);

    // restore the default workspace, otherwise create a new one
    if (!image || !euxlRestoreImage(image_name))
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

        euxcInitWorkspace(5000);
    }

    // do the initialization code first
    euxlValue code = euxcEnterModule("*INITIALIZE*", euxcRootModule);
    code = (euxmBoundp(code) ? euxmGetValue(code) : euxmNil);

    // trap errors
    if (euxmSetJmp(euxmStackTopLevel))
    {
        code = euxcEnterModule("*toplevel*", euxcRootModule);
        code = (euxmBoundp(code) ? euxmGetValue(code) : euxmNil);
        xlfun = xlenv = xlval = euxmNil;
        euxcStackPtr = euxcStackTop;
    }

    // SIGINT
    struct sigaction eux_sa_int;
    eux_sa_int.sa_handler = eux_signal_handler_int;
    sigemptyset(&eux_sa_int.sa_mask);
    eux_sa_int.sa_flags = 0;
    if (sigaction(SIGINT, &eux_sa_int, NULL))
    {
        fprintf(stderr,"\nWarning: cannot install SIGINT signal handler\n");
    }

    // SIGQUIT
    struct sigaction eux_sa_quit;
    eux_sa_quit.sa_handler = eux_signal_handler_quit;
    sigemptyset(&eux_sa_quit.sa_mask);
    eux_sa_quit.sa_flags = 0;
    if (sigaction(SIGQUIT, &eux_sa_quit, NULL))
    {
        fprintf(stderr,"\nWarning: cannot install SIGQUIT signal handler\n");
    }

    #ifdef SOCK
    // SIGPIPE
    struct sigaction eux_sa_pipe;
    eux_sa_pipe.sa_handler = eux_signal_handler_pipe;
    sigemptyset(&eux_sa_pipe.sa_mask);
    eux_sa_pipe.sa_flags = 0;
    if (sigaction(SIGPIPE, &eux_sa_pipe, NULL))
    {
        fprintf(stderr,"\nWarning: cannot install SIGPIPE signal handler\n");
    }

    #endif

    // execute the main loop
    if (code != euxmNil)
    {
        euxcExecute(code);
    }

    euxcWrapup(1);
}

///  print_usage - helper function
static void print_usage(FILE* stream, int exit_code)
{
    fprintf(stream, "Usage: euxlisp [OPTION]... [FILE]...\n");
    fprintf
    (
        stream,
        "  -h  --help               Print this usage information.\n"
        "  -q  --quiet              Print no messages, prompts or values.\n"
        "  -n  --no-image           Do not read in the initial Lisp image.\n"
        "  -N  --no-sys-calls       Disable system calls.\n"
        "  -s  --script file        Read and execute script from file.\n"
        "  -m  --module module-name Read and execute module from module-name.em.\n"
        "  -i  --image file         Read the given image file rather than the default.\n"
        "  -t  --trace              Switch on byte-code level tracing.\n"
    );
    exit(exit_code);
}

///  euxcLoad -
void euxcLoad()
{
}

///  euxcContinue -
void euxcContinue()
{
}

///  euxcCleanup -
void euxcCleanup()
{
}

///  top_level - return to the euxmStackTop level
static void top_level(int cc)
{
    if (euxmGetValue(euxls_general_error) == euxls_unbound)
    {   // no conditions yet
        euxcStdPutString("[ back to top level ]\n");
        euxmLongJmp(euxmStackTopLevel, 1);
    }

    euxcDoError("user interrupt", euxmNil, euxls_user_intr, cc);
}

///  euxcToplevel - return to the euxmStackTop level
void euxcToplevel()
{
    top_level(euxmFalse);
}

///  euxcToplevelInt -
void euxcToplevelInt()
{
    top_level(euxmTrue);
}

///  euxcFail - report an error
void euxcFail(const char *msg, euxlValue err)
{
    euxcCerror(msg, euxls_unbound, err);
}

///  euxcError - report an error
void euxcError(const char *msg, euxlValue arg)
{
    // print the error message
    euxcErrorPutString("Error: ");
    euxcErrorPutString(msg);
    euxcErrorPutString("\n");

    // print the argument on a separate line
    if (arg != euxls_unbound)
    {
        euxcErrorPutString("  ");
        euxcErrorPrint(arg);
    }

    // print the function where the error occurred
    euxcErrorPutString("happened in: ");
    euxcErrorPrint(xlfun);

    // call the handler
    euxcCallErrorHandler();
}

///  euxcSetFrame -
void euxcSetFrame(int n)
{
    euxlValue *ptr = euxcStackPtr + n;
    while(!ieuxls_cont(ptr) && ptr < euxcStackTop)
    {
        ptr++;
    }

    if (ptr == euxcStackTop)
    {
        ptr = euxcStackPtr;
    }

    euxmSetValue
    (
        euxls_xlframe,
        euxmMakeSmallFPI((euxmFPIType) (euxcStackTop - ptr))
    );
}

///  euxcDoError - cc is euxmTrue if return address is needed,
//    e.g., in interpreter
void euxcDoError(const char *msg, euxlValue arg, euxlValue errname, int cc)
{
    #ifdef NOERROR
    euxcError(msg, arg);
    #endif

    euxlValue cond;
    if (errname == euxmNil)
    {
        cond = euxmGetValue(euxls_general_error);
    }
    else
    {
        cond = euxmGetValue(errname);
    }

    xlval = euxmGetValue(euxls_signal);

    if (cond == euxls_unbound || xlval == euxls_unbound)
    {
        // no conditions yet
        euxcErrorPutString("Run-Time ");
        euxcError(msg, arg);
    }
    else
    {
        euxcSetFrame(1);
        euxmSetIVar(cond, 1, euxcMakeString(msg));
        euxlValue condcl = euxmGetClass(cond);
        if (euxmGetSmallFPI(euxmGetIVar(condcl, euxmInstanceSizeId)) > 1)
        {
            euxmSetIVar(cond, 2, arg);
        }
        euxcOSCheck();
        euxlValue cont = euxcCurrentContinuation(cc);
        euxmStackCheck(2);
        euxmStackPush(cont);     // resume continuation
        euxmStackPush(cond);     // condition
        euxcArgC = 2;
        euxcApply();
        euxmLongJmp(bc_dispatch, 1);
    }
}

///  euxcIntError - report run-time error in the interpreter
void euxcIntError(const char *msg, euxlValue arg, euxlValue errname)
{
    euxcDoError(msg, arg, errname, euxmTrue);
}

///  euxcCerror - report run-time error in C code
void euxcCerror(const char *msg, euxlValue arg, euxlValue errname)
{
    // diseuxmCard everything until the continuation
    while (!ieuxls_cont(euxcStackPtr))
    {
        euxmStackDrop(1);
    }

    euxcDoError(msg, arg, errname, euxmFalse);
}

///  euxcCallErrorHandler - call the error handler
void euxcCallErrorHandler()
{
    if (euxmGetValue(euxls_backtracep) != euxmNil)
    {
        euxcDoBacktrace(euxcStackPtr);
    }

    // no handler, just reset back to the euxmStackTop level
    // this probably leaves the thread state in a state
    euxmLongJmp(euxmStackTopLevel, 1);
}

///  euxcAbort - print an error message and abort
void euxcAbort(const char *msg)
{
    // print the error message
    euxcErrorPutString("Abort: ");
    euxcErrorPutString(msg);
    euxcErrorPutString("\n");

    // print the function where the error occurred
    euxcErrorPutString("happened in: ");
    euxcErrorPrint(xlfun);

    // reset back to the euxmStackTop level
    euxcOSCheck();  // an opportunity to break out
    euxmLongJmp(euxmStackTopLevel, 1);
}

///  euxcFatal - print a fatal error message and exit
void euxcFatal(const char *msg)
{
    euxcOSError(msg);
    exit(4);
}

///  euxcWrapup - clean up and exit to the operating system
void euxcWrapup(int n)
{
    if (tfp)
    {
        euxcOSClose(tfp);
    }
    euxcOSFinish();
    exit(n);
}

#define euxcErrorPrin(x) euxcPrin1(x, errout)
#define errstr(x)  euxcPutString(errout, x)
static euxlValue errout;

///  indent -
static void indent(int n)
{
    while (n++ < 15)
    {
        errstr(" ");
    }
}

///  trace_function -
static void trace_function(euxlValue fun, euxlValue env)
{
    errout = euxmGetValue(euxls_stderr);

    if (euxmCodep(fun))
    {
        errstr("function ");
        if (euxmGetCName(fun) == euxmNil)
        {
            errstr("<anon>");
        }
        else
        {
            euxcErrorPrin(euxmGetCName(fun));
        }
        errstr(" ");
        euxcErrorPrin(euxmGetVNames(fun));
    }
    else if (euxmXFunContp(fun))
    {
        errstr("xfuncont ");
        errstr(euxmGetFunName(fun));
    }
    else
    {
        errstr("???");
    }
    euxcTerpri(errout);

    // Check to see if the trace has reached the euxmStackTop-level and return
    if (euxmGetCName(fun) ==  euxcEnterModule("*toplevel*", euxcRootModule))
    {
        return;
    }

    if (env != euxmNil)
    {
        if (euxmEnvp(env))
        {
            euxlValue frame = euxmCar(env);
            if (euxmVectorp(frame))
            {
                euxlValue vars = euxmGetElement(frame, 0);
                for (int i = 1; vars; vars = euxmCdr(vars), i++)
                {
                    euxcErrorPrin(euxmCar(vars));
                    errstr(": ");
                    indent(strlen(euxmGetString(euxmGetPName(euxmCar(vars)))));
                    euxcErrorPrin(euxmGetElement(frame, i));
                    euxcTerpri(errout);
                }
            }
            else
            {
                errstr("mangled frame: ");
                euxcErrorPrin(frame);
                euxcTerpri(errout);
            }
        }
        else
        {
            errstr("mangled environment list: ");
            euxcErrorPrin(env);
            euxcTerpri(errout);
        }
    }

    euxcTerpri(errout);
}

///  euxcDoBacktrace - print a backtrace
void euxcDoBacktrace(euxlValue * from)
{
    errout = euxmGetValue(euxls_stderr);

    errstr("\nStack backtrace:\n\n");

    for (euxlValue *sp = from; sp < euxcStackTop; sp++)
    {
        if (ieuxls_cont(sp))
        {
            trace_function(sp[1], sp[0]);
            sp += 2;
        }
    }
}

///  euxlBacktrace - lisp backtrace
euxlValue euxlBacktrace()
{
    static char *functionName = "backtrace";

    euxlValue *ptr;
    if (euxmMoreArgs())
    {
        euxlValue frameptr = euxmGetArgFPI();
        ptr = euxcStackTop - euxmGetFPI(frameptr);
        euxmLastArg();
    }
    else
    {
        ptr = euxcStackPtr;
    }

    euxcDoBacktrace(ptr);

    return euxl_true;
}

///  euxlFrameUp - debugging -- move up a frame
euxlValue euxlFrameUp()
{
    static char *functionName = "frame-up";

    euxlValue frameptr = euxmGetArgFPI();
    euxlValue arg = euxmGetArg();   // cc
    arg = euxmGetArg();        // condition
    int n;
    if (euxmMoreArgs())
    {
        arg = euxmGetArgFPI();
        euxmLastArg();
        n = euxmGetFPI(arg);
    }
    else
    {
        n = 1;
    }

    euxlValue *ptr = euxcStackTop - euxmGetFPI(frameptr);

    for (; n > 0; n--)
    {
        euxlValue *old = ptr;
        for (ptr++; ptr < euxcStackTop; ptr++)
        {
            if (ieuxls_cont(ptr))
            {
                break;
            }
        }

        if (ptr >= euxcStackTop)
        {
            errstr("(no more frames)\n");
            ptr = old;
            break;
        }
    }

    trace_function(ptr[1], ptr[0]);
    return euxcMakeFPI((euxmFPIType) (euxcStackTop - ptr));
}

///  euxlFrameDown - debugging -- move down a frame
euxlValue euxlFrameDown()
{
    static char *functionName = "frame-down";

    euxlValue frameptr = euxmGetArgFPI();
    euxlValue arg = euxmGetArg();   // cc
    arg = euxmGetArg();   // condition
    int n;
    if (euxmMoreArgs())
    {
        arg = euxmGetArgFPI();
        euxmLastArg();
        n = euxmGetFPI(arg);
    }
    else
    {
        n = 1;
    }

    euxlValue *ptr = euxcStackTop - euxmGetFPI(frameptr);

    for (; n > 0; n--)
    {
        euxlValue *old = ptr;
        for (ptr--; ptr >= euxcStackPtr; ptr--)
        {
            if (ieuxls_cont(ptr))
            {
                break;
            }
        }

        if (ptr < euxcStackPtr)
        {
            errstr("(no more frames)\n");
            ptr = old;
            break;
        }
    }

    trace_function(ptr[1], ptr[0]);
    return euxcMakeFPI((euxmFPIType) (euxcStackTop - ptr));
}

///  euxlFrameEnv - debugging -- get current env
euxlValue euxlFrameEnv()
{
    static char *functionName = "frame-env";

    euxlValue frameptr = euxmGetArgFPI();
    euxmLastArg();
    euxlValue *ptr = euxcStackTop - euxmGetFPI(frameptr);

    return *ptr;
}

///  euxlFrameFun - debugging -- get current fun
euxlValue euxlFrameFun()
{
    static char *functionName = "frame-fun";

    euxlValue frameptr = euxmGetArgFPI();
    euxmLastArg();
    euxlValue *ptr = euxcStackTop - euxmGetFPI(frameptr);

    return ptr[1];
}


///-----------------------------------------------------------------------------
