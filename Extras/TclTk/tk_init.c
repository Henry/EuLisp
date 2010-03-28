/**  By J Garcia & University of Bath. All rights reserved.                 **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: tcltk
 **  Authors: J Garcia
 **  Description: tk initializations and bindings
 ** ----------------------------------------------------------------------- **/

#include "globalvariables.h"
#include "tk.h"
#include "eulisp.h"

// Forward declaration of local functions
static void StructureProc(ClientData, XEvent *);
static int UpdateInfoEvent(ClientData, XEvent *);
static LispRef eul_callbacks;

int eul_interpret
(
    ClientData clientData,
    Tcl_Interp *interp,
    int argc,
    char *argv[]
)
{
    char *function_key = argv[1];
    LispRef entry = eul_fast_table_ref(eul_callbacks, function_key);
    LispRef fn = eul_car(entry);
    LispRef args = eul_cdr(entry);

    if (!eul_is_lambda(fn))
    {
        if (!eul_is_gf(fn))
        {
            printf("\n*** ERROR [system]: bad foreign function in-call\n");
            fprint_ref(stdout, fn);
            exit(-1);
        }
        else
        {
            fn = GF_DISC_FN(fn);
        }
    }

    INITIALISE_REGISTER_SET(tame_regs);
    tame_regs->reg_value_stack->sp = tame_regs->reg_value_stack->base;
    tame_regs->reg_context_stack->sp = tame_regs->reg_context_stack->base;
    tame_regs->reg_current_cv = (Instruction *)LAMBDA_CODE(fn);
    tame_regs->reg_pc = tame_regs->reg_current_cv;
    tame_regs->reg_env = LAMBDA_ENV(fn);
    tame_regs->reg_next_methods = eul_nil;

    LispRef loc;

    for (int i = 2; i < argc; i++)
    {
        eul_allocate_string(loc, argv[i]);
        EXTERNAL_PUSHVAL1(loc);
    };

    while (args != eul_nil)
    {
        argc++;
        EXTERNAL_PUSHVAL1(eul_car(args));
        args = eul_cdr(args);
    };
    tame_regs->reg_arg_count = argc - 2;

    interpret(tame_regs);

    // We're not interested in the result
    LispRef res;
    EXTERNAL_POPVAL1(res);

    return 0;
}


int Tcl_AppInit(Tcl_Interp *interp)
{
    if (Tcl_Init(interp) == TCL_ERROR)
    {
        return TCL_ERROR;
    }

    if (Tk_Init(interp) == TCL_ERROR)
    {
        return TCL_ERROR;
    }

    Tcl_StaticPackage(interp, "Tk", Tk_Init, (Tcl_PackageInitProc *) NULL);

    /*
     * Call the init procedures for included packages.  Each call should
     * look like this:
     *
     * if (Mod_Init(interp) == TCL_ERROR) {
     *     return TCL_ERROR;
     * }
     *
     * where "Mod" is the name of the module.
     */

    /*
     * Call Tcl_CreateCommand for application-specific commands, if
     * they weren't already created by the init procedures called above.
     */

    /*
     * Specify a user-specific startup file to invoke if the application
     * is run interactively.  Typically the startup file is "~/.apprc"
     * where "app" is the name of the application.  If this line is deleted
     * then no user-specific startup file will be run under any conditions.
     */

    Tcl_SetVar(interp, "tcl_rcFileName", "~/.wishrc", TCL_GLOBAL_ONLY);

    return TCL_OK;
}


LispRef eul_initialize_tk()
{
    interp = Tcl_CreateInterp();

    // Get argc and argv
    static int argc;
    argc = eul_int_as_c_int(eul_argc);

    static char **argv;
    argv = (char **)gc_malloc(argc*sizeof(LispRef));

    char buf[10];
    char args[40];

    for (int i = 0; i < argc; i++)
    {
        argv[i] = eul_string_as_c_string(slot_ref(eul_argv, i));
        if (strcmp(argv[i], "-display") == 0)
        {
            i++;
            if (i == argc)
            {
                fprintf(stdout, "Display information missing......\n");
                fflush(stdout);
                return eul_nil;
            }
            else
            {
                argv[i] = eul_string_as_c_string(slot_ref(eul_argv, i));
                sprintf(args, "%s %s", argv[i - 1], argv[i]);
                sprintf(buf, "2");
                Tcl_SetVar(interp, "argc", buf, TCL_GLOBAL_ONLY);
                Tcl_SetVar(interp, "argv", args, TCL_GLOBAL_ONLY);
            }
        }
    }

    static int tty;
    char *fileName = NULL;

    tty = isatty(0);
    Tcl_SetVar(interp, "tcl_interactive",
        ((fileName == NULL) && tty) ? "1" : "0", TCL_GLOBAL_ONLY);

    // Invoke application-specific initialization.

    if (Tcl_AppInit(interp) != TCL_OK)
    {
        fprintf(stderr, "Tcl_AppInit failed: %s\n", interp->result);
        return eul_nil;
    }
    else
    {

        Tcl_ResetResult(interp);

        mainWindow = Tk_MainWindow(interp);

        Tk_GeometryRequest(mainWindow, 200, 200);

        Tk_CreateEventHandler(mainWindow, StructureNotifyMask, StructureProc,
            (ClientData) NULL);

        Tk_3DBorder border = Tk_Get3DBorder(interp, mainWindow, "#cccccc");
        XSynchronize(Tk_Display(mainWindow), True);

        Tcl_CreateCommand(interp, "eul_interpret", eul_interpret,
            (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

        eul_allocate_table(eul_callbacks, eul_nil);
        return eul_true;
    }
}


/** ----------------------------------------------------------------------- **
 **  EuLisp callbacks
 **    Fist step: create tame_registers and store them in the hash table
 **    eul_callbacks.
 **    Second step: when callback envoked get tame_registers out of the
 **    hash table to feed interpret.
 ** ----------------------------------------------------------------------- **/

LispRef tk_allocate_registers(char *nameFunction, LispRef fn)
{
    eul_fast_table_set(eul_callbacks, nameFunction, fn);

    return fn;
}


/** ----------------------------------------------------------------------- **
 **  StructureProc --
 **
 **      This procedure is invoked whenever a structure-related event
 **      occurs on the main window.  If the window is deleted, the
 **      procedure modifies "w" to record that fact.
 **
 **  Results:
 **      None.
 *
 **  Side effects:
 **      Variable "w" may get set to NULL.
 ** ----------------------------------------------------------------------- **/

static void StructureProc(ClientData clientData, XEvent *eventPtr)
{
    if (eventPtr->type == DestroyNotify)
    {
        mainWindow = NULL;
    }
}


int Tcl_DoOneEventAux(int flag)
{
    if (flag == 0)
    {
        return Tcl_DoOneEvent(0);
    }
    else if (flag == 1)
    {
        return Tcl_DoOneEvent(TCL_DONT_WAIT);
    }
    else
    {
        return TCL_ERROR;
    }
}
