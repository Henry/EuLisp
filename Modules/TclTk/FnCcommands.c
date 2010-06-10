#include "globalvariables.h"
#include "FnCwidgets.h"
#include "StrOperations.h"
#include <assert.h>


LispRef eul_tk_pack(char *name, LispRef options)
{
    CreateFnInfo("pack");
    struct infoargs infoArgs;
    ParseArguments2(&infoArgs, command, name, options);

    int result = (info.proc)
    (
        info.clientData,
        interp,
        infoArgs.argc,
        infoArgs.argv
    );

    if (result == TCL_ERROR)
    {
        return eul_nil;
    }
    else
    {
        return eul_true;
    }
}


LispRef eul_tk_bind(char *nameWidget, char *event, char *fn_key, LispRef args)
{
    CreateFnInfo("bind");

    int argc = 4;
    const char **argv = (const char **)gc_malloc(argc*sizeof(char*));
    char tk_command[] = "eul_interpret ";
    argv[0] = command;
    argv[1] = nameWidget;
    argv[2] = event;

    // We need to store "eul_interpret fn" as a string.
    argv[3] = gc_malloc(maxScriptLen);
    strcpy((char*)argv[3], tk_command);
    strcat((char*)argv[3], fn_key);

    // The next piece of code is adding the possible arguments to the bind
    // function.
    while (args != eul_nil)
    {
        strcat((char*)argv[3], " ");
        strcat((char*)argv[3], eul_string_as_c_string(eul_car(args)));
        args = eul_cdr(args);
    }

    return eul_tk_result
    (
        (info.proc)
        (
            info.clientData,
            interp,
            argc,
            argv
        )
    );
}


LispRef eul_tk_destroy(char *nameWidget, LispRef moreObjects)
{
    CreateFnInfo("destroy");

    struct infoargs infoArgs;
    ParseArguments2(&infoArgs, command, nameWidget, moreObjects);

    int result = (info.proc)
    (
        info.clientData,
        interp,
        infoArgs.argc,
        infoArgs.argv
    );

    if (result == TCL_ERROR)
    {
        return eul_nil;
    }
    else
    {
        return eul_true;
    }
}


LispRef eul_tk_set_variable(char *nameVar, char *value)
{
    // If there is and error, 0 will be returned.
    const char *tcl_result = Tcl_SetVar(interp, nameVar, value, 0);

    if (tcl_result == NULL)
    {
        return eul_true;
    }
    else
    {
        return eul_nil;
    }
}


int eul_tk_get_variable(char *nameVar)
{
    const char *tcl_result = Tcl_GetVar(interp, nameVar, 0);

    if (tcl_result == NULL)
    {
        return 0;
    }
    else
    {
        return atoi(tcl_result);
    }
}


LispRef eul_tk_wm(char *operation, LispRef listArgs)
{
    CreateFnInfo("wm");

    struct infoargs infoArgs;
    ParseArguments2(&infoArgs, command, operation, listArgs);

    return eul_tk_result
    (
        (info.proc)
        (
            info.clientData,
            interp,
            infoArgs.argc,
            infoArgs.argv
        )
    );
}


LispRef eul_tk_selection_get(LispRef option)
{
    CreateFnInfo("selection");

    int argc = (option != eul_nil)? 3 : 2;
    const char **argv = (const char **)gc_malloc(argc*sizeof(char*));
    char command2[] = "get";
    argv[0] = command;
    argv[1] = command2;
    if (argc == 3)
    {
        argv[2] = eul_string_as_c_string(eul_car(option));
    }

    int result = (info.proc)
    (
        info.clientData,
        interp,
        argc,
        argv
    );

    if (result == TCL_ERROR)
    {
        return eul_nil;
    }
    else
    {
        LispRef loc;
        eul_intern_symbol(loc, interp->result);
        return loc;
    }
}


LispRef eul_tk_get_result()
{
    int length = strlen(interp->result);

    if (length == 0)
    {
        return eul_nil;
    }
    else
    {
        char *string = gc_malloc(length);
        strcpy(string, interp->result);
        LispRef loc;
        eul_allocate_string(loc, string);
        return loc;
    }
}


LispRef eul_tk_focus(char *name, char *  id)
{
    CreateFnInfo("focus");

    int argc;
    if (strcmp(name, "nameWindow") == 0)
    {
        argc = 1;
    }
    else
    {
        if (strlen(id) == 0)
        {
            argc = 2;
        }
        else
        {
            argc = 3;
        }
    }

    const char **argv = (const char **)gc_malloc(argc*sizeof(char*));
    argv[0] = command;

    if (argc >= 2)
    {
        argv[1] = name;
    }

    // We are allocating 8 byte for the identifier.

    if (argc == 3)
    {
        argv[2] = gc_malloc(8);
        argv[2] = id;
    }

    int result = (info.proc)
    (
        info.clientData,
        interp,
        argc,
        argv
    );

    if (result == TCL_ERROR)
    {
        return eul_nil;
    }
    else
    {
        if (argc == 1)
        {
            LispRef loc;
            eul_intern_symbol(loc, interp->result);
            return loc;
        }
        else
        {
            return eul_true;
        }
    }
}


LispRef eul_tk_map_widget(char *name)
{
    Tk_MapWindow(Tk_NameToWindow(interp, name, mainWindow));
    return eul_true;

}


LispRef eul_tk_unmap_widget(char *name)
{
    Tk_UnmapWindow(Tk_NameToWindow(interp, name, mainWindow));
    return eul_true;
}


LispRef eul_tk_grab_set(char *name)
{
    CreateFnInfo("grab");

    int argc = 3;
    const char **argv = (const char **)gc_malloc(argc*sizeof(char*));
    char str1[] = "set";
    argv[0] = command;
    argv[1] = str1;
    argv[2] = name;

    return eul_tk_result
    (
        (info.proc)
        (
            info.clientData,
            interp,
            argc,
            argv
        )
    );
}


LispRef eul_tk_bell()
{
    CreateFnInfo("bell");

    int argc = 1;
    const char **argv = (const char **)gc_malloc(sizeof(char*));
    argv[0] = command;

    return eul_tk_result
    (
        (info.proc)
        (
            info.clientData,
            interp,
            argc,
            argv
        )
    );
}
