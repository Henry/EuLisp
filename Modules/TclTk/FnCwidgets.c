/// Copyright 1997 J. Garcia & University of Bath
/// Copyright 2010 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                         EuLisp System 'Youtoo'
///-----------------------------------------------------------------------------
//
//  Youtoo is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
///-----------------------------------------------------------------------------
/// Title: FnC Widgets
///  Library: tcltk
///  Authors: J. Garcia
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "globalvariables.h"
#include "FnCwidgets.h"
#include "StrOperations.h"

// Find and return the requested function details.
// TclTk since version 8.3 make you jump through hoops
// to get access to the primitive functions.
Tcl_CmdInfo FindCreationFn(const char *type)
{
    Tcl_CmdInfo cmdInfo;
    int result = Tcl_GetCommandInfo
    (
        interp,
        type,
        &cmdInfo
    );

    assert(result);

    return cmdInfo;
}


Tcl_CmdInfo *eul_tk_create_widget(char *type, char *name, LispRef listArgs)
{
    struct infoargs infoArgs;
    ParseArguments2(&infoArgs, type, name, listArgs);

    Tcl_CmdInfo cmdInfo = FindCreationFn(type);

    int result = cmdInfo.proc
    (
        cmdInfo.clientData,
        interp,
        infoArgs.argc,
        infoArgs.argv
    );

    Tcl_CmdInfo *newCmdInfo = (Tcl_CmdInfo *)gc_malloc(sizeof(Tcl_CmdInfo));
    *newCmdInfo = (Tcl_CmdInfo){0, NULL, 0, NULL, 0, NULL, 0, NULL};

    // It isn't clear what should be returned on error so return an empty
    // structure allocated on free-store
    if (result == TCL_ERROR)
    {
        return newCmdInfo;
    }

    result = Tcl_GetCommandInfo
    (
        interp,
        Tcl_GetString(Tcl_GetObjResult(interp)),
        newCmdInfo
    );

    return newCmdInfo;
}


LispRef eul_create_item_canvas
(
    char *type,
    char *name,
    Tcl_CmdInfo *cmdPtr,
    LispRef listArgs
)
{
    static const char command[] = "create";
    struct infoargs infoArgs;
    ParseArguments3(&infoArgs, name, command, type, listArgs);

    Tcl_ResetResult(interp);

    int result = (cmdPtr->proc)
    (
        cmdPtr->clientData,
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
        char *string = gc_malloc(strlen(interp->result));
        strcpy(string, interp->result);
        LispRef loc;
        eul_allocate_string(loc, string);
        return loc;
    }
}


LispRef eul_tk_conf_widget(char *name, Tcl_CmdInfo * cmdPtr, LispRef listArgs)
{
    // Attention to the change of the order in the arguments, name and command.
    static const char command[] = "configure";
    struct infoargs infoArgs;
    ParseArguments2(&infoArgs, name, command, listArgs);

    int result = (cmdPtr->proc)
    (
        cmdPtr->clientData,
        interp,
        infoArgs.argc,
        infoArgs.argv
    );

    return eul_tk_result(result);
}


LispRef eul_tk_cmd_item_canvas
(
    char *nameWidget,
    Tcl_CmdInfo *cmdPtr,
    char *aux,
    LispRef args,
    char *nameCommand
)
{
    // aux can be an id, a tag or another kind of argument. In find command
    // aux coulb be: above, all, below, enclosed, overlapping, withtag

    Tcl_ResetResult(interp);

    struct infoargs infoArgs;
    ParseArguments3(&infoArgs, nameWidget, nameCommand, aux, args);

    Tcl_ResetResult(interp);

    int result = (cmdPtr->proc)
    (
        cmdPtr->clientData,
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
        if (((args == eul_nil) && (strcmp(nameCommand, "coords") == 0)) ||
            (strcmp(nameCommand, "find") == 0))
        {
            char *list = gc_malloc(strlen(interp->result));
            strcpy(list, interp->result);
            Tcl_ResetResult(interp);

            int argc;
            char **argv;
            result = Tcl_SplitList(interp, list, &argc, (const char***)&argv);

            if (result == TCL_OK)
            {
                for (int i = (argc - 1); i >= 0; i--)
                {
                    LispRef list_element;
                    LispRef list_result = eul_nil;

                    eul_allocate_string(list_element, argv[i]);
                    eul_allocate_cons(list_result, list_element, list_result);
                }

                return eul_true;
            }
            else
            {
                return eul_nil;
            }
        }
        else
        {
            return eul_true;
        }
    }
}


LispRef eul_add_menu_command
(
    char *nameMenu,
    Tcl_CmdInfo *cmdPtr,
    LispRef listArgs
)
{
    static const char command[] = "add";
    struct infoargs infoArgs;
    ParseArguments2(&infoArgs, nameMenu, command, listArgs);

    int result = (cmdPtr->proc)
    (
        cmdPtr->clientData,
        interp,
        infoArgs.argc,
        infoArgs.argv
    );

    return eul_tk_result(result);
}


LispRef eul_insert_command
(
    char *nameWidget,
    Tcl_CmdInfo *cmdPtr,
    char *type,
    char *text
)
{
    static const char command[] = "insert";

    // Attention to the change of the order in the arguments, name and command

    int argc = 4;
    const char **argv = (const char **)gc_malloc(argc*sizeof(char*));
    argv[0] = nameWidget;
    argv[1] = command;
    argv[2] = type;
    argv[3] = text;

    int result = (cmdPtr->proc)
    (
        cmdPtr->clientData,
        interp,
        argc,
        argv
    );

    return eul_tk_result(result);
}


LispRef eul_delete_command
(
    char *nameWidget,
    Tcl_CmdInfo *cmdPtr,
    char *first,
    LispRef last
)
{
    static const char command[] = "delete";

    // Attention to the change of the order in the arguments, name and command

    int argc = (last != eul_nil) ? 4 : 3;
    const char **argv = (const char **)gc_malloc(argc*sizeof(char*));
    argv[0] = nameWidget;
    argv[1] = command;
    argv[2] = first;
    if (argc == 4)
    {
        argv[3] = eul_string_as_c_string(eul_car(last));
    }

    int result = (cmdPtr->proc)
    (
        cmdPtr->clientData,
        interp,
        argc,
        argv
    );

    return eul_tk_result(result);
}


LispRef eul_view_cmd
(
    char *nameWidget,
    Tcl_CmdInfo *cmdPtr,
    char *type,
    char *entry,
    LispRef units,
    char *xcmd
)
{
    int argc = (units != eul_nil) ? 5 : 4;
    const char **argv = (const char **)gc_malloc(argc*sizeof(char*));
    argv[0] = nameWidget;
    argv[1] = xcmd;
    argv[2] = type;
    argv[3] = entry;
    if (argc == 5)
    {
        argv[4] = eul_string_as_c_string(eul_car(units));
    }

    int result = (cmdPtr->proc)
    (
        cmdPtr->clientData,
        interp,
        argc,
        argv
    );

    return eul_tk_result(result);
}


LispRef eul_set_scrollbar_command
(
    char *nameWidget,
    Tcl_CmdInfo *cmdPtr,
    char *first_entry,
    char *last_entry
)
{
    static const char command[] = "set";

    // Attention to the change of the order in the arguments, name and command

    int argc = 4;
    const char **argv = (const char **)gc_malloc(argc*sizeof(char*));
    argv[0] = nameWidget;
    argv[1] = command;
    argv[2] = first_entry;
    argv[3] = last_entry;

    int result = (cmdPtr->proc)
    (
        cmdPtr->clientData,
        interp,
        argc,
        argv
    );

    return eul_tk_result(result);
}


LispRef eul_tk_get_value_widget
(
    char *name,
    Tcl_CmdInfo *cmdPtr,
    LispRef indexs
)
{
    static const char command[] = "get";
    struct infoargs infoArgs;
    ParseArguments2(&infoArgs, name, command, indexs);

    Tcl_ResetResult(interp);

    int result = (cmdPtr->proc)
    (
        cmdPtr->clientData,
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
        char *string = gc_malloc(strlen(interp->result));
        strcpy(string, interp->result);

        LispRef loc;
        eul_allocate_string(loc, string);
        return loc;
    }
}


LispRef eul_tk_set_value_widget
(
    char *name,
    Tcl_CmdInfo *cmdPtr,
    char *index
)
{
    static const char command[] = "set";
    struct infoargs infoArgs;
    ParseArguments3(&infoArgs, name, command, index, eul_nil);

    Tcl_ResetResult(interp);

    int result = (cmdPtr->proc)
    (
        cmdPtr->clientData,
        interp,
        infoArgs.argc,
        infoArgs.argv
    );

    return eul_tk_result(result);
}


LispRef eul_tk_cmd_text
(
    char *name,
    Tcl_CmdInfo *cmdPtr,
    char *command,
    LispRef args
)
{
    struct infoargs infoArgs;
    ParseArguments2(&infoArgs, name, command, args);

    Tcl_ResetResult(interp);

    int result = (cmdPtr->proc)
    (
        cmdPtr->clientData,
        interp,
        infoArgs.argc,
        infoArgs.argv
    );

    if (result == TCL_OK)
    {
        if (strcmp(command, "index") == 0)
        {
            LispRef loc = eul_true;;
            eul_allocate_string(loc, interp->result);
            return loc;
        }
        else
        {
            return eul_nil;
        }
    }
    else
    {
        return eul_nil;
    }
}


LispRef eul_tk_bind_element
(
    char *nameWidget,
    Tcl_CmdInfo *cmdPtr,
    char *type,
    char *id_or_tag,
    char *event,
    char *fn_key,
    LispRef args
)
{
    static const char command[] = "bind";
    static const char tk_command[] = "eul_interpret ";

    int argc = (strcmp(type, "tag")) ? 6 : 5;
    const char **argv = (const char **)gc_malloc(argc*sizeof(char*));

    int i = 0;
    argv[i++] = nameWidget;
    if (argc == 6)
    {
        argv[i++] = type;
    }
    argv[i++] = command;
    argv[i++] = id_or_tag;
    argv[i++] = event;
    argv[i] = gc_malloc(maxScriptLen);
    strcpy((char*)argv[i], tk_command);
    strcat((char*)argv[i], fn_key);

    // The next piece of code is adding the possible arguments to the bind
    // function.
    while (args != eul_nil)
    {
        strcat((char*)argv[i], " ");
        strcat((char*)argv[i], eul_string_as_c_string(eul_car(args)));
        args = eul_cdr(args);
    }

    int result = (cmdPtr->proc)
    (
        cmdPtr->clientData,
        interp,
        argc,
        argv
    );

    return eul_tk_result(result);
}


LispRef eul_tk_button_flash(char *name, Tcl_CmdInfo * cmdPtr)
{
    static const char command[] = "flash";
    struct infoargs infoArgs;
    ParseArguments2(&infoArgs, name, command, eul_nil);

    int result = (cmdPtr->proc)
    (
        cmdPtr->clientData,
        interp,
        infoArgs.argc,
        infoArgs.argv
    );

    return eul_tk_result(result);
}


LispRef eul_tk_listbox_cmd(char *name, Tcl_CmdInfo * cmdPtr, char *command)
{
    command = "curselection";

    int argc = 2;
    const char **argv;
    argv = (const char **)gc_malloc(argc*sizeof(char*));
    argv[0] = name;
    argv[1] = command;

    Tcl_ResetResult(interp);
    int result = (cmdPtr->proc)
    (
        cmdPtr->clientData,
        interp,
        argc,
        argv
    );

    if (result == TCL_OK)
    {
        char *list = gc_malloc(strlen(interp->result));
        strcpy(list, interp->result);
        Tcl_ResetResult(interp);

        int argc1;
        const char **argv1;
        int result = Tcl_SplitList(interp, list, &argc1, &argv1);

        if (result == TCL_OK)
        {
            LispRef list_element, list_result = eul_nil;

            for (int i = (argc1 - 1); i >= 0; i--)
            {
                eul_allocate_string(list_element, (char*)argv1[i]);
                eul_allocate_cons(list_result, list_element, list_result);
            }

            return list_result;
        }
        else
        {
            return eul_nil;
        }
    }
    else
    {
        return eul_nil;
    }
}


///-----------------------------------------------------------------------------
