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
/// Title: FvC Widgets
///  Library: tcltk
///  Authors: J. Garcia
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "tk.h"
#include "eulisp.h"
#include <assert.h>

#define CreateFnInfo(commandName)                                              \
    static const char command[] = commandName;                                 \
    static Tcl_CmdInfo info = {0, NULL, 0, NULL, 0, NULL, 0, NULL};            \
    if (!info.proc)                                                            \
    {                                                                          \
        info = FindCreationFn(command);                                        \
        assert(info.proc);                                                     \
    }

static inline LispRef eul_tk_result(int result)
{
    if (result == TCL_ERROR)
    {
        return eul_nil;
    }
    else
    {
        return eul_true;
    }
}

extern Tcl_CmdInfo FindCreationFn(const char *name);

extern Tcl_CmdInfo *eul_tk_create_widget
(
    char *type,
    char *name,
    LispRef listArgs
);

extern LispRef eul_create_item_canvas
(
    char *type,
    char *name,
    Tcl_CmdInfo *cmdPtr,
    LispRef listArgs
);

extern LispRef eul_tk_conf_widget
(
    char *name,
    Tcl_CmdInfo *cmdPtr,
    LispRef listArgs
);

extern LispRef eul_tk_cmd_item_canvas
(
    char *nameWidget,
    Tcl_CmdInfo *cmdPtr,
    char *aux,
    LispRef args,
    char *nameCommand
);

extern LispRef eul_add_menu_command
(
    char *nameMenu,
    Tcl_CmdInfo *cmdPtr,
    LispRef listArgs
);

extern LispRef eul_insert_command
(
    char *nameWidget,
    Tcl_CmdInfo *cmdPtr,
    char *type,
    char *text
);

extern LispRef eul_delete_command
(
    char *nameWidget,
    Tcl_CmdInfo *cmdPtr,
    char *first,
    LispRef last
);

extern LispRef eul_view_cmd
(
    char *nameWidget,
    Tcl_CmdInfo *cmdPtr,
    char *type,
    char *entry,
    LispRef units,
    char *xcmd
);

extern LispRef eul_set_scrollbar_command
(
    char *nameWidget,
    Tcl_CmdInfo *cmdPtr,
    char *first_entry,
    char *last_entry
);

extern LispRef eul_tk_get_value_widget
(
    char *name,
    Tcl_CmdInfo *cmdPtr,
    LispRef indexs
);

extern LispRef eul_tk_set_value_widget
(
    char *name,
    Tcl_CmdInfo *cmdPtr,
    char *index
);

extern LispRef eul_tk_cmd_text
(
    char *name,
    Tcl_CmdInfo *cmdPtr,
    char *command,
    LispRef args
);

extern LispRef eul_tk_bind_element
(
    char *nameWidget,
    Tcl_CmdInfo *cmdPtr,
    char *type,
    char *id_or_tag,
    char *event,
    char *fn_key,
    LispRef args
);

extern LispRef eul_tk_button_flash
(
    char *name,
    Tcl_CmdInfo *cmdPtr
);

extern LispRef eul_tk_listbox_cmd
(
    char *name,
    Tcl_CmdInfo *cmdPtr,
    char *command
);

///-----------------------------------------------------------------------------
