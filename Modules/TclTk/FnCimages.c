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
/// Title: FnC Images
///  Library: tcltk
///  Authors: J. Garcia
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "globalvariables.h"
#include "FnCwidgets.h"
#include "StrOperations.h"

LispRef eul_tk_image_cmd(char *nameImage, char *imgCmd, LispRef args)
{
    CreateFnInfo("image");

    struct infoargs infoArgs;
    ParseArguments2(&infoArgs, nameImage, imgCmd, args);

    Tcl_ResetResult(interp);

    int result = info.proc
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
        LispRef loc = eul_true;

        if
        (
            (strcmp(imgCmd, "create") == 0)
         || (strcmp(imgCmd, "height") == 0)
         || (strcmp(imgCmd, "type") == 0)
         || (strcmp(imgCmd, "width") == 0)
        )
        {
            char *string = gc_malloc(strlen(interp->result));
            strcpy(string, interp->result);
            eul_allocate_string(loc, string);
        }

        if ((strcmp(imgCmd, "names") == 0) || (strcmp(imgCmd, "types") == 0))
        {
            char *list = gc_malloc(strlen(interp->result));
            strcpy(list, interp->result);

            Tcl_ResetResult(interp);

            int argc;
            const char **argv;
            result = Tcl_SplitList(interp, list, &argc, &argv);

            if (result == TCL_OK)
            {
                loc = eul_nil;
                for (int i = (argc - 1); i >= 0; i--)
                {
                    LispRef list_element;
                    eul_allocate_string(list_element, (char *)argv[i]);
                    eul_allocate_cons(loc, list_element, loc);
                }
            }
        }

        return loc;
    }
}


LispRef eul_tk_image_photo_cmd(char *cmd)
{
    int result = Tcl_Eval(interp, cmd);

    if (result == TCL_ERROR)
    {
        return eul_nil;
    }
    else
    {
        return eul_true;
    }
}


///-----------------------------------------------------------------------------
