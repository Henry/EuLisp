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
