/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: level1, eval, youtoo
///  Authors: Julian Padget, Andreas Kind
///  Description: Low-level read
///-----------------------------------------------------------------------------

#include "eul-ext.h"

#ifdef READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

#ifdef READLINE
extern char *rl_histfile;
#endif

int read_into_buffer(int _file, char *buf, int n)
{
    /*
    fprintf
    (
        stderr,
        "About to read with %d, %" ptrIntPM "x, %d\n",
        _file, (ptrInt)(buf+(n>>1)), n>>1
    );
    */

    static int status;

    #ifdef READLINE
    if (_file == 0)
    {
        // Get a line from the user.
        char* lbuf = readline("");

        // If the line has any text in it, save it on the history.
        if (lbuf)
        {
            if (*lbuf)
            {
                add_history(lbuf);
                if (rl_histfile)
                {
                    write_history(rl_histfile);
                }
            }

            // Transfer the readline buffer to buf
            status = strlen(lbuf) + 1;
            strcpy(buf + (n >> 1), lbuf);

            // Free the readline buffer allocated with malloc
            free(lbuf);
        }
        else
        {
            if (status == -1)
            {
                perror("\n*** WARNING [read]");
                fflush(stderr);
            }
        }
    }
    else
    #endif
    {
        status = read(_file, buf + (n >> 1), n >> 1);

        if (status == -1)
        {
            perror("\n*** WARNING [read]");
            fflush(stderr);
        }
    }

    return status;
}

///-----------------------------------------------------------------------------
