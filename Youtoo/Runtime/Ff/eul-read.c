/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: level1
///  Authors: Julian Padget, Andreas Kind
///  Description: Low-level read with readline support
///-----------------------------------------------------------------------------
#include "eul-ext.h"

#ifdef READLINE
#include <readline/readline.h>
#include <readline/history.h>

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
    // Check if the rl_histfile is set to determine if readline is active
    // i.e. the REPL is being used
    if (rl_histfile && _file == 0)
    {
        // Print the prompt and get a line from the user.
        char* lbuf = readline(eul_prompt_string());

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
            status = strlen(lbuf);
            strcpy(buf + (n >> 1), lbuf);

            // Add the \n that readline removes from the input
            *(buf + (n >> 1) + status++) = '\n';

            // Re-terminate the string
            *(buf + (n >> 1) + status) = '\0';

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
