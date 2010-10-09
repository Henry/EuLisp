/// Copyright 1997 A. Kind & University of Bath
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
///  Title: Low-level read with readline support
///  Library: level1
///  Authors: Julian Padget, Andreas Kind
///  Maintainer: Henry G. Weller
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
