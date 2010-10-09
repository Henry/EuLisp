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
///  Title: Initialization of readline
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Henry G. Weller
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------

#ifdef READLINE
#include <stdlib.h>
#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>

#include "../../RLCompletion/eulisp_keywords.h"
#include "../../RLCompletion/youtoo_keywords.h"
#include "../../RLCompletion/keyword_completion.h"

// Name of the readline command history file
char* rl_histfile = NULL;

int eul_rl_initialize()
{
    rl_attempted_completion_function = keyword_completion;
    rl_bind_key('\t', rl_complete);

    const char* eulisp_history = "/.eulisp_history";
    char* home = getenv("HOME");

    if (home == NULL)
    {
        fprintf
        (
            stderr,
            "Cannot find environment variable HOME for reading ~%s\n",
            eulisp_history
        );

        return 0;
    }
    else
    {
        rl_histfile = malloc(strlen(home) + strlen(eulisp_history) + 1);
        strcpy(rl_histfile, home);
        strcat(rl_histfile, eulisp_history);

        if (!read_history(rl_histfile))
        {
            printf("Reading readline history from %s\n", rl_histfile);
            fflush(stdout);
        }

        return 1;
    }
}

#else
int eul_rl_initialize()
{
    return 0;
}
#endif


///-----------------------------------------------------------------------------
