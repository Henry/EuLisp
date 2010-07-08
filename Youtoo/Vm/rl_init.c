///-----------------------------------------------------------------------------
/// ---               EuLisp Systems 'Youtoo' and 'EuXLisp'
///-----------------------------------------------------------------------------
///  Description: initialization of readline
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
