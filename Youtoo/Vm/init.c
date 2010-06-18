//  Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.

///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: initialization of the bytecode interpreter
///-----------------------------------------------------------------------------

#include "stdc.h"
#include "config.h"
#include "init.h"
#include "notify.h"
#include "shared-mem.h"
#include "tag.h"
#include "io.h"
#include "object.h"
#include "bytevec.h"
#include "fpi.h"
#include "list.h"
#include "character.h"
#include "eul-string.h"
#include "register.h"
#include "signals.h"
#include "util.h"
#include "class.h"

extern long eul_profiling_table[];

///-----------------------------------------------------------------------------
/// Initialise the system
///-----------------------------------------------------------------------------

int EUL_INITIALIZE(int argc, char **argv)
{
    if (eul_initialize_runtime_system() != 1)
    {
        return 0;
    }

    eul_allocate_int(eul_argc, argc);
    eul_allocate_string_vector(eul_argv, argc, argv);

    return 1;
}


int eul_initialize_runtime_system()
{
    NOTIFY0("Initialize runtime system ...");

    if (gc_expand_hp(DEFAULT_HEAP) != 1)
    {
        return 0;
    }

    eul_initialize_register();
    eul_initialize_signal();
    eul_initialize_util();
    eul_initialize_fpi();

    #ifdef WITH_CONS_TAG
    eul_initialize_cons();
    #endif // WITH_CONS_TAG

    eul_initialize_char();

    #ifdef INSTRUMENTED
    for (int i = 0; i < HIGHEST_BYTE_CODE + 1; ++i)
    {
        eul_profiling_table[i] = 0;
    }
    #endif // INSTRUMENTED

    return 1;
}


#ifdef READLINE
#include <readline/readline.h>
#include <readline/history.h>

char* rl_histfile = NULL;

int eul_rl_initialize()
{
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
    return 1;
}
#endif


///-----------------------------------------------------------------------------
