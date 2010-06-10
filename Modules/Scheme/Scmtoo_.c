/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Description: C hook file of EuLisp module scmtoo
///-----------------------------------------------------------------------------

#include "eulisp.h"

// Initialize module scmtoo ...
extern void initialize_module_scmtoo();

extern LispRef scmtoo_bindings[];

// Run application scmtoo ...
void run_application()
{
    // Ignore signals until system is up (see also i-rep.em)
    eul_signal_enabled = 0;

    // Fast prompt (for psychological reasons) ...
    if (eul_int_as_c_int(eul_argc) == 1)
    {
        printf("EuLisp/Scheme System 'youtoo %s'\n\n", getenv("EUL_VERSION"));
        fflush(stdout);
    }
    initialize_module_scmtoo();
    execute_lambda(scmtoo_bindings[0]);
}

///-----------------------------------------------------------------------------
