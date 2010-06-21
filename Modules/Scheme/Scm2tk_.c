/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Description: C hook file of EuLisp module scm2tk
///-----------------------------------------------------------------------------

#include <eulisp.h>

// Initialize module scm2tk ...
extern void initialize_module_scm2tk();

extern LispRef scm2tk_bindings[];

// Run application scm2tk ...
void run_application()
{
    // Ignore signals until system is up (see also i-rep.em)
    eul_signal_enabled = 0;

    initialize_module_scm2tk();
    execute_lambda(scm2tk_bindings[0]);
}

///-----------------------------------------------------------------------------
