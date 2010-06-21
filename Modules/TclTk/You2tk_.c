/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.

///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Description: C hook file of EuLisp module you2tk
///-----------------------------------------------------------------------------

#include "eulisp.h"

// Initialize module you2tk ...
extern void initialize_module_you2tk();

extern LispRef you2tk_bindings[];

// Run application you2tk ...
void run_application()
{
    // Ignore signals until system is up (see also i-rep.em)
    eul_signal_enabled = 0;

    initialize_module_you2tk();
    execute_lambda(you2tk_bindings[0]);
}
