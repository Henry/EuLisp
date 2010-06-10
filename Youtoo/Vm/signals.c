//  Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.

///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: signals
///-----------------------------------------------------------------------------

#include "stdc.h"
#include "config.h"
#include "notify.h"
#include "shared-mem.h"
#include "tag.h"
#include "object.h"
#include "callback.h"
#include <signal.h>

int eul_signal_cb = 0;
int eul_signal = 0;
int eul_signal_enabled = 0;


///-----------------------------------------------------------------------------
/// With signals ...
///-----------------------------------------------------------------------------

#ifdef WITH_SIGNALS

///-----------------------------------------------------------------------------
/// Signal handlers
///-----------------------------------------------------------------------------

static void eul_signal_handler_int(int signo)
{
    if (eul_signal_enabled == 0)
    {
        exit(signo);
    }

    eul_signal = signo;
    eul_signal_cb = CB_FIRST_SIGNAL + 0;
}


static void eul_signal_handler_bus(int signo)
{
    if (eul_signal_enabled == 0)
    {
        exit(signo);
    }

    WARNING0("bus error");
    eul_signal_cb = CB_FIRST_SIGNAL + 1;
}


static void eul_signal_handler_segv(int signo)
{
    if (eul_signal_enabled == 0)
    {
        exit(signo);
    }

    WARNING0("segmentation fault");
    eul_signal_cb = CB_FIRST_SIGNAL + 2;
}


///-----------------------------------------------------------------------------
/// Initialize signals
///-----------------------------------------------------------------------------

void eul_initialize_signal()
{
    struct sigaction eul_sa_int, eul_sa_bus, eul_sa_segv; ;

    // Interupt
    eul_sa_int.sa_handler = eul_signal_handler_int;
    sigemptyset(&eul_sa_int.sa_mask);
    eul_sa_int.sa_flags = 0;

    if (sigaction(SIGINT, &eul_sa_int, NULL))
    {
        WARNING0("cannot install signal handler");
    }

    #ifndef WITH_PCR_THREADS
    // Bus error
    eul_sa_bus.sa_handler = eul_signal_handler_bus;
    sigemptyset(&eul_sa_bus.sa_mask);
    eul_sa_bus.sa_flags = 0;

    if (sigaction(SIGBUS, &eul_sa_bus, NULL))
    {
        WARNING0("cannot install signal handler");
    }
    #endif // WITH_PCR_THREADS

    // Segmentation fault
    eul_sa_segv.sa_handler = eul_signal_handler_segv;
    sigemptyset(&eul_sa_segv.sa_mask);
    eul_sa_segv.sa_flags = 0;

    if (sigaction(SIGSEGV, &eul_sa_segv, NULL))
    {
        WARNING0("cannot install signal handler");
    }

    eul_signal = 0;
    eul_signal_enabled = 1;
}


int eul_signal_enable(int flag)
{
    int res = eul_signal_enabled;
    eul_signal_enabled = flag;

    return res;
}


///-----------------------------------------------------------------------------
/// Without signals ...
///-----------------------------------------------------------------------------

#else

void eul_initialize_signal()
{
}

int eul_signal_enable(int flag)
{
    return 0;
}

#endif // WITH_SIGNALS
