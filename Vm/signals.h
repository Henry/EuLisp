/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: eulvm (Bytecode Interpreter -- Eutopia)
 **  Authors: Keith Playford, Andreas Kind
 **  Description: signals
 ** ----------------------------------------------------------------------- **/

#ifndef EUL_SIGNALS_H
#define EUL_SIGNALS_H

#include "config.h"

extern void eul_initialize_signal();
extern int eul_signal_enable(int flag);
extern int eul_signal_cb;
extern int eul_signal;
extern int eul_signal_enabled;

/** ----------------------------------------------------------------- **
 ** With signals
 ** ----------------------------------------------------------------- **/

#ifdef WITH_SIGNALS

#define eul_no_signal() ((eul_signal==0)||(eul_signal_enabled==0))
#define eul_clear_signal() (eul_signal=0)
#define eul_set_signal() (eul_signal=1)

/** ----------------------------------------------------------------- **
 ** Without signals
 ** ----------------------------------------------------------------- **/

#else

#define eul_no_signal() (1)
#define eul_clear_signal()
#define eul_set_signal()

#endif // WITH_SIGNALS


#endif // EUL_SIGNALS_H
