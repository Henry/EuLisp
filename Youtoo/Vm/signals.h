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
///  Title: signals
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------

#ifndef EUL_SIGNALS_H
#define EUL_SIGNALS_H

#include "config.h"

extern void eul_initialize_signal();
extern int eul_signal_enable(int flag);
extern int eul_signal_cb;
extern int eul_signal;
extern int eul_signal_enabled;

///-----------------------------------------------------------------------------
/// With signals
///-----------------------------------------------------------------------------

#ifdef WITH_SIGNALS

#define eul_no_signal() ((eul_signal==0)||(eul_signal_enabled==0))
#define eul_clear_signal() (eul_signal=0)
#define eul_set_signal() (eul_signal=1)

///-----------------------------------------------------------------------------
/// Without signals
///-----------------------------------------------------------------------------

#else

#define eul_no_signal() (1)
#define eul_clear_signal()
#define eul_set_signal()

#endif // WITH_SIGNALS

///-----------------------------------------------------------------------------
#endif // EUL_SIGNALS_H
///-----------------------------------------------------------------------------
