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
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: export of the bytecode interpreter for foreign-functions
///-----------------------------------------------------------------------------

#ifndef EULISP_H
#define EULISP_H

#include "stdc.h"
#include "config.h"
#include "signals.h"
#include "init.h"
#include "notify.h"
#include "shared-mem.h"
#include "callback.h"
#include "tag.h"
#include "object.h"
#include "bytevec.h"
#include "operator.h"
#include "util.h"
#include "fpi.h"
#include "double.h"
#include "character.h"
#include "list.h"
#include "register.h"
#include "stack.h"
#include "io.h"
#include "handler.h"
#include "cache.h"
#include "ff.h"
#include "eul-string.h"
#include "table.h"
#include "symbol.h"
#include "keyword.h"
#include "class.h"
#include "level-1i.h"
#include "../Runtime/Ff/eul-ext.h"
#include "../Runtime/Ff/eul-dbl.h"
#include "../Runtime/Ff/eul-sock.h"
#include "../Comptime2/Ff/eul-dld.h"

ptrInt interpret(RegisterRef set);

///-----------------------------------------------------------------------------
#endif // EULISP_H
///-----------------------------------------------------------------------------
