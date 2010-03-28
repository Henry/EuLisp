/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: eulvm (Bytecode Interpreter -- Eutopia)
 **  Authors: Keith Playford, Andreas Kind
 **  Description: export of the bytecode interpreter for foreign-functions
 ** ----------------------------------------------------------------------- **/

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
#include "level1i.h"
#include "../Runtime/Ff/eul-ext.h"
#include "../Runtime/Ff/eul-dbl.h"
#include "../Runtime/Ff/eul-sock.h"
#include "../Comptime2/Ff/eul-dld.h"

ptrInt interpret(RegisterRef set);

#endif // EULISP_H
