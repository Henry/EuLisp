/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: eulvm (Bytecode Interpreter -- Eutopia)
 **  Authors: Andreas Kind
 **  Description: fast access to level1 bindings
 ** ----------------------------------------------------------------------- **/

#ifndef LEVEL1I_H
#define LEVEL1I_H

extern LispRef eul_initialize_level1_tables();

extern LispRef eul_dyn_level1_binding_ref (LispRef, LispRef);

extern LispRef eul_dyn_level1_binding_info (LispRef);

extern LispRef eul_dyn_level1_syntax_binding_info (LispRef);

#endif // LEVEL1I_H
