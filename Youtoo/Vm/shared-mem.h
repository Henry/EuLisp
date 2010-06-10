/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: global variables
///-----------------------------------------------------------------------------

#ifndef SHARED_MEM_H
#define SHARED_MEM_H

///-----------------------------------------------------------------------------
/// Global variable access and declaration
///-----------------------------------------------------------------------------

#define DEFINE_PGLOBAL(loc) LispRef loc
#define PGLOBAL_EXTERN(loc) extern LispRef loc
#define PGLOBAL_EXTERN_CLASS(loc) extern LispRef loc[]
#define PGLOBAL(loc) loc

///-----------------------------------------------------------------------------
#endif // SHARED_MEM_H
///-----------------------------------------------------------------------------
