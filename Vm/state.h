/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: eulvm (Bytecode Interpreter -- Eutopia)
 **  Authors: Keith Playford, Andreas Kind
 **  Description: state used for call/cc
 ** ----------------------------------------------------------------------- **/

#ifndef STATE_H
#define STATE_H


#define STATE_VALUE_STACK(x)        (slot_ref(x, 0))
#define STATE_VALUE_STACK_SIZE(x)   (slot_ref(x, 1))
#define STATE_CONTEXT_STACK(x)      (slot_ref(x, 2))
#define STATE_CONTEXT_STACK_SIZE(x) (slot_ref(x, 3))


#endif // STATE_H
