/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: eulvm (Bytecode Interpreter -- Eutopia)
 **  Authors: Keith Playford, Andreas Kind
 **  Description: characters
 ** ----------------------------------------------------------------------- **/

#ifndef CHARACTER_H
#define CHARACTER_H

/** ----------------------------------------------------------------- **
 ** Initialization
 ** ----------------------------------------------------------------- **/

extern void eul_initialize_char();


/** ----------------------------------------------------------------- **
 ** Character access
 ** ----------------------------------------------------------------- **/

#define char_value(ref) (((ptrInt)ref)>>TAG_BITS)
#define eul_char_as_c_char(x) char_value(x)
#define c_char_as_eul_char(x) ((LispRef) ((((ptrInt)(x))<<TAG_BITS)|CHAR_TAG))


/** ----------------------------------------------------------------- **
 ** Character allocation
 ** ----------------------------------------------------------------- **/

#define eul_allocate_char(loc, x) (loc) = c_char_as_eul_char(x)


#endif // CHARACTER_H
