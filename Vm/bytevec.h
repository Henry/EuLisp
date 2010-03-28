/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: eulvm (Bytecode Interpreter -- Eutopia)
 **  Authors: Keith Playford, Andreas Kind
 **  Description: bytevectors (i.e. code vectors and strings)
 ** ----------------------------------------------------------------------- **/

#ifndef BYTEVEC_H
#define BYTEVEC_H

/** ----------------------------------------------------------------- **
 ** Bytevector access
 ** ----------------------------------------------------------------- **/

#define BYTEVECTOR_DATA(x) (slot_ref((x), 0))

#define bytevector_ref(bv, i) (*(((Instruction *) BYTEVECTOR_DATA(bv))+i))
#define bytevector_set(bv, i, x) bytevector_ref(bv, i) = (Instruction) x
#define bytevector_size(bv) object_size(bv)

#if ptrNBytes==4
#  if BYTE_ORDER == LITTLE_ENDIAN
#    define I(x1, x2, x3, x4)                                                  \
    (void *)0x##x4##x3##x2##x1
#  else
#    define I(x1, x2, x3, x4)                                                  \
    (void *)0x##x1##x2##x3##x4
#  endif
#elif ptrNBytes==8
#  if BYTE_ORDER == LITTLE_ENDIAN
#    define I(x1, x2, x3, x4)                                                  \
    (void *)0x##00##x4##00##x3##00##x2##00##x1
#  else
#    define I(x1, x2, x3, x4)                                                  \
    (void *)0x##x1##00##x2##00##x3##00##x4##00
#  endif
#else
#  error ptrNBytes set incorrectly
#endif /* ptrNBytes */


#define B(module, index)                                                       \
    (void *)&(module##_bindings[index])

#define BB(module, index)                                                      \
    (void *)&(module##_bindings[module##_##index##_index])


/** ----------------------------------------------------------------- **
 ** Bytevector allocation
 ** ----------------------------------------------------------------- **/

#define eul_allocate_bytevector(loc, data)                                     \
    eul_allocate_bytevector1(loc, data, sizeof(data));

#define eul_allocate_bytevector1(loc, data, size)                              \
    ALLOCATE_WARM_OBJECT(loc, PGLOBAL(glob_bytevector_class), 1);              \
    bytevector_size(loc) = c_int_as_eul_int(size);                             \
                         BYTEVECTOR_DATA(loc) = (LispRef) data

#define eul_allocate_empty_bytevector(loc, size)                               \
    ALLOCATE_WARM_OBJECT(loc, PGLOBAL(glob_bytevector_class), 1);              \
    bytevector_size(loc) = c_int_as_eul_int(size);                             \
                         BYTEVECTOR_DATA(loc) = (LispRef) gc_malloc(size);


#endif // BYTEVEC_H
