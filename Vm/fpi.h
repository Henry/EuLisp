/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: eulvm (Bytecode Interpreter -- Eutopia)
 **  Authors: Keith Playford, Andreas Kind
 **  Description: fixed precision integers
 ** ----------------------------------------------------------------------- **/

#ifndef FPI_H
#define FPI_H

/** ----------------------------------------------------------------- **
 ** Initialization
 ** ----------------------------------------------------------------- **/

extern void eul_initialize_fpi();

/** ----------------------------------------------------------------- **
 ** Fpi allocation
 ** ----------------------------------------------------------------- **/

#define fpi_value(x) (((ptrInt) x) >> TAG_BITS)
#define eul_int_as_c_int(x) fpi_value(x)
#define eul_bool_as_c_bool(x) (eul_null(x) ? false : true)
#define c_int_as_eul_int(x) ((LispRef) ((((ptrInt)(x)) << TAG_BITS)|FPI_TAG))
#define c_bool_as_eul_bool(x) ((bool) x ? eul_true : eul_nil)
#define eul_allocate_int(loc, x) (loc) = c_int_as_eul_int(x)

/* Backward compatability ... */
#define eul_allocate_fpi(loc, x) (loc) = c_int_as_eul_int(x)

#define MAX_FPI ((ptrInt)0x1fffffff)
#define fpi_value_overflow(n) (top_bit_set(((ptrInt)n) ^ (((ptrInt)n)<<1)))
#define top_bit_set(n) (((ptrInt)(n)) < 0)


#endif // FPI_H
