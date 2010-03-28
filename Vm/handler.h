/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: eulvm (Bytecode Interpreter -- Eutopia)
 **  Authors: Andreas Kind
 **  Description: handlers (int*, double* and string*)
 ** ----------------------------------------------------------------------- **/

#ifndef HANDLER_H
#define HANDLER_H

/** ----------------------------------------------------------------- **
 ** HANDLER access
 ** ----------------------------------------------------------------- **/

#define HANDLER_SIZE (1)
#define HANDLER_HANDLE(x) (slot_ref((x), 0))

#define eul_is_int_ref(x)                                                      \
    (!is_immediate(x) && (object_class(x)==PGLOBAL(glob_fpi_ref_class)))
#define eul_is_double_ref(x)                                                   \
    (!is_immediate(x) && (object_class(x)==PGLOBAL(glob_double_ref_class)))
#define eul_is_string_ref(x)                                                   \
    (!is_immediate(x) && (object_class(x)==PGLOBAL(glob_string_ref_class)))

#define eul_int_ref_as_c_int_ref(x) ((int *) HANDLER_HANDLE(x))
#define eul_double_ref_as_c_double_ref(x) ((double *) HANDLER_HANDLE(x))
#define eul_string_ref_as_c_string_ref(x) ((char **) HANDLER_HANDLE(x))

#define eul_int_ref_as_c_int(x)                                                \
    (*((int *) HANDLER_HANDLE((LispRef) x)))
#define eul_double_ref_as_c_double(x)                                          \
    (*((double *) HANDLER_HANDLE((LispRef) x)))
#define eul_string_ref_as_c_string(x)                                          \
    (*((char **) HANDLER_HANDLE((LispRef) x)))

#define eul_c_vector_ref(x, i) (*((x)+(i)))
#define eul_c_vector_set(x, i, y) ((*((x)+(i)))=(y))

/** ----------------------------------------------------------------- **
 ** Handler allocation
 ** ----------------------------------------------------------------- **/

#define eul_allocate_int_ref(loc, x)                                           \
    eul_allocate_object(loc, PGLOBAL(glob_fpi_ref_class), HANDLER_SIZE,        \
    (LispRef) x)

#define eul_allocate_double_ref(loc, x)                                        \
    eul_allocate_object(loc, PGLOBAL(glob_double_ref_class), HANDLER_SIZE,     \
    (LispRef) x)

#define eul_allocate_string_ref(loc, x)                                        \
    eul_allocate_object(loc, PGLOBAL(glob_string_ref_class), HANDLER_SIZE,     \
    (LispRef) x)


#endif // HANDLER_H
