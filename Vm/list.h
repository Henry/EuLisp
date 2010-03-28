/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: eulvm (Bytecode Interpreter -- Eutopia)
 **  Authors: Keith Playford, Andreas Kind
 **  Description: list processing
 ** ----------------------------------------------------------------------- **/

#ifndef LIST_H
#define LIST_H

/** ----------------------------------------------------------------- **
 ** Initialization
 ** ----------------------------------------------------------------- **/

extern void fprint_cons (FILE *, LispRef);

extern void fprint_vector (FILE *, LispRef);

#ifdef WITH_CONS_TAG
extern void eul_initialize_cons();
#endif /* WITH_CONS_TAG */

/** ----------------------------------------------------------------- **
 ** Cons access
 ** ----------------------------------------------------------------- **/

#define CONS_SIZE (2)
#define EUL_CONS_SIZE c_int_as_eul_int(CONS_SIZE)

#ifdef WITH_CONS_TAG
#define eul_car(loc) object_size(untag(loc, CONS_TAG))
#define eul_cdr(loc) object_class(untag(loc, CONS_TAG))
#else
#define eul_car(x) (slot_ref((x), 0))
#define eul_cdr(x) (slot_ref((x), 1))
#endif /* WITH_CONS_TAG */

#define eul_null(x) ((x) == eul_nil)
#define eul_is_list(x) (eul_null(x) || eul_is_cons(x))

/** ----------------------------------------------------------------- **
 ** Dynamic cons allocation
 ** ----------------------------------------------------------------- **/

#ifdef WITH_CONS_TAG
#define eul_allocate_cons(loc, the_car, the_cdr)                               \
    {                                                                          \
        LispRef new___;                                                        \
        ALLOCATE_RAW_OBJECT(new___, CONS_SIZE*sizeof(LispRef));                \
        object_size(new___) = the_car;                                         \
        object_class(new___) = the_cdr;                                        \
        loc = tag(new___, CONS_TAG);                                           \
    }

#define eul_allocate_empty_cons(loc)                                           \
    {                                                                          \
        LispRef new___;                                                        \
        ALLOCATE_RAW_OBJECT(new___, CONS_SIZE*sizeof(LispRef));                \
        object_size(new___) = eul_nil;                                         \
        object_class(new___) = eul_nil;                                        \
        loc = tag(new___, CONS_TAG);                                           \
    }

#else

#define eul_allocate_cons(loc, the_car, the_cdr)                               \
    {                                                                          \
        LispRef new___;                                                        \
        ALLOCATE_WARM_OBJECT(new___, PGLOBAL(glob_cons_class), CONS_SIZE);     \
        eul_car(new___) = (the_car);                                           \
        eul_cdr(new___) = (the_cdr);                                           \
        loc = new___;                                                          \
    }

#define eul_allocate_empty_cons(loc)                                           \
    {                                                                          \
        LispRef new___;                                                        \
        ALLOCATE_WARM_OBJECT(new___, PGLOBAL(glob_cons_class), CONS_SIZE);     \
        eul_car(new___) = eul_nil;                                             \
        eul_cdr(new___) = eul_nil;                                             \
        loc = new___;                                                          \
    }

#define eul_allocate_static_cons(loc, the_car, the_cdr)                        \
    static LispRef loc##_static[] =                                            \
    {EUL_CONS_SIZE, NULL, the_car, the_cdr};                                   \
    static LispRef loc = eul_as_static(loc)

#endif /* WITH_CONS_TAG */

#define LISTIFY(count)                                                         \
    {                                                                          \
        int i = (count);                                                       \
        PUSHVAL1(eul_nil);                                                     \
        while (i--)                                                            \
        {                                                                      \
            LispRef first;                                                     \
            POPVAL1(first);                                                    \
            eul_allocate_cons(LVPEEKVAL(), LVPEEKVAL(), first);                \
        }                                                                      \
    }

#define eul_allocate_cons_old(loc, the_car, the_cdr)                           \
    GC_CONS(loc, the_car, the_cdr)


#endif // LIST_H
