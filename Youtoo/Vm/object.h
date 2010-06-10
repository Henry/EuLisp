/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: general instances
///-----------------------------------------------------------------------------

#ifndef OBJECT_H
#define OBJECT_H

///-----------------------------------------------------------------------------
/// Default object
///-----------------------------------------------------------------------------

typedef struct eul_object_structure
{
    struct eul_object_structure *size;
    struct eul_object_structure *class;
    struct eul_object_structure *slot1;
} EulObject;

typedef EulObject *LispRef;

#define HEADER_SIZE (2)
#define HEADER_BYTE_SIZE (HEADER_SIZE*sizeof(LispRef))

///-----------------------------------------------------------------------------
/// Object access
///-----------------------------------------------------------------------------

#define object_size(loc) ((loc)->size)
#define object_class(loc) ((loc)->class)
#define slot_ref(loc, i) (*((&((loc)->slot1))+i))
#define slots_to_bytes(s) (HEADER_BYTE_SIZE+((s)*sizeof(LispRef)))
#define eul_size_of(loc) computed_object_size(loc)
#define eul_class_of(loc) computed_object_class(loc)
#define eul_slot_ref(loc, i) slot_ref(loc, i)
#define eul_vector_size(x) eul_int_as_c_int(object_size(x))

///-----------------------------------------------------------------------------
/// Dynamic object allocation
///-----------------------------------------------------------------------------

#define INITIALIZE_OBJECT(loc, cl, nslots)                                     \
    object_size(loc) = c_int_as_eul_int(nslots);                               \
    object_class(loc) = (cl)

#define ALLOCATE_RAW_OBJECT(loc, bytesize)                                     \
    WHEN_INSTRUMENTED(eul_allocated_memory += bytesize; )                      \
    (loc) = (LispRef) gc_malloc(bytesize);

#define ALLOCATE_WARM_OBJECT(loc, class, nslots)                               \
    ALLOCATE_RAW_OBJECT(loc, slots_to_bytes(nslots))                           \
    INITIALIZE_OBJECT(loc, class, nslots)

#define eul_allocate_object(loc, class, nslots, init)                          \
    {                                                                          \
        int iii, nnn = nslots;                                                 \
        ALLOCATE_WARM_OBJECT(loc, class, nnn);                                 \
        for (iii = 0; iii < nnn; ++iii)                                        \
            slot_ref(loc, iii) = (init);                                       \
    }

///-----------------------------------------------------------------------------
#endif // OBJECT_H
///-----------------------------------------------------------------------------
