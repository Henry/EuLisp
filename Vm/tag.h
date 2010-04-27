/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: tagging
///-----------------------------------------------------------------------------

#ifndef TAG_H
#define TAG_H

///-----------------------------------------------------------------------------
/// Tag codes
///-----------------------------------------------------------------------------

#define TAG_BITS (2)

#define OBJECT_TAG ((char) 0)
#define FPI_TAG ((char) 1)
#define CHAR_TAG ((char) 2)

#ifdef WITH_CONS_TAG
#define CONS_TAG ((char) 3)
#else
#define SPARE_TAG ((char) 3)
#endif /* WITH_CONS_TAG */

///-----------------------------------------------------------------------------
/// Tag access
///-----------------------------------------------------------------------------

#define tag_field(loc) (((ptrInt)(loc))&0x3)
#define tag(loc, tag_value) ((LispRef) (((ptrInt) loc)|tag_value))
#define untag(loc, tag_value) ((LispRef) (((ptrInt) (loc))-tag_value))
#define is_immediate(loc) (tag_field(loc))
#define eul_is_int(loc) (tag_field(loc) == FPI_TAG)
#define eul_is_char(loc) (tag_field(loc) == CHAR_TAG)

#ifdef WITH_CONS_TAG

#define eul_is_cons(loc) (tag_field(loc) == CONS_TAG)

#define computed_object_class(loc)                                             \
    (is_immediate(loc) ?                                                       \
    (eul_is_int(loc) ? PGLOBAL(glob_fpi_class) :                               \
    (eul_is_cons(loc) ? PGLOBAL(glob_cons_class):                              \
    PGLOBAL(glob_char_class))) :                                               \
    object_class(loc))

#else

#define eul_is_cons(x)                                                         \
    (computed_object_class(x) == PGLOBAL(glob_cons_class))

#define computed_object_class(loc)                                             \
    (is_immediate(loc) ?                                                       \
    (eul_is_int(loc) ? PGLOBAL(glob_fpi_class) :                               \
    PGLOBAL(glob_char_class)) :                                                \
    object_class(loc))

#endif // WITH_CONS_TAG

#define computed_object_size(loc)                                              \
    (is_immediate(loc) ? c_int_as_eul_int(0) : object_size(loc))

///-----------------------------------------------------------------------------
#endif // TAG_H
///-----------------------------------------------------------------------------
