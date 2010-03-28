/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: eul-serial
 **  Authors: Andreas Kind
 **  Description: C support for marshaling EuLisp objects
 ** ----------------------------------------------------------------------- **/

#ifndef EUL_SERIAL_H
#define EUL_SERIAL_H

#include "double.h"

/** ----------------------------------------------------------------- **
 ** Object stream
 ** ----------------------------------------------------------------- **/

#define EUL_OBJECT_STREAM_CACHE(x) slot_ref(x,6)
#define EUL_OBJECT_STREAM_CACHE_INDEX(x) slot_ref(x,7)

/** ----------------------------------------------------------------- **
 ** Constants
 ** ----------------------------------------------------------------- **/

#define STREAM_MAGIC 0xaced
#define STREAM_VERSION 1

#define TC_NULL '\x70'
#define TC_REFERENCE '\x71'
#define TC_CLASS '\x72'
#define TC_OBJECT '\x73'
#define TC_STRING '\x74'
#define TC_VECTOR '\x75'
#define TC_KEYWORD '\x76'
#define TC_STREAM '\x77'
#define TC_STATE '\x78'
#define TC_RESET '\x79'
#define TC_SELF '\x7A'
#define TC_FUNCTION '\x7C'
#define TC_BYTEVECTOR '\x7D'
#define TC_INT '\x7E'
#define TC_DOUBLE '\x7F'
#define TC_SYMBOL '\x80'
#define TC_CHAR '\x81'
#define TC_CONS '\x82'

/** ----------------------------------------------------------------- **
 ** Macros for reading primitives
 ** ----------------------------------------------------------------- **/

#define read_bytes(n)                                                          \
  eul_string_as_c_string(eul_read_bytes(n))

#define eul_read_bytes(n)                                                      \
  eul_serial_read_bytes(stream, c_int_as_eul_int(n), eos_error_p, eos_value)

#define read_byte(x)                                                           \
    {                                                                          \
        char *buf = read_bytes(1);                                             \
        x = (*((char *) buf));                                                 \
    }

#define eul_read_byte(x)                                                       \
    {                                                                          \
        char *buf = read_bytes(1);                                             \
        x = c_char_as_eul_char(*((char *) buf));                               \
    }

#define read_short(x)                                                          \
    {                                                                          \
        char *buf = read_bytes(sizeof(unsigned short));                        \
        x = (*((unsigned short *) buf));                                       \
    }

#define eul_read_short(x)                                                      \
    {                                                                          \
        char *buf = read_bytes(sizeof(unsigned short));                        \
        x = c_int_as_eul_int(*((unsigned short *) buf));                       \
    }

#define read_int(x)                                                            \
    {                                                                          \
        char *buf = read_bytes(sizeof(int));                                   \
        x = (*((int *) buf));                                                  \
    }

#define eul_read_int(x)                                                        \
    {                                                                          \
        char *buf = read_bytes(sizeof(int));                                   \
        x = c_int_as_eul_int(*((int *) buf));                                  \
    }

#define eul_read_double(x)                                                     \
    {                                                                          \
        char *buf = read_bytes(sizeof(double));                                \
        eul_allocate_double(x, *((double *) buf));                             \
    }

#define read_utf(x)                                                            \
    {                                                                          \
        unsigned short n;                                                      \
        read_short(n);                                                         \
        WITH_DEBUG(printf("  reading %d bytes ...\n", n));                     \
        x = read_bytes(n);                                                     \
        if ((*x) & 128)                                                        \
        {                                                                      \
            LispRef str;                                                       \
            eul_allocate_string(str, "non standard characters not supported yet"); \
            eul_serial_error(stream, str, eul_nil);                            \
        }                                                                      \
    }

#define eul_read_utf(x)                                                        \
    {                                                                          \
        unsigned short n;                                                      \
        read_short(n);                                                         \
        WITH_DEBUG(printf("  reading %d bytes ...\n", n));                     \
        x = eul_read_bytes(n);                                                 \
        if ((*(eul_string_as_c_string(x))) & 128)                              \
        {                                                                      \
            LispRef str;                                                       \
            eul_allocate_string(str, "non standard characters not supported yet"); \
            eul_serial_error(stream, str, eul_nil);                            \
        }                                                                      \
    }


/** ----------------------------------------------------------------- **
 ** Foreign function declarations
 ** ----------------------------------------------------------------- **/

extern LispRef eul_serial_read_header
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef eul_serial_read_object
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef eul_serial_error
(
    LispRef stream,
    LispRef str,
    LispRef args
);

extern LispRef header
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef content
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef reset
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef newObject
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef newCons
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef newFunction
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef newBytevector
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef newVector
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef newStream
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef newState
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef newString
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef newSymbol
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef newKeyword
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef newInt
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef newDouble
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef newChar
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef newClass
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef prevObject
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef nullReference
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern LispRef reset
(
    LispRef stream,
    LispRef eos_error_p,
    LispRef eos_value
);

extern void newHandle
(
    LispRef stream,
    LispRef obj
);


/** ----------------------------------------------------------------------- **/

#endif // EUL_SERIAL_H

/** ----------------------------------------------------------------------- **/
