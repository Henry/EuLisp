/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: utilities (e.g. printing)
///-----------------------------------------------------------------------------

#ifndef UTIL_H
#define UTIL_H

#include "list.h"

extern LispRef eul_with_backtrace();
extern void eul_initialize_util();
extern void fprint_ref (FILE *, LispRef);
extern LispRef eul_is_object(void *o);

#ifdef WITH_TRACE
extern int eul_trace;
#endif

///-----------------------------------------------------------------------------
/// Generic printing extention
///-----------------------------------------------------------------------------

#define PRINTER_TABLE_SIZE (5)

extern void (*printer_table[]) (FILE *, LispRef);

#define define_printer(tag, fn) (printer_table[(ptrInt) tag]=(fn))
#define printer(ref) (printer_table[(ptrInt) tag_field(ref)])

#define fprimitive_print_object(fd, o)                                         \
    fprintf(fd, "#<class=%" ptrIntPM "x nslots=%" ptrIntPM "d &=%" ptrIntPM "x>", \
    (ptrInt) computed_object_class(o),                                         \
    fpi_value(computed_object_size(o)),                                        \
    (ptrInt) o);

///-----------------------------------------------------------------------------
/// Print function call
///-----------------------------------------------------------------------------

#ifdef WITH_DB
#define PRINT_OPERATOR_CALL(entry)                                             \
    {                                                                          \
        LispRef loc;                                                           \
        if ((reg_arg_operator==NULL) || !eul_is_function(reg_arg_operator))    \
        {                                                                      \
            SERIOUS_WARNING0("bad operator");                                  \
            fprintf(stderr, "    OPERATOR: ");                                 \
            fprint_ref(stderr, reg_arg_operator);                              \
            fprintf(stderr, "\n");                                             \
        }                                                                      \
        else                                                                   \
        {                                                                      \
            switch (entry)                                                     \
            {                                                                  \
                case 1: printf("___TAIL CALL "); break;                        \
                case 2: printf("___CALL BACK "); break;                        \
                case 3: printf("___CALL "); break;                             \
            }                                                                  \
            fprint_ref(stdout, LAMBDA_NAME(reg_arg_operator));                 \
            printf("\n___  DOMAIN = ");                                        \
            fprint_ref(stdout, LAMBDA_DOMAIN(reg_arg_operator));               \
            printf("\n");                                                      \
        }                                                                      \
        for(int i=0; i<reg_arg_count; i++)                                     \
        {                                                                      \
            printf("___  ARG%d = ", i);                                        \
            REFVAL(reg_arg_count-i-1, loc);                                    \
            fprint_ref(stdout, loc);                                           \
            printf(" \n");                                                     \
        }                                                                      \
        fflush(stdout);                                                        \
    }
#else
#define PRINT_OPERATOR_CALL(entry)
#endif // WITH_DB

///-----------------------------------------------------------------------------
#endif // UTIL_H
///-----------------------------------------------------------------------------
