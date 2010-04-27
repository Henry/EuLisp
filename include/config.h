/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Andreas Kind, Keith Playford
///  Description: configuration of the bytecode interpreter
///-----------------------------------------------------------------------------

#ifndef CONFIG_H
#define CONFIG_H

#if WORD_LENGTH==32 // 32bit OS

    #define ptrNBytes 4
    #define ptrIntPM
    typedef int ptrInt;
    typedef unsigned int uPtrInt;
    typedef uint8_t Instruction;

#elif WORD_LENGTH==64 // 64bit OS

    #define ptrNBytes 8
    #define ptrIntPM "l"
    typedef long ptrInt;
    typedef unsigned long uPtrInt;
    typedef uint16_t Instruction;

#else
#  error WORD_LENGTH set incorrectly
#endif


///-----------------------------------------------------------------------------
/// Compiler configuration
///-----------------------------------------------------------------------------

// We use first-class labels with GCC
#ifdef __GNUC__
#  define WITH_LABELS
#endif

#ifdef WITH_PCR_THREADS
// Because read is used quite often where it must not expand to PCR_read
// The only case where read() should be PCR_read() is in Ff/eul_ntok.c
#  undef read
#endif


///-----------------------------------------------------------------------------
/// Interpreter configuration
///-----------------------------------------------------------------------------

#define SILENT
//#define WITH_DB
//#define WITH_TRACE ""
// #define WITH_TRACE "initialize-cg-gen"

// #define WITH_PRIMITIVE_PRINTER
// #define WITH_BC

#define SAFETY
// #define WITH_SIGNALS

// #define WITH_CONS_TAG

#define WITH_GC
#define DEFAULT_HEAP (600000)

// #define INSTRUMENTED

#define HIGHEST_BYTE_CODE (255)


///-----------------------------------------------------------------------------
/// Debugging
///-----------------------------------------------------------------------------

#ifdef WITH_DB
#  define WHEN_DB(x) x; fflush(stdout);
#  define WITH_PRIMITIVE_PRINTER
#else
#  define WHEN_DB(x)
#endif


///-----------------------------------------------------------------------------
/// Bytecodes
///-----------------------------------------------------------------------------

#ifdef WITH_BC
#  define WHEN_BC(x) x
#else
#  define WHEN_BC(x)
#endif


///-----------------------------------------------------------------------------
/// GC
///-----------------------------------------------------------------------------

#ifdef WITH_GC

#  define GC_PTHREADS 1
#  include <pthread.h>
#  include <gc.h>

#  define gc_malloc(n) GC_malloc(n)
#  define gc_expand_hp(x) GC_expand_hp(x)
#  define gc_free(x) GC_free(x)
#  define gc_realloc(p, n) GC_realloc(p, n)

#else
#  define gc_malloc(n) malloc(n)
#  define gc_expand_hp(x) 1
#  define gc_free(x)
#  define gc_realloc(p, n)
#endif


///-----------------------------------------------------------------------------
/// Instrumentation
///-----------------------------------------------------------------------------

#ifdef INSTRUMENTED
#  define WHEN_INSTRUMENTED(x) x
extern int eul_allocated_memory;

extern int eul_executed_instructions;
#else
#  define WHEN_INSTRUMENTED(x)
#endif


///-----------------------------------------------------------------------------
/// Lambda tracing
///-----------------------------------------------------------------------------

#ifdef WITH_TRACE

#  define TRACE_LAMBDA()                                                       \
    {                                                                          \
        char *operator_name =                                                  \
            (char *) STRING_DATA(SYMBOL_NAME(LAMBDA_NAME(reg_arg_operator)));  \
        if                                                                     \
        (                                                                      \
            (strcmp("", WITH_TRACE)) &&                                        \
            (strcmp(operator_name, WITH_TRACE))                                \
        )                                                                      \
        {                                                                      \
            eul_trace = 0;                                                     \
        }                                                                      \
        else                                                                   \
        {                                                                      \
            printf("*** TRACE [%s]:\n", operator_name);                        \
            eul_trace = 1;                                                     \
        }                                                                      \
    }

#  define TRACE_FOO()                                                          \
    {                                                                          \
        char *operator_name =                                                  \
            (char *) STRING_DATA(SYMBOL_NAME(LAMBDA_NAME(reg_arg_operator)));  \
        printf("%s\n", operator_name); fflush(stdout);                         \
        if (!(strcmp(operator_name, "foo")))                                   \
        {                                                                      \
            fprint_ref(stdout, reg_arg_operator);                              \
            fflush(stdout);                                                    \
        }                                                                      \
    }

#  define RESUME_TRACE_LAMBDA()                                                \
    {                                                                          \
        char *operator_name;                                                   \
        if (eul_is_symbol(LAMBDA_NAME(reg_arg_operator)))\
        {                                                                      \
            operator_name =                                                    \
            (char *) STRING_DATA(SYMBOL_NAME(LAMBDA_NAME(reg_arg_operator)));  \
        }                                                                      \
        else                                                                   \
        {                                                                      \
            operator_name = "???";                                             \
        }                                                                      \
                                                                               \
        if                                                                     \
        (                                                                      \
            (strcmp("", WITH_TRACE)) &&                                        \
            (strcmp(operator_name, WITH_TRACE))                                \
        )                                                                      \
        {                                                                      \
            eul_trace = 0;                                                     \
        }                                                                      \
        else                                                                   \
        {                                                                      \
            printf("*** RESUME TRACE [%s]:\n", operator_name);                 \
            eul_trace = 1;                                                     \
        }                                                                      \
    }

#else

#  define TRACE_LAMBDA()
#  define RESUME_TRACE_LAMBDA()

#endif


///-----------------------------------------------------------------------------
/// Diagnostic messages
///-----------------------------------------------------------------------------

#define PRINT_REG_PC0(message)                                                 \
    printf                                                                     \
    (                                                                          \
        "*** REG_PC " message " *reg_pc reg_pc reg_arg_operator "              \
        "%"ptrIntPM"x %"ptrIntPM"x %"ptrIntPM"x\n",                            \
        (*((ptrInt *)reg_pc)),                                                 \
        (ptrInt)reg_pc,                                                        \
        (ptrInt)LAMBDA_CODE(reg_arg_operator)                                  \
    )

#define PRINT_REG_PC(message)


///-----------------------------------------------------------------------------
#endif // CONFIG_H
///-----------------------------------------------------------------------------
