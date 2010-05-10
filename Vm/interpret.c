//  Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.

///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Andreas Kind, Keith Playford
///  Description: interpreter function
///-----------------------------------------------------------------------------

#include "stdc.h"
#include "config.h"
#include "notify.h"
#include "shared-mem.h"
#include "class.h"
#include "io.h"
#include "tag.h"
#include "object.h"
#include "register.h"
#include "stack.h"
#include "signals.h"
#include "cache.h"
#include "bytecode.h"
#include "bytevec.h"
#include "fpi.h"
#include "double.h"
#include "character.h"
#include "list.h"
#include "ff.h"
#include "operator.h"
#include "state.h"
#include "eul-string.h"
#include "table.h"
#include "symbol.h"
#include "callback.h"
#include "util.h"


///-----------------------------------------------------------------------------
/// Instrumentation
///-----------------------------------------------------------------------------

#ifdef INSTRUMENTED
ptrInt eul_allocated_memory = 0;
ptrInt eul_gf_calls = 0;
ptrInt eul_gf_calls1 = 0;
ptrInt eul_gf_calls2 = 0;
ptrInt eul_gf_calls3 = 0;
ptrInt eul_gf_calls4 = 0;
ptrInt eul_gf_cache_misses = 0;
ptrInt eul_gf_cache_exts = 0;
ptrInt eul_executed_instructions = 0;
long eul_profiling_table[HIGHEST_BYTE_CODE + 1];
#endif // INSTRUMENTED


///-----------------------------------------------------------------------------
/// Get inlined arguments when little endian
///-----------------------------------------------------------------------------

#if BYTE_ORDER == LITTLE_ENDIAN

ptrInt get_fixarg_fn(Instruction ** pcptr, uPtrInt rel_reg_pc_arg)
{
    *pcptr += 3 + (4 - (rel_reg_pc_arg % 4));

    ptrInt res = 0;
    char *valbytes = (char*) ((*pcptr) - 3);
    char *resbytes = (char*) &res;

    #if ptrNBytes==4
    *resbytes = *(valbytes + 3);
    *(resbytes + 1) = *(valbytes + 2);
    *(resbytes + 2) = *(valbytes + 1);
    *(resbytes + 3) = *valbytes;
    #else
    *resbytes = *(valbytes + 6);
    *(resbytes + 1) = *(valbytes + 4);
    *(resbytes + 2) = *(valbytes + 2);
    *(resbytes + 3) = *valbytes;
    #endif

    return res;
}

#endif // LITTLE_ENDIAN


///-----------------------------------------------------------------------------
/// Main emulation loop
///-----------------------------------------------------------------------------

ptrInt interpret(RegisterRef set)
{
    #if BYTE_ORDER == LITTLE_ENDIAN
    DECLARE_UNOPTIMISED_LOCAL_REGISTER_SET();
    #else
    DECLARE_LOCAL_REGISTER_SET();
    #endif
    DECLARE_STACK_REGISTERS();
    DECLARE_BC_VARIABLES();

    #ifdef WITH_LABELS
    static void *label_table[] =
    {
        &&BCL_NOP,
        &&BCL_PRIMITIVE_ALLOCATE,
        &&BCL_PRIMITIVE_REF,
        &&BCL_SET_PRIMITIVE_REF,
        &&BCL_PRIMITIVE_CLASS_OF,
        &&BCL_SET_PRIMITIVE_CLASS_OF,
        &&BCL_PRIMITIVE_SIZE,
        &&BCL_TEST_AND_SET_LOCK,
        &&BCL_PRIMITIVE_RELATIVE_REF,
        &&BCL_SET_PRIMITIVE_RELATIVE_REF,
        &&BCL_NOP,
        &&BCL_STRING_REF,
        &&BCL_SET_STRING_REF,
        &&BCL_THE_CAR2,
        &&BCL_THE_CDR2,
        &&BCL_THE_CONS,
        &&BCL_THE_CAR,
        &&BCL_THE_CDR,
        &&BCL_NULLP,
        &&BCL_NOP,
        &&BCL_FPI_SUM,
        &&BCL_FPI_DIFFERENCE,
        &&BCL_FPI_PRODUCT,
        &&BCL_FPI_QUOTIENT,
        &&BCL_FPI_REMAINDER,
        &&BCL_FPI_EQUAL,
        &&BCL_FPI_LT,
        &&BCL_STACK_REF0,
        &&BCL_STACK_REF1,
        &&BCL_STACK_REF2,
        &&BCL_SWAP,
        &&BCL_STACK_REF,
        &&BCL_SET_STACK_REF,
        &&BCL_POP,
        &&BCL_NOBBLE,
        &&BCL_STATIC_REF,
        &&BCL_BINDING_REF,
        &&BCL_SET_BINDING_REF,
        &&BCL_STATIC_FPI_REF,
        &&BCL_STATIC_CHARACTER_REF,
        &&BCL_REGISTER_REF,
        &&BCL_SET_REGISTER_REF,
        &&BCL_POP1,
        &&BCL_FPI_INC,
        &&BCL_FPI_DEC,
        &&BCL_FPI_ZEROP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_BRANCH,
        &&BCL_BRANCH_TRUE,
        &&BCL_BRANCH_NIL,
        /* &&BCL_BRANCH_NEG, */ &&BCL_NOP,
        &&BCL_BRANCH_POS,
        &&BCL_CALL_NEXT_METHOD,
        &&BCL_TAIL_CALL_NEXT_METHOD,
        &&BCL_BRANCH_LONG_POS,
        &&BCL_NOP,
        &&BCL_MAKE_LAMBDA,
        &&BCL_CALL_OPERATOR,
        &&BCL_TAIL_CALL_OPERATOR,
        &&BCL_BRANCH_TRUE_POS,
        &&BCL_BRANCH_TRUE_LONG_POS,
        &&BCL_NOP,
        &&BCL_CALL_FOREIGN_FUNCTION,
        &&BCL_WRITE_OBJECT,
        &&BCL_CHECK_ARGUMENTS,
        &&BCL_BRANCH_NIL_POS,
        &&BCL_RETURN,
        &&BCL_ALLOC,
        &&BCL_DISPLAY_REF,
        &&BCL_SET_DISPLAY_REF,
        &&BCL_DEALLOC,
        &&BCL_BRANCH_NIL_LONG_POS,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_EQ,
        &&BCL_EQL,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_UNFLUSH_STACKS,
        &&BCL_NOP,
        &&BCL_FILL_THREAD_STATE,
        &&BCL_RESTORE_THREAD_STATE,
        &&BCL_FILL_STATE,
        &&BCL_RESTORE_STATE,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_CHARACTER_TO_FPI,
        &&BCL_FPI_TO_CHARACTER,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_EXIT,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_INTERN,
        &&BCL_ASSQ,
        &&BCL_INIQ,
        &&BCL_MEMQ,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_THE_CAAR,
        &&BCL_THE_CADR,
        &&BCL_THE_CDAR,
        &&BCL_THE_CDDR,
        &&BCL_THE_CADDR,
        &&BCL_THE_CADDDR,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_CONSP,
        &&BCL_LISTP,
        &&BCL_SYMBOLP,
        &&BCL_STRINGP,
        &&BCL_FPIP,
        &&BCL_LAMBDAP,
        &&BCL_GFP,
        &&BCL_CHARACTERP,
        &&BCL_STATIC_REF0,
        &&BCL_STATIC_REF1,
        &&BCL_STATIC_REF2,
        &&BCL_STATIC_REF_1,
        &&BCL_STATIC_REF_NIL,
        &&BCL_STATIC_REF_T,
        &&BCL_NOP,
        &&BCL_SET_AND_GET_BINDING_REF,
        &&BCL_STATIC_FPI_BYTE_REF,
        &&BCL_NOP,
        &&BCL_CONTEXT_STACK_REF,
        &&BCL_VALUE_STACK_REF,
        &&BCL_APPLY,
        &&BCL_SET_CAR,
        &&BCL_SET_CDR,
        &&BCL_SETTER,
        &&BCL_SET_SETTER,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_CHECK_ARGUMENTS_2,
        &&BCL_CHECK_ARGUMENTS_1,
        &&BCL_CHECK_ARGUMENTS0,
        &&BCL_CHECK_ARGUMENTS1,
        &&BCL_CHECK_ARGUMENTS2,
        &&BCL_RETURN0,
        &&BCL_RETURN1,
        &&BCL_RETURN2,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP,
        &&BCL_NOP
    };

    #endif

    NOTIFY0("Entered emulation loop ...");
    FILL_LOCAL_REGISTER_SET(set);
    CACHE_STACK_REGISTERS();
    reg_arg_operator = eul_nil;

    // Make sure stacks are not empty to help light-weight stack handling
    if (reg_value_stack->sp == reg_value_stack->base)
    {
        PUSHVAL1(eul_nil);
    }

    if (reg_context_stack->sp == reg_context_stack->base)
    {
        PUSHEMPTYCONTEXT();
    }

    #ifdef WITH_LABELS
    NOTIFY0("indexed ordered ...\n");
    reg_current_op = *reg_pc;
    WHEN_INSTRUMENTED(++eul_executed_instructions; )
    WHEN_INSTRUMENTED(++(eul_profiling_table[(ptrInt) reg_current_op]); )
    goto *label_table[reg_current_op];
    #else

    while (1)
    {
        reg_current_op = *reg_pc;

        WHEN_INSTRUMENTED(++eul_executed_instructions; )
        WHEN_INSTRUMENTED(++(eul_profiling_table[(ptrInt) reg_current_op]); )

        switch (reg_current_op)
        {
            #endif
            BC_CASE(STACK_REF0)
            BC_CASE(STACK_REF)
            BC_CASE(STACK_REF1)
            BC_CASE(STACK_REF2)
            BC_CASE(BRANCH_NIL_POS)
            BC_CASE(THE_CAR)
            BC_CASE(DISPLAY_REF)
            BC_CASE(THE_CDR)
            BC_CASE(TAIL_CALL_OPERATOR)
            BC_CASE(NULLP)
            BC_CASE(BINDING_REF)
            BC_CASE(CHECK_ARGUMENTS2)
            BC_CASE(STATIC_REF)
            BC_CASE(SET_DISPLAY_REF)
            BC_CASE(RETURN)
            BC_CASE(CALL_OPERATOR)
            BC_CASE(STATIC_REF_NIL)
            BC_CASE(BRANCH_POS)
            BC_CASE(THE_CONS)
            BC_CASE(CHECK_ARGUMENTS)
            BC_CASE(MAKE_LAMBDA)
            BC_CASE(ALLOC)
            BC_CASE(STATIC_FPI_BYTE_REF)
            BC_CASE(PRIMITIVE_REF)
            BC_CASE(EQ)
            BC_CASE(EQL)
            BC_CASE(PRIMITIVE_CLASS_OF)
            BC_CASE(CHECK_ARGUMENTS_2)
            BC_CASE(NOBBLE)
            BC_CASE(STATIC_REF_T)
            BC_CASE(CHECK_ARGUMENTS_1)
            BC_CASE(POP1)
            BC_CASE(SET_REGISTER_REF)
            BC_CASE(STATIC_REF2)
            BC_CASE(STATIC_REF1)
            BC_CASE(FPI_INC)
            BC_CASE(FPIP)
            BC_CASE(FPI_DEC)
            BC_CASE(FPI_ZEROP)
            BC_CASE(PRIMITIVE_ALLOCATE)
            BC_CASE(PRIMITIVE_RELATIVE_REF)
            BC_CASE(SET_PRIMITIVE_REF)
            BC_CASE(SET_PRIMITIVE_RELATIVE_REF)
            BC_CASE(SET_PRIMITIVE_CLASS_OF)
            BC_CASE(PRIMITIVE_SIZE)
            BC_CASE(STRING_REF)
            BC_CASE(SET_STRING_REF)
            BC_CASE(THE_CAAR)
            BC_CASE(THE_CADR)
            BC_CASE(THE_CDAR)
            BC_CASE(THE_CDDR)
            BC_CASE(THE_CADDR)
            BC_CASE(THE_CADDDR)
            BC_CASE(SET_CAR)
            BC_CASE(SET_CDR)
            BC_CASE(CONSP)
            BC_CASE(LISTP)
            BC_CASE(CHARACTERP)
            BC_CASE(STRINGP)
            BC_CASE(SYMBOLP)
            BC_CASE(LAMBDAP)
            BC_CASE(GFP)
            BC_CASE(SETTER)
            BC_CASE(SET_SETTER)
            BC_CASE(FPI_LT)
            BC_CASE(FPI_PRODUCT)
            BC_CASE(FPI_QUOTIENT)
            BC_CASE(FPI_REMAINDER)
            BC_CASE(FPI_EQUAL)
            BC_CASE(SWAP)
            BC_CASE(SET_STACK_REF)
            BC_CASE(POP)
            BC_CASE(SET_BINDING_REF)
            BC_CASE(SET_AND_GET_BINDING_REF)
            BC_CASE(STATIC_REF0)
            BC_CASE(STATIC_REF_1)
            BC_CASE(STATIC_FPI_REF)
            BC_CASE(STATIC_CHARACTER_REF)
            BC_CASE(REGISTER_REF)
            BC_CASE(CONTEXT_STACK_REF)
            BC_CASE(VALUE_STACK_REF)
            BC_CASE(BRANCH)
            BC_CASE(FPI_SUM)
            BC_CASE(FPI_DIFFERENCE)
            BC_CASE(BRANCH_LONG_POS)
            BC_CASE(BRANCH_NIL)
            BC_CASE(BRANCH_NIL_LONG_POS)
            BC_CASE(BRANCH_TRUE)
            BC_CASE(BRANCH_TRUE_POS)
            BC_CASE(BRANCH_TRUE_LONG_POS)
            BC_CASE(CALL_NEXT_METHOD)
            BC_CASE(TAIL_CALL_NEXT_METHOD)
            BC_CASE(APPLY)
            BC_CASE(CHECK_ARGUMENTS0)
            BC_CASE(CHECK_ARGUMENTS1)
            BC_CASE(RETURN0)
            BC_CASE(RETURN1)
            BC_CASE(RETURN2)
            BC_CASE(CALL_FOREIGN_FUNCTION)
            BC_CASE(WRITE_OBJECT)
            BC_CASE(DEALLOC)
            BC_CASE(TEST_AND_SET_LOCK)
            BC_CASE(UNFLUSH_STACKS)
            BC_CASE(FILL_THREAD_STATE)
            BC_CASE(RESTORE_THREAD_STATE)
            BC_CASE(FILL_STATE)
            BC_CASE(RESTORE_STATE)
            BC_CASE(ASSQ)
            BC_CASE(INIQ)
            BC_CASE(MEMQ)
            BC_CASE(FPI_TO_CHARACTER)
            BC_CASE(CHARACTER_TO_FPI)
            BC_CASE(INTERN)
            BC_CASE(THE_CAR2)
            BC_CASE(THE_CDR2) BC_CASE(EXIT) BC_CASE(NOP)
            #ifndef WITH_LABELS
            default:
            SERIOUS_WARNING1("unknown bytecode %d\n", reg_current_op);
            return (0);
        }
    }
    #endif // WITH_LABELS

exit:

    #ifdef INSTRUMENTED
    {
        static char *extension = ".instr";

        char *prog_name = eul_string_as_c_string
        (
            slot_ref(PGLOBAL(glob_argv), 0)
        );

        ptrInt n = strlen(prog_name);
        ptrInt m = strlen(extension);

        char *file_name = (char *)gc_malloc(n + m);
        strcpy(file_name, prog_name);
        strcpy(file_name + n, extension);

        FILE *fp = fopen(file_name, "w");

        fprintf(fp, "Program: %s", prog_name);
        fprintf(fp, "\nAllocated_memory: %d bytes\n", eul_allocated_memory);

        fprintf
        (
            fp,
            "Value_stack: %d values\n",
            (ptrInt) (sreg_value_sp - sreg_value_sb - 1)
        );

        fprintf
        (
            fp,
            "Context_stack: %d frames\n\n",
            (((ptrInt)(sreg_context_sp - sreg_context_sb))
           /CONTEXT_FRAME_SIZE) - 1
        );

        fprintf(fp, "Generic_function_calls: %d\n", eul_gf_calls);
        fprintf(fp, "Generic_function_calls_with_arity_1: %d\n", eul_gf_calls1);
        fprintf(fp, "Generic_function_calls_with_arity_2: %d\n", eul_gf_calls2);
        fprintf(fp, "Generic_function_calls_with_arity_3: %d\n", eul_gf_calls3);

        fprintf
        (
            fp,
            "Generic_function_calls_with_arity_>3: %d\n",
            eul_gf_calls4
        );

        fprintf(fp, "Generic_function_cache_misses: %d\n", eul_gf_cache_misses);

        fprintf
        (
            fp,
            "Generic_function_cache_extensions: %d\n",
            eul_gf_cache_exts
        );

        fprintf(fp, "Bytecodes_executed: %d\n", eul_executed_instructions);

        for (ptrInt i = 0; i < HIGHEST_BYTE_CODE + 1; ++i)
        {
            if ((ptrInt x = eul_profiling_table[i]) != 0)
            {
                ptrInt p1 = x * 100 / eul_executed_instructions;

                ptrInt p2 = x * 500 / eul_executed_instructions;

                fprintf(fp, "%-3d  %-8ld %2ld\t|", i, x, p1);

                for (ptrInt j = 0; j < p2; j++)
                {
                    fprintf(fp, "+");
                }

                fprintf(fp, "\n");
            }
            else
            {
                fprintf(fp, "                \t|\n");
            }
        }

        fprintf(fp, "\n");
        fclose(fp);
    }
    #endif // INSTRUMENTED

    FLUSH_STACK_REGISTERS();
    DUMP_LOCAL_REGISTER_SET(set);

    return 0;
}
