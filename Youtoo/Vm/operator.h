/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: lambdas, generic functions, methods
///-----------------------------------------------------------------------------

#ifndef OPERATOR_H
#define OPERATOR_H

///-----------------------------------------------------------------------------
/// Lambda access
///-----------------------------------------------------------------------------

#define LAMBDA_SIZE (5)

#define LAMBDA_NAME(x) (slot_ref((x), 0))
#define LAMBDA_DOMAIN(x) (slot_ref((x), 1))
#define LAMBDA_SETTER(x) (slot_ref((x), 2))
#define LAMBDA_ENV(x) (slot_ref((x), 3))
#define LAMBDA_CODE(x) (slot_ref((x), 4))

#define eul_is_lambda(x) (eul_class_of(x) == PGLOBAL(glob_lambda_class))

///-----------------------------------------------------------------------------
/// Dynamic lambda allocation
///-----------------------------------------------------------------------------

#define eul_allocate_lambda(loc, name, arity, bv)                              \
    ALLOCATE_WARM_OBJECT(loc, PGLOBAL(glob_lambda_class), LAMBDA_SIZE);        \
    eul_intern_symbol(LAMBDA_NAME(loc), name);                                 \
    eul_allocate_int(LAMBDA_DOMAIN(loc), arity);                               \
    LAMBDA_ENV(loc) = eul_nil;                                                 \
                    LAMBDA_CODE(loc) = BYTEVECTOR_DATA(bv);                    \
                                     LAMBDA_SETTER(loc) = eul_nil;             \
                                                        object_size(loc) = object_size(bv)

#define eul_allocate_lambda1(loc, name, arity, bv)                             \
    ALLOCATE_WARM_OBJECT(loc, PGLOBAL(glob_lambda_class), LAMBDA_SIZE);        \
    LAMBDA_NAME(loc) = name;                                                   \
                     LAMBDA_DOMAIN(loc) = arity;                               \
                                        LAMBDA_ENV(loc) = eul_nil;             \
                                                        LAMBDA_CODE(loc) = BYTEVECTOR_DATA(bv); \
                                                                         LAMBDA_SETTER(loc) = eul_nil; \
                                                                                            object_size(loc) = object_size(bv)

#define eul_clone_lambda(loc, lambda)                                          \
    { int n;                                                                   \
        n = eul_int_as_c_int(object_size(lambda));                             \
        ALLOCATE_WARM_OBJECT(loc, PGLOBAL(glob_lambda_class), LAMBDA_SIZE);    \
        LAMBDA_NAME(loc) = LAMBDA_NAME(lambda);                                \
        LAMBDA_DOMAIN(loc) = LAMBDA_DOMAIN(lambda);                            \
        LAMBDA_ENV(loc) = LAMBDA_ENV(lambda);                                  \
        LAMBDA_CODE(loc) = (LispRef) gc_malloc(n);                             \
        strncpy((char *) LAMBDA_CODE(loc), (char *) LAMBDA_CODE(lambda), n);   \
        LAMBDA_SETTER(loc) = eul_nil;                                          \
        object_size(loc) = object_size(lambda);                                \
    }

///-----------------------------------------------------------------------------
/// Lambda execution
///-----------------------------------------------------------------------------

#define execute_lambda(lambda)                                                 \
    tame_regs->reg_value_stack->sp = tame_regs->reg_value_stack->base;         \
    tame_regs->reg_context_stack->sp = tame_regs->reg_context_stack->base;     \
    tame_regs->reg_arg_count = 0;                                              \
    tame_regs->reg_current_cv = (Instruction *) LAMBDA_CODE(lambda);           \
    tame_regs->reg_pc = tame_regs->reg_current_cv;                             \
    tame_regs->reg_env = LAMBDA_ENV(lambda);                                   \
    tame_regs->reg_next_methods = eul_nil;                                     \
    interpret(tame_regs);                                                      \
    fflush(stdout)

///-----------------------------------------------------------------------------
/// GF shape
///-----------------------------------------------------------------------------

#define GF_SIZE 9

#define GF_NAME(x) (slot_ref((x), 0))
#define GF_DOMAIN(x) (slot_ref((x), 1))
#define OPERATOR_SETTER(x) LAMBDA_SETTER(x)
#define GF_METHOD_CLASS(x) (slot_ref((x), 3))
#define GF_METHOD_KEYWORD(x)(slot_ref((x), 4))
#define GF_METHODS(x) (slot_ref((x), 5))
#define GF_LOOKUP_FN(x) (slot_ref((x), 6))
#define GF_DISC_FN(x) (slot_ref((x), 7))
#define GF_METHOD_CACHE(x) (slot_ref((x), 8))

#define eul_is_function(x) (eul_is_lambda(x) || eul_is_gf(x))
#define eul_is_simple_gf(x) (eul_class_of(x) == PGLOBAL(glob_gf_class))
#define eul_is_gf(x)                                                           \
    (eul_is_simple_gf(x) ||                                                    \
    (!is_immediate(x) &&                                                       \
    fpi_value(object_size(x)) >= GF_SIZE &&                                    \
    eul_is_lambda(GF_DISC_FN(x))))

///-----------------------------------------------------------------------------
/// Method shape
///-----------------------------------------------------------------------------

#define METHOD_SIZE (3)

#define METHOD_GF(m)        (slot_ref((m), 0))
#define METHOD_DOMAIN(m)    (slot_ref((m), 1))
#define METHOD_FUNCTION(m)  (slot_ref((m), 2))

///-----------------------------------------------------------------------------
#endif // OPERATOR_H
///-----------------------------------------------------------------------------
