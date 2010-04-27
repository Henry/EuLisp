/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: foreign function converters
///-----------------------------------------------------------------------------
#ifndef FF_H
#define FF_H

///-----------------------------------------------------------------------------
/// Foreign function argument conversion
///-----------------------------------------------------------------------------

#define FF_ARG_CONVERT(x, conv_index) FF_ARG_CONVERT##conv_index(x)

#define FF_ARG_CONVERT0(x) eul_int_as_c_int_checked(x)
#define FF_ARG_CONVERT1(x) eul_char_as_c_char_checked(x)
#define FF_ARG_CONVERT2(x) eul_double_as_c_double(x)
#define FF_ARG_CONVERT3(x) eul_string_as_c_string_checked(x)
#define FF_ARG_CONVERT4(x) ((void **) x)
#define FF_ARG_CONVERT5(x) eul_stream_as_c_fd(x)
#define FF_ARG_CONVERT6(x) eul_symbol_as_c_string_checked(x)
#define FF_ARG_CONVERT7(x) eul_bool_as_c_bool(x)
#define FF_ARG_CONVERT8(x) ((void *) x)
#define FF_ARG_CONVERT9(x) eul_int_ref_as_c_int_ref_checked(x)
#define FF_ARG_CONVERT10(x) eul_double_ref_as_c_double_ref_checked(x)
#define FF_ARG_CONVERT11(x) eul_string_ref_as_c_string_ref_checked(x)

///-----------------------------------------------------------------------------
/// Foreign function argument conversion error notification
///-----------------------------------------------------------------------------

#ifdef WITH_SIGNALS

#define ff_notify_arg_error(x, clazz)                                          \
    (fprintf(stderr, "\n*** WARNING [system]: bad foreign function argument"), \
    fprintf(stderr, "\n    value: "),                                          \
    fprint_ref(stderr, x),                                                     \
    fprintf(stderr, "\n    expected instance of class: %s\n", #clazz),         \
    fflush(stderr),                                                            \
    eul_signal = 1,                                                            \
    eul_signal_cb = CB_FIRST_SIGNAL+3)

#else

#define ff_notify_arg_error(x, clazz)                                          \
    (fprintf(stderr, "\n*** ERROR [system]: bad foreign function argument"),   \
    fprintf(stderr, "\n    value: "),                                          \
    fprint_ref(stderr, x),                                                     \
    fprintf(stderr, "\n    expected instance of class: %s\n", #clazz),         \
    fflush(stderr),                                                            \
    exit(-1))

#endif

#define eul_int_as_c_int_checked(x)                                            \
    (eul_is_int(x) ? eul_int_as_c_int(x) :                                     \
    (ff_notify_arg_error(x, <int>), 0))

#define eul_double_as_c_double_checked(x)                                      \
    (eul_is_double(x) ? eul_double_as_c_double(x) :                            \
    (ff_notify_arg_error(x, <double>), 0.0))

#define eul_char_as_c_char_checked(x)                                          \
    (eul_is_char(x) ? eul_char_as_c_char(x) :                                  \
    (ff_notify_arg_error(x, <character>), '\0'))

#define eul_string_as_c_string_checked(x)                                      \
    (eul_is_string(x) ? eul_string_as_c_string(x) :                            \
    (ff_notify_arg_error(x, <string>), ""))

#define eul_int_ref_as_c_int_ref_checked(x)                                    \
    (eul_is_int_ref(x) ? eul_int_ref_as_c_int_ref(x) :                         \
    (ff_notify_arg_error(x, <int*>), (int *)NULL))

#define eul_double_ref_as_c_double_ref_checked(x)                              \
    (eul_is_double_ref(x) ? eul_double_ref_as_c_double_ref(x) :                \
    (ff_notify_arg_error(x, <double*>), (double *)NULL))

#define eul_string_ref_as_c_string_ref_checked(x)                              \
    (eul_is_string_ref(x) ? eul_string_ref_as_c_string_ref(x) :                \
    (ff_notify_arg_error(x, <string*>), (char **)NULL))

///-----------------------------------------------------------------------------
/// Foreign function result conversion
///-----------------------------------------------------------------------------

#define FF_RES_CONVERT(loc, x, conv_index)                                     \
    FF_RES_CONVERT##conv_index(loc, x)

#define FF_RES_CONVERT0(loc, x) eul_allocate_int(loc, (int) x)
#define FF_RES_CONVERT1(loc, x) eul_allocate_char(loc, (char) x)
#define FF_RES_CONVERT2(loc, x) eul_allocate_double(loc, (double) x)
#define FF_RES_CONVERT3(loc, x) eul_allocate_string_checked(loc, (char *) x)
#define FF_RES_CONVERT4(loc, x) eul_intern_symbol_checked(loc, (char *) x)
#define FF_RES_CONVERT5(loc, x) loc = c_bool_as_eul_bool((bool) x)
#define FF_RES_CONVERT6(loc, x) loc = ((LispRef) x)
#define FF_RES_CONVERT7(loc, x) eul_allocate_int_ref(loc, x)
#define FF_RES_CONVERT8(loc, x) eul_allocate_double_ref(loc, x)
#define FF_RES_CONVERT9(loc, x) eul_allocate_string_ref(loc, x)
#define FF_RES_CONVERT10(loc, x) { x ; loc = eul_nil; }

///-----------------------------------------------------------------------------
/// Foreign function result conversion error notification
///-----------------------------------------------------------------------------

#define eul_allocate_string_checked(loc, x)                                    \
    {                                                                          \
        char *tmp_value;                                                       \
        tmp_value = (char *) (x);                                              \
        if (tmp_value != NULL)                                                 \
        {                                                                      \
            eul_allocate_string(loc, tmp_value);                               \
        }                                                                      \
        else                                                                   \
        {                                                                      \
            loc = eul_nil;                                                     \
        }                                                                      \
    }

#define eul_intern_symbol_checked(loc, x)                                      \
    {                                                                          \
        char *tmp_value;                                                       \
        tmp_value = (char *) (x);                                              \
        if (tmp_value!=NULL)                                                   \
        {                                                                      \
            eul_intern_symbol(loc, tmp_value);                                 \
        }                                                                      \
        else                                                                   \
        {                                                                      \
            loc = eul_nil;                                                     \
        }                                                                      \
    }

///-----------------------------------------------------------------------------
/// Foreign in-calls
///-----------------------------------------------------------------------------

#define EUL_DEFINTERN(c_fn_name, eul_fn_name, arity, eul_module_name)          \
    EUL_DEFINTERN##arity(c_fn_name, eul_fn_name, eul_module_name)

#define get_ff(fn_index, module_name)                                          \
    (module_name##_bindings[fn_index])

#define DEFINTERN_DECLARATIONS(c_fn_name, arity, eul_module_name)              \
    LispRef res;                                                               \
    LispRef fn = get_ff(c_fn_name##_fn_index, eul_module_name);                \
    INITIALISE_REGISTER_SET(tame_regs);                                        \
    tame_regs->reg_next_methods = eul_nil;                                     \
    if (!eul_is_lambda(fn))                                                    \
    {                                                                          \
        if (!eul_is_gf(fn))                                                    \
        {                                                                      \
            printf("\n*** ERROR [system]: bad foreign function in-call\n");    \
            fprint_ref(stdout, fn);                                            \
            exit(-1);                                                          \
        }                                                                      \
        else                                                                   \
        {                                                                      \
            EXTERNAL_FAST_METHOD_LOOKUP(fn, arity, 0, tame_regs->reg_next_methods); \
        }                                                                      \
    }                                                                          \
    tame_regs->reg_value_stack->sp = tame_regs->reg_value_stack->base;         \
    tame_regs->reg_context_stack->sp = tame_regs->reg_context_stack->base;     \
    tame_regs->reg_arg_count = arity;                                          \
    tame_regs->reg_current_cv = (Instruction *) LAMBDA_CODE(fn);               \
    tame_regs->reg_pc = tame_regs->reg_current_cv;                             \
    tame_regs->reg_env = LAMBDA_ENV(fn);

#define EXTERNAL_POPVAL1(v1) POP1(tame_regs->reg_value_stack->sp, v1)
#define EXTERNAL_PUSHVAL1(v1) PUSH1(tame_regs->reg_value_stack->sp, v1)

#define DEFINTERN_TAIL()                                                       \
    interpret(tame_regs);                                                      \
    EXTERNAL_POPVAL1(res);                                                     \
    return res;

///-----------------------------------------------------------------------------
/// Foreign in-call (arity 0)
///-----------------------------------------------------------------------------

#define EUL_DEFINTERN0(c_fn_name, eul_fn_name, eul_module_name)                \
    extern int eul_module_name##_index;                                        \
    LispRef c_fn_name ()                                                       \
    {                                                                          \
        DEFINTERN_DECLARATIONS(c_fn_name, 0, eul_module_name)                  \
        DEFINTERN_TAIL()                                                       \
        }

///-----------------------------------------------------------------------------
/// Foreign in-call (arity 1)
///-----------------------------------------------------------------------------

#define EUL_DEFINTERN1(c_fn_name, eul_fn_name, eul_module_name)                \
    extern int eul_module_name##_index;                                        \
    LispRef c_fn_name (LispRef c_arg1)                                         \
    {                                                                          \
        DEFINTERN_DECLARATIONS(c_fn_name, 1, eul_module_name)                  \
        EXTERNAL_PUSHVAL1(c_arg1);                                             \
        DEFINTERN_TAIL()                                                       \
        }

///-----------------------------------------------------------------------------
/// Foreign in-call (arity 2)
///-----------------------------------------------------------------------------

#define EUL_DEFINTERN2(c_fn_name, eul_fn_name, eul_module_name)                \
    extern int eul_module_name##_index;                                        \
    LispRef c_fn_name (LispRef c_arg1, LispRef c_arg2)                         \
    {                                                                          \
        DEFINTERN_DECLARATIONS(c_fn_name, 2, eul_module_name)                  \
        EXTERNAL_PUSHVAL1(c_arg1);                                             \
        EXTERNAL_PUSHVAL1(c_arg2);                                             \
        DEFINTERN_TAIL()                                                       \
        }

///-----------------------------------------------------------------------------
/// Foreign in-call (arity 3)
///-----------------------------------------------------------------------------

#define EUL_DEFINTERN3(c_fn_name, eul_fn_name, eul_module_name)                \
    extern int eul_module_name##_index;                                        \
    LispRef c_fn_name (LispRef c_arg1, LispRef c_arg2, LispRef c_arg3)         \
    {                                                                          \
        DEFINTERN_DECLARATIONS(c_fn_name, 3, eul_module_name)                  \
        EXTERNAL_PUSHVAL1(c_arg1);                                             \
        EXTERNAL_PUSHVAL1(c_arg2);                                             \
        EXTERNAL_PUSHVAL1(c_arg3);                                             \
        DEFINTERN_TAIL()                                                       \
        }

///-----------------------------------------------------------------------------
/// Foreign in-call (arity 4)
///-----------------------------------------------------------------------------

#define EUL_DEFINTERN4(c_fn_name, eul_fn_name, eul_module_name)                \
    extern int eul_module_name##_index;                                        \
    LispRef c_fn_name (LispRef c_arg1, LispRef c_arg2, LispRef c_arg3, LispRef c_arg4) \
    {                                                                          \
        DEFINTERN_DECLARATIONS(c_fn_name, 4, eul_module_name)                  \
        EXTERNAL_PUSHVAL1(c_arg1);                                             \
        EXTERNAL_PUSHVAL1(c_arg2);                                             \
        EXTERNAL_PUSHVAL1(c_arg3);                                             \
        EXTERNAL_PUSHVAL1(c_arg4);                                             \
        DEFINTERN_TAIL()                                                       \
        }

///-----------------------------------------------------------------------------
/// Foreign in-call (arity 5)
///-----------------------------------------------------------------------------

#define EUL_DEFINTERN5(c_fn_name, eul_fn_name, eul_module_name)                \
    extern int eul_module_name##_index;                                        \
    LispRef c_fn_name (LispRef c_arg1, LispRef c_arg2, LispRef c_arg3, LispRef c_arg4, LispRef c_arg5) \
    {                                                                          \
        DEFINTERN_DECLARATIONS(c_fn_name, 5, eul_module_name)                  \
        EXTERNAL_PUSHVAL1(c_arg1);                                             \
        EXTERNAL_PUSHVAL1(c_arg2);                                             \
        EXTERNAL_PUSHVAL1(c_arg3);                                             \
        EXTERNAL_PUSHVAL1(c_arg4);                                             \
        EXTERNAL_PUSHVAL1(c_arg5);                                             \
        DEFINTERN_TAIL()                                                       \
        }

///-----------------------------------------------------------------------------
/// Initialise and run the system
///-----------------------------------------------------------------------------

extern int EUL_INITIALIZE (int, char **);

#define EUL_IMPORT(module_name)                                                \
    initialize_module_##module_name();                                         \
    execute_lambda(module_name##_bindings[0]);

///-----------------------------------------------------------------------------
#endif // FF_H
///-----------------------------------------------------------------------------
