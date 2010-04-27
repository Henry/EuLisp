/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: instruction set of the bytecode interpreter
///-----------------------------------------------------------------------------
#ifndef BYTECODE_H
#define BYTECODE_H
///-----------------------------------------------------------------------------
/// Instruction case macro
///-----------------------------------------------------------------------------

#ifndef WITH_TRACE

#ifdef WITH_LABELS
#define BC_CASE(name)                                                          \
    BCL_##name :                                                               \
    {                                                                          \
        BCA_##name()                                                           \
        reg_current_op = *reg_pc;                                              \
        WHEN_INSTRUMENTED(++eul_executed_instructions; )                       \
        WHEN_INSTRUMENTED(++(eul_profiling_table[(ptrInt)reg_current_op]);)    \
        goto *label_table[reg_current_op];                                     \
    }

#else

#define BC_CASE(name)                                                          \
    case ((Instruction)BC_##name) :                                            \
    {                                                                          \
        BCA_##name()                                                           \
        break;                                                                 \
    }

#endif // WITH_LABELS

#else

int eul_trace=0;

#ifdef WITH_LABELS
#define BC_CASE(name)                                                          \
    BCL_##name :                                                               \
    {                                                                          \
        if (eul_trace)                                                         \
        {                                                                      \
            printf(">>%s [%d] \n", #name, (unsigned char) reg_current_op);     \
            fflush(stdout);                                                    \
        }                                                                      \
        {                                                                      \
            BCA_##name()                                                       \
            reg_current_op = *reg_pc;                                          \
            WHEN_INSTRUMENTED(++eul_executed_instructions; )                   \
            WHEN_INSTRUMENTED(++(eul_profiling_table[(ptrInt)reg_current_op]);) \
            goto *label_table[reg_current_op];                                 \
        }                                                                      \
    }

#else

#define BC_CASE(name)                                                          \
    case ((Instruction)BC_##name) :                                            \
    {                                                                          \
        if (eul_trace)                                                         \
        {                                                                      \
            printf(">>%s [%d] \n", #name, (unsigned char) reg_current_op);     \
            fflush(stdout);                                                    \
        }                                                                      \
        {                                                                      \
            BCA_##name()                                                       \
            break;                                                             \
        }                                                                      \
    }

#endif // WITH_LABELS
#endif // WITH_TRACE


///-----------------------------------------------------------------------------
/// Setup lambda context
///-----------------------------------------------------------------------------

#define SET_LAMBDA_CONTEXT()                                                   \
    reg_env = LAMBDA_ENV(reg_arg_operator);                                    \
    reg_current_cv = (Instruction *) LAMBDA_CODE(reg_arg_operator);            \
    reg_pc = reg_current_cv


///-----------------------------------------------------------------------------
/// Get inlined argruments
///-----------------------------------------------------------------------------

#define get_unsigned_bytearg() (*((unsigned char *)(++reg_pc)))
#define get_signed_bytearg() (*((signed char *)(++reg_pc)))

#if BYTE_ORDER == LITTLE_ENDIAN
#  define get_fixarg() get_fixarg_fn(&reg_pc, rel_reg_pc)
#else
#  define get_fixarg()                                                         \
      ((reg_pc += 3 + (4 - (rel_reg_pc%4))), *((ptrInt *)(reg_pc - 3)))
#endif

#define get_binding()                                                          \
    ((reg_pc += 3 + (4 - (rel_reg_pc%4))), **((LispRef **)(reg_pc - 3)));

#define set_binding(x)                                                         \
    ((reg_pc += 3 + (4 - (rel_reg_pc%4))), (**((LispRef **)(reg_pc - 3))) = x)


///-----------------------------------------------------------------------------
/// These variables are used in nearly all instruction definitions
///-----------------------------------------------------------------------------

#define DECLARE_BC_VARIABLES()                                                 \
    LispRef arg1, arg2, arg3, arg4; /* Stack-passed arguments */               \
    LispRef tmp1, tmp2, tmp3;       /* Intermediate results */                 \
    LispRef res;                    /* Final results */                        \
    unsigned char imm1;             /* Immediate arguments */                  \
    ptrInt fix1, fix2               /* Random ptrInts */


///-----------------------------------------------------------------------------
/// Define instruction NOP
///-----------------------------------------------------------------------------

#define BC_NOP 0
#define BCA_NOP()                                                              \
    ++reg_pc;                                                                  \
    PRINT_REG_PC("NOP");


///-----------------------------------------------------------------------------
/// Object manipulation
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (allocate-object) (class, fpi)->(obj)

#define BC_PRIMITIVE_ALLOCATE 1
#define BCA_PRIMITIVE_ALLOCATE()                                               \
    POPVAL1(arg2);                                                             \
    eul_allocate_object(res, LVPEEKVAL(), fpi_value(arg2), eul_nil);           \
    LVPEEKVAL() = res;                                                         \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (primitive-ref) (obj1, fpi)->(obj2)

#define BC_PRIMITIVE_REF 2
#define BCA_PRIMITIVE_REF()                                                    \
    POPVAL1(arg2);                                                             \
    CALLBACK_TRAP\
    (                                                                          \
        ((!is_immediate(LVPEEKVAL()))                                          \
     && (fpi_value(object_size(LVPEEKVAL())) > fpi_value(arg2))),              \
        PUSHVAL1(arg2); ,                                                      \
        CB_BAD_SLOT_ACCESS,                                                    \
        2                                                                      \
    )                                                                          \
    LVPEEKVAL() = slot_ref(LVPEEKVAL(), fpi_value(arg2));                      \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (primitive-relative-ref) (obj1, fpi, class)->(obj2)

#define BC_PRIMITIVE_RELATIVE_REF 8
#define BCA_PRIMITIVE_RELATIVE_REF()                                           \
    POPVAL1(arg3);                                                             \
    POPVAL1(arg2);                                                             \
    ptrInt i = fpi_value(slot_ref(arg3, 1)) - fpi_value(arg2) - 1;             \
    CALLBACK_TRAP                                                              \
    (                                                                          \
        (fpi_value(object_size(LVPEEKVAL())) > i),                             \
        PUSHVAL1(c_int_as_eul_int(i)); ,                                       \
        CB_BAD_SLOT_ACCESS,                                                    \
        2                                                                      \
    )                                                                          \
    LVPEEKVAL() = slot_ref(LVPEEKVAL(), i);                                    \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (set-primitive-ref) (obj1, fpi, ob2)->() **/

#define BC_SET_PRIMITIVE_REF 3
#define BCA_SET_PRIMITIVE_REF()                                                \
    POPVAL3(arg3, arg2, arg1);                                                 \
    CALLBACK_TRAP(((!is_immediate(arg1)) &&                                    \
    (fpi_value(object_size(arg1)) > fpi_value(arg2))),                         \
    {PUSHVAL1(arg1); PUSHVAL1(arg2); PUSHVAL1(arg3); },                        \
    CB_BAD_SLOT_ACCESS, 3)                                                     \
    slot_ref(arg1, fpi_value(arg2)) = arg3;                                    \
    PUSHVAL1(arg3);                                                            \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (set-primitive-relative-ref) (obj1, fpi, ob2, class)->(obj2) **/

#define BC_SET_PRIMITIVE_RELATIVE_REF 9
#define BCA_SET_PRIMITIVE_RELATIVE_REF()                                       \
    POPVAL1(arg4);                                                             \
    POPVAL3(arg3, arg2, arg1);                                                 \
    ptrInt i = (fpi_value(slot_ref(arg4, 1)) - fpi_value(arg2)) - 1;           \
    CALLBACK_TRAP((fpi_value(object_size(arg1)) > i),                          \
    {PUSHVAL1(arg1); PUSHVAL1(c_int_as_eul_int(i));                            \
        PUSHVAL1(arg3); }, CB_BAD_SLOT_ACCESS, 3)                              \
    slot_ref(arg1, i) = arg3;                                                  \
                      PUSHVAL1(arg3);                                          \
                      ++reg_pc;

///-----------------------------------------------------------------------------
///  (object-class) (obj)->(class) **/

#define BC_PRIMITIVE_CLASS_OF 4
#define BCA_PRIMITIVE_CLASS_OF()                                               \
    LVPEEKVAL() = eul_class_of(LVPEEKVAL());                                   \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (set-object-class) (obj, class)->() **/

#define BC_SET_PRIMITIVE_CLASS_OF 5
#define BCA_SET_PRIMITIVE_CLASS_OF()                                           \
    POPVAL2(arg2, arg1);                                                       \
    CALLBACK_TRAP(!is_immediate(arg1), PUSHVAL1(arg1); PUSHVAL1(arg2); ,       \
    CB_BAD_SET_CLASS_OF, 2)                                                    \
    object_class(arg1) = arg2;                                                 \
                       PUSHVAL1(arg2);                                         \
                       ++reg_pc;

///-----------------------------------------------------------------------------
///  (object-size) (obj)->(fpi) **/

#define BC_PRIMITIVE_SIZE 6
#define BCA_PRIMITIVE_SIZE()                                                   \
    LVPEEKVAL() = computed_object_size(LVPEEKVAL());                           \
    ++reg_pc;


///-----------------------------------------------------------------------------
/// String manipulation
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (string-ref) (str, fpi)->(char) **/

#define BC_STRING_REF 11
#define BCA_STRING_REF()                                                       \
    POPVAL1(arg2);                                                             \
    fix1 = fpi_value(arg2);                                                    \
    CALLBACK_TRAP((eul_is_string(LVPEEKVAL()) && eul_is_int(arg2) &&           \
    (fix1 < fpi_value(eul_string_size(LVPEEKVAL())))),                         \
    PUSHVAL1(arg2); , CB_BAD_STRING_ACCESS, 2)                                 \
    fix1 = (ptrInt) eul_string_ref(LVPEEKVAL(), fix1);                         \
    eul_allocate_char(LVPEEKVAL(), fix1);                                      \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (set-string-ref) (str, fpi, char)->(char) **/

#define BC_SET_STRING_REF 12
#define BCA_SET_STRING_REF()                                                   \
    POPVAL3(arg3, arg2, arg1);                                                 \
    fix1 = fpi_value(arg2);                                                    \
    CALLBACK_TRAP((eul_is_string(arg1) && eul_is_int(arg2) &&                  \
    eul_is_char(arg3) &&                                                       \
    (fix1 < fpi_value(eul_string_size(arg1)))),                                \
    PUSHVAL1(arg1); PUSHVAL1(arg2); PUSHVAL1(arg3); ,                          \
    CB_BAD_STRING_ACCESS, 3)                                                   \
    eul_string_ref(arg1, fix1) = eul_char_as_c_char(arg3);                     \
                               PUSHVAL1(arg3);                                 \
                               ++reg_pc;


///-----------------------------------------------------------------------------
/// List Manipulation
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (cons) (obj1, obj2)->(cons) **/

#define BC_THE_CONS 15
#define BCA_THE_CONS()                                                         \
    POPVAL1(arg2);                                                             \
    arg1 = LVPEEKVAL();                                                        \
    eul_allocate_cons(res, arg1, arg2);                                        \
    LVPEEKVAL() = res;                                                         \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (car) (cons)->(obj) **/

#define BC_THE_CAR 16
#define BCA_THE_CAR()                                                          \
    arg1 = LVPEEKVAL();                                                        \
    CALLBACK_TRAP(eul_is_cons(arg1), ,CB_FIRST_CONS+0, 1)                      \
    LVPEEKVAL() = eul_car(arg1);                                               \
    ++reg_pc;

#define BC_THE_CAR2 13
#define BCA_THE_CAR2()                                                         \
    /* Fast CommonLisp compatibility */                                        \
    if ((arg1 = LVPEEKVAL()) == eul_nil)                                       \
        ++reg_pc;                                                              \
    else                                                                       \
    {                                                                          \
        CALLBACK_TRAP(eul_is_cons(arg1), ,CB_FIRST_CONS+0, 1)                  \
        LVPEEKVAL() = eul_car(arg1);                                           \
        ++reg_pc;                                                              \
    }

///-----------------------------------------------------------------------------
///  (cdr) (cons)->(obj) **/

#define BC_THE_CDR 17
#define BCA_THE_CDR()                                                          \
    arg1 = LVPEEKVAL();                                                        \
    CALLBACK_TRAP(eul_is_cons(arg1), ,CB_FIRST_CONS+1, 1)                      \
    LVPEEKVAL() = eul_cdr(arg1);                                               \
    ++reg_pc;

#define BC_THE_CDR2 14
#define BCA_THE_CDR2()                                                         \
    /* Fast CommonLisp compatibility */                                        \
    if ((arg1 = LVPEEKVAL()) == eul_nil)                                       \
        ++reg_pc;                                                              \
    else                                                                       \
    {                                                                          \
        CALLBACK_TRAP(eul_is_cons(arg1), ,CB_FIRST_CONS+0, 1)                  \
        LVPEEKVAL() = eul_cdr(arg1);                                           \
        ++reg_pc;                                                              \
    }

///-----------------------------------------------------------------------------
///  (c*r) (cons)->(obj) **/

#define BC_THE_CAAR 114
#define BCA_THE_CAAR()                                                         \
    arg1 = LVPEEKVAL();                                                        \
    CALLBACK_TRAP(eul_is_cons(arg1), ,CB_FIRST_CONS+1, 1)                      \
    tmp1 = eul_car(arg1);                                                      \
    CALLBACK_TRAP(eul_is_cons(tmp1), LVPEEKVAL()=tmp1, CB_FIRST_CONS+1, 1)     \
    LVPEEKVAL() = eul_car(tmp1);                                               \
    ++reg_pc;

#define BC_THE_CADR 115
#define BCA_THE_CADR()                                                         \
    arg1 = LVPEEKVAL();                                                        \
    CALLBACK_TRAP(eul_is_cons(arg1), ,CB_FIRST_CONS+1, 1)                      \
    tmp1 = eul_cdr(arg1);                                                      \
    CALLBACK_TRAP(eul_is_cons(tmp1), LVPEEKVAL()=tmp1, CB_FIRST_CONS+1, 1)     \
    LVPEEKVAL() = eul_car(tmp1);                                               \
    ++reg_pc;

#define BC_THE_CDAR 116
#define BCA_THE_CDAR()                                                         \
    arg1 = LVPEEKVAL();                                                        \
    CALLBACK_TRAP(eul_is_cons(arg1), ,CB_FIRST_CONS+1, 1)                      \
    tmp1 = eul_car(arg1);                                                      \
    CALLBACK_TRAP(eul_is_cons(tmp1), LVPEEKVAL()=tmp1, CB_FIRST_CONS+1, 1)     \
    LVPEEKVAL() = eul_cdr(tmp1);                                               \
    ++reg_pc;

#define BC_THE_CDDR 117
#define BCA_THE_CDDR()                                                         \
    arg1 = LVPEEKVAL();                                                        \
    CALLBACK_TRAP(eul_is_cons(arg1), ,CB_FIRST_CONS+1, 1)                      \
    tmp1 = eul_cdr(arg1);                                                      \
    CALLBACK_TRAP(eul_is_cons(tmp1), LVPEEKVAL()=tmp1, CB_FIRST_CONS+1, 1)     \
    LVPEEKVAL() = eul_cdr(tmp1);                                               \
    ++reg_pc;

#define BC_THE_CADDR 118
#define BCA_THE_CADDR()                                                        \
    arg1 = LVPEEKVAL();                                                        \
    CALLBACK_TRAP(eul_is_cons(arg1), ,CB_FIRST_CONS+1, 1)                      \
    tmp1 = eul_cdr(arg1);                                                      \
    CALLBACK_TRAP(eul_is_cons(tmp1), LVPEEKVAL()=tmp1, CB_FIRST_CONS+1, 1)     \
    tmp2 = eul_cdr(tmp1);                                                      \
    CALLBACK_TRAP(eul_is_cons(tmp2), LVPEEKVAL()=tmp2, CB_FIRST_CONS+1, 1)     \
    LVPEEKVAL() = eul_car(tmp2);                                               \
    ++reg_pc;

#define BC_THE_CADDDR 119
#define BCA_THE_CADDDR()                                                       \
    arg1 = LVPEEKVAL();                                                        \
    CALLBACK_TRAP(eul_is_cons(arg1), ,CB_FIRST_CONS+1, 1)                      \
    tmp1 = eul_cdr(arg1);                                                      \
    CALLBACK_TRAP(eul_is_cons(tmp1), LVPEEKVAL()=tmp1, CB_FIRST_CONS+1, 1)     \
    tmp2 = eul_cdr(tmp1);                                                      \
    CALLBACK_TRAP(eul_is_cons(tmp2), LVPEEKVAL()=tmp2, CB_FIRST_CONS+1, 1)     \
    tmp3 = eul_cdr(tmp2);                                                      \
    CALLBACK_TRAP(eul_is_cons(tmp3), LVPEEKVAL()=tmp3, CB_FIRST_CONS+1, 1)     \
    LVPEEKVAL() = eul_car(tmp3);                                               \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (setter car) (cons, obj)->(obj) **/

#define BC_SET_CAR 143
#define BCA_SET_CAR()                                                          \
    POPVAL1(arg2);                                                             \
    arg1 = LVPEEKVAL();                                                        \
    CALLBACK_TRAP(eul_is_cons(arg1), PUSHVAL1(arg2), CB_FIRST_SETTER+3, 2)     \
    eul_car(arg1) = arg2;                                                      \
                  LVPEEKVAL() = arg2;                                          \
                  ++reg_pc;

///-----------------------------------------------------------------------------
///  (setter cdr) (cons, obj)->(obj) **/

#define BC_SET_CDR 144
#define BCA_SET_CDR()                                                          \
    POPVAL1(arg2);                                                             \
    arg1 = LVPEEKVAL();                                                        \
    CALLBACK_TRAP(eul_is_cons(arg1), PUSHVAL1(arg2), CB_FIRST_SETTER+4, 2)     \
    eul_cdr(arg1) = arg2;                                                      \
    LVPEEKVAL() = arg2;                                                        \
   ++reg_pc;


///-----------------------------------------------------------------------------
/// Class membership
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (null) (obj)->(obj) **/

#define BC_NULLP 18
#define BCA_NULLP()                                                            \
    LVPEEKVAL() = (eul_null(LVPEEKVAL()) ? eul_true : eul_nil);                \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (consp) (obj)->(obj) **/

#define BC_CONSP 122
#define BCA_CONSP()                                                            \
    if (!eul_is_cons(LVPEEKVAL())) LVPEEKVAL() = eul_nil;                      \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (listp) (obj)->(obj) **/

#define BC_LISTP 123
#define BCA_LISTP()                                                            \
    LVPEEKVAL() = (eul_is_list(LVPEEKVAL()) ? eul_true : eul_nil);             \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (symbolp) (obj)->(obj) **/

#define BC_SYMBOLP 124
#define BCA_SYMBOLP()                                                          \
    if (!eul_is_symbol(LVPEEKVAL())) LVPEEKVAL() = eul_nil;                    \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (stringp) (obj)->(obj) **/

#define BC_STRINGP 125
#define BCA_STRINGP()                                                          \
    if (!eul_is_string(LVPEEKVAL())) LVPEEKVAL() = eul_nil;                    \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (fpip) (obj)->(obj) **/

#define BC_FPIP 126
#define BCA_FPIP()                                                             \
    if (!eul_is_int(LVPEEKVAL())) LVPEEKVAL() = eul_nil;                       \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (lambdap) (obj)->(obj) **/

#define BC_LAMBDAP 127
#define BCA_LAMBDAP()                                                          \
    if (!eul_is_lambda(LVPEEKVAL())) LVPEEKVAL() = eul_nil;                    \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (gfp) (obj)->(obj) **/

#define BC_GFP 128
#define BCA_GFP()                                                              \
    if (!eul_is_gf(LVPEEKVAL())) LVPEEKVAL() = eul_nil;                        \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (characterp) (obj)->(obj) **/

#define BC_CHARACTERP 129
#define BCA_CHARACTERP()                                                       \
    if (!eul_is_char(LVPEEKVAL())) LVPEEKVAL() = eul_nil;                      \
    ++reg_pc;


///-----------------------------------------------------------------------------
/// Setter operations
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (setter) (obj)->(obj) **/

#define BC_SETTER 145
#define BCA_SETTER()                                                           \
    arg1 = LVPEEKVAL();                                                        \
    CALLBACK_TRAP(eul_is_function(arg1), ,CB_FIRST_SETTER+0, 1)                \
    LVPEEKVAL() = OPERATOR_SETTER(arg1);                                       \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (setter setter) (obj, obj)->(obj) **/

#define BC_SET_SETTER 146
#define BCA_SET_SETTER()                                                       \
    POPVAL1(arg2);                                                             \
    CALLBACK_TRAP(eul_is_function(arg2), PUSHVAL1(arg2), CB_FIRST_SETTER+1, 2) \
    arg1 = LVPEEKVAL();                                                        \
    CALLBACK_TRAP(eul_is_function(arg1), PUSHVAL1(arg2), CB_FIRST_SETTER+2, 2) \
    OPERATOR_SETTER(arg1) = arg2;                                              \
    LVPEEKVAL() = arg2;                                                        \
    ++reg_pc;


///-----------------------------------------------------------------------------
/// Fpi operations
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (fpi-sum) (fpi, fpi)->(fpi) **/

#define BC_FPI_SUM 20
#define BCA_FPI_SUM()                                                          \
    register LispRef res;                                                      \
    POPVAL1(arg2);                                                             \
    arg1 = LVPEEKVAL();                                                        \
    if (eul_is_int(arg1))                                                      \
    {                                                                          \
        if (eul_is_int(arg2))                                                  \
        {                                                                      \
            eul_allocate_int(res, fpi_value(arg1)+fpi_value(arg2));            \
            CALLBACK_TRAP(!fpi_value_overflow(res), PUSHVAL1(arg2), CB_SUM_OVERFLOW, 2) \
            }                                                                  \
        else if (eul_is_double(arg2))                                          \
        {                                                                      \
            eul_allocate_double(res, fpi_value(arg1)+eul_double_as_c_double(arg2)) \
        }                                                                      \
        else                                                                   \
        {                                                                      \
            CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+0, 2)              \
        }                                                                      \
    }                                                                          \
    else if (eul_is_double(arg1))                                              \
    {                                                                          \
        if (eul_is_int(arg2))                                                  \
        {                                                                      \
            eul_allocate_double(res, eul_double_as_c_double(arg1)+fpi_value(arg2)) \
       }                                                                       \
        else if (eul_is_double(arg2))                                          \
        {                                                                      \
            eul_allocate_double(res, eul_double_as_c_double(arg1)+             \
            eul_double_as_c_double(arg2))                                      \
        }                                                                      \
        else                                                                   \
        {                                                                      \
            CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+0, 2)              \
        }                                                                      \
    }                                                                          \
    else                                                                       \
    {                                                                          \
        CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+0, 2)                  \
    }                                                                          \
    LVPEEKVAL() = res;                                                         \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (fpi-difference) (fpi, fpi)->(fpi) **/

#define BC_FPI_DIFFERENCE 21
#define BCA_FPI_DIFFERENCE()                                                   \
    register LispRef res;                                                      \
    POPVAL1(arg2);                                                             \
    arg1 = LVPEEKVAL();                                                        \
    if (eul_is_int(arg1))                                                      \
        if (eul_is_int(arg2)) {                                                \
            eul_allocate_int(res, fpi_value(arg1)-fpi_value(arg2));            \
            CALLBACK_TRAP(!fpi_value_overflow(res), PUSHVAL1(arg2), CB_DIFFERENCE_OVERFLOW, 2) \
            }                                                                  \
        else if (eul_is_double(arg2))                                          \
            eul_allocate_double(res, fpi_value(arg1)-eul_double_as_c_double(arg2)) \
            else                                                               \
                CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+1, 2)          \
                else if (eul_is_double(arg1))                                  \
                    if (eul_is_int(arg2))                                      \
                        eul_allocate_double(res, eul_double_as_c_double(arg1)-fpi_value(arg2)) \
                        else if (eul_is_double(arg2))                          \
                            eul_allocate_double(res, eul_double_as_c_double(arg1)- \
                            eul_double_as_c_double(arg2))                      \
                            else                                               \
                                CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+1, 2) \
                                else                                           \
                                    CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+1, 2) \
                                    LVPEEKVAL() = res;                         \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (fpi-product) (fpi, fpi)->(fpi) **/

#define BC_FPI_PRODUCT 22
#define BCA_FPI_PRODUCT()                                                      \
    register LispRef res;                                                      \
    register double dbl_res;                                                   \
    POPVAL1(arg2);                                                             \
    arg1 = LVPEEKVAL();                                                        \
    if (eul_is_int(arg1))                                                      \
        if (eul_is_int(arg2)) {                                                \
            dbl_res = ((double)fpi_value(arg1))*((double)fpi_value(arg2));     \
            CALLBACK_TRAP(ABS(dbl_res)<=((double)MAX_FPI),                     \
            PUSHVAL1(arg2), CB_PRODUCT_OVERFLOW, 2)                            \
            eul_allocate_int(res, (ptrInt)dbl_res);                            \
        }                                                                      \
        else if (eul_is_double(arg2))                                          \
            eul_allocate_double(res, fpi_value(arg1)*eul_double_as_c_double(arg2)) \
            else                                                               \
                CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+2, 2)          \
                else if (eul_is_double(arg1))                                  \
                    if (eul_is_int(arg2))                                      \
                        eul_allocate_double(res, eul_double_as_c_double(arg1)*fpi_value(arg2)) \
                        else if (eul_is_double(arg2))                          \
                            eul_allocate_double(res, eul_double_as_c_double(arg1)* \
                            eul_double_as_c_double(arg2))                      \
                            else                                               \
                                CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+2, 2) \
                                else                                           \
                                    CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+2, 2) \
                                    LVPEEKVAL() = res;                         \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (fpi-quotient) (fpi, fpi)->(fpi) **/

#define BC_FPI_QUOTIENT 23
#define BCA_FPI_QUOTIENT()                                                     \
    register LispRef res;                                                      \
    POPVAL1(arg2);                                                             \
    arg1 = LVPEEKVAL();                                                        \
    if (eul_is_int(arg1))                                                      \
        if (eul_is_int(arg2)) {                                                \
            CALLBACK_TRAP(fpi_value(arg2), PUSHVAL1(arg2), CB_DIVISION_BY_ZERO, 2) \
            eul_allocate_int(res, fpi_value(arg1)/fpi_value(arg2));            \
        }                                                                      \
        else if (eul_is_double(arg2))                                          \
            eul_allocate_double(res, fpi_value(arg1)/eul_double_as_c_double(arg2)) \
            else                                                               \
                CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+3, 2)          \
                else if (eul_is_double(arg1))                                  \
                    if (eul_is_int(arg2))                                      \
                        eul_allocate_double(res, eul_double_as_c_double(arg1)/fpi_value(arg2)) \
                        else if (eul_is_double(arg2))                          \
                            eul_allocate_double(res, eul_double_as_c_double(arg1)/ \
                            eul_double_as_c_double(arg2))                      \
                            else                                               \
                                CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+3, 2) \
                                else                                           \
                                    CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+3, 2) \
                                    LVPEEKVAL() = res;                         \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (fpi-remainder) (fpi, fpi)->(fpi) **/

#define BC_FPI_REMAINDER 24
#define BCA_FPI_REMAINDER()                                                    \
    register LispRef res;                                                      \
    POPVAL1(arg2);                                                             \
    arg1 = LVPEEKVAL();                                                        \
    if (eul_is_int(arg1))                                                      \
        if (eul_is_int(arg2)) {                                                \
            CALLBACK_TRAP(fpi_value(arg2), PUSHVAL1(arg2), CB_DIVISION_BY_ZERO, 2) \
            eul_allocate_int(res, fpi_value(arg1)%fpi_value(arg2));            \
        }                                                                      \
        else if (eul_is_double(arg2))                                          \
            eul_allocate_double(res, fmod(fpi_value(arg1), eul_double_as_c_double(arg2))) \
            else                                                               \
                CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+4, 2)          \
                else if (eul_is_double(arg1))                                  \
                    if (eul_is_int(arg2))                                      \
                        eul_allocate_double(res, fmod(eul_double_as_c_double(arg1), fpi_value(arg2))) \
                        else if (eul_is_double(arg2))                          \
                            eul_allocate_double(res, fmod(eul_double_as_c_double(arg1), \
                            eul_double_as_c_double(arg2)))                     \
                            else                                               \
                                CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+4, 2) \
                                else                                           \
                                    CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+4, 2) \
                                    LVPEEKVAL() = res;                         \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (fpi-equal) (fpi, fpi)->(bool) **/

#define BC_FPI_EQUAL 25
#define BCA_FPI_EQUAL()                                                        \
    POPVAL1(arg2);                                                             \
    arg1 = LVPEEKVAL();                                                        \
    if (eul_is_int(arg1))                                                      \
        if (eul_is_int(arg2)) {                                                \
            /*       if (fpi_value(arg1) != fpi_value(arg2)) LVPEEKVAL() = eul_nil; */ \
            if (arg1 != arg2) LVPEEKVAL() = eul_nil;                           \
        }                                                                      \
        else if (eul_is_double(arg2)) {                                        \
            if (fpi_value(arg1) != eul_double_as_c_double(arg2))               \
                LVPEEKVAL() = eul_nil;                                         \
        }                                                                      \
        else                                                                   \
            CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+5, 2)              \
            else if (eul_is_double(arg1))                                      \
                if (eul_is_int(arg2)) {                                        \
                    if (eul_double_as_c_double(arg1) != fpi_value(arg2))       \
                        LVPEEKVAL() = eul_nil;                                 \
                }                                                              \
                else if (eul_is_double(arg2)) {                                \
                    if (eul_double_as_c_double(arg1) != eul_double_as_c_double(arg2)) \
                        LVPEEKVAL() = eul_nil;                                 \
                }                                                              \
                else                                                           \
                    CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+5, 2)      \
                    else                                                       \
                        CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+5, 2)  \
                        ++reg_pc;

///-----------------------------------------------------------------------------
///  (fpi-inc) (fpi)->(bool) **/

#define BC_FPI_LT 26
#define BCA_FPI_LT()                                                           \
    POPVAL1(arg2);                                                             \
    arg1 = LVPEEKVAL();                                                        \
    if (eul_is_int(arg1))                                                      \
        if (eul_is_int(arg2)) {                                                \
            if (!(fpi_value(arg1) < fpi_value(arg2))) LVPEEKVAL() = eul_nil;   \
        }                                                                      \
        else if (eul_is_double(arg2)) {                                        \
            if (!(fpi_value(arg1) < eul_double_as_c_double(arg2)))             \
                LVPEEKVAL() = eul_nil;                                         \
        }                                                                      \
        else                                                                   \
            CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+6, 2)              \
            else if (eul_is_double(arg1))                                      \
                if (eul_is_int(arg2)) {                                        \
                    if (!(eul_double_as_c_double(arg1) < fpi_value(arg2)))     \
                        LVPEEKVAL() = eul_nil;                                 \
                }                                                              \
                else if (eul_is_double(arg2)) {                                \
                    if (!(eul_double_as_c_double(arg1) < eul_double_as_c_double(arg2))) \
                        LVPEEKVAL() = eul_nil;                                 \
                }                                                              \
                else                                                           \
                    CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+6, 2)      \
                    else                                                       \
                        CALLBACK_TRAP(0, PUSHVAL1(arg2), CB_FIRST_ARITH+6, 2)  \
                        ++reg_pc;

///-----------------------------------------------------------------------------
///  (fpi-inc) (fpi)->(fpi) **/

#define BC_FPI_INC 43
#define BCA_FPI_INC()                                                          \
    register LispRef res;                                                      \
    arg1 = LVPEEKVAL();                                                        \
    if (eul_is_int(arg1)) {                                                    \
        eul_allocate_int(res, fpi_value(arg1)+1);                              \
        CALLBACK_TRAP(!fpi_value_overflow(res), ,CB_SUM_OVERFLOW, 1)           \
        }                                                                      \
    else if (eul_is_double(arg1))                                              \
        eul_allocate_double(res, eul_double_as_c_double(arg1)+1.0)             \
        else                                                                   \
            CALLBACK_TRAP(0, PUSHVAL1(c_int_as_eul_int(1)), CB_FIRST_ARITH+0, 2) \
            LVPEEKVAL() = res;                                                 \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (fpi-dec) (fpi)->(fpi) **/

#define BC_FPI_DEC 44
#define BCA_FPI_DEC()                                                          \
    register LispRef res;                                                      \
    arg1 = LVPEEKVAL();                                                        \
    if (eul_is_int(arg1)) {                                                    \
        eul_allocate_int(res, fpi_value(arg1)-1);                              \
        CALLBACK_TRAP(!fpi_value_overflow(res), ,CB_DIFFERENCE_OVERFLOW, 1)    \
        }                                                                      \
    else if (eul_is_double(arg1))                                              \
        eul_allocate_double(res, eul_double_as_c_double(arg1)-1.0)             \
        else                                                                   \
            CALLBACK_TRAP(0, PUSHVAL1(c_int_as_eul_int(1)), CB_FIRST_ARITH+1, 2) \
            LVPEEKVAL() = res;                                                 \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (fpi-zerop) (fpi)->(bool) **/

#define BC_FPI_ZEROP 45
#define BCA_FPI_ZEROP()                                                        \
    arg1 = LVPEEKVAL();                                                        \
    if (eul_is_int(arg1)) {                                                    \
        if (fpi_value(arg1) != 0) LVPEEKVAL() = eul_nil;                       \
    }                                                                          \
    else if (eul_is_double(arg1)) {                                            \
        if (eul_double_as_c_double(arg1) != 0.0)                               \
            LVPEEKVAL() = eul_nil;                                             \
    }                                                                          \
    else                                                                       \
        CALLBACK_TRAP(0, PUSHVAL1(c_int_as_eul_int(0)), CB_FIRST_ARITH+5, 2)   \
        ++reg_pc;


///-----------------------------------------------------------------------------
/// Value stack access
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (stack-ref0) ()->(obj) **/

#define BC_STACK_REF0 27
#define BCA_STACK_REF0()                                                       \
    REFVAL(0, tmp1);                                                           \
    PUSHVAL1(tmp1);                                                            \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (stack-ref1) ()->(obj) **/

#define BC_STACK_REF1 28
#define BCA_STACK_REF1()                                                       \
    REFVAL(1, tmp1);                                                           \
    PUSHVAL1(tmp1);                                                            \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (stack-ref2) ()->(obj) **/

#define BC_STACK_REF2 29
#define BCA_STACK_REF2()                                                       \
    PRINT_REG_PC("STACK_REF2");                                            \
    REFVAL(2, tmp1);                                                           \
    PRINT_REG_PC("STACK_REF2");                                            \
    PUSHVAL1(tmp1);                                                            \
    PRINT_REG_PC("STACK_REF2");                                            \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (stack-ref fpi) ()->(obj) **/

#define BC_STACK_REF 31
#define BCA_STACK_REF()                                                        \
    fix1 = (ptrInt) get_unsigned_bytearg();                                    \
    REFVAL(fix1, tmp1);                                                        \
    PUSHVAL1(tmp1);                                                            \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (swap) (o1, o2)->(o1, o2) **/

#define BC_SWAP 30
#define BCA_SWAP()                                                             \
    tmp1 = LVPEEKVAL();                                                        \
    REFVAL(1, LVPEEKVAL());                                                    \
    SET_REFVAL(1, tmp1);                                                       \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (value_stack_ref) (obj)->(obj) **/

#define BC_VALUE_STACK_REF 141
#define BCA_VALUE_STACK_REF()                                                  \
    arg1 = LVPEEKVAL(); /* index */                                            \
    REFVAL(fpi_value(arg1), LVPEEKVAL());                                      \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (set-stack-ref fix) (obj)->() **/

#define BC_SET_STACK_REF 32
#define BCA_SET_STACK_REF()                                                    \
    fix1 = (ptrInt) get_unsigned_bytearg();                                    \
    POPVAL1(tmp1);                                                             \
    SET_REFVAL(fix1-1, tmp1);                                                  \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (pop n) (obj*n)->() **/

#define BC_POP 33
#define BCA_POP()                                                              \
    fix1 = (ptrInt) get_unsigned_bytearg();                                    \
    POPVALN(fix1);                                                             \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (pop*) (obj*)->() **/

#define BC_POP1 42
#define BCA_POP1()                                                             \
    POPVALN(1);                                                                \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (nobble n) (obj*n, obj1)->(obj1) **/

#define BC_NOBBLE 34
#define BCA_NOBBLE()                                                           \
    fix1 = (ptrInt) get_unsigned_bytearg();                                    \
    tmp1 = LVPEEKVAL();                                                        \
    POPVALN(fix1);                                                             \
    LVPEEKVAL() = tmp1;                                                        \
    ++reg_pc;


///-----------------------------------------------------------------------------
/// Access to context stack
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (context_stack_ref) (obj)->(obj) **/

#define BC_CONTEXT_STACK_REF 140
#define BCA_CONTEXT_STACK_REF()                                                \
    {                                                                          \
        arg1 = LVPEEKVAL();                                                    \
        fix1 = fpi_value(arg1);                                                \
        REFCONTEXT(fix1+1, tmp1);                                              \
        LVPEEKVAL() = tmp1;                                                    \
        ++reg_pc;                                                              \
    }


///-----------------------------------------------------------------------------
/// Binding access
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (binding-ref fix) ()->(obj) **/

#define BC_BINDING_REF 36
#define BCA_BINDING_REF()                                                      \
    PRINT_REG_PC("BINDING_REF");                                           \
    tmp1 = get_binding();                                                      \
    PRINT_REG_PC("BINDING_REF");                                           \
    /* fprint_ref(stdout, tmp1); fflush(stdout); */                            \
    PUSHVAL1(tmp1);                                                            \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (set-binding-ref fix) (obj)->() **/

#define BC_SET_BINDING_REF 37
#define BCA_SET_BINDING_REF()                                                  \
    POPVAL1(tmp1);                                                             \
    PRINT_REG_PC("SET_BINDING_REF");                                       \
    set_binding(tmp1);                                                         \
    PRINT_REG_PC("SET_BINDING_REF");                                       \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (set-and-get-binding-ref fix) (obj)->(obj) **/

#define BC_SET_AND_GET_BINDING_REF 137
#define BCA_SET_AND_GET_BINDING_REF()                                          \
    PRINT_REG_PC("SET_AND_GET_BINDING_REF");                               \
    set_binding(LVPEEKVAL());                                                  \
    PRINT_REG_PC("SET_AND_GET_BINDING_REF");                               \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (static-ref*) ()->(obj) **/

#define BC_STATIC_REF0 130
#define BCA_STATIC_REF0()                                                      \
    PUSHVAL1(c_int_as_eul_int(0));                                             \
    ++reg_pc;

#define BC_STATIC_REF1 131
#define BCA_STATIC_REF1()                                                      \
    PUSHVAL1(c_int_as_eul_int(1));                                             \
    ++reg_pc;

#define BC_STATIC_REF2 132
#define BCA_STATIC_REF2()                                                      \
    PUSHVAL1(c_int_as_eul_int(2));                                             \
    ++reg_pc;

#define BC_STATIC_REF_1 133
#define BCA_STATIC_REF_1()                                                     \
    PUSHVAL1(c_int_as_eul_int(-1));                                            \
    ++reg_pc;

#define BC_STATIC_REF_NIL 134
#define BCA_STATIC_REF_NIL()                                                   \
    PUSHVAL1(eul_nil);                                                         \
    ++reg_pc;

#define BC_STATIC_REF_T 135
#define BCA_STATIC_REF_T()                                                     \
    PUSHVAL1(eul_true);                                                        \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (static-ref fix) ()->(obj) **/

#define BC_STATIC_REF 35
#define BCA_STATIC_REF() BCA_BINDING_REF()

///-----------------------------------------------------------------------------
///  (static-fpi-ref fix) ()->(obj) **/

#define BC_STATIC_FPI_REF 38
#define BCA_STATIC_FPI_REF()                                                   \
    fix1 = get_fixarg();                                                       \
    eul_allocate_int(tmp1, fix1);                                              \
    PUSHVAL1(tmp1);                                                            \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (static-fpi-byte-ref byte) ()->(obj) **/

#define BC_STATIC_FPI_BYTE_REF 138
#define BCA_STATIC_FPI_BYTE_REF()                                              \
    fix1 = (ptrInt) get_signed_bytearg();                                      \
    eul_allocate_int(tmp1, fix1);                                              \
    PUSHVAL1(tmp1);                                                            \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (static-character-ref byte) ()->(obj) **/

#define BC_STATIC_CHARACTER_REF 39
#define BCA_STATIC_CHARACTER_REF()                                             \
    fix1 = (ptrInt) get_unsigned_bytearg();                                    \
    eul_allocate_char(tmp1, fix1);                                             \
    PUSHVAL1(tmp1);                                                            \
    ++reg_pc;


///-----------------------------------------------------------------------------
/// Register codes
///-----------------------------------------------------------------------------

#define REG_CODE_ARGC            (22)
#define REG_CODE_ARGV            (23)
#define REG_CODE_FPI_CLASS        (2)
#define REG_CODE_BI_CLASS         (5)
#define REG_CODE_DOUBLE_CLASS     (6)
#define REG_CODE_CHAR_CLASS      (10)
#define REG_CODE_BYTEVECTOR_CLASS (3)
#define REG_CODE_NULL_CLASS      (25)
#define REG_CODE_CONS_CLASS       (4)
#define REG_CODE_VECTOR_CLASS    (26)
#define REG_CODE_TABLE_CLASS     (28)
#define REG_CODE_ENV_CLASS       (11)
#define REG_CODE_METHOD_CLASS    (13)
#define REG_CODE_STRING_CLASS    (15)
#define REG_CODE_SYMBOL_CLASS    (16)
#define REG_CODE_KEYWORD_CLASS   (20)
#define REG_CODE_LAMBDA_CLASS    (18)
#define REG_CODE_GENERIC_CLASS   (19)
#define REG_CODE_NEXT_METHODS    (24)
#define REG_CODE_CALLBACKS       (14)
#define REG_CODE_SYMBOLS         (17)
#define REG_CODE_KEYWORDS        (27)

#define REG_CODE_FPI_REF_CLASS    (40)
#define REG_CODE_DOUBLE_REF_CLASS (41)
#define REG_CODE_STRING_REF_CLASS (42)

/* #define REG_CODE_ARGC              0 */
/* #define REG_CODE_ARGV              1 */

/* #define REG_CODE_BYTEVECTOR_CLASS  2 */
/* #define REG_CODE_CHAR_CLASS        3 */
/* #define REG_CODE_CONS_CLASS        4 */
/* #define REG_CODE_DOUBLE_CLASS      5 */
/* #define REG_CODE_DOUBLE_REF_CLASS  6 */
/* #define REG_CODE_ENV_CLASS         7 */
/* #define REG_CODE_FPI_CLASS         8 */
/* #define REG_CODE_FPI_REF_CLASS     9 */
/* #define REG_CODE_GENERIC_CLASS    10 */
/* #define REG_CODE_KEYWORD_CLASS    11 */
/* #define REG_CODE_LAMBDA_CLASS     12 */
/* #define REG_CODE_METHOD_CLASS     13 */
/* #define REG_CODE_NULL_CLASS       14 */
/* #define REG_CODE_STRING_CLASS     15 */
/* #define REG_CODE_STRING_REF_CLASS 16 */
/* #define REG_CODE_SYMBOL_CLASS     17 */
/* #define REG_CODE_TABLE_CLASS      18 */
/* #define REG_CODE_VECTOR_CLASS     19 */

/* #define REG_CODE_CALLBACKS        20 */
/* #define REG_CODE_NEXT_METHODS     21 */
/* #define REG_CODE_KEYWORDS         22 */
/* #define REG_CODE_SYMBOLS          23 */


///-----------------------------------------------------------------------------
/// Register manipulation
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (register-ref byte) ()->(obj) **/

#define BC_REGISTER_REF 40
#define BCA_REGISTER_REF()                                                     \
    imm1 = get_unsigned_bytearg();                                             \
    switch (imm1) {                                                            \
        case REG_CODE_ARGC: PUSHVAL1(eul_argc);                                \
            break;                                                             \
        case REG_CODE_ARGV: PUSHVAL1(eul_argv);                                \
            break;                                                             \
        case REG_CODE_BYTEVECTOR_CLASS: PUSHVAL1(PGLOBAL(glob_bytevector_class)); \
            break;                                                             \
        case REG_CODE_CHAR_CLASS: PUSHVAL1(PGLOBAL(glob_char_class));          \
            break;                                                             \
        case REG_CODE_CONS_CLASS: PUSHVAL1(PGLOBAL(glob_cons_class));          \
            break;                                                             \
        case REG_CODE_DOUBLE_CLASS: PUSHVAL1(PGLOBAL(glob_double_class));      \
            break;                                                             \
        case REG_CODE_DOUBLE_REF_CLASS: PUSHVAL1(PGLOBAL(glob_double_ref_class)); \
            break;                                                             \
     /* case REG_CODE_ENV_CLASS: PUSHVAL1(PGLOBAL(glob_env_class)); break; */  \
        case REG_CODE_FPI_CLASS: PUSHVAL1(PGLOBAL(glob_fpi_class));            \
            break;                                                             \
        case REG_CODE_FPI_REF_CLASS: PUSHVAL1(PGLOBAL(glob_fpi_ref_class));    \
            break;                                                             \
        case REG_CODE_GENERIC_CLASS: PUSHVAL1(PGLOBAL(glob_gf_class));         \
            break;                                                             \
        case REG_CODE_KEYWORD_CLASS: PUSHVAL1(PGLOBAL(glob_keyword_class));    \
            break;                                                             \
        case REG_CODE_LAMBDA_CLASS: PUSHVAL1(PGLOBAL(glob_lambda_class));      \
            break;                                                             \
        case REG_CODE_METHOD_CLASS: PUSHVAL1(PGLOBAL(glob_method_class));      \
            break;                                                             \
        case REG_CODE_NULL_CLASS: PUSHVAL1(PGLOBAL(glob_null_class));          \
            break;                                                             \
        case REG_CODE_STRING_CLASS: PUSHVAL1(PGLOBAL(glob_string_class));      \
            break;                                                             \
        case REG_CODE_STRING_REF_CLASS: PUSHVAL1(PGLOBAL(glob_string_ref_class)); \
            break;                                                             \
        case REG_CODE_SYMBOL_CLASS: PUSHVAL1(PGLOBAL(glob_symbol_class));      \
            break;                                                             \
        case REG_CODE_TABLE_CLASS: PUSHVAL1(PGLOBAL(glob_table_class));        \
            break;                                                             \
        case REG_CODE_VECTOR_CLASS: PUSHVAL1(PGLOBAL(glob_vector_class));      \
            break;                                                             \
        case REG_CODE_CALLBACKS: PUSHVAL1(PGLOBAL(glob_callbacks));            \
            break;                                                             \
        case REG_CODE_NEXT_METHODS: PUSHVAL1(reg_next_methods);                \
            break;                                                             \
        case REG_CODE_KEYWORDS: PUSHVAL1(eul_keywords);                        \
            break;                                                             \
        case REG_CODE_SYMBOLS: PUSHVAL1(eul_symbols);                          \
            break;                                                             \
        default: SERIOUS_WARNING1("unknown register %d", imm1);                \
            goto exit;                                                         \
    }                                                                          \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (set-register byte) (obj)->() **/

#define BC_SET_REGISTER_REF 41
#define BCA_SET_REGISTER_REF()                                                 \
    imm1 = get_unsigned_bytearg();                                             \
    switch (imm1) {                                                            \
        case REG_CODE_NEXT_METHODS: POPVAL1(reg_next_methods); break;          \
        default: SERIOUS_WARNING1("unknown register %d", imm1); goto exit;     \
    }                                                                          \
    ++reg_pc;


///-----------------------------------------------------------------------------
/// Branching
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (branch fix) ()->() **/

#define BC_BRANCH 50
#define BCA_BRANCH()                                                           \
    Instruction *save_pc = reg_pc;                                             \
    WHEN_DB(printf("reg_pc 1: %" ptrIntPM "x \n", (ptrInt)reg_pc); )           \
    fix1 = get_fixarg();                                                       \
    WHEN_DB(printf("reg_pc 2: %" ptrIntPM "x \n", (ptrInt)reg_pc); )           \
    NOTIFY1("{BRANCHING: %" ptrIntPM "d}", fix1);                              \
    reg_pc = save_pc + fix1;                                                   \
    WHEN_DB(printf("reg_pc 3: %" ptrIntPM "x \n", (ptrInt)reg_pc); )

///-----------------------------------------------------------------------------
///  (branch-long-neg byte) ()->() **/

#define BC_BRANCH_LONG_NEG 75
#define BCA_BRANCH_LONG_NEG()                                                  \
    fix1 = (ptrInt) get_unsigned_bytearg();                                    \
    NOTIFY1("{BRANCHING: %" ptrIntPM "d}", -fix1 - 256);                       \
    reg_pc -= fix1 + 256;

///-----------------------------------------------------------------------------
///  (branch-neg byte) ()->() **/

#define BC_BRANCH_NEG 53
#define BCA_BRANCH_NEG()                                                       \
    fix1 = (ptrInt) get_unsigned_bytearg();                                    \
    NOTIFY1("{BRANCHING: %" ptrIntPM "d}", -fix1);                             \
    reg_pc -= fix1;

///-----------------------------------------------------------------------------
///  (branch-pos byte) ()->() **/

#define BC_BRANCH_POS 54
#define BCA_BRANCH_POS()                                                       \
    fix1 = (ptrInt) get_unsigned_bytearg();                                    \
    NOTIFY1("{BRANCHING: %" ptrIntPM "d}", fix1);                              \
    reg_pc += fix1;

///-----------------------------------------------------------------------------
///  (branch-long-pos byte) ()->() **/

#define BC_BRANCH_LONG_POS 57
#define BCA_BRANCH_LONG_POS()                                                  \
    fix1 = (ptrInt) get_unsigned_bytearg();                                    \
    NOTIFY1("{BRANCHING: %" ptrIntPM "d}", fix1 + 256);                        \
    reg_pc += fix1 + 256;

///-----------------------------------------------------------------------------
///  (branch-true fix) (bool)->() **/

#define BC_BRANCH_TRUE 51
#define BCA_BRANCH_TRUE()                                                      \
    POPVAL1(arg1);                                                             \
    if (!eul_null(arg1)) {BCA_BRANCH()}                                        \
    else                                                                       \
    {                                                                          \
        NOTIFY0("{STAYING}");                                                  \
        (void) get_fixarg();                                                   \
        ++reg_pc;                                                              \
    }

///-----------------------------------------------------------------------------
///  (branch-true-long-neg byte) (bool)->() **/

#define BC_BRANCH_TRUE_LONG_NEG 76
#define BCA_BRANCH_TRUE_LONG_NEG()                                             \
    POPVAL1(arg1);                                                             \
    if (!eul_null(arg1)) {BCA_BRANCH_LONG_NEG()}                               \
    else                                                                       \
    {                                                                          \
        NOTIFY0("{STAYING}");                                                  \
        reg_pc = reg_pc+2;                                                     \
    }

///-----------------------------------------------------------------------------
///  (branch-true-neg byte) (bool)->() **/

#define BC_BRANCH_TRUE_NEG 58
#define BCA_BRANCH_TRUE_NEG()                                                  \
    POPVAL1(arg1);                                                             \
    if (!eul_null(arg1)) {BCA_BRANCH_NEG()}                                    \
    else                                                                       \
    {                                                                          \
        NOTIFY0("{STAYING}");                                                  \
        reg_pc = reg_pc+2;                                                     \
    }

///-----------------------------------------------------------------------------
///  (branch-true-pos byte) (bool)->() **/

#define BC_BRANCH_TRUE_POS 62
#define BCA_BRANCH_TRUE_POS()                                                  \
    POPVAL1(arg1);                                                             \
    if (!eul_null(arg1)) {BCA_BRANCH_POS()}                                    \
    else                                                                       \
    {                                                                          \
        NOTIFY0("{STAYING}");                                                  \
        reg_pc = reg_pc+2;                                                     \
    }

///-----------------------------------------------------------------------------
///  (branch-true-long-pos byte) (bool)->() **/

#define BC_BRANCH_TRUE_LONG_POS 63
#define BCA_BRANCH_TRUE_LONG_POS()                                             \
    POPVAL1(arg1);                                                             \
    if (!eul_null(arg1)) {BCA_BRANCH_LONG_POS()}                               \
    else                                                                       \
    {                                                                          \
        NOTIFY0("{STAYING}");                                                  \
        reg_pc = reg_pc+2;                                                     \
    }

///-----------------------------------------------------------------------------
///  (branch-nil fix) (bool)->() **/

#define BC_BRANCH_NIL 52
#define BCA_BRANCH_NIL()                                                       \
    POPVAL1(arg1);                                                             \
    WHEN_DB(printf("reg_pc 0: %" ptrIntPM "x \n", (ptrInt)reg_pc); )           \
    if (eul_null(arg1)) {BCA_BRANCH()}                                         \
    else                                                                       \
    {                                                                          \
        NOTIFY0("{STAYING}");                                                  \
        (void) get_fixarg();                                                   \
        ++reg_pc;                                                              \
    }

///-----------------------------------------------------------------------------
///  (branch-nil-long-neg byte) (bool)->() **/

#define BC_BRANCH_NIL_LONG_NEG 77
#define BCA_BRANCH_NIL_LONG_NEG()                                              \
    POPVAL1(arg1);                                                             \
    if (eul_null(arg1)) {BCA_BRANCH_LONG_NEG()}                                \
    else                                                                       \
    {                                                                          \
        NOTIFY0("{STAYING}");                                                  \
        reg_pc = reg_pc+2;                                                     \
    }

///-----------------------------------------------------------------------------
///  (branch-nil-neg byte) (bool)->() **/

#define BC_BRANCH_NIL_NEG 64
#define BCA_BRANCH_NIL_NEG()                                                   \
    POPVAL1(arg1);                                                             \
    if (eul_null(arg1)) {BCA_BRANCH_NEG()}                                     \
    else                                                                       \
    {                                                                          \
        NOTIFY0("{STAYING}");                                                  \
        reg_pc = reg_pc+2;                                                     \
    }

///-----------------------------------------------------------------------------
///  (branch-nil-pos byte) (bool)->() **/

#define BC_BRANCH_NIL_POS 68
#define BCA_BRANCH_NIL_POS()                                                   \
    POPVAL1(arg1);                                                             \
    if (eul_null(arg1)) {BCA_BRANCH_POS()}                                     \
    else                                                                       \
    {                                                                          \
        NOTIFY0("{STAYING}");                                                  \
        reg_pc = reg_pc+2;                                                     \
    }

///-----------------------------------------------------------------------------
///  (branch-nil-long-pos byte) (bool)->() **/

#define BC_BRANCH_NIL_LONG_POS 74
#define BCA_BRANCH_NIL_LONG_POS()                                              \
    POPVAL1(arg1);                                                             \
    if (eul_null(arg1)) {BCA_BRANCH_LONG_POS()}                                \
    else                                                                       \
    {                                                                          \
        NOTIFY0("{STAYING}");                                                  \
        reg_pc = reg_pc+2;                                                     \
    }


///-----------------------------------------------------------------------------
/// Calling
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (call-next-method byte) **/

#define BC_CALL_NEXT_METHOD 55
#define BCA_CALL_NEXT_METHOD()                                                 \
    fix1 = (ptrInt) get_unsigned_bytearg();                                    \
    PUSHCONTEXT();                                                             \
    reg_arg_count = fix1;                                                      \
tail_c_n_m_entry_point:                                                        \
CALLBACK_TRAP(reg_next_methods != eul_nil,                                     \
PUSHVAL1(reg_arg_operator), CB_NO_NEXT_METHOD, fix1+1)                         \
reg_arg_operator = eul_car(reg_next_methods);                                  \
reg_next_methods = eul_cdr(reg_next_methods);                                  \
SET_LAMBDA_CONTEXT();

///-----------------------------------------------------------------------------
///  (tail-call-next-method byte byte) **/

#define BC_TAIL_CALL_NEXT_METHOD 56
#define BCA_TAIL_CALL_NEXT_METHOD()                                            \
    reg_arg_count = (int) get_unsigned_bytearg();                              \
    fix1 = (ptrInt) get_unsigned_bytearg();                                    \
    POPVALN(fix1);                                                             \
    goto tail_c_n_m_entry_point;

///-----------------------------------------------------------------------------

#define BC_MAKE_LAMBDA 59
#define BCA_MAKE_LAMBDA()                                                      \
    PRINT_REG_PC("MAKE_LAMBDA");                                           \
    POPVAL1(arg2);  /* bv */                                                   \
    PRINT_REG_PC("MAKE_LAMBDA");                                           \
    arg1 = LVPEEKVAL(); /* name */                                             \
    PRINT_REG_PC("MAKE_LAMBDA");                                           \
    fix1 = get_signed_bytearg();  /* arity */                                  \
    PRINT_REG_PC("MAKE_LAMBDA");                                           \
    eul_allocate_lambda1(res, arg1, c_int_as_eul_int(fix1), arg2);             \
    PRINT_REG_PC("MAKE_LAMBDA");                                           \
    LAMBDA_ENV(res) = reg_env;                                                 \
                    PRINT_REG_PC("MAKE_LAMBDA");                           \
                    LVPEEKVAL() = res;                                         \
                    PRINT_REG_PC("MAKE_LAMBDA");                           \
                    ++reg_pc;

///-----------------------------------------------------------------------------
///  (call-operator byte) (arg* op)->(result) **/

#define BC_CALL_OPERATOR 60
#define BCA_CALL_OPERATOR()                                                    \
    WHEN_DB(ptrInt entry = 1; )                                                \
    /* Get number of actual parameters */                                      \
    PRINT_REG_PC("CALL_OPERATOR1");                                            \
    reg_arg_count = (int) get_unsigned_bytearg();                              \
    POPVAL1(reg_arg_operator);                                                 \
callback_entry_point:                                                          \
    PRINT_REG_PC("CALL_OPERATOR2");                                            \
    WHEN_DB(entry++; )                                                         \
    PRINT_REG_PC("CALL_OPERATOR3");                                            \
    PUSHCONTEXT();                                                             \
    PRINT_REG_PC("CALL_OPERATOR4");                                            \
tail_call_entry_point:                                                         \
    WHEN_DB(entry++; )                                                         \
    WHEN_DB(PRINT_OPERATOR_CALL(entry); )                                      \
    WHEN_DB(entry = 0; )                                                       \
    /* Check for signals */                                                    \
    CALLBACK_TRAP(eul_no_signal(),                                             \
    eul_clear_signal(); PUSHVAL1(reg_arg_operator),                            \
    eul_signal_cb, reg_arg_count+1)                                            \
    /* Is it lambda call ? */                                                  \
    WHEN_DB(printf("CALL LAMBDA OR GF ? \n"); )                                \
    if (eul_is_lambda(reg_arg_operator))                                       \
    {                                                                          \
        WHEN_DB(printf("{CALL LAMBDA} \n"); )                                  \
        TRACE_LAMBDA();                                                        \
        SET_LAMBDA_CONTEXT();                                                  \
        PRINT_REG_PC("CALL_OPERATOR5");                                        \
        PGLOBAL(glob_encl_lambda) = reg_arg_operator;                          \
        PRINT_REG_PC("CALL_OPERATOR6");                                        \
    }                                                                          \
    else                                                                       \
    {                                                                          \
        CALLBACK_TRAP(eul_is_gf(reg_arg_operator), PUSHVAL1(reg_arg_operator), \
        CB_BAD_OPERATOR, reg_arg_count+1)                                      \
        WHEN_DB                                                                \
        (                                                                      \
            printf("CALL GF %s\n",                                             \
            eul_symbol_as_c_string(LAMBDA_NAME(reg_arg_operator)));            \
        )                                                                      \
        WHEN_INSTRUMENTED                                                      \
        (                                                                      \
            eul_gf_calls++;                                                    \
            if (reg_arg_count==1)                                              \
                eul_gf_calls1++;                                               \
            else if (reg_arg_count==2)                                         \
                eul_gf_calls2++;                                               \
            else if (reg_arg_count==3)                                         \
                eul_gf_calls3++;                                               \
            else                                                               \
                eul_gf_calls4++;                                               \
        )                                                                      \
        /* FULL_METHOD_LOOKUP(reg_arg_operator, 0); */                         \
        /* SLOW_METHOD_LOOKUP(reg_arg_operator, reg_next_methods); */          \
        FAST_METHOD_LOOKUP(reg_arg_operator, reg_pc, reg_next_methods);        \
        SET_LAMBDA_CONTEXT();                                                  \
    }


///-----------------------------------------------------------------------------
///  (tail-call-operator byte byte) (arg* op)->(result) **/

#define BC_TAIL_CALL_OPERATOR 61
#define BCA_TAIL_CALL_OPERATOR()                                               \
    POPVAL1(reg_arg_operator);                                                 \
    reg_arg_count = (int) get_unsigned_bytearg();  /* nargs */                 \
    fix1 = (ptrInt) get_unsigned_bytearg();  /* to pop */                      \
    /* Copy the real arguments down the stack */                               \
    for (int i=0; i<reg_arg_count; ++i) {                                      \
        REFVAL(reg_arg_count-i-1, tmp1);                                       \
        SET_REFVAL(fix1+reg_arg_count-i-1, tmp1);                              \
    }                                                                          \
    POPVALN(fix1);                                                             \
    goto tail_call_entry_point;

///-----------------------------------------------------------------------------
///  (apply) (list args rest)->(result) **/

#define BC_APPLY 142
#define BCA_APPLY()                                                            \
    LispRef args, rest = 0;                                                    \
    reg_arg_count = 0;                                                         \
    POPVAL1(args);                                                             \
    POPVAL1(reg_arg_operator);                                                 \
    while (args != eul_nil) {                                                  \
        if (eul_cdr(args) == eul_nil) {                                        \
            rest = eul_car(args);                                              \
            break;                                                             \
        }                                                                      \
        else                                                                   \
        {                                                                      \
            reg_arg_count++;                                                   \
            PUSHVAL1(eul_car(args));                                           \
            args = eul_cdr(args);                                              \
        }                                                                      \
    }                                                                          \
    CALLBACK_TRAP(eul_is_list(rest),                                           \
    {POPVALN(reg_arg_count); PUSHVAL1(reg_arg_operator); },                    \
    CB_FIRST_ERROR+2, 1)                                                       \
    while (rest != eul_nil) {                                                  \
        reg_arg_count++;                                                       \
        PUSHVAL1(eul_car(rest));                                               \
        rest = eul_cdr(rest);                                                  \
    }                                                                          \
    goto callback_entry_point;

///-----------------------------------------------------------------------------
///  (check-arguments nargs) ()->() **/

#define ABS(x) (x < 0 ? -x : x)

#define BC_CHECK_ARGUMENTS 67
#define BCA_CHECK_ARGUMENTS()                                                  \
    fix1 = (ptrInt) get_signed_bytearg(); /* expected nargs */                 \
    if (reg_arg_count != fix1) {                                               \
        /* Either listify or just plain wrong */                               \
        if ((fix1 < 0) && ((fix2 = ABS(fix1) - 1) <= reg_arg_count)) {         \
            LISTIFY(reg_arg_count - fix2); }                                   \
        else                                                                   \
        {                                                                      \
            CALLBACK_TRAP(0, PUSHVAL1(reg_arg_operator),                       \
            CB_FIRST_ERROR+0, reg_arg_count+1); } }                            \
    ++reg_pc;

#define BC_CHECK_ARGUMENTS_2 167
#define BCA_CHECK_ARGUMENTS_2()                                                \
    if (1 <= reg_arg_count) {                                                  \
        LISTIFY(reg_arg_count - 1); }                                          \
    else                                                                       \
    {                                                                          \
        CALLBACK_TRAP(0, PUSHVAL1(reg_arg_operator),                           \
        CB_FIRST_ERROR+0, reg_arg_count+1); }                                  \
    ++reg_pc;

#define BC_CHECK_ARGUMENTS_1 168
#define BCA_CHECK_ARGUMENTS_1()                                                \
    LISTIFY(reg_arg_count);                                                    \
    ++reg_pc;

#define BC_CHECK_ARGUMENTS0 169
#define BCA_CHECK_ARGUMENTS0()                                                 \
    if (reg_arg_count !=0)                                                     \
        CALLBACK_TRAP(0, PUSHVAL1(reg_arg_operator),                           \
        CB_FIRST_ERROR+0, reg_arg_count+1);                                    \
    ++reg_pc;

#define BC_CHECK_ARGUMENTS1 170
#define BCA_CHECK_ARGUMENTS1()                                                 \
    if (reg_arg_count != 1)                                                    \
        CALLBACK_TRAP(0, PUSHVAL1(reg_arg_operator),                           \
        CB_FIRST_ERROR+0, reg_arg_count+1);                                    \
    ++reg_pc;

#define BC_CHECK_ARGUMENTS2 171
#define BCA_CHECK_ARGUMENTS2()                                                 \
    if (reg_arg_count != 2)                                                    \
        CALLBACK_TRAP(0, PUSHVAL1(reg_arg_operator),                           \
        CB_FIRST_ERROR+0, reg_arg_count+1);                                    \
    ++reg_pc;


///-----------------------------------------------------------------------------
/// Foreign function call
///-----------------------------------------------------------------------------

#define BC_CALL_FOREIGN_FUNCTION 65
#define BCA_CALL_FOREIGN_FUNCTION()                                            \
    {                                                                          \
        LispRef (*ff_stub_fn) (Stack *, LispRef *, LispRef *) =                \
            (LispRef (*) (Stack *, LispRef *, LispRef *)) get_binding();       \
        LispRef res =                                                          \
            ff_stub_fn(reg_value_stack, sreg_value_sp, sreg_value_sb);         \
        /* Check for signals */                                                \
        CALLBACK_TRAP                                                          \
        (                                                                      \
            eul_no_signal(),                                                   \
            eul_clear_signal(); PUSHVAL1(reg_arg_operator);,                   \
            eul_signal_cb,                                                     \
            1                                                                  \
        )                                                                      \
        PUSHVAL1(res);                                                         \
        ++reg_pc;                                                              \
    }


///-----------------------------------------------------------------------------
/// Write object (only used for booting)
///-----------------------------------------------------------------------------

#define BC_WRITE_OBJECT 66

#define EUL_STDOUT (1)
#define EUL_STDERR (2)

#define BCA_WRITE_OBJECT()                                                     \
    POPVAL1(arg1);                                                             \
    fix1 = fpi_value(arg1);                                                    \
    fprint_ref(((fix1 == EUL_STDERR) ? stderr : stdout), LVPEEKVAL());         \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (return n) (arg*n ret)->(ret) **/

#define BC_RETURN 69
#define BCA_RETURN()                                                           \
    fix1 = (ptrInt) get_unsigned_bytearg();                                    \
    res = LVPEEKVAL();                                                         \
    POPVALN(fix1);                                                             \
    LVPEEKVAL() = res;                                                         \
    POPCONTEXT();

#define BC_RETURN0 172
#define BCA_RETURN0()                                                          \
    POPCONTEXT();

#define BC_RETURN1 173
#define BCA_RETURN1()                                                          \
    res = LVPEEKVAL();                                                         \
    POPVALN(1);                                                                \
    LVPEEKVAL() = res;                                                         \
    POPCONTEXT();

#define BC_RETURN2 174
#define BCA_RETURN2()                                                          \
    res = LVPEEKVAL();                                                         \
    POPVALN(2);                                                                \
    LVPEEKVAL() = res;                                                         \
    POPCONTEXT();


///-----------------------------------------------------------------------------
/// Displays (hosting captured variables)
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (alloc byte) ()->() **/

#define BC_ALLOC 70
#define BCA_ALLOC()                                                            \
    fix1 = (ptrInt) get_unsigned_bytearg(); /* size */                         \
    eul_allocate_object(res, PGLOBAL(glob_vector_class), fix1+1, eul_nil);     \
    slot_ref(res, 0) = reg_env;                                                \
    reg_env = res;                                                             \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (dealloc) ()->() **/

#define BC_DEALLOC 73
#define BCA_DEALLOC()                                                          \
    reg_env = slot_ref(reg_env, 0);                                            \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (display-ref byte byte) ()->(obj) **/

#define BC_DISPLAY_REF 71
#define BCA_DISPLAY_REF()                                                      \
    fix1 = (ptrInt) get_unsigned_bytearg(); /* display index*/                 \
    fix2 = (ptrInt) get_unsigned_bytearg(); /* offset */                       \
    if (fix1 == 0) {                                                           \
        PUSHVAL1(slot_ref(reg_env, fix2+1)); }                                 \
    else                                                                       \
    {                                                                          \
        LispRef env = slot_ref(reg_env, 0);                                    \
        while (--fix1) env = slot_ref(env, 0);                                 \
        PUSHVAL1(slot_ref(env, fix2+1)); }                                     \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (set-display-ref byte byte) (obj)->() **/

#define BC_SET_DISPLAY_REF 72
#define BCA_SET_DISPLAY_REF()                                                  \
    fix1 = (ptrInt) get_unsigned_bytearg(); /* display index*/                 \
    fix2 = (ptrInt) get_unsigned_bytearg(); /* offset */                       \
    if (fix1 == 0) {                                                           \
        POPVAL1(slot_ref(reg_env, fix2+1)); }                                  \
    else                                                                       \
    {                                                                          \
        LispRef env = slot_ref(reg_env, 0);                                    \
        while (--fix1) env = slot_ref(env, 0);                                 \
        POPVAL1(slot_ref(env, fix2+1)); }                                      \
    ++reg_pc;


///-----------------------------------------------------------------------------
/// Lock
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (test-and-set-lock) (lock)->(obj) **/

#define BC_TEST_AND_SET_LOCK 7
#define BCA_TEST_AND_SET_LOCK()                                                \
    tmp1 = LVPEEKVAL();                                                        \
    if (fpi_value(slot_ref(tmp1, 0)))                                          \
        LVPEEKVAL() = eul_nil;                                                 \
    else                                                                       \
        slot_ref(tmp1, 0) = c_int_as_eul_int(1);                               \
    ++reg_pc;


///-----------------------------------------------------------------------------
/// State
///-----------------------------------------------------------------------------

/* All of the following is a little shaky in one way or another. We know
   that the stack contains at least one value so FLUSHVAL(1) will work
   without the need for a check. Later on though, we reset the stack
   to its base value (ie empty) which contravenes this rule but luckily
   enough we push something immediately so all is cool. The context stack
   is still rather shaky... */

///-----------------------------------------------------------------------------
///  (fill-state) (state)->(state) **/

#define BC_FILL_STATE 92
#define BCA_FILL_STATE()                                                       \
    LispRef state = LVPEEKVAL();                                               \
    FLUSH_STACK(reg_value_stack, sreg_value_sp, 1);                            \
    FLUSH_STACK(reg_context_stack, sreg_context_sp, 0);                        \
    STATE_VALUE_STACK(state) = reg_value_stack->segment;                       \
                             eul_allocate_int(STATE_VALUE_STACK_SIZE(state),   \
                             (ptrInt)(reg_value_stack->size));                 \
                             STATE_CONTEXT_STACK(state) = reg_context_stack->segment; \
                                                        eul_allocate_int(STATE_CONTEXT_STACK_SIZE(state), \
                                                        (ptrInt)(reg_context_stack->size)); \
                                                        ++reg_pc;

///-----------------------------------------------------------------------------
///  (restore-state) (state, obj)->(obj) **/

#define BC_RESTORE_STATE 93
#define BCA_RESTORE_STATE()                                                    \
    LispRef state, value;                                                      \
    POPVAL2(value, state);                                                     \
    reg_value_stack->segment = STATE_VALUE_STACK(state);                       \
    reg_value_stack->size =                                                    \
    fpi_value(STATE_VALUE_STACK_SIZE(state));                                  \
    reg_context_stack->segment = STATE_CONTEXT_STACK(state);                   \
    reg_context_stack->size =                                                  \
    fpi_value(STATE_CONTEXT_STACK_SIZE(state));                                \
    sreg_value_sp = sreg_value_sb;                                             \
    sreg_context_sp = sreg_context_sb;                                         \
    reg_value_stack->sp = sreg_value_sp;                                       \
    reg_context_stack->sp = sreg_context_sp;                                   \
    POPVALN(8); /* Embarrasing... */                                           \
    PUSHVAL1(value);                                                           \
    POPCONTEXT();

///-----------------------------------------------------------------------------
///  (fill-thread-state) (state)->(state) **/

#define BC_FILL_THREAD_STATE 90
#define BCA_FILL_THREAD_STATE()                                                \
    LispRef state = LVPEEKVAL();                                               \
    FLUSH_STACK(reg_value_stack, sreg_value_sp, 1);                            \
    FLUSH_STACK(reg_context_stack, sreg_context_sp, 0);                        \
    STATE_VALUE_STACK(state) = copy_stack_segment(reg_value_stack->segment);   \
                             eul_allocate_int(STATE_VALUE_STACK_SIZE(state),   \
                             (ptrInt)(reg_value_stack->size));                 \
                             STATE_CONTEXT_STACK(state) = copy_stack_segment(reg_context_stack->segment); \
                                                        eul_allocate_int(STATE_CONTEXT_STACK_SIZE(state), \
                                                        (ptrInt)(reg_context_stack->size)); \
                                                        ++reg_pc;


///-----------------------------------------------------------------------------
///  (restore-thread-state) (value, state)->(obj) **/

#define BC_RESTORE_THREAD_STATE 91
#define BCA_RESTORE_THREAD_STATE()                                             \
    LispRef state, value;                                                      \
    POPVAL2(value, state);                                                     \
    reg_value_stack->segment = STATE_VALUE_STACK(state);                       \
    reg_value_stack->size =                                                    \
    fpi_value(STATE_VALUE_STACK_SIZE(state));                                  \
    reg_context_stack->segment = STATE_CONTEXT_STACK(state);                   \
    reg_context_stack->size =                                                  \
    fpi_value(STATE_CONTEXT_STACK_SIZE(state));                                \
    sreg_value_sp = sreg_value_sb;                                             \
    sreg_context_sp = sreg_context_sb;                                         \
    reg_value_stack->sp = sreg_value_sp;                                       \
    reg_context_stack->sp = sreg_context_sp;                                   \
    POPVALN(2); /* Embarrasing... */                                           \
    PUSHVAL1(value);                                                           \
    POPCONTEXT();


///-----------------------------------------------------------------------------
/// Explicit unflush of both stack buffers
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (unflush-stacks) ()->(obj) **/

#define BC_UNFLUSH_STACKS 88
#define BCA_UNFLUSH_STACKS()                                                   \
    UNFLUSH_STACK(reg_value_stack, sreg_value_sp, value_stack_size+1);         \
    UNFLUSH_STACK(reg_context_stack, sreg_context_sp, context_stack_size+CONTEXT_FRAME_SIZE); \
    PUSHVAL1(eul_nil);                                                         \
    ++reg_pc;


///-----------------------------------------------------------------------------
/// Character conversion
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (character->fpi) (char)->(fpi) **/

#define BC_CHARACTER_TO_FPI 98
#define BCA_CHARACTER_TO_FPI()                                                 \
    eul_allocate_int(res, char_value(LVPEEKVAL()));                            \
    LVPEEKVAL() = res;                                                         \
    ++reg_pc;

///-----------------------------------------------------------------------------
///  (fpi->character) (fpi)->(char) **/

#define BC_FPI_TO_CHARACTER 99
#define BCA_FPI_TO_CHARACTER()                                                 \
    eul_allocate_char(res, fpi_value(LVPEEKVAL()));                            \
    LVPEEKVAL() = res;                                                         \
    ++reg_pc;


///-----------------------------------------------------------------------------
/// Exit
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
///  (exit) ()->() **/

#define BC_EXIT 102
#define BCA_EXIT()                                                             \
    /* Return code is handeled in eul-appl.c; exit is rest argument function */ \
    res = LVPEEKVAL();                                                         \
    if (eul_is_cons(res))                                                      \
        LVPEEKVAL() = eul_car(res);                                            \
    goto exit;


///-----------------------------------------------------------------------------
/// Symbols and list searching
///-----------------------------------------------------------------------------

#define BC_INTERN 105
#define BCA_INTERN()                                                           \
    LispRef str, loc;                                                          \
    POPVAL1(str);                                                              \
    eul_intern_symbol(loc, eul_string_as_c_string(str));                       \
    PUSHVAL1(loc);                                                             \
    ++reg_pc;

#define BC_ASSQ 106
#define BCA_ASSQ()                                                             \
    LispRef list, key, fail;                                                   \
    POPVAL3(fail, list, key);                                                  \
    PUSHVAL1(fail);                                                            \
    while (eul_is_cons(list)) {                                                \
        tmp1 = eul_car(list);                                                  \
        CALLBACK_TRAP(eul_is_cons(tmp1), PUSHVAL1(key); PUSHVAL1(tmp1),        \
        CB_FIRST_CONS+2, 2)                                                    \
        if (eul_car(tmp1)==key) {                                              \
            LVPEEKVAL() = tmp1;                                                \
            break; }                                                           \
        else                                                                   \
            list=eul_cdr(list); }                                              \
    ++reg_pc;

#define BC_INIQ 107
#define BCA_INIQ()                                                             \
    LispRef list, key, fail;                                                   \
    POPVAL3(fail, list, key);                                                  \
    PUSHVAL1(fail);                                                            \
    while (eul_is_cons(list)) {                                                \
        tmp1 = eul_cdr(list);                                                  \
        CALLBACK_TRAP(eul_is_cons(tmp1), PUSHVAL1(key); PUSHVAL1(tmp1),        \
        CB_FIRST_CONS+3, 2)                                                    \
        if (eul_car(list)==key) {                                              \
            LVPEEKVAL() = eul_car(tmp1);                                       \
            break; }                                                           \
        else                                                                   \
            list = eul_cdr(tmp1); }                                            \
    ++reg_pc;

#define BC_MEMQ 108
#define BCA_MEMQ()                                                             \
    LispRef list, key, fail;                                                   \
    POPVAL3(fail, list, key);                                                  \
    PUSHVAL1(fail);                                                            \
    while (eul_is_cons(list))                                                  \
        if (eul_car(list)==key) {                                              \
            LVPEEKVAL() = list;                                                \
            break; }                                                           \
        else                                                                   \
            list = eul_cdr(list);                                              \
    ++reg_pc;


///-----------------------------------------------------------------------------
/// Logic
///-----------------------------------------------------------------------------

#define BC_EQ 80
#define BCA_EQ()                                                               \
    POPVAL1(tmp1);                                                             \
    LVPEEKVAL() = (tmp1 == LVPEEKVAL() ? eul_true : eul_nil);                  \
    ++reg_pc;


///-----------------------------------------------------------------------------
#endif // BYTECODE_H
///-----------------------------------------------------------------------------
