/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: eulvm (Bytecode Interpreter -- Eutopia)
 **  Authors: Keith Playford, Andreas Kind
 **  Description: callbacks
 ** ----------------------------------------------------------------------- **/

#ifndef CALLBACK_H
#define CALLBACK_H

/** ----------------------------------------------------------------- **
 ** Callback codes
 ** ----------------------------------------------------------------- **/

#define HIGHEST_CB (50)

#define CB_FIRST_ARITH          (1)
#define CB_FIRST_CONS          (10)
#define CB_FIRST_ERROR         (15)
#define CB_FIRST_SIGNAL        (20)
#define CB_FIRST_SETTER        (45)

#define CB_SYSTEM_ERROR         (0)
#define CB_NO_NEXT_METHOD      (16)
#define CB_BAD_STRING_ACCESS   (25)
#define CB_BAD_SET_CLASS_OF    (26)
#define CB_SUM_OVERFLOW        (30)
#define CB_DIFFERENCE_OVERFLOW (31)
#define CB_PRODUCT_OVERFLOW    (32)
#define CB_DIVISION_BY_ZERO    (33)
#define CB_READ_OVERFLOW       (34)
#define CB_BAD_OPERATOR        (35)
#define CB_BAD_SLOT_ACCESS     (36)

/** ----------------------------------------------------------------- **
 ** Callback trap
 ** ----------------------------------------------------------------- **/

#ifdef SAFETY

#define CALLBACK_OPERATOR(cb) (slot_ref(PGLOBAL(glob_callbacks), (cb)))

#define CALLBACK_TRAP(test, fn, cb, nargs)                                     \
    {                                                                          \
        if (!(test))                                                           \
        {                                                                      \
            {fn; }                                                             \
            if                                                                 \
            (                                                                  \
                reg_arg_operator = CALLBACK_OPERATOR(cb),                      \
                eul_is_function(reg_arg_operator)                              \
            )                                                                  \
            {                                                                  \
                reg_arg_count = (nargs);                                       \
                goto callback_entry_point;                                     \
            }                                                                  \
            else                                                               \
            {                                                                  \
                LispRef arg;                                                   \
                fprintf(stderr, "\n*** ERROR [system]: uninstalled callback %d \n", (cb)); \
                fprintf(stderr, "    instruction: %d \n", (unsigned char) reg_current_op); \
                for (int i = nargs; i > 0; i--)                                \
                {                                                              \
                    fprintf(stderr, "    argument%d: ", i);                    \
                    POPVAL1(arg);                                              \
                    fprint_ref(stderr, arg);                                   \
                    fprintf(stderr, "\n");                                     \
                    fflush(stderr);                                            \
                }                                                              \
                goto exit;                                                     \
            }                                                                  \
        }                                                                      \
    }

#else

#define CALLBACK_OPERATOR(cb) 0

#define CALLBACK_TRAP(test, fn, cb, nargs) {;}

#endif // SAFETY

#endif // CALLBACK_H
