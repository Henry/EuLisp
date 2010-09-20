/// Copyright 1997 A. Kind & University of Bath
/// Copyright 2010 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                         EuLisp System 'Youtoo'
///-----------------------------------------------------------------------------
//
//  Youtoo is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: global registers
///-----------------------------------------------------------------------------

#ifndef REGISTER_H
#define REGISTER_H

#include "stack.h"

extern void eul_initialize_register();

///-----------------------------------------------------------------------------
/// Global registers
///-----------------------------------------------------------------------------

PGLOBAL_EXTERN(glob_null_class);
PGLOBAL_EXTERN(glob_cons_class);
PGLOBAL_EXTERN(glob_fpi_class);
PGLOBAL_EXTERN(glob_double_class);
PGLOBAL_EXTERN(glob_char_class);
PGLOBAL_EXTERN(glob_string_class);
PGLOBAL_EXTERN(glob_symbol_class);
PGLOBAL_EXTERN(glob_keyword_class);
PGLOBAL_EXTERN(glob_lambda_class);
PGLOBAL_EXTERN(glob_bytevector_class);
PGLOBAL_EXTERN(glob_vector_class);
PGLOBAL_EXTERN(glob_table_class);
PGLOBAL_EXTERN(glob_method_class);
PGLOBAL_EXTERN(glob_gf_class);
PGLOBAL_EXTERN(glob_fpi_ref_class);
PGLOBAL_EXTERN(glob_double_ref_class);
PGLOBAL_EXTERN(glob_string_ref_class);

/* PGLOBAL_EXTERN(glob_env_class); */

extern LispRef glob_cons_class_static[];

extern LispRef glob_string_class_static[];

PGLOBAL_EXTERN(glob_true);
PGLOBAL_EXTERN(glob_nil);
PGLOBAL_EXTERN(glob_argc);
PGLOBAL_EXTERN(glob_argv);
PGLOBAL_EXTERN(glob_callbacks);
PGLOBAL_EXTERN(glob_symbols);
PGLOBAL_EXTERN(glob_keywords);
PGLOBAL_EXTERN(glob_modules);
PGLOBAL_EXTERN(glob_encl_lambda);

///-----------------------------------------------------------------------------
/// Optimized register set components
///-----------------------------------------------------------------------------

#define DECLARE_LOCAL_REGISTER_SET()                                           \
    register Instruction *reg_current_cv;     /* Code vector */                \
    register Instruction *reg_pc;             /* Program counter */            \
    register Instruction reg_current_op;      /* Op being processed */         \
    register Stack       *reg_value_stack;    /* Value stack */                \
    register Stack       *reg_context_stack;  /* Context stack */              \
    LispRef     reg_env;             /* Current environment */                 \
    LispRef     reg_next_methods;    /* List of next applicable methods */     \
    LispRef     reg_arg_operator;    /* Current operator call */               \
    int         reg_arg_count     /* Arguments sent (transient) */

#define rel_reg_pc                                                             \
    ((uPtrInt) (reg_pc - reg_current_cv))

///-----------------------------------------------------------------------------
/// Unoptimized register set
///-----------------------------------------------------------------------------

#define DECLARE_UNOPTIMISED_LOCAL_REGISTER_SET()                               \
    Instruction *reg_current_cv;     /* Code vector */                         \
    Instruction *reg_pc;             /* Program counter */                     \
    Instruction reg_current_op;      /* Op being processed */                  \
    Stack       *reg_value_stack;    /* Value stack */                         \
    Stack       *reg_context_stack;  /* Context stack */                       \
    LispRef     reg_env;             /* Current environment */                 \
    LispRef     reg_next_methods;    /* List of next applicable methods */     \
    LispRef     reg_arg_operator;    /* Current operator call */               \
    int         reg_arg_count     /* Arguments sent (transient) */

///-----------------------------------------------------------------------------
/// Register Set
///-----------------------------------------------------------------------------

typedef struct register_set
{
    DECLARE_UNOPTIMISED_LOCAL_REGISTER_SET();
} RegisterSet;

typedef RegisterSet *RegisterRef;

extern RegisterRef tame_regs;

///-----------------------------------------------------------------------------
/// Initialize register set
///-----------------------------------------------------------------------------

#define INITIALISE_REGISTER_SET(set)                                           \
    (set)->reg_value_stack = eul_allocate_stack();                             \
         (set)->reg_context_stack = eul_allocate_stack()

///-----------------------------------------------------------------------------
/// Fill local register set
///-----------------------------------------------------------------------------

#define FILL_LOCAL_REGISTER_SET(set)                                           \
    reg_current_cv = (set)->reg_current_cv;                                    \
    reg_current_op = (set)->reg_current_op;                                    \
    reg_pc = (set)->reg_pc;                                                    \
    reg_value_stack = (set)->reg_value_stack;                                  \
    reg_context_stack = (set)->reg_context_stack;                              \
    reg_env = (set)->reg_env;                                                  \
    reg_next_methods = (set)->reg_next_methods;                                \
    reg_arg_operator = (set)->reg_arg_operator;                                \
    reg_arg_count = (set)->reg_arg_count

///-----------------------------------------------------------------------------
/// Dump local register set
///-----------------------------------------------------------------------------

#define DUMP_LOCAL_REGISTER_SET(set)                                           \
    (set)->reg_current_cv = reg_current_cv;                                    \
         (set)->reg_current_op = reg_current_op;                               \
              (set)->reg_pc = reg_pc;                                          \
                   (set)->reg_value_stack = reg_value_stack;                   \
                        (set)->reg_context_stack = reg_context_stack;          \
                             (set)->reg_env = reg_env;                         \
                                  (set)->reg_next_methods = reg_next_methods;  \
                                       (set)->reg_arg_operator = reg_arg_operator; \
                                            (set)->reg_arg_count = reg_arg_count

///-----------------------------------------------------------------------------
#endif // REGISTER_H
///-----------------------------------------------------------------------------
