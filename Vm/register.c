//  Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.

///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: global register declaration/initialization
///-----------------------------------------------------------------------------

///-----------------------------------------------------------------------------
/// Global registers and their initialisation
///-----------------------------------------------------------------------------

#include "stdc.h"
#include "config.h"
#include "notify.h"
#include "shared-mem.h"
#include "tag.h"
#include "object.h"
#include "class.h"
#include "symbol.h"
#include "eul-string.h"
#include "list.h"
#include "table.h"
#include "bytevec.h"
#include "fpi.h"
#include "register.h"
#include "callback.h"

///-----------------------------------------------------------------------------
/// Global Register set
///-----------------------------------------------------------------------------

static RegisterSet tame_regs_struct;
RegisterRef tame_regs = &tame_regs_struct;


///-----------------------------------------------------------------------------
/// Define registers
///-----------------------------------------------------------------------------

eul_allocate_static_nil(glob_nil);
DEFINE_PGLOBAL(glob_symbols);
DEFINE_PGLOBAL(glob_keywords);
DEFINE_PGLOBAL(glob_modules);
DEFINE_PGLOBAL(glob_true);
DEFINE_PGLOBAL(glob_argc);
DEFINE_PGLOBAL(glob_argv);
DEFINE_PGLOBAL(glob_callbacks);
DEFINE_PGLOBAL(glob_encl_lambda);

eul_allocate_static_class(glob_bytevector_class);
eul_allocate_static_class(glob_char_class);

// eul_allocate_static_class(glob_env_class);
eul_allocate_static_class(glob_fpi_class);
eul_allocate_static_class(glob_double_class);
eul_allocate_static_class(glob_lambda_class);
eul_allocate_static_class(glob_gf_class);
eul_allocate_static_class(glob_fpi_ref_class);
eul_allocate_static_class(glob_double_ref_class);
eul_allocate_static_class(glob_string_ref_class);
eul_allocate_static_class(glob_keyword_class);
eul_allocate_static_class(glob_method_class);
eul_allocate_static_class(glob_null_class);
eul_allocate_static_class(glob_cons_class);
eul_allocate_static_class(glob_string_class);
eul_allocate_static_class(glob_symbol_class);
eul_allocate_static_class(glob_vector_class);
eul_allocate_static_class(glob_table_class);


///-----------------------------------------------------------------------------
/// Initialize registers
///-----------------------------------------------------------------------------

void eul_initialize_register()
{
    INITIALISE_REGISTER_SET(tame_regs);

    eul_allocate_class(PGLOBAL(glob_null_class));
    eul_finalize_nil(PGLOBAL(glob_nil));

    eul_allocate_table(PGLOBAL(glob_symbols), eul_nil);
    eul_allocate_table(PGLOBAL(glob_keywords), eul_nil);
    eul_allocate_table(PGLOBAL(glob_modules), eul_nil);
    eul_allocate_object(PGLOBAL(glob_callbacks),
        PGLOBAL(glob_vector_class), HIGHEST_CB, eul_nil);
    eul_allocate_string(slot_ref(glob_nil, 0), "nil");
    eul_intern_symbol(eul_true, "t");
    PGLOBAL(glob_encl_lambda) = eul_nil;
}
