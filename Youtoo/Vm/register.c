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
///  Title: global register declaration/initialization
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Maintainer: Henry G. Weller
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
    eul_intern_symbol(eul_true, "t");
    PGLOBAL(glob_encl_lambda) = eul_nil;
}


///-----------------------------------------------------------------------------
