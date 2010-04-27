//  Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.

///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Andreas Kind
///  Description: fast access to level1 bindings
 **
///                !!! CHANGES ONLY TO leveli.skl.c !!!
 **
///-----------------------------------------------------------------------------

#include "eulisp.h"

static LispRef level1_tab;
static LispRef level1_syntax_tab;
static char *eul_level1_module_names;
static char *eul_level1_lexical_module_name;
static char *eul_level1_syntax_module_name;

void fill_level1_table();
void fill_level1_syntax_table();


LispRef eul_dyn_level1_binding_info(LispRef binding_name)
{
    char *binding_name_str = eul_symbol_as_c_string(binding_name);
    return eul_fast_table_ref(level1_tab, binding_name_str);
}


LispRef eul_dyn_level1_syntax_binding_info(LispRef binding_name)
{
    char *binding_name_str = eul_symbol_as_c_string(binding_name);
    return eul_fast_table_ref(level1_syntax_tab, binding_name_str);
}


LispRef eul_dyn_level1_binding_ref(LispRef binding_name, LispRef absent)
{
    LispRef entry = eul_dyn_level1_binding_info(binding_name);

    if (eul_null(entry))
    {
        return absent;
    }

    int pos = eul_int_as_c_int(eul_car(entry));
    char *module_name_str = eul_symbol_as_c_string(eul_car(eul_cdr(entry)));

    return eul_dyn_binding_ref(module_name_str, pos);
}


LispRef eul_initialize_level1_tables()
{
    LispRef res;

    // Allocate and register binding vector for user modules; see cg-dld.em
    eul_dyn_create_module("user", 1024);
    // eul_dyn_create_module("macros", 1024);

    // Initialize the fast lookup table for level1/user bindings
    eul_allocate_table(level1_tab, eul_nil);
    eul_allocate_table(level1_syntax_tab, eul_nil);
    fill_level1_table();
    fill_level1_syntax_table();

    eul_allocate_vector(res, 5, eul_nil);
    slot_ref(res, 0) = level1_tab;
    slot_ref(res, 1) = level1_syntax_tab;
    eul_allocate_string(slot_ref(res, 2), eul_level1_module_names);
    eul_allocate_string(slot_ref(res, 3), eul_level1_lexical_module_name);
    eul_allocate_string(slot_ref(res, 4), eul_level1_syntax_module_name);

    return res;
}


///-----------------------------------------------------------------------------
/// Initialize level1_tab with liblevel1.i contents
/// e.g. eul_fast_table_set(level1_tab, <key>, make_entry(<pos>, <module_name>));
///-----------------------------------------------------------------------------

LispRef make_entry(int pos, char *module_str, char *origin_str)
{
    LispRef res, tmp, module_name, origin_name;

    eul_intern_symbol(module_name, module_str);
    eul_intern_symbol(origin_name, origin_str);
    eul_allocate_cons(tmp, module_name, origin_name);
    eul_allocate_cons(res, c_int_as_eul_int(pos), tmp);

    return res;
}
