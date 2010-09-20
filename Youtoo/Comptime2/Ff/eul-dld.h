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
///  Library: eval, youtoo
///  Authos: Andreas Kind
///  Description: dynamic linker
///-----------------------------------------------------------------------------
#ifndef EUL_DLD_H
#define EUL_DLD_H
///-----------------------------------------------------------------------------

#define eul_as_C_module_name(x)                                                \
    (eul_is_symbol(x) ? as_C_module_name(STRING_VALUE(SYMBOL_NAME(x))) :       \
    (eul_is_string(x) ? as_C_module_name(STRING_VALUE(x)) : ""))

extern char *eul_module_name_as_C_module_name_string(LispRef);

extern char *as_C_module_name(char *);

extern LispRef *eul_dyn_create_module(char *, int);

extern int fill_bytevector(FILE * fp, LispRef bv, int size);

extern LispRef *create_binding_vector(char *module_name, FILE * fp);

extern int load_compiled_module(char *module_name, FILE * fp);

extern int create_next_bytevector(LispRef * bv_ptr, FILE * fp,
LispRef * binding_vec);

extern int eul_dyn_load_module(char *module_name, char *file_name);

extern LispRef eul_write_next_bv_binding_ref1(LispRef bv, int bv_index,
char *module_name, int local_index);

extern LispRef eul_allocate_lambda2(LispRef name, LispRef arity, LispRef bv);

///-----------------------------------------------------------------------------
#endif // EUL_DLD_H
///-----------------------------------------------------------------------------
