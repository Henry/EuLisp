/// Copyright 1988 David Michael Betz
/// Copyright 1994 Russell Bradford
/// Copyright 2010, 2011 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                           EuLisp System 'EuXLisp'
///-----------------------------------------------------------------------------
//
//  EuXLisp is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
///-----------------------------------------------------------------------------
/// Title: EuXLisp symbol declarations
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#ifndef EUXLSYMBOLS_H
#define EUXLSYMBOLS_H

extern euxlValue true, eof_object, default_object, s_unassigned;
extern euxlValue cs_map1, cs_foreach1, cs_withfile1, cs_load1, cs_force1;
extern euxlValue cs_initloop1, c_lpar, c_rpar, c_dot, c_quote, s_quote;
extern euxlValue s_eval_cm, s_unbound, s_stdin, s_stdout, s_stderr, s_filein;
extern euxlValue s_fixfmt, s_flofmt;
extern euxlValue s_direct_slots, s_direct_keywords, s_name, s_default, s_requiredp;
extern euxlValue s_keyword;
extern euxlValue s_abstractp, s_predicate, s_constructor, s_keywords, s_superclasses;
extern euxlValue s_reader, s_writer, s_accessor, s_class, s_defclass;
extern euxlValue s_find_slot_index, s_getivar, s_setivar, s_list, s_lambda, s_defun;
extern euxlValue s_object, s_value, s_backtracep, s_eq, s_eqv, s_equal, s_equals;
extern euxlValue s_import, s_only, s_except, s_rename, s_rename_flag;
extern euxlValue s_syntax, s_export, s_expose;
extern euxlValue s_callcc, s_make, s_apply, s_setter, s_signal, s_unwind_protect;
extern euxlValue s_general_error, s_no_applic_error, s_no_next_md_error;
extern euxlValue s_bad_type_error, s_telos_error, s_telos_bad_ref, s_incompatible_md;
extern euxlValue s_unbound_error, s_arith_error, s_user_intr, s_syntax_error;
extern euxlValue s_compile_error;
extern euxlValue s_letname, s_progn, s_compile, s_setmodule, s_getmodule, s_reintern;
extern euxlValue s_module_directives;
extern euxlValue s_binary_plus, s_binary_minus, s_binary_times, s_binary_divide;
extern euxlValue s_quotient, s_binary_less, s_binary_equal, s_current_thread;
extern euxlValue s_thread_class, s_qualified_symbols;
extern euxlValue s_set_generic_args, s_syntax_error, s_supplied_env, s_debug;
extern euxlValue s_xlframe, s_gcmsgs, s_arg_list, s_next_methods;
#ifndef NO_CHECK_REF
extern euxlValue s_check_ref;
#endif
#ifdef SOCK
extern euxlValue s_socket_error;
#endif

///-----------------------------------------------------------------------------
#endif // EUXLSYMBOLS_H
///-----------------------------------------------------------------------------
