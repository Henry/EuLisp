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

extern euxlValue euxl_true, euxl_eof_object, euxl_default_object, euxls_unassigned;
extern euxlValue euxls_map_list_cont, euxls_do_list_cont, euxls_with_file_cont, euxls_load_cont, euxls_force_cont;
extern euxlValue euxls_init_loop_cont, euxlp_left_paren, euxlp_right_paren, euxlp_dot, euxlp_quote, euxls_quote;
extern euxlValue euxls_eval_cm, euxls_unbound, euxls_stdin, euxls_stdout, euxls_stderr, euxls_filein;
extern euxlValue euxls_fixfmt, euxls_flofmt;
extern euxlValue euxls_direct_slots, euxls_direct_keywords, euxls_name, euxls_default, euxls_requiredp;
extern euxlValue euxls_keyword;
extern euxlValue euxls_abstractp, euxls_predicate, euxls_constructor, euxls_keywords, euxls_superclasses;
extern euxlValue euxls_reader, euxls_writer, euxls_accessor, euxls_class, euxls_defclass;
extern euxlValue euxls_find_slot_index, euxls_getivar, euxls_setivar, euxls_list, euxls_lambda, euxls_defun;
extern euxlValue euxls_object, euxls_value, euxls_backtracep, euxls_eq, euxls_eqv, euxls_equal, euxls_equals;
extern euxlValue euxls_import, euxls_only, euxls_except, euxls_rename, euxls_rename_flag;
extern euxlValue euxls_syntax, euxls_export, euxls_expose;
extern euxlValue euxls_callcc, euxls_make, euxls_apply, euxls_setter, euxls_signal, euxls_unwind_protect;
extern euxlValue euxls_general_error, euxls_no_applic_error, euxls_no_next_md_error;
extern euxlValue euxls_bad_type_error, euxls_telos_error, euxls_teloeuxls_bad_ref, euxls_incompatible_md;
extern euxlValue euxls_unbound_error, euxls_arith_error, euxls_user_intr, euxls_syntax_error;
extern euxlValue euxls_compile_error;
extern euxlValue euxls_letname, euxls_progn, euxls_compile, euxls_set_module, euxls_get_module, euxls_reintern;
extern euxlValue euxls_module_directives;
extern euxlValue euxls_binary_plus, euxls_binary_minus, euxls_binary_times, euxls_binary_divide;
extern euxlValue euxls_quotient, euxls_binary_less, euxls_binary_equal, euxls_current_thread;
extern euxlValue euxlc_thread, euxls_qualified_symbols;
extern euxlValue euxls_set_generic_args, euxls_syntax_error, euxls_supplied_env, euxls_debug;
extern euxlValue euxls_xlframe, euxls_gcmsgs, euxls_arg_list, euxls_next_methods;

#ifndef NO_CHECK_REF
extern euxlValue euxls_check_ref;
#endif
#ifdef SOCK
extern euxlValue euxls_socket_error;
#endif

///-----------------------------------------------------------------------------
#endif // EUXLSYMBOLS_H
///-----------------------------------------------------------------------------
