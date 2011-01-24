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

extern euxlValue
euxs_t,
euxs_eof,
euxs_default,
euxls_unassigned,
euxls_quote,
euxls_eval_cm,
euxls_unbound,
euxls_stdin,
euxls_stdout,
euxls_stderr,
euxls_filein,
euxls_fixfmt,
euxls_flofmt,
euxls_direct_slots,
euxls_direct_keywords,
euxls_name,
euxls_default,
euxls_requiredp,
euxls_keyword,
euxls_abstractp,
euxls_predicate,
euxls_constructor,
euxls_keywords,
euxls_superclasses,
euxls_reader,
euxls_writer,
euxls_accessor,
euxls_class,
euxls_defclass,
euxls_find_slot_index,
euxls_getivar,
euxls_setivar,
euxls_list,
euxls_lambda,
euxls_defun,
euxls_object,
euxls_value,
euxls_backtracep,
euxls_eq,
euxls_eqv,
euxls_equal,
euxls_equals,
euxls_import,
euxls_only,
euxls_except,
euxls_rename,
euxls_rename_flag,
euxls_syntax,
euxls_export,
euxls_expose,
euxls_callcc,
euxls_make,
euxls_apply,
euxls_setter,
euxls_signal,
euxls_unwind_protect,
euxls_general_error,
euxls_no_applic_error,
euxls_no_next_md_error,
euxls_bad_type_error,
euxls_telos_error,
euxls_teloeuxls_bad_ref,
euxls_incompatible_md,
euxls_unbound_error,
euxls_arith_error,
euxls_user_intr,
euxls_syntax_error,
euxls_compile_error,
euxls_letname,
euxls_progn,
euxls_compile,
euxls_set_module,
euxls_get_module,
euxls_reintern,
euxls_module_directives,
euxls_binary_plus,
euxls_binary_minus,
euxls_binary_times,
euxls_binary_divide,
euxls_quotient,
euxls_binary_less,
euxls_binary_equal,
euxls_ptr,
euxlc_fpi,
euxlc_character,
euxlc_double_float,
euxlc_string,
euxlc_boolean,
euxlc_thread,
euxls_current_thread,
euxls_qualified_symbols,
euxls_set_generic_args,
euxls_syntax_error,
euxls_supplied_env,
euxls_debug,
euxls_xlframe,
euxls_gcmsgs,
euxls_arg_list,
euxls_next_methods,
euxls_defextern;

///  Continuations
euxlValue
euxls_map_list_cont,
euxls_do_list_cont,
euxls_with_file_cont,
euxls_load_cont,
euxls_force_cont,
euxls_init_loop_cont;

///  Optional symbols
#ifndef NO_CHECK_REF
extern euxlValue euxls_check_ref;
#endif
#ifdef SOCK
extern euxlValue euxls_socket_error;
#endif

#ifdef READLINE
extern euxlValue euxls_readline;
#endif

///-----------------------------------------------------------------------------
#endif // EUXLSYMBOLS_H
///-----------------------------------------------------------------------------
