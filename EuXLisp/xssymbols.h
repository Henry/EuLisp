#ifndef XSSYMBOLS_H
#define XSSYMBOLS_H

extern LVAL true, eof_object, default_object, s_unassigned;
extern LVAL cs_map1, cs_foreach1, cs_withfile1, cs_load1, cs_force1;
extern LVAL cs_initloop1, c_lpar, c_rpar, c_dot, c_quote, s_quote;
extern LVAL s_eval, s_unbound, s_stdin, s_stdout, s_stderr, s_filein;
extern LVAL s_fixfmt, s_flofmt;
extern LVAL s_direct_slots, s_direct_keywords, s_name, s_default, s_requiredp;
extern LVAL s_keyword;
extern LVAL s_abstractp, s_predicate, s_constructor, s_keywords, s_superclasses;
extern LVAL s_reader, s_writer, s_accessor, s_class, s_defclass;
extern LVAL s_find_slot_index, s_getivar, s_setivar, s_list, s_lambda, s_defun;
extern LVAL s_object, s_value, s_backtracep, s_eq, s_eqv, s_equal, s_equals;
extern LVAL s_import, s_only, s_except, s_rename, s_rename_flag;
extern LVAL s_callcc, s_make, s_apply, s_setter, s_signal, s_unwind_protect;
extern LVAL s_general_error, s_no_applic_error, s_no_next_md_error;
extern LVAL s_bad_type_error, s_telos_error, s_telos_bad_ref, s_incompatible_md;
extern LVAL s_unbound_error, s_arith_error, s_user_intr, s_syntax_error;
extern LVAL s_compile_error;
extern LVAL s_letname, s_begin, s_compile, s_setmodule, s_getmodule, s_reintern;
extern LVAL s_module_directives;
extern LVAL s_binary_plus, s_binary_minus, s_binary_times, s_binary_divide;
extern LVAL s_quotient, s_binary_less, s_binary_equal, s_current_thread;
extern LVAL s_thread_class, s_qualified_symbols, s_structure;
extern LVAL s_set_generic_args, s_macro_error, s_supplied_env, s_debug;
extern LVAL s_xlframe, s_gcmsgs, s_arg_list, s_next_methods;
#ifndef NO_CHECK_REF
extern LVAL s_check_ref;
#endif
#ifdef SOCK
extern LVAL s_socket_error;
#endif

#endif
