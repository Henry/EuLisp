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
///  Library: boot, telos, level1, eval, youtoo
///  Authos: Andreas Kind
///  Description: built-in foreign functions
///-----------------------------------------------------------------------------
#ifndef EUL_EXT_H
#define EUL_EXT_H

#include "eulisp.h"

///-----------------------------------------------------------------------------

extern LispRef eul_init_string(LispRef str, int n, char c);
extern LispRef c_strn_as_eul_str(char *str, int n);
// extern LispRef c_string_as_eul_string(char * str);
extern LispRef eul_int_as_hex_str(int x);
extern LispRef eul_make_vector(int n, LispRef list);
extern LispRef eul_list_as_eul_string(LispRef x);
extern LispRef eul_make_symbol(char *str);
extern LispRef eul_c_str_as_eul_symbol(LispRef str);
extern LispRef eul_init_symbol(LispRef sym);
extern LispRef eul_make_keyword(char *str);
extern LispRef eul_init_keyword(LispRef key);
extern LispRef c_strn_as_eul_symbol_or_keyword(char *str, int n);
extern char *eul_int_as_str(int x);
extern char *eul_str_tolower(char *str);
extern char *eul_str_toupper(char *str);
extern int eul_find_char(char c, char *str);
extern char *eul_substr(char *str, int i, int j);
extern char *eul_tailstr(char *str, int i);
extern LispRef eul_str_member1(char c, char *str);
extern char *eul_reverse_str(char *str);
extern char *eul_reverse_des_str(char *str);
extern char *eul_str_append(char *str1, char *str2);
extern char *eul_addr_str(void *obj);
extern char *eul_str_copy(char *str);
extern LispRef eul_table_ref(LispRef tab, char *key);
extern LispRef eul_table_set(LispRef tab, char *key, LispRef value);
extern LispRef eul_dyn_binding_ref(char *module_name, int i);
extern LispRef eul_dyn_binding_set(char *module_name, int i, LispRef x);
extern int get_errno();
extern int eul_sprintf(char *buf, int buf_offset, char *fmt_str, LispRef x);
extern int eul_sprintf_string
(
    char *buf,
    int buf_offset,
    int n,
    int x_offset,
    char *str,
    char *x
);
extern LispRef c_double_as_eul_double(double x);
extern char *eul_strerror();
extern LispRef eul_posix_codes();
extern LispRef eul_format_info(char *str);
extern LispRef eul_file_lookup(char *name, LispRef dirs);
extern LispRef eul_file_newer_p(char *file_name1, char *file_name2);
extern int eul_rand_max();
extern int eul_srand(int n);
extern LispRef eul_int_as_eul_int_ref(LispRef x);
extern LispRef eul_double_as_eul_double_ref(LispRef x);
extern LispRef eul_string_as_eul_string_ref(LispRef x);
extern int eul_ticks_per_second();
extern LispRef eul_cpu_time();
extern LispRef eul_get_lambda_code(LispRef lambda);
extern char *eul_set_lambda_code(LispRef lambda, char *code);
extern int eul_bit_and(int x, int y);
extern int eul_bit_ior(int x, int y);
extern int eul_bit_xor(int x, int y);
extern int eul_bit_not(int x);
extern int eul_bit_shift(int x, int n);
extern LispRef eul_ffoo(LispRef x, int y, char *z);

// In eul-ntok.c
extern LispRef ntok(LispRef stream, LispRef special_tokens);

// In eul-read.c
extern int read_into_buffer(int _file, char *buf, int n);

// Return prompt-string
extern char* eul_prompt_string();

///-----------------------------------------------------------------------------
#endif // EUL_EXT_H
///-----------------------------------------------------------------------------
