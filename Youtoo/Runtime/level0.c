/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module level0
 **  Copyright: See file level0.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_bit();
extern void initialize_module_callback();
extern void initialize_module_character();
extern void initialize_module_collect();
extern void initialize_module_compare();
extern void initialize_module_condition();
extern void initialize_module_convert();
extern void initialize_module_copy();
extern void initialize_module_dynamic();
extern void initialize_module_event();
extern void initialize_module_float();
extern void initialize_module_fpi();
extern void initialize_module_format();
extern void initialize_module_read();
extern void initialize_module_handler();
extern void initialize_module_integer();
extern void initialize_module_let_cc();
extern void initialize_module_list();
extern void initialize_module_lock();
extern void initialize_module_number();
extern void initialize_module_random();
extern void initialize_module_stream();
extern void initialize_module_stream3();
extern void initialize_module_string();
extern void initialize_module_symbol();
extern void initialize_module_telos();
extern void initialize_module_thread();
extern void initialize_module_table();
extern void initialize_module_vector();
extern LispRef vector_bindings[];
extern LispRef table_bindings[];
extern LispRef thread_bindings[];
extern LispRef telos_bindings[];
extern LispRef symbol_bindings[];
extern LispRef string_bindings[];
extern LispRef stream3_bindings[];
extern LispRef stream_bindings[];
extern LispRef random_bindings[];
extern LispRef number_bindings[];
extern LispRef lock_bindings[];
extern LispRef list_bindings[];
extern LispRef let_cc_bindings[];
extern LispRef integer_bindings[];
extern LispRef handler_bindings[];
extern LispRef read_bindings[];
extern LispRef format_bindings[];
extern LispRef fpi_bindings[];
extern LispRef float_bindings[];
extern LispRef event_bindings[];
extern LispRef dynamic_bindings[];
extern LispRef copy_bindings[];
extern LispRef convert_bindings[];
extern LispRef condition_bindings[];
extern LispRef compare_bindings[];
extern LispRef collect_bindings[];
extern LispRef character_bindings[];
extern LispRef callback_bindings[];
extern LispRef bit_bindings[];

/* Module bindings with size 2 */
LispRef level0_bindings[2];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module level0 */
void initialize_module_level0()
{
  if (is_initialized) return;
  initialize_module_bit();
  initialize_module_callback();
  initialize_module_character();
  initialize_module_collect();
  initialize_module_compare();
  initialize_module_condition();
  initialize_module_convert();
  initialize_module_copy();
  initialize_module_dynamic();
  initialize_module_event();
  initialize_module_float();
  initialize_module_fpi();
  initialize_module_format();
  initialize_module_read();
  initialize_module_handler();
  initialize_module_integer();
  initialize_module_let_cc();
  initialize_module_list();
  initialize_module_lock();
  initialize_module_number();
  initialize_module_random();
  initialize_module_stream();
  initialize_module_stream3();
  initialize_module_string();
  initialize_module_symbol();
  initialize_module_telos();
  initialize_module_thread();
  initialize_module_table();
  initialize_module_vector();
  eul_fast_table_set(eul_modules,"level0",(LispRef) level0_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef G0010692;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 148 is_init: 1 index: 0 binding: initialize-level0 */
  static const void *G0010691[] = {I(87,25,00,00),B(level0 ,1),I(24,00,00,00),B(vector ,1),I(3e,0b,24,00),B(vector ,0),I(3c,00,21,01),I(24,00,00,00),B(table ,1),I(3e,0b,24,00),B(table ,0),I(3c,00,21,01),I(24,00,00,00),B(thread ,1),I(3e,0b,24,00),B(thread ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(24,00,00,00),B(symbol ,1),I(3e,0b,24,00),B(symbol ,0),I(3c,00,21,01),I(24,00,00,00),B(string ,1),I(3e,0b,24,00),B(string ,0),I(3c,00,21,01),I(24,00,00,00),B(stream3 ,1),I(3e,0b,24,00),B(stream3 ,0),I(3c,00,21,01),I(24,00,00,00),B(stream ,1),I(3e,0b,24,00),B(stream ,0),I(3c,00,21,01),I(24,00,00,00),B(random ,1),I(3e,0b,24,00),B(random ,0),I(3c,00,21,01),I(24,00,00,00),B(number ,1),I(3e,0b,24,00),B(number ,0),I(3c,00,21,01),I(24,00,00,00),B(lock ,1),I(3e,0b,24,00),B(lock ,0),I(3c,00,21,01),I(24,00,00,00),B(list ,1),I(3e,0b,24,00),B(list ,0),I(3c,00,21,01),I(24,00,00,00),B(let_cc ,1),I(3e,0b,24,00),B(let_cc ,0),I(3c,00,21,01),I(24,00,00,00),B(integer ,1),I(3e,0b,24,00),B(integer ,0),I(3c,00,21,01),I(24,00,00,00),B(handler ,1),I(3e,0b,24,00),B(handler ,0),I(3c,00,21,01),I(24,00,00,00),B(read ,1),I(3e,0b,24,00),B(read ,0),I(3c,00,21,01),I(24,00,00,00),B(format ,1),I(3e,0b,24,00),B(format ,0),I(3c,00,21,01),I(24,00,00,00),B(fpi ,1),I(3e,0b,24,00),B(fpi ,0),I(3c,00,21,01),I(24,00,00,00),B(float ,1),I(3e,0b,24,00),B(float ,0),I(3c,00,21,01),I(24,00,00,00),B(event ,1),I(3e,0b,24,00),B(event ,0),I(3c,00,21,01),I(24,00,00,00),B(dynamic ,1),I(3e,0b,24,00),B(dynamic ,0),I(3c,00,21,01),I(24,00,00,00),B(copy ,1),I(3e,0b,24,00),B(copy ,0),I(3c,00,21,01),I(24,00,00,00),B(convert ,1),I(3e,0b,24,00),B(convert ,0),I(3c,00,21,01),I(24,00,00,00),B(condition ,1),I(3e,0b,24,00),B(condition ,0),I(3c,00,21,01),I(24,00,00,00),B(compare ,1),I(3e,0b,24,00),B(compare ,0),I(3c,00,21,01),I(24,00,00,00),B(collect ,1),I(3e,0b,24,00),B(collect ,0),I(3c,00,21,01),I(24,00,00,00),B(character ,1),I(3e,0b,24,00),B(character ,0),I(3c,00,21,01),I(24,00,00,00),B(callback ,1),I(3e,0b,24,00),B(callback ,0),I(3c,00,21,01),I(24,00,00,00),B(bit ,1),I(3e,0b,24,00),B(bit ,0),I(3c,00,21,01),I(86,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G0010692,G0010691);

  /* Set local bindings */
  level0_bindings[ 1] = eul_nil;
  eul_allocate_lambda( level0_bindings[0], "initialize-level0", 0, G0010692);

  }
}


/* eof */
