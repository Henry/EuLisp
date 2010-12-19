/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module syntax-eval
 **  Copyright: See file syntax-eval.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_level_1();
extern void initialize_module_sx_obj1();
extern LispRef level_1_bindings[];
extern LispRef boot_bindings[];
extern LispRef sx_obj1_bindings[];
extern LispRef dynamic_bindings[];

/* Module bindings with size 15 */
LispRef syntax_eval_bindings[15];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module syntax-eval */
void initialize_module_syntax_eval()
{
  if (is_initialized) return;
  initialize_module_level_1();
  initialize_module_sx_obj1();
  eul_fast_table_set(eul_modules,"syntax_eval",(LispRef) syntax_eval_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_76, G0075, sym_73, sym_72, sym_71, sym_70, sym_69, sym_68, sym_67, sym_66, sym_65, sym_64, G0063;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 54 is_init: 0 index: 13 binding: eval */
  static const void *G0062[] = {I(a7,23,00,00),B(syntax_eval ,3),I(24,00,00,00),B(dynamic ,3),I(3c,01,8a,15),I(24,00,00,00),B(sx_obj1 ,59),I(08,1b,86,0f),I(23,00,00,00),B(syntax_eval ,4),I(1c,0f,1b,86),I(0f,1f,04,1c),I(24,00,00,00),B(boot ,8),I(3c,02,23,00),B(syntax_eval ,5),I(1c,0f,1b,86),I(0f,23,00,00),B(syntax_eval ,6),I(1c,0f,1b,86),I(0f,23,00,00),B(syntax_eval ,6),I(86,0f,23,00),B(syntax_eval ,7),I(1c,0f,1f,0c),I(86,0f,23,00),B(syntax_eval ,8),I(1c,0f,1b,86),I(0f,23,00,00),B(syntax_eval ,9),I(1c,0f,1b,86),I(0f,1f,0f,86),I(0f,23,00,00),B(syntax_eval ,4),I(1c,0f,1b,86),I(0f,23,00,00),B(syntax_eval ,10),I(1c,0f,1b,86),I(0f,23,00,00),B(syntax_eval ,3),I(1c,0f,23,00),B(syntax_eval ,11),I(1c,0f,23,00),B(syntax_eval ,9),I(86,0f,1c,1c),I(0f,1f,09,1c),I(0f,23,00,00),B(syntax_eval ,12),I(1c,0f,1b,86),I(0f,1f,11,1c),I(0f,1f,14,1c),I(0f,23,00,00),B(syntax_eval ,12),I(1c,0f,45,20)};

  /* Byte-vector with size: 19 is_init: 1 index: 0 binding: initialize-syntax-eval */
  static const void *G0074[] = {I(87,25,00,00),B(syntax_eval ,1),I(24,00,00,00),B(sx_obj1 ,1),I(3e,0b,24,00),B(sx_obj1 ,0),I(3c,00,21,01),I(24,00,00,00),B(level_1 ,1),I(3e,0b,24,00),B(level_1 ,0),I(3c,00,21,01),I(23,00,00,00),B(syntax_eval ,14),I(23,00,00,00),B(syntax_eval ,13),I(3b,fe,25,00),B(syntax_eval ,2),I(86,ac,00,00)};


  /* Initializations */
  eul_intern_symbol(sym_64,"*actual-module*");
  eul_intern_symbol(sym_65,"quasiquote");
  eul_intern_symbol(sym_66,"or");
  eul_intern_symbol(sym_67,"eval-module-name");
  eul_intern_symbol(sym_68,"set-eval-module");
  eul_intern_symbol(sym_69,"eval/cm");
  eul_intern_symbol(sym_70,"res");
  eul_intern_symbol(sym_71,"get-module");
  eul_intern_symbol(sym_72,"dynamic-setq");
  eul_intern_symbol(sym_73,"let");
  eul_allocate_bytevector( G0063,G0062);
  eul_intern_symbol(sym_76,"eval");
  eul_allocate_bytevector( G0075,G0074);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 3; i++)
      syntax_eval_bindings[i] = eul_nil;
  }

  syntax_eval_bindings[ 3] = sym_64;
  syntax_eval_bindings[ 4] = sym_65;
  syntax_eval_bindings[ 5] = sym_66;
  syntax_eval_bindings[ 6] = sym_67;
  syntax_eval_bindings[ 7] = sym_68;
  syntax_eval_bindings[ 8] = sym_69;
  syntax_eval_bindings[ 9] = sym_70;
  syntax_eval_bindings[ 10] = sym_71;
  syntax_eval_bindings[ 11] = sym_72;
  syntax_eval_bindings[ 12] = sym_73;
  syntax_eval_bindings[ 13] = G0063;
  syntax_eval_bindings[ 1] = eul_nil;
  syntax_eval_bindings[ 14] = sym_76;
  eul_allocate_lambda( syntax_eval_bindings[0], "initialize-syntax-eval", 0, G0075);

  }
}


/* eof */
