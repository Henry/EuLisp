/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module _ex-aux0
 **  Copyright: See file _ex-aux0.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_level1();
extern LispRef level1_bindings[];

/* Module bindings with size 48 */
LispRef _ex_aux0_bindings[48];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module _ex-aux0 */
void initialize_module__ex_aux0()
{
  if (is_initialized) return;
  initialize_module_level1();
  eul_fast_table_set(eul_modules,"_ex_aux0",(LispRef) _ex_aux0_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_248, sym_247, sym_246, sym_245, sym_244, sym_243, sym_242, sym_241, G00240, G00238, G00236, G00234, sym_231, G00230, sym_227, sym_226, sym_225, G00224, G00222, sym_220, sym_219, sym_218, sym_217, sym_216, sym_214, sym_213, sym_212, sym_211, sym_210, sym_209, sym_208, sym_207, sym_206, G00205, sym_203, G00202;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 3 is_init: 0 index: 11 binding: get-params */
  static const void *G00201[] = {I(aa,86,0f,23),B(_ex_aux0 ,10),I(1c,0f,45,01)};

  eul_allocate_static_string(str_215, "bad value ~a", 12);
  /* Byte-vector with size: 58 is_init: 0 index: 27 binding: get-name */
  static const void *G00204[] = {I(aa,86,0f,23),B(_ex_aux0 ,12),I(1c,0f,1b,86),I(0f,23,00,00),B(_ex_aux0 ,13),I(1c,0f,1b,86),I(0f,23,00,00),B(_ex_aux0 ,13),I(86,0f,23,00),B(_ex_aux0 ,14),I(1c,0f,23,00),B(_ex_aux0 ,13),I(86,0f,23,00),B(_ex_aux0 ,15),I(1c,0f,23,00),B(_ex_aux0 ,13),I(86,0f,23,00),B(_ex_aux0 ,16),I(1c,0f,23,00),B(_ex_aux0 ,17),I(86,0f,23,00),B(_ex_aux0 ,18),I(1c,0f,1b,86),I(0f,1f,03,1c),I(0f,23,00,00),B(_ex_aux0 ,19),I(1c,0f,1b,86),I(0f,1f,08,1c),I(0f,23,00,00),B(_ex_aux0 ,20),I(1c,0f,23,00),B(_ex_aux0 ,13),I(86,0f,23,00),B(_ex_aux0 ,21),I(1c,0f,23,00),B(_ex_aux0 ,22),I(1c,0f,1b,86),I(0f,23,00,00),B(_ex_aux0 ,23),I(1c,0f,23,00),B(_ex_aux0 ,24),I(1c,0f,1b,86),I(0f,23,00,00),B(_ex_aux0 ,13),I(1c,0f,1f,08),I(1c,0f,23,00),B(_ex_aux0 ,25),I(1c,0f,1b,86),I(0f,23,00,00),B(_ex_aux0 ,13),I(1c,0f,1f,18),I(1c,0f,23,00),B(_ex_aux0 ,25),I(1c,0f,1b,86),I(0f,1f,1d,1c),I(0f,23,00,00),B(_ex_aux0 ,26),I(1c,0f,45,23)};

  /* Byte-vector with size: 3 is_init: 0 index: 28 binding: get-directives */
  static const void *G00221[] = {I(aa,86,0f,23),B(_ex_aux0 ,10),I(1c,0f,45,01)};

  eul_allocate_static_string(str_228, "body ~a not a list", 18);
  /* Byte-vector with size: 40 is_init: 0 index: 33 binding: get-lambda-body */
  static const void *G00223[] = {I(aa,86,0f,23),B(_ex_aux0 ,29),I(1c,0f,1b,86),I(0f,23,00,00),B(_ex_aux0 ,13),I(1c,0f,1b,86),I(0f,23,00,00),B(_ex_aux0 ,13),I(86,0f,23,00),B(_ex_aux0 ,30),I(1c,0f,23,00),B(_ex_aux0 ,13),I(86,0f,23,00),B(_ex_aux0 ,15),I(1c,0f,1b,86),I(0f,1f,03,1c),I(0f,23,00,00),B(_ex_aux0 ,31),I(1c,0f,23,00),B(_ex_aux0 ,13),I(86,0f,23,00),B(_ex_aux0 ,32),I(1c,0f,23,00),B(_ex_aux0 ,22),I(1c,0f,1b,86),I(0f,23,00,00),B(_ex_aux0 ,23),I(1c,0f,23,00),B(_ex_aux0 ,24),I(1c,0f,1b,86),I(0f,23,00,00),B(_ex_aux0 ,13),I(1c,0f,1f,08),I(1c,0f,23,00),B(_ex_aux0 ,25),I(1c,0f,1b,86),I(0f,1f,12,1c),I(0f,23,00,00),B(_ex_aux0 ,26),I(1c,0f,45,18)};

  eul_allocate_static_string(str_232, "body ~a not a list", 18);
  /* Byte-vector with size: 43 is_init: 0 index: 36 binding: get-body */
  static const void *G00229[] = {I(aa,86,0f,23),B(_ex_aux0 ,29),I(1c,0f,1b,86),I(0f,23,00,00),B(_ex_aux0 ,34),I(1c,0f,1b,86),I(0f,23,00,00),B(_ex_aux0 ,13),I(1c,0f,1b,86),I(0f,23,00,00),B(_ex_aux0 ,13),I(86,0f,23,00),B(_ex_aux0 ,30),I(1c,0f,23,00),B(_ex_aux0 ,13),I(86,0f,23,00),B(_ex_aux0 ,15),I(1c,0f,1b,86),I(0f,1f,03,1c),I(0f,23,00,00),B(_ex_aux0 ,31),I(1c,0f,23,00),B(_ex_aux0 ,13),I(86,0f,23,00),B(_ex_aux0 ,35),I(1c,0f,23,00),B(_ex_aux0 ,22),I(1c,0f,1b,86),I(0f,23,00,00),B(_ex_aux0 ,23),I(1c,0f,23,00),B(_ex_aux0 ,24),I(1c,0f,1b,86),I(0f,23,00,00),B(_ex_aux0 ,13),I(1c,0f,1f,08),I(1c,0f,23,00),B(_ex_aux0 ,25),I(1c,0f,1b,86),I(0f,1f,12,1c),I(0f,23,00,00),B(_ex_aux0 ,26),I(1c,0f,45,1a)};

  /* Byte-vector with size: 3 is_init: 0 index: 37 binding: get-lambda-params */
  static const void *G00233[] = {I(aa,86,0f,23),B(_ex_aux0 ,12),I(1c,0f,45,01)};

  /* Byte-vector with size: 6 is_init: 0 index: 38 binding: get-top-level-forms */
  static const void *G00235[] = {I(aa,86,0f,23),B(_ex_aux0 ,29),I(1c,0f,1b,86),I(0f,23,00,00),B(_ex_aux0 ,34),I(1c,0f,45,03)};

  /* Byte-vector with size: 3 is_init: 0 index: 39 binding: get-value */
  static const void *G00237[] = {I(aa,86,0f,23),B(_ex_aux0 ,10),I(1c,0f,45,01)};

  /* Byte-vector with size: 56 is_init: 1 index: 0 binding: initialize-_ex-aux0 */
  static const void *G00239[] = {I(87,25,00,00),B(_ex_aux0 ,1),I(24,00,00,00),B(level1 ,1),I(3e,0b,24,00),B(level1 ,0),I(3c,00,21,01),I(23,00,00,00),B(_ex_aux0 ,40),I(23,00,00,00),B(_ex_aux0 ,39),I(3b,01,25,00),B(_ex_aux0 ,9),I(23,00,00,00),B(_ex_aux0 ,41),I(23,00,00,00),B(_ex_aux0 ,38),I(3b,01,25,00),B(_ex_aux0 ,8),I(23,00,00,00),B(_ex_aux0 ,42),I(23,00,00,00),B(_ex_aux0 ,37),I(3b,01,25,00),B(_ex_aux0 ,7),I(23,00,00,00),B(_ex_aux0 ,43),I(23,00,00,00),B(_ex_aux0 ,36),I(3b,01,25,00),B(_ex_aux0 ,6),I(23,00,00,00),B(_ex_aux0 ,44),I(23,00,00,00),B(_ex_aux0 ,33),I(3b,01,25,00),B(_ex_aux0 ,5),I(23,00,00,00),B(_ex_aux0 ,45),I(23,00,00,00),B(_ex_aux0 ,28),I(3b,01,25,00),B(_ex_aux0 ,4),I(23,00,00,00),B(_ex_aux0 ,46),I(23,00,00,00),B(_ex_aux0 ,27),I(3b,01,25,00),B(_ex_aux0 ,3),I(23,00,00,00),B(_ex_aux0 ,47),I(23,00,00,00),B(_ex_aux0 ,11),I(3b,01,25,00),B(_ex_aux0 ,2),I(86,ac,00,00)};


  /* Initializations */
  eul_intern_symbol(sym_203,"caddr");
  eul_allocate_bytevector( G00202,G00201);
  eul_intern_symbol(sym_206,"cadr");
  eul_intern_symbol(sym_207,"x");
  eul_intern_symbol(sym_208,"symbol?");
  eul_intern_symbol(sym_209,"cons?");
  eul_intern_symbol(sym_210,"car");
  eul_intern_symbol(sym_211,"setter");
  eul_intern_symbol(sym_212,"quote");
  eul_intern_symbol(sym_213,"eq");
  eul_intern_symbol(sym_214,"and");
  object_class(str_215) = eul_static_string_class;
  eul_intern_symbol(sym_216,"fmt");
  eul_intern_symbol(sym_217,"<condition>");
  eul_intern_symbol(sym_218,"error");
  eul_intern_symbol(sym_219,"if");
  eul_intern_symbol(sym_220,"let");
  eul_allocate_bytevector( G00205,G00204);
  eul_allocate_bytevector( G00222,G00221);
  eul_intern_symbol(sym_225,"cddr");
  eul_intern_symbol(sym_226,"null?");
  eul_intern_symbol(sym_227,"or");
  object_class(str_228) = eul_static_string_class;
  eul_allocate_bytevector( G00224,G00223);
  eul_intern_symbol(sym_231,"cdr");
  object_class(str_232) = eul_static_string_class;
  eul_allocate_bytevector( G00230,G00229);
  eul_allocate_bytevector( G00234,G00233);
  eul_allocate_bytevector( G00236,G00235);
  eul_allocate_bytevector( G00238,G00237);
  eul_intern_symbol(sym_241,"get-value");
  eul_intern_symbol(sym_242,"get-top-level-forms");
  eul_intern_symbol(sym_243,"get-lambda-params");
  eul_intern_symbol(sym_244,"get-body");
  eul_intern_symbol(sym_245,"get-lambda-body");
  eul_intern_symbol(sym_246,"get-directives");
  eul_intern_symbol(sym_247,"get-name");
  eul_intern_symbol(sym_248,"get-params");
  eul_allocate_bytevector( G00240,G00239);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 10; i++)
      _ex_aux0_bindings[i] = eul_nil;
  }

  _ex_aux0_bindings[ 10] = sym_203;
  _ex_aux0_bindings[ 11] = G00202;
  _ex_aux0_bindings[ 12] = sym_206;
  _ex_aux0_bindings[ 13] = sym_207;
  _ex_aux0_bindings[ 14] = sym_208;
  _ex_aux0_bindings[ 15] = sym_209;
  _ex_aux0_bindings[ 16] = sym_210;
  _ex_aux0_bindings[ 17] = sym_211;
  _ex_aux0_bindings[ 18] = sym_212;
  _ex_aux0_bindings[ 19] = sym_213;
  _ex_aux0_bindings[ 20] = sym_214;
  _ex_aux0_bindings[ 21] = str_215;
  _ex_aux0_bindings[ 22] = sym_216;
  _ex_aux0_bindings[ 23] = sym_217;
  _ex_aux0_bindings[ 24] = sym_218;
  _ex_aux0_bindings[ 25] = sym_219;
  _ex_aux0_bindings[ 26] = sym_220;
  _ex_aux0_bindings[ 27] = G00205;
  _ex_aux0_bindings[ 28] = G00222;
  _ex_aux0_bindings[ 29] = sym_225;
  _ex_aux0_bindings[ 30] = sym_226;
  _ex_aux0_bindings[ 31] = sym_227;
  _ex_aux0_bindings[ 32] = str_228;
  _ex_aux0_bindings[ 33] = G00224;
  _ex_aux0_bindings[ 34] = sym_231;
  _ex_aux0_bindings[ 35] = str_232;
  _ex_aux0_bindings[ 36] = G00230;
  _ex_aux0_bindings[ 37] = G00234;
  _ex_aux0_bindings[ 38] = G00236;
  _ex_aux0_bindings[ 39] = G00238;
  _ex_aux0_bindings[ 1] = eul_nil;
  _ex_aux0_bindings[ 40] = sym_241;
  _ex_aux0_bindings[ 41] = sym_242;
  _ex_aux0_bindings[ 42] = sym_243;
  _ex_aux0_bindings[ 43] = sym_244;
  _ex_aux0_bindings[ 44] = sym_245;
  _ex_aux0_bindings[ 45] = sym_246;
  _ex_aux0_bindings[ 46] = sym_247;
  _ex_aux0_bindings[ 47] = sym_248;
  eul_allocate_lambda( _ex_aux0_bindings[0], "initialize-_ex-aux0", 0, G00240);

  }
}


/* eof */