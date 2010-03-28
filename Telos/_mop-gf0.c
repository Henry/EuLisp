/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module _mop-gf0
 **  Copyright: See file _mop-gf0.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_level1();
extern LispRef level1_bindings[];
extern LispRef symbol_bindings[];
extern LispRef collect_bindings[];
extern LispRef boot_bindings[];
extern LispRef mop_key_bindings[];

/* Module bindings with size 40 */
LispRef _mop_gf0_bindings[40];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module _mop-gf0 */
void initialize_module__mop_gf0()
{
  if (is_initialized) return;
  initialize_module_level1();
  eul_fast_table_set(eul_modules,"_mop_gf0",(LispRef) _mop_gf0_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_402, sym_401, sym_400, sym_399, G00398, sym_396, G00390, G00388, G00386, sym_384, G00383, sym_381, sym_380, key_378, key_376, sym_372, sym_371, sym_370, sym_366, sym_363, sym_362, sym_361, key_360, sym_359, key_358, sym_357, key_356, G00355, sym_353, G00352;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 8 is_init: 0 index: 7 binding: anonymous */
  static const void *G00351[] = {I(aa,1b,7a,12),I(1b,34,00,00),I(00,00,00,17),I(23,00,00,00),B(_mop_gf0 ,6),I(32,00,00,00),I(00,00,00,0a),I(1c,73,45,02)};

  eul_allocate_static_cons(cons_365, NULL, NULL);
  eul_allocate_static_cons(cons_364, NULL, eul_as_static(cons_365));
  eul_allocate_static_cons(cons_368, NULL, NULL);
  eul_allocate_static_cons(cons_367, NULL, eul_as_static(cons_368));
  eul_allocate_static_string(str_369, "bad name for generic function ~a", 32);
  eul_allocate_static_cons(cons_379, NULL, NULL);
  eul_allocate_static_cons(cons_377, NULL, eul_as_static(cons_379));
  eul_allocate_static_cons(cons_375, NULL, eul_as_static(cons_377));
  eul_allocate_static_cons(cons_374, NULL, eul_as_static(cons_375));
  eul_allocate_static_cons(cons_373, NULL, eul_as_static(cons_374));
  /* Byte-vector with size: 106 is_init: 0 index: 28 binding: defgeneric */
  static const void *G00354[] = {I(43,fd,23,00),B(_mop_gf0 ,8),I(1c,23,00,00),B(_mop_gf0 ,9),I(24,00,00,00),B(mop_key ,2),I(3c,03,23,00),B(_mop_gf0 ,10),I(1d,23,00,00),B(_mop_gf0 ,11),I(24,00,00,00),B(mop_key ,2),I(3c,03,23,00),B(_mop_gf0 ,12),I(1f,03,86,24),B(mop_key ,2),I(3c,03,1f,04),I(24,00,00,00),B(_mop_gf0 ,4),I(3c,01,23,00),B(_mop_gf0 ,13),I(23,00,00,00),B(_mop_gf0 ,7),I(3b,01,1c,24),B(boot ,15),I(3c,02,1f,07),I(1f,08,7c,1b),I(34,00,00,00),I(00,00,00,18),I(23,00,00,00),B(_mop_gf0 ,14),I(32,00,00,00),I(00,00,00,8e),I(1f,09,7a,1b),I(34,00,00,00),I(00,00,00,18),I(1f,0a,10,23),B(_mop_gf0 ,15),I(50,32,00,00),I(00,00,00,08),I(86,1b,34,00),I(00,00,00,1a),I(1f,0b,73,1b),I(20,05,23,00),B(_mop_gf0 ,16),I(22,01,32,00),I(00,00,00,52),I(1f,0b,7a,1b),I(34,00,00,00),I(00,00,00,18),I(1f,0c,10,23),B(_mop_gf0 ,17),I(50,32,00,00),I(00,00,00,08),I(86,1b,34,00),I(00,00,00,1a),I(1f,0d,73,1b),I(20,07,23,00),B(_mop_gf0 ,18),I(22,01,32,00),I(00,00,00,18),I(23,00,00,00),B(_mop_gf0 ,19),I(1f,0e,24,00),B(boot ,21),I(3c,02,22,02),I(22,02,1f,0a),I(86,0f,23,00),B(_mop_gf0 ,20),I(1c,0f,1f,05),I(24,00,00,00),B(collect ,8),I(3c,01,1b,1f),I(07,0f,23,00),B(_mop_gf0 ,21),I(1c,0f,23,00),B(_mop_gf0 ,22),I(1f,0b,0f,1f),I(0e,23,00,00),B(_mop_gf0 ,25),I(24,00,00,00),B(mop_key ,3),I(3c,02,23,00),B(_mop_gf0 ,22),I(1c,0f,1b,86),I(0f,1f,03,1c),I(0f,1f,10,1c),I(0f,1f,12,1c),I(0f,1f,07,1c),I(0f,1f,0b,1c),I(0f,23,00,00),B(_mop_gf0 ,26),I(1c,0f,1b,86),I(0f,1f,12,1c),I(0f,1f,11,1c),I(0f,1f,1c,1f),I(1b,24,00,00),B(_mop_gf0 ,3),I(3c,02,1f,1d),I(86,0f,1c,1c),I(24,00,00,00),B(boot ,11),I(3c,02,1f,03),I(1c,0f,23,00),B(_mop_gf0 ,27),I(1c,0f,45,21)};

  /* Byte-vector with size: 25 is_init: 0 index: 30 binding: do-defgeneric-methods */
  static const void *G00382[] = {I(ab,1b,12,1b),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,52),I(1c,10,23,00),B(_mop_gf0 ,23),I(50,1b,34,00),I(00,00,00,2e),I(1d,73,1f,04),I(1c,0f,23,00),B(_mop_gf0 ,29),I(1c,0f,1f,05),I(75,1f,07,1c),I(24,00,00,00),B(_mop_gf0 ,3),I(3c,02,1d,1c),I(0f,22,05,32),I(00,00,00,16),I(1d,75,1f,04),I(1c,24,00,00),B(_mop_gf0 ,3),I(3d,02,05,22),I(01,22,01,45),I(03,00,00,00)};

  /* Byte-vector with size: 11 is_init: 0 index: 31 binding: required-args */
  static const void *G00385[] = {I(aa,1b,7a,12),I(1b,34,00,00),I(00,00,00,0f),I(86,32,00,00),I(00,00,00,1a),I(1c,10,1d,11),I(1b,24,00,00),B(_mop_gf0 ,4),I(3c,01,1d,1c),I(0f,22,03,45),I(02,00,00,00)};

  /* Byte-vector with size: 8 is_init: 0 index: 32 binding: anonymous */
  static const void *G00387[] = {I(aa,1b,7a,12),I(1b,34,00,00),I(00,00,00,17),I(23,00,00,00),B(_mop_gf0 ,6),I(32,00,00,00),I(00,00,00,0a),I(1c,73,45,02)};

  eul_allocate_static_cons(cons_395, NULL, NULL);
  eul_allocate_static_cons(cons_394, NULL, eul_as_static(cons_395));
  eul_allocate_static_cons(cons_393, NULL, eul_as_static(cons_394));
  eul_allocate_static_cons(cons_392, NULL, eul_as_static(cons_393));
  eul_allocate_static_cons(cons_391, NULL, eul_as_static(cons_392));
  /* Byte-vector with size: 73 is_init: 0 index: 35 binding: generic-lambda */
  static const void *G00389[] = {I(43,fe,23,00),B(_mop_gf0 ,8),I(1c,23,00,00),B(_mop_gf0 ,9),I(24,00,00,00),B(mop_key ,2),I(3c,03,23,00),B(_mop_gf0 ,10),I(1d,23,00,00),B(_mop_gf0 ,11),I(24,00,00,00),B(mop_key ,2),I(3c,03,23,00),B(_mop_gf0 ,12),I(1f,03,86,24),B(mop_key ,2),I(3c,03,23,00),B(_mop_gf0 ,24),I(1f,04,23,00),B(_mop_gf0 ,13),I(24,00,00,00),B(mop_key ,2),I(3c,03,1f,05),I(24,00,00,00),B(_mop_gf0 ,4),I(3c,01,23,00),B(_mop_gf0 ,13),I(23,00,00,00),B(_mop_gf0 ,32),I(3b,01,1c,24),B(collect ,2),I(3c,02,24,00),B(symbol ,6),I(3c,00,1f,03),I(86,0f,23,00),B(_mop_gf0 ,20),I(1c,0f,1f,03),I(24,00,00,00),B(collect ,8),I(3c,01,1b,1f),I(05,0f,23,00),B(_mop_gf0 ,21),I(1c,0f,23,00),B(_mop_gf0 ,22),I(1f,0a,0f,1f),I(0d,23,00,00),B(_mop_gf0 ,33),I(24,00,00,00),B(mop_key ,3),I(3c,02,23,00),B(_mop_gf0 ,22),I(1c,0f,1b,86),I(0f,1f,03,1c),I(0f,1f,0f,1c),I(0f,1f,11,1c),I(0f,1f,07,1c),I(0f,1f,0b,1c),I(0f,23,00,00),B(_mop_gf0 ,26),I(1c,0f,1b,86),I(0f,1f,10,1c),I(0f,1b,86,0f),I(1f,12,1f,1a),I(24,00,00,00),B(_mop_gf0 ,3),I(3c,02,1f,13),I(86,0f,1c,1c),I(24,00,00,00),B(boot ,11),I(3c,02,1f,03),I(1c,0f,23,00),B(_mop_gf0 ,34),I(1c,0f,45,1f)};

  /* Byte-vector with size: 32 is_init: 1 index: 0 binding: initialize-_mop-gf0 */
  static const void *G00397[] = {I(87,25,00,00),B(_mop_gf0 ,1),I(24,00,00,00),B(level1 ,1),I(3e,0b,24,00),B(level1 ,0),I(3c,00,21,01),I(23,00,00,00),B(_mop_gf0 ,36),I(23,00,00,00),B(_mop_gf0 ,35),I(3b,fe,25,00),B(_mop_gf0 ,5),I(23,00,00,00),B(_mop_gf0 ,37),I(23,00,00,00),B(_mop_gf0 ,31),I(3b,01,25,00),B(_mop_gf0 ,4),I(23,00,00,00),B(_mop_gf0 ,38),I(23,00,00,00),B(_mop_gf0 ,30),I(3b,02,25,00),B(_mop_gf0 ,3),I(23,00,00,00),B(_mop_gf0 ,39),I(23,00,00,00),B(_mop_gf0 ,28),I(3b,fd,25,00),B(_mop_gf0 ,2),I(86,ac,00,00)};


  /* Initializations */
  eul_intern_symbol(sym_353,"<object>");
  eul_allocate_bytevector( G00352,G00351);
  eul_intern_keyword(key_356,"class");
  eul_intern_symbol(sym_357,"<simple-generic-function>");
  eul_intern_keyword(key_358,"method-class");
  eul_intern_symbol(sym_359,"<simple-method>");
  eul_intern_keyword(key_360,"method-keywords");
  eul_intern_symbol(sym_361,"anonymous");
  eul_intern_symbol(sym_362,"deflocal");
  eul_intern_symbol(sym_363,"setter");
  object_class(cons_365) = eul_static_cons_class;
  eul_car(cons_365) = sym_363;
  eul_cdr(cons_365) = eul_nil;
  object_class(cons_364) = eul_static_cons_class;
  eul_car(cons_364) = sym_363;
  eul_intern_symbol(sym_366,"converter");
  object_class(cons_368) = eul_static_cons_class;
  eul_car(cons_368) = sym_366;
  eul_cdr(cons_368) = eul_nil;
  object_class(cons_367) = eul_static_cons_class;
  eul_car(cons_367) = sym_363;
  object_class(str_369) = eul_static_string_class;
  eul_intern_symbol(sym_370,"quote");
  eul_intern_symbol(sym_371,"make-vector");
  eul_intern_symbol(sym_372,"list");
  eul_intern_keyword(key_376,"method");
  eul_intern_keyword(key_378,"name");
  object_class(cons_379) = eul_static_cons_class;
  eul_car(cons_379) = key_360;
  eul_cdr(cons_379) = eul_nil;
  object_class(cons_377) = eul_static_cons_class;
  eul_car(cons_377) = key_378;
  object_class(cons_375) = eul_static_cons_class;
  eul_car(cons_375) = key_376;
  object_class(cons_374) = eul_static_cons_class;
  eul_car(cons_374) = key_358;
  object_class(cons_373) = eul_static_cons_class;
  eul_car(cons_373) = key_356;
  eul_intern_symbol(sym_380,"make-generic-function");
  eul_intern_symbol(sym_381,"progn");
  eul_allocate_bytevector( G00355,G00354);
  eul_intern_symbol(sym_384,"defmethod");
  eul_allocate_bytevector( G00383,G00382);
  eul_allocate_bytevector( G00386,G00385);
  eul_allocate_bytevector( G00388,G00387);
  object_class(cons_395) = eul_static_cons_class;
  eul_car(cons_395) = key_360;
  eul_cdr(cons_395) = eul_nil;
  object_class(cons_394) = eul_static_cons_class;
  eul_car(cons_394) = key_378;
  object_class(cons_393) = eul_static_cons_class;
  eul_car(cons_393) = key_376;
  object_class(cons_392) = eul_static_cons_class;
  eul_car(cons_392) = key_358;
  object_class(cons_391) = eul_static_cons_class;
  eul_car(cons_391) = key_356;
  eul_intern_symbol(sym_396,"let");
  eul_allocate_bytevector( G00390,G00389);
  eul_intern_symbol(sym_399,"generic-lambda");
  eul_intern_symbol(sym_400,"required-args");
  eul_intern_symbol(sym_401,"do-defgeneric-methods");
  eul_intern_symbol(sym_402,"defgeneric");
  eul_allocate_bytevector( G00398,G00397);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 6; i++)
      _mop_gf0_bindings[i] = eul_nil;
  }

  _mop_gf0_bindings[ 6] = sym_353;
  _mop_gf0_bindings[ 7] = G00352;
  _mop_gf0_bindings[ 8] = key_356;
  _mop_gf0_bindings[ 9] = sym_357;
  _mop_gf0_bindings[ 10] = key_358;
  _mop_gf0_bindings[ 11] = sym_359;
  _mop_gf0_bindings[ 12] = key_360;
  _mop_gf0_bindings[ 13] = sym_361;
  _mop_gf0_bindings[ 14] = sym_362;
  _mop_gf0_bindings[ 15] = sym_363;
  _mop_gf0_bindings[ 16] = cons_364;
  _mop_gf0_bindings[ 17] = sym_366;
  _mop_gf0_bindings[ 18] = cons_367;
  _mop_gf0_bindings[ 19] = str_369;
  _mop_gf0_bindings[ 20] = sym_370;
  _mop_gf0_bindings[ 21] = sym_371;
  _mop_gf0_bindings[ 22] = sym_372;
  _mop_gf0_bindings[ 23] = key_376;
  _mop_gf0_bindings[ 24] = key_378;
  _mop_gf0_bindings[ 25] = cons_373;
  _mop_gf0_bindings[ 26] = sym_380;
  _mop_gf0_bindings[ 27] = sym_381;
  _mop_gf0_bindings[ 28] = G00355;
  _mop_gf0_bindings[ 29] = sym_384;
  _mop_gf0_bindings[ 30] = G00383;
  _mop_gf0_bindings[ 31] = G00386;
  _mop_gf0_bindings[ 32] = G00388;
  _mop_gf0_bindings[ 33] = cons_391;
  _mop_gf0_bindings[ 34] = sym_396;
  _mop_gf0_bindings[ 35] = G00390;
  _mop_gf0_bindings[ 1] = eul_nil;
  _mop_gf0_bindings[ 36] = sym_399;
  _mop_gf0_bindings[ 37] = sym_400;
  _mop_gf0_bindings[ 38] = sym_401;
  _mop_gf0_bindings[ 39] = sym_402;
  eul_allocate_lambda( _mop_gf0_bindings[0], "initialize-_mop-gf0", 0, G00398);

  }
}


/* eof */
