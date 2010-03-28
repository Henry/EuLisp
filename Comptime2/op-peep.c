/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module op-peep
 **  Copyright: See file op-peep.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_i_all();
extern LispRef i_all_bindings[];
extern LispRef compare_bindings[];
extern LispRef boot1_bindings[];
extern LispRef i_param_bindings[];
extern LispRef table_bindings[];
extern LispRef list_bindings[];
extern LispRef i_notify_bindings[];
extern LispRef boot_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef table1_bindings[];

/* Module bindings with size 26 */
LispRef op_peep_bindings[26];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module op-peep */
void initialize_module_op_peep()
{
  if (is_initialized) return;
  initialize_module_i_all();
  eul_fast_table_set(eul_modules,"op_peep",(LispRef) op_peep_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_5628, sym_5627, sym_5626, sym_5625, sym_5624, sym_5623, G005622, G005620, G005618, G005616, G005614, G005612, G005610, G005608, sym_5606, G005605, G005602, G005600;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 7 is_init: 0 index: 8 binding: top-level */
  static const void *G005599[] = {I(a9,24,00,00),B(table1 ,4),I(24,00,00,00),B(mop_gf ,2),I(3c,01,1b,89),B(op_peep ,6),I(45,01,00,00)};

  eul_allocate_static_string(str_5603, "[~a => ~a]", 10);
  /* Byte-vector with size: 49 is_init: 0 index: 10 binding: anonymous */
  static const void *G005601[] = {I(aa,1b,12,1b),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,b2),I(1c,10,1b,10),I(1c,73,1d,11),I(1b,73,1f,03),I(47,00,00,24),B(op_peep ,7),I(3c,02,1b,34),I(00,00,00,2d),I(1f,03,34,00),I(00,00,00,1e),I(1b,10,1f,04),I(1c,24,00,00),B(boot ,9),I(3c,02,22,01),I(32,00,00,00),I(00,00,00,09),I(87,32,00,00),I(00,00,00,08),I(86,1b,34,00),I(00,00,00,5a),I(1c,10,1f,03),I(1c,24,00,00),B(boot ,9),I(3c,02,1f,08),I(11,1b,11,1b),I(11,1b,10,1f),I(04,1c,23,00),B(op_peep ,9),I(1f,0e,1f,03),I(24,00,00,00),B(i_notify ,4),I(3c,03,2a,47),I(00,00,1c,24),B(list ,2),I(3c,02,1d,1c),I(24,00,00,00),B(boot ,11),I(3c,02,47,00),I(01,1c,90,2a),I(87,22,0a,32),I(00,00,00,0e),I(1f,08,11,47),I(00,02,3d,01),I(09,22,07,45),I(02,00,00,00)};

  /* Byte-vector with size: 17 is_init: 0 index: 12 binding: apply-rule */
  static const void *G005604[] = {I(ab,46,03,1c),I(48,00,00,1b),I(48,00,01,86),I(1b,48,00,02),I(23,00,00,00),B(op_peep ,11),I(23,00,00,00),B(op_peep ,10),I(3b,01,48,00),I(02,47,00,00),I(10,1b,10,24),B(op_peep ,6),I(1c,24,00,00),B(table ,3),I(3c,02,47,00),I(02,3d,01,05),I(45,05,00,00)};

  /* Byte-vector with size: 18 is_init: 0 index: 13 binding: anonymous */
  static const void *G005607[] = {I(aa,1b,11,12),I(1b,34,00,00),I(00,00,00,0f),I(47,00,00,32),I(00,00,00,34),I(1c,11,1b,1f),I(03,24,00,00),B(op_peep ,2),I(3c,02,1b,34),I(00,00,00,15),I(1f,03,47,00),I(01,3d,01,04),I(32,00,00,00),I(00,00,00,11),I(1f,03,11,47),I(00,01,3d,01),I(04,22,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 18 is_init: 0 index: 14 binding: peep-hole-optimize */
  static const void *G005609[] = {I(aa,46,02,1b),I(48,00,00,86),I(1b,48,00,01),I(23,00,00,00),B(op_peep ,11),I(23,00,00,00),B(op_peep ,13),I(3b,01,48,00),I(01,24,00,00),B(i_param ,51),I(12,1b,34,00),I(00,00,00,0e),I(47,00,00,32),I(00,00,00,10),I(86,47,00,00),I(0f,47,00,01),I(3d,01,03,45),I(03,00,00,00)};

  /* Byte-vector with size: 27 is_init: 0 index: 15 binding: anonymous */
  static const void *G005611[] = {I(ab,1c,12,1b),I(34,00,00,00),I(00,00,00,1c),I(47,00,00,11),I(24,00,00,00),B(boot1 ,26),I(3d,01,03,32),I(00,00,00,4e),I(1d,10,1d,10),I(1c,7c,1b,34),I(00,00,00,19),I(1f,05,11,1f),I(05,11,47,00),I(01,3d,02,06),I(32,00,00,00),I(00,00,00,2f),I(1d,1d,24,00),B(compare ,7),I(3c,02,1b,34),I(00,00,00,19),I(1f,06,11,1f),I(06,11,47,00),I(01,3d,02,07),I(32,00,00,00),I(00,00,00,09),I(86,22,01,22),I(03,45,03,00)};

  /* Byte-vector with size: 18 is_init: 0 index: 16 binding: match-pattern */
  static const void *G005613[] = {I(ab,46,02,1b),I(48,00,00,86),I(1b,48,00,01),I(23,00,00,00),B(op_peep ,11),I(23,00,00,00),B(op_peep ,15),I(3b,02,48,00),I(01,1d,10,47),I(00,00,10,50),I(1b,34,00,00),I(00,00,00,1b),I(1f,03,11,47),I(00,00,11,47),I(00,01,3d,02),I(04,32,00,00),I(00,00,00,08),I(86,45,04,00)};

  /* Byte-vector with size: 19 is_init: 0 index: 17 binding: add-rule */
  static const void *G005615[] = {I(43,04,1f,03),I(72,24,00,00),B(op_peep ,6),I(1c,24,00,00),B(table ,3),I(3c,02,24,00),B(table ,3),I(24,00,00,00),B(boot1 ,41),I(3c,01,1f,06),I(1f,06,1f,06),I(1f,06,24,00),B(boot1 ,26),I(3c,04,1b,1f),I(03,0f,24,00),B(op_peep ,6),I(1f,05,1d,1f),I(05,3c,03,2a),I(1f,08,45,09)};

  /* Byte-vector with size: 27 is_init: 0 index: 18 binding: anonymous */
  static const void *G005617[] = {I(43,03,1d,12),I(1b,34,00,00),I(00,00,00,17),I(1c,24,00,00),B(boot1 ,26),I(3d,01,04,32),I(00,00,00,52),I(1d,12,1b,34),I(00,00,00,0d),I(86,32,00,00),I(00,00,00,42),I(1f,04,10,1f),I(04,10,24,00),B(op_peep ,4),I(3c,02,1b,34),I(00,00,00,29),I(1f,05,11,1f),I(05,11,1d,10),I(1f,06,1c,24),B(boot ,11),I(3c,02,1f,03),I(1f,03,1d,47),I(00,00,3d,03),I(0a,22,04,32),I(00,00,00,06),I(86,22,01,22),I(01,45,04,00)};

  /* Byte-vector with size: 10 is_init: 0 index: 19 binding: match-rule */
  static const void *G005619[] = {I(ab,46,01,86),I(1b,48,00,00),I(23,00,00,00),B(op_peep ,11),I(23,00,00,00),B(op_peep ,18),I(3b,03,48,00),I(00,1d,1d,86),I(47,00,00,3d),I(03,03,45,03)};

  /* Byte-vector with size: 45 is_init: 1 index: 0 binding: initialize-op-peep */
  static const void *G005621[] = {I(87,25,00,00),B(op_peep ,1),I(24,00,00,00),B(i_all ,1),I(3e,0b,24,00),B(i_all ,0),I(3c,00,21,01),I(23,00,00,00),B(op_peep ,20),I(23,00,00,00),B(op_peep ,19),I(3b,02,25,00),B(op_peep ,7),I(86,25,00,00),B(op_peep ,6),I(23,00,00,00),B(op_peep ,21),I(23,00,00,00),B(op_peep ,17),I(3b,04,25,00),B(op_peep ,5),I(23,00,00,00),B(op_peep ,22),I(23,00,00,00),B(op_peep ,16),I(3b,02,25,00),B(op_peep ,4),I(23,00,00,00),B(op_peep ,23),I(23,00,00,00),B(op_peep ,14),I(3b,01,25,00),B(op_peep ,3),I(23,00,00,00),B(op_peep ,24),I(23,00,00,00),B(op_peep ,12),I(3b,02,25,00),B(op_peep ,2),I(23,00,00,00),B(op_peep ,25),I(23,00,00,00),B(op_peep ,8),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G005600,G005599);
  object_class(str_5603) = eul_static_string_class;
  eul_allocate_bytevector( G005602,G005601);
  eul_intern_symbol(sym_5606,"anonymous");
  eul_allocate_bytevector( G005605,G005604);
  eul_allocate_bytevector( G005608,G005607);
  eul_allocate_bytevector( G005610,G005609);
  eul_allocate_bytevector( G005612,G005611);
  eul_allocate_bytevector( G005614,G005613);
  eul_allocate_bytevector( G005616,G005615);
  eul_allocate_bytevector( G005618,G005617);
  eul_allocate_bytevector( G005620,G005619);
  eul_intern_symbol(sym_5623,"match-rule");
  eul_intern_symbol(sym_5624,"add-rule");
  eul_intern_symbol(sym_5625,"match-pattern");
  eul_intern_symbol(sym_5626,"peep-hole-optimize");
  eul_intern_symbol(sym_5627,"apply-rule");
  eul_intern_symbol(sym_5628,"top-level");
  eul_allocate_bytevector( G005622,G005621);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 8; i++)
      op_peep_bindings[i] = eul_nil;
  }

  op_peep_bindings[ 8] = G005600;
  op_peep_bindings[ 9] = str_5603;
  op_peep_bindings[ 10] = G005602;
  op_peep_bindings[ 11] = sym_5606;
  op_peep_bindings[ 12] = G005605;
  op_peep_bindings[ 13] = G005608;
  op_peep_bindings[ 14] = G005610;
  op_peep_bindings[ 15] = G005612;
  op_peep_bindings[ 16] = G005614;
  op_peep_bindings[ 17] = G005616;
  op_peep_bindings[ 18] = G005618;
  op_peep_bindings[ 19] = G005620;
  op_peep_bindings[ 1] = eul_nil;
  op_peep_bindings[ 20] = sym_5623;
  op_peep_bindings[ 21] = sym_5624;
  op_peep_bindings[ 22] = sym_5625;
  op_peep_bindings[ 23] = sym_5626;
  op_peep_bindings[ 24] = sym_5627;
  op_peep_bindings[ 25] = sym_5628;
  eul_allocate_lambda( op_peep_bindings[0], "initialize-op-peep", 0, G005622);

  }
}


/* eof */
