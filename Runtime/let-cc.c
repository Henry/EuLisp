/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module let-cc
 **  Copyright: See file let-cc.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern void initialize_module_thread();
extern void initialize_module_dynamic();
extern LispRef telos_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef thread_bindings[];
extern LispRef boot_bindings[];
extern LispRef dynamic_bindings[];

/* Module bindings with size 10 */
LispRef let_cc_bindings[10];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module let-cc */
void initialize_module_let_cc()
{
  if (is_initialized) return;
  initialize_module_telos();
  initialize_module_thread();
  initialize_module_dynamic();
  eul_fast_table_set(eul_modules,"let_cc",(LispRef) let_cc_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_809, G00808, sym_806, G00805, sym_803, sym_802, G00801, G00799;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 11 is_init: 0 index: 3 binding: anonymous */
  static const void *G00798[] = {I(ab,47,01,01),I(1d,1a,1b,34),I(00,00,00,21),I(1c,10,1b,3c),I(00,2a,1f,03),I(2c,1f,03,11),I(47,00,00,3d),I(02,04,22,01),I(32,00,00,00),I(00,00,00,09),I(86,45,03,00)};

  /* Byte-vector with size: 28 is_init: 0 index: 6 binding: k */
  static const void *G00800[] = {I(aa,46,02,86),I(1b,48,00,00),I(23,00,00,00),B(let_cc ,4),I(23,00,00,00),B(let_cc ,3),I(3b,02,48,00),I(00,23,00,00),B(let_cc ,5),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,24),B(boot ,8),I(3c,01,1b,1d),I(47,00,00,3c),I(02,2a,24,00),B(dynamic ,4),I(47,01,02,15),I(24,00,00,00),B(dynamic ,6),I(3c,01,2a,24),B(dynamic ,7),I(47,01,03,15),I(24,00,00,00),B(dynamic ,8),I(3c,01,2a,47),I(01,00,1f,04),I(5d,45,04,00)};

  /* Byte-vector with size: 30 is_init: 0 index: 8 binding: call/ep */
  static const void *G00804[] = {I(aa,46,04,24),B(thread ,25),I(24,00,00,00),B(mop_gf ,2),I(3c,01,23,00),B(let_cc ,5),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,24),B(boot ,8),I(3c,01,1d,1c),I(24,00,00,00),B(dynamic ,4),I(24,00,00,00),B(dynamic ,7),I(1f,03,48,00),I(00,1d,48,00),I(01,1c,48,00),I(02,1b,48,00),I(03,47,00,00),I(5c,2a,23,00),B(let_cc ,7),I(23,00,00,00),B(let_cc ,6),I(3b,01,1f,08),I(3c,01,1b,34),I(00,00,00,0d),I(1b,32,00,00),I(00,00,00,08),I(86,45,09,00)};

  /* Byte-vector with size: 24 is_init: 1 index: 0 binding: initialize-let-cc */
  static const void *G00807[] = {I(87,25,00,00),B(let_cc ,1),I(24,00,00,00),B(dynamic ,1),I(3e,0b,24,00),B(dynamic ,0),I(3c,00,21,01),I(24,00,00,00),B(thread ,1),I(3e,0b,24,00),B(thread ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(23,00,00,00),B(let_cc ,9),I(23,00,00,00),B(let_cc ,8),I(3b,01,25,00),B(let_cc ,2),I(86,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G00799,G00798);
  eul_intern_symbol(sym_802,"anonymous");
  eul_intern_symbol(sym_803,"*clean-ups*");
  eul_allocate_bytevector( G00801,G00800);
  eul_intern_symbol(sym_806,"k");
  eul_allocate_bytevector( G00805,G00804);
  eul_intern_symbol(sym_809,"call/ep");
  eul_allocate_bytevector( G00808,G00807);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 3; i++)
      let_cc_bindings[i] = eul_nil;
  }

  let_cc_bindings[ 3] = G00799;
  let_cc_bindings[ 4] = sym_802;
  let_cc_bindings[ 5] = sym_803;
  let_cc_bindings[ 6] = G00801;
  let_cc_bindings[ 7] = sym_806;
  let_cc_bindings[ 8] = G00805;
  let_cc_bindings[ 1] = eul_nil;
  let_cc_bindings[ 9] = sym_809;
  eul_allocate_lambda( let_cc_bindings[0], "initialize-let-cc", 0, G00808);

  }
}


/* eof */
