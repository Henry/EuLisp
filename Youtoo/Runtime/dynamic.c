/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module dynamic
 **  Copyright: See file dynamic.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern void initialize_module_thread();
extern LispRef telos_bindings[];
extern LispRef boot1_bindings[];
extern LispRef thread_bindings[];
extern LispRef boot_bindings[];

/* Module bindings with size 30 */
LispRef dynamic_bindings[30];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module dynamic */
void initialize_module_dynamic()
{
  if (is_initialized) return;
  initialize_module_telos();
  initialize_module_thread();
  eul_fast_table_set(eul_modules,"dynamic",(LispRef) dynamic_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_767, sym_766, sym_765, sym_764, sym_763, sym_762, G00761, G00759, G00757, G00755, G00753, G00751, G00749, G00746, G00744, sym_742, G00741, sym_739, G00738, G00735;

  /* Code vector and literal definitions */
  eul_allocate_static_string(str_736, "dynamic variable ~a not available", 33);
  /* Byte-vector with size: 22 is_init: 0 index: 10 binding: anonymous */
  static const void *G00734[] = {I(aa,1b,12,1b),I(34,00,00,00),I(00,00,00,20),I(86,23,00,00),B(dynamic ,9),I(47,00,00,24),B(boot ,13),I(3d,03,02,32),I(00,00,00,34),I(1c,10,47,00),I(00,1c,50,1b),I(34,00,00,00),I(00,00,00,18),I(1f,03,11,1b),I(47,00,01,8f),I(22,01,32,00),I(00,00,00,13),I(1f,03,11,1b),I(11,47,00,02),I(3d,01,05,22),I(01,22,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 17 is_init: 0 index: 12 binding: (setter-dynamic-variable-ref) */
  static const void *G00737[] = {I(ab,46,03,1c),I(48,00,00,1b),I(48,00,01,86),I(1b,48,00,02),I(23,00,00,00),B(dynamic ,11),I(23,00,00,00),B(dynamic ,10),I(3b,01,48,00),I(02,24,00,00),B(thread ,22),I(3c,00,1b,84),I(24,00,00,00),B(thread ,8),I(08,47,00,02),I(3d,01,04,45),I(04,00,00,00)};

  /* Byte-vector with size: 16 is_init: 0 index: 14 binding: top-level */
  static const void *G00740[] = {I(a9,82,89,00),B(dynamic ,7),I(2a,82,89,00),B(dynamic ,4),I(2a,24,00,00),B(boot1 ,42),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(dynamic ,3),I(23,00,00,00),B(dynamic ,13),I(23,00,00,00),B(dynamic ,12),I(3b,02,1d,3d),I(02,01,45,01)};

  /* Byte-vector with size: 14 is_init: 0 index: 15 binding: push-dynamic-variable */
  static const void *G00743[] = {I(ab,24,00,00),B(thread ,22),I(3c,00,24,00),B(dynamic ,7),I(2b,1b,89,00),B(dynamic ,7),I(2a,1c,84,24),B(thread ,8),I(08,1f,03,1c),I(0f,1f,05,1c),I(0f,1f,04,1c),I(1c,84,1d,24),B(thread ,8),I(09,45,09,00)};

  eul_allocate_static_string(str_747, "dynamic variable ~a not available", 33);
  /* Byte-vector with size: 20 is_init: 0 index: 17 binding: anonymous */
  static const void *G00745[] = {I(aa,1b,12,1b),I(34,00,00,00),I(00,00,00,20),I(86,23,00,00),B(dynamic ,16),I(47,00,00,24),B(boot ,13),I(3d,03,02,32),I(00,00,00,2c),I(1c,10,47,00),I(00,1c,50,1b),I(34,00,00,00),I(00,00,00,10),I(1f,03,73,32),I(00,00,00,12),I(1f,03,11,1b),I(11,47,00,01),I(3d,01,05,22),I(01,22,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 16 is_init: 0 index: 18 binding: dynamic-variable-ref */
  static const void *G00748[] = {I(aa,46,02,1b),I(48,00,00,86),I(1b,48,00,01),I(23,00,00,00),B(dynamic ,11),I(23,00,00,00),B(dynamic ,17),I(3b,01,48,00),I(01,24,00,00),B(thread ,22),I(3c,00,1b,84),I(24,00,00,00),B(thread ,8),I(08,47,00,01),I(3d,01,03,45),I(03,00,00,00)};

  /* Byte-vector with size: 17 is_init: 0 index: 19 binding: push-error-handler */
  static const void *G00750[] = {I(aa,24,00,00),B(thread ,22),I(3c,00,24,00),B(dynamic ,4),I(2b,1b,89,00),B(dynamic ,4),I(2a,1c,26,00),I(00,00,00,03),I(24,00,00,00),B(thread ,8),I(08,1f,03,1c),I(0f,1f,03,1c),I(1c,26,00,00),I(00,00,00,03),I(1d,24,00,00),B(thread ,8),I(09,45,07,00)};

  /* Byte-vector with size: 8 is_init: 0 index: 20 binding: anonymous */
  static const void *G00752[] = {I(ab,1c,2d,1b),I(34,00,00,00),I(00,00,00,10),I(1c,32,00,00),I(00,00,00,11),I(1d,2c,1d,11),I(47,00,00,3d),I(02,03,45,03)};

  /* Byte-vector with size: 26 is_init: 0 index: 21 binding: pop-error-handlers */
  static const void *G00754[] = {I(aa,46,01,86),I(1b,48,00,00),I(23,00,00,00),B(dynamic ,11),I(23,00,00,00),B(dynamic ,20),I(3b,02,48,00),I(00,24,00,00),B(thread ,22),I(3c,00,24,00),B(dynamic ,4),I(1f,03,15,1b),I(89,00,00,00),B(dynamic ,4),I(2a,1c,26,00),I(00,00,00,03),I(24,00,00,00),B(thread ,8),I(08,1f,04,1c),I(47,00,00,3c),I(02,1f,03,1c),I(1c,26,00,00),I(00,00,00,03),I(1d,24,00,00),B(thread ,8),I(09,45,08,00)};

  /* Byte-vector with size: 10 is_init: 0 index: 22 binding: anonymous */
  static const void *G00756[] = {I(ab,1c,2d,1b),I(34,00,00,00),I(00,00,00,10),I(1c,32,00,00),I(00,00,00,17),I(1d,2c,1d,11),I(1b,11,1d,1c),I(47,00,00,3d),I(02,06,22,03),I(45,03,00,00)};

  /* Byte-vector with size: 22 is_init: 0 index: 23 binding: pop-dynamic-variables */
  static const void *G00758[] = {I(aa,46,01,86),I(1b,48,00,00),I(23,00,00,00),B(dynamic ,11),I(23,00,00,00),B(dynamic ,22),I(3b,02,48,00),I(00,24,00,00),B(thread ,22),I(3c,00,24,00),B(dynamic ,7),I(1f,03,15,1b),I(89,00,00,00),B(dynamic ,7),I(2a,1c,84,24),B(thread ,8),I(08,1f,04,1c),I(47,00,00,3c),I(02,1f,03,1c),I(1c,84,1d,24),B(thread ,8),I(09,45,08,00)};

  /* Byte-vector with size: 52 is_init: 1 index: 0 binding: initialize-dynamic */
  static const void *G00760[] = {I(87,25,00,00),B(dynamic ,1),I(24,00,00,00),B(thread ,1),I(3e,0b,24,00),B(thread ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(23,00,00,00),B(dynamic ,24),I(23,00,00,00),B(dynamic ,23),I(3b,01,25,00),B(dynamic ,8),I(86,25,00,00),B(dynamic ,7),I(23,00,00,00),B(dynamic ,25),I(23,00,00,00),B(dynamic ,21),I(3b,01,25,00),B(dynamic ,6),I(23,00,00,00),B(dynamic ,26),I(23,00,00,00),B(dynamic ,19),I(3b,01,25,00),B(dynamic ,5),I(86,25,00,00),B(dynamic ,4),I(23,00,00,00),B(dynamic ,27),I(23,00,00,00),B(dynamic ,18),I(3b,01,25,00),B(dynamic ,3),I(23,00,00,00),B(dynamic ,28),I(23,00,00,00),B(dynamic ,15),I(3b,02,25,00),B(dynamic ,2),I(23,00,00,00),B(dynamic ,29),I(23,00,00,00),B(dynamic ,14),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  object_class(str_736) = eul_static_string_class;
  eul_allocate_bytevector( G00735,G00734);
  eul_intern_symbol(sym_739,"anonymous");
  eul_allocate_bytevector( G00738,G00737);
  eul_intern_symbol(sym_742,"(setter dynamic-variable-ref)");
  eul_allocate_bytevector( G00741,G00740);
  eul_allocate_bytevector( G00744,G00743);
  object_class(str_747) = eul_static_string_class;
  eul_allocate_bytevector( G00746,G00745);
  eul_allocate_bytevector( G00749,G00748);
  eul_allocate_bytevector( G00751,G00750);
  eul_allocate_bytevector( G00753,G00752);
  eul_allocate_bytevector( G00755,G00754);
  eul_allocate_bytevector( G00757,G00756);
  eul_allocate_bytevector( G00759,G00758);
  eul_intern_symbol(sym_762,"pop-dynamic-variables");
  eul_intern_symbol(sym_763,"pop-error-handlers");
  eul_intern_symbol(sym_764,"push-error-handler");
  eul_intern_symbol(sym_765,"dynamic-variable-ref");
  eul_intern_symbol(sym_766,"push-dynamic-variable");
  eul_intern_symbol(sym_767,"top-level");
  eul_allocate_bytevector( G00761,G00760);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 9; i++)
      dynamic_bindings[i] = eul_nil;
  }

  dynamic_bindings[ 9] = str_736;
  dynamic_bindings[ 10] = G00735;
  dynamic_bindings[ 11] = sym_739;
  dynamic_bindings[ 12] = G00738;
  dynamic_bindings[ 13] = sym_742;
  dynamic_bindings[ 14] = G00741;
  dynamic_bindings[ 15] = G00744;
  dynamic_bindings[ 16] = str_747;
  dynamic_bindings[ 17] = G00746;
  dynamic_bindings[ 18] = G00749;
  dynamic_bindings[ 19] = G00751;
  dynamic_bindings[ 20] = G00753;
  dynamic_bindings[ 21] = G00755;
  dynamic_bindings[ 22] = G00757;
  dynamic_bindings[ 23] = G00759;
  dynamic_bindings[ 1] = eul_nil;
  dynamic_bindings[ 24] = sym_762;
  dynamic_bindings[ 25] = sym_763;
  dynamic_bindings[ 26] = sym_764;
  dynamic_bindings[ 27] = sym_765;
  dynamic_bindings[ 28] = sym_766;
  dynamic_bindings[ 29] = sym_767;
  eul_allocate_lambda( dynamic_bindings[0], "initialize-dynamic", 0, G00761);

  }
}


/* eof */
