/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module copy
 **  Copyright: See file copy.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern void initialize_module_thread();
extern void initialize_module_condition();
extern LispRef condition_bindings[];
extern LispRef telos_bindings[];
extern LispRef thread_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef boot_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef boot1_bindings[];
extern LispRef mop_defcl_bindings[];

/* Module bindings with size 23 */
LispRef copy_bindings[23];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module copy */
void initialize_module_copy()
{
  if (is_initialized) return;
  initialize_module_telos();
  initialize_module_thread();
  initialize_module_condition();
  eul_fast_table_set(eul_modules,"copy",(LispRef) copy_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_1748, G001747, sym_1745, sym_1744, sym_1743, sym_1742, G001741, G001739, G001737, G001735, G001733, G001731, G001729, G001727, G001725, G001723, G001721, sym_1719, G001718, G001716;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 19 is_init: 0 index: 4 binding: anonymous */
  static const void *G001715[] = {I(aa,1b,12,1b),I(34,00,00,00),I(00,00,00,10),I(47,00,00,32),I(00,00,00,38),I(1c,10,24,00),B(mop_defcl ,10),I(24,00,00,00),B(boot1 ,41),I(3c,01,1c,47),I(00,00,24,00),B(mop_defcl ,10),I(3c,02,1b,24),B(copy ,3),I(3c,01,1f,03),I(47,00,00,1d),I(1f,05,3d,03),I(06,22,04,45),I(02,00,00,00)};

  /* Byte-vector with size: 13 is_init: 0 index: 6 binding: (method-deep-copy) */
  static const void *G001717[] = {I(aa,46,02,04),I(1b,86,24,00),B(mop_gf ,9),I(3c,02,1b,48),I(00,00,86,23),B(copy ,5),I(23,00,00,00),B(copy ,4),I(3b,01,20,01),I(1d,26,00,00),I(00,00,00,04),I(02,1c,3d,01),I(03,45,03,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 7 binding: (method-deep-copy) */
  static const void *G001720[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 8 binding: (method-deep-copy) */
  static const void *G001722[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 9 binding: (method-deep-copy) */
  static const void *G001724[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 16 is_init: 0 index: 10 binding: anonymous */
  static const void *G001726[] = {I(aa,1b,12,1b),I(34,00,00,00),I(00,00,00,10),I(47,00,00,32),I(00,00,00,2f),I(1c,10,24,00),B(mop_defcl ,10),I(24,00,00,00),B(boot1 ,41),I(3c,01,1c,47),I(00,00,24,00),B(mop_defcl ,10),I(3c,02,1d,47),I(00,00,1d,1f),I(04,3d,03,05),I(22,03,45,02)};

  /* Byte-vector with size: 13 is_init: 0 index: 11 binding: (method-shallow-copy) */
  static const void *G001728[] = {I(aa,46,02,04),I(1b,86,24,00),B(mop_gf ,9),I(3c,02,1b,48),I(00,00,86,23),B(copy ,5),I(23,00,00,00),B(copy ,10),I(3b,01,20,01),I(1d,26,00,00),I(00,00,00,04),I(02,1c,3d,01),I(03,45,03,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 12 binding: (method-deep-copy) */
  static const void *G001730[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 13 binding: (method-shallow-copy) */
  static const void *G001732[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 14 binding: (method-shallow-copy) */
  static const void *G001734[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 15 binding: (method-shallow-copy) */
  static const void *G001736[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 16 binding: (method-shallow-copy) */
  static const void *G001738[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 324 is_init: 0 index: 21 binding: top-level */
  static const void *G001740[] = {I(a9,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(copy ,17),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(copy ,2),I(2a,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(copy ,18),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(copy ,3),I(2a,24,00,00),B(copy ,2),I(2a,24,00,00),B(copy ,2),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,75),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(copy ,2),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(copy ,19),I(23,00,00,00),B(copy ,16),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(copy ,2),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(copy ,2),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,28),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(copy ,2),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(copy ,19),I(23,00,00,00),B(copy ,15),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(copy ,2),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(copy ,2),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(thread ,7),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(copy ,2),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(copy ,19),I(23,00,00,00),B(copy ,14),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(copy ,2),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(copy ,2),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,34),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(copy ,2),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(copy ,19),I(23,00,00,00),B(copy ,13),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(copy ,2),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(copy ,3),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,34),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(copy ,3),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(copy ,20),I(23,00,00,00),B(copy ,12),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(copy ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(copy ,2),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(copy ,2),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(copy ,19),I(23,00,00,00),B(copy ,11),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(copy ,2),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(copy ,3),I(2a,24,00,00),B(copy ,3),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,75),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(copy ,3),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(copy ,20),I(23,00,00,00),B(copy ,9),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(copy ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(copy ,3),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,28),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(copy ,3),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(copy ,20),I(23,00,00,00),B(copy ,8),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(copy ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(copy ,3),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(thread ,7),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(copy ,3),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(copy ,20),I(23,00,00,00),B(copy ,7),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(copy ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(copy ,3),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(copy ,3),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(copy ,20),I(23,00,00,00),B(copy ,6),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(copy ,3),I(1c,24,00,00),B(mop_meth ,5),I(3d,02,44,45),I(44,00,00,00)};

  /* Byte-vector with size: 27 is_init: 1 index: 0 binding: initialize-copy */
  static const void *G001746[] = {I(87,25,00,00),B(copy ,1),I(24,00,00,00),B(condition ,1),I(3e,0b,24,00),B(condition ,0),I(3c,00,21,01),I(24,00,00,00),B(thread ,1),I(3e,0b,24,00),B(thread ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(86,25,00,00),B(copy ,3),I(86,25,00,00),B(copy ,2),I(23,00,00,00),B(copy ,22),I(23,00,00,00),B(copy ,21),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G001716,G001715);
  eul_intern_symbol(sym_1719,"anonymous");
  eul_allocate_bytevector( G001718,G001717);
  eul_allocate_bytevector( G001721,G001720);
  eul_allocate_bytevector( G001723,G001722);
  eul_allocate_bytevector( G001725,G001724);
  eul_allocate_bytevector( G001727,G001726);
  eul_allocate_bytevector( G001729,G001728);
  eul_allocate_bytevector( G001731,G001730);
  eul_allocate_bytevector( G001733,G001732);
  eul_allocate_bytevector( G001735,G001734);
  eul_allocate_bytevector( G001737,G001736);
  eul_allocate_bytevector( G001739,G001738);
  eul_intern_symbol(sym_1742,"shallow-copy");
  eul_intern_symbol(sym_1743,"deep-copy");
  eul_intern_symbol(sym_1744,"(method shallow-copy)");
  eul_intern_symbol(sym_1745,"(method deep-copy)");
  eul_allocate_bytevector( G001741,G001740);
  eul_intern_symbol(sym_1748,"top-level");
  eul_allocate_bytevector( G001747,G001746);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 4; i++)
      copy_bindings[i] = eul_nil;
  }

  copy_bindings[ 4] = G001716;
  copy_bindings[ 5] = sym_1719;
  copy_bindings[ 6] = G001718;
  copy_bindings[ 7] = G001721;
  copy_bindings[ 8] = G001723;
  copy_bindings[ 9] = G001725;
  copy_bindings[ 10] = G001727;
  copy_bindings[ 11] = G001729;
  copy_bindings[ 12] = G001731;
  copy_bindings[ 13] = G001733;
  copy_bindings[ 14] = G001735;
  copy_bindings[ 15] = G001737;
  copy_bindings[ 16] = G001739;
  copy_bindings[ 17] = sym_1742;
  copy_bindings[ 18] = sym_1743;
  copy_bindings[ 19] = sym_1744;
  copy_bindings[ 20] = sym_1745;
  copy_bindings[ 21] = G001741;
  copy_bindings[ 1] = eul_nil;
  copy_bindings[ 22] = sym_1748;
  eul_allocate_lambda( copy_bindings[0], "initialize-copy", 0, G001747);

  }
}


/* eof */
