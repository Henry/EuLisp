/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module table1
 **  Copyright: See file table1.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern void initialize_module_condition();
extern void initialize_module_convert();
extern void initialize_module_copy();
extern void initialize_module_collect();
extern void initialize_module_compare();
extern void initialize_module_list();
extern void initialize_module_format();
extern void initialize_module_fpi();
extern void initialize_module_string();
extern void initialize_module_vector();
extern LispRef string_bindings[];
extern LispRef fpi_bindings[];
extern LispRef list_bindings[];
extern LispRef copy_bindings[];
extern LispRef convert_bindings[];
extern LispRef telos_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef collect_bindings[];
extern LispRef mop_key_bindings[];
extern LispRef boot1_bindings[];
extern LispRef compare_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef mop_inspect_bindings[];
extern LispRef vector_bindings[];
extern LispRef boot_bindings[];
extern LispRef condition_bindings[];
extern LispRef format_bindings[];

/* Module bindings with size 105 */
LispRef table1_bindings[105];

/* Foreign functions */
static LispRef ff_stub_eul_hash_object8085 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G008205, res;

  POPVAL1(G008205);
  FF_RES_CONVERT0(res,eul_hash_object(FF_ARG_CONVERT8(G008205)));
  return res;
}


/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module table1 */
void initialize_module_table1()
{
  if (is_initialized) return;
  initialize_module_telos();
  initialize_module_condition();
  initialize_module_convert();
  initialize_module_copy();
  initialize_module_collect();
  initialize_module_compare();
  initialize_module_list();
  initialize_module_format();
  initialize_module_fpi();
  initialize_module_string();
  initialize_module_vector();
  eul_fast_table_set(eul_modules,"table1",(LispRef) table1_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_8204, sym_8203, sym_8202, sym_8201, sym_8200, sym_8199, sym_8198, G008197, G008195, G008193, G008191, G008189, G008187, G008185, sym_8183, sym_8182, sym_8181, sym_8180, sym_8179, sym_8178, sym_8177, sym_8176, sym_8175, sym_8174, sym_8173, sym_8172, sym_8171, sym_8170, sym_8169, sym_8168, sym_8167, sym_8166, sym_8165, sym_8164, sym_8163, sym_8160, sym_8159, sym_8158, key_8156, key_8155, key_8154, sym_8153, sym_8152, key_8151, sym_8150, key_8149, key_8148, sym_8147, sym_8146, key_8145, G008144, G008142, G008140, G008138, G008136, G008134, G008132, G008130, G008128, G008126, G008124, G008122, G008120, G008118, G008116, G008114, G008112, key_8107, key_8106, G008105, G008103, G008101, G008099, G008097, G008095, sym_8093, G008092, G008090, G008087;

  /* Code vector and literal definitions */
  eul_allocate_static_string(str_8088, "keys of table ~a not accessable", 31);
  /* Byte-vector with size: 10 is_init: 0 index: 23 binding: (method-table-keys) */
  static const void *G008086[] = {I(aa,23,00,00),B(table1 ,22),I(1c,24,00,00),B(format ,2),I(3c,02,24,00),B(condition ,8),I(1c,24,00,00),B(boot ,13),I(3d,02,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 24 binding: anonymous */
  static const void *G008089[] = {I(aa,1b,12,1b),I(44,04,86,36),I(11,1c,10,1b),I(47,00,00,0f),I(1b,48,00,00),I(47,00,00,22),I(02,45,02,00)};

  /* Byte-vector with size: 17 is_init: 0 index: 26 binding: (method-table-keys) */
  static const void *G008091[] = {I(aa,46,01,8a),I(03,24,00,00),B(table1 ,3),I(08,86,1b,48),I(00,00,1c,24),B(vector ,6),I(3c,01,1b,44),I(24,23,00,00),B(table1 ,25),I(23,00,00,00),B(table1 ,24),I(3b,01,1f,03),I(24,00,00,00),B(vector ,16),I(3c,02,2a,47),I(00,00,36,02),I(86,45,03,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 27 binding: anonymous */
  static const void *G008094[] = {I(aa,1b,12,1b),I(44,04,86,36),I(11,1c,11,1b),I(47,00,00,0f),I(1b,48,00,00),I(47,00,00,22),I(02,45,02,00)};

  /* Byte-vector with size: 17 is_init: 0 index: 28 binding: (method-table-values) */
  static const void *G008096[] = {I(aa,46,01,8a),I(03,24,00,00),B(table1 ,3),I(08,86,1b,48),I(00,00,1c,24),B(vector ,6),I(3c,01,1b,44),I(24,23,00,00),B(table1 ,25),I(23,00,00,00),B(table1 ,27),I(3b,01,1f,03),I(24,00,00,00),B(vector ,16),I(3c,02,2a,47),I(00,00,36,02),I(86,45,03,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 29 binding: (method-size) */
  static const void *G008098[] = {I(aa,83,24,00),B(table1 ,3),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 30 binding: (method-reset) */
  static const void *G008100[] = {I(aa,24,00,00),B(table1 ,14),I(3d,01,00,00)};

  /* Byte-vector with size: 14 is_init: 0 index: 31 binding: (method-clear-table) */
  static const void *G008102[] = {I(aa,1b,86,1c),I(8a,03,1d,24),B(table1 ,3),I(09,22,02,2a),I(1b,82,1c,83),I(1d,24,00,00),B(table1 ,3),I(09,22,02,2a),I(8a,10,8a,04),I(15,1c,1c,1c),I(82,1d,24,00),B(table1 ,3),I(09,22,02,2a),I(1c,45,02,00)};

  eul_allocate_static_cons(cons_8109, NULL, NULL);
  eul_allocate_static_cons(cons_8108, NULL, eul_as_static(cons_8109));
  eul_allocate_static_string(str_8110, "table initialization of ~a without hash function", 48);
  /* Byte-vector with size: 64 is_init: 0 index: 36 binding: (method-initialize) */
  static const void *G008104[] = {I(ab,1c,04,24),B(table1 ,3),I(50,1b,44,ee),I(1c,23,00,00),B(table1 ,32),I(24,00,00,00),B(boot ,32),I(3c,02,1d,23),B(table1 ,33),I(24,00,00,00),B(boot ,32),I(3c,02,24,00),B(mop_inspect ,8),I(3c,01,1b,44),I(1d,24,00,00),B(mop_gf ,2),I(24,00,00,00),B(table1 ,13),I(1f,05,24,00),B(boot ,5),I(3d,03,05,36),I(a1,1c,24,00),B(compare ,9),I(50,1b,44,1e),I(24,00,00,00),B(mop_gf ,2),I(24,00,00,00),B(table1 ,13),I(1f,06,24,00),B(boot ,5),I(3d,03,06,36),I(77,86,24,00),B(boot1 ,22),I(24,00,00,00),B(boot1 ,45),I(24,00,00,00),B(boot1 ,26),I(3c,03,1f,03),I(1c,86,6c,1b),I(44,32,1f,06),I(23,00,00,00),B(table1 ,34),I(24,00,00,00),B(mop_key ,3),I(3c,02,24,00),B(mop_gf ,2),I(24,00,00,00),B(table1 ,18),I(1d,24,00,00),B(boot ,5),I(3d,03,09,22),I(01,36,23,23),B(table1 ,35),I(1f,08,24,00),B(format ,2),I(3c,02,24,00),B(condition ,8),I(1c,24,00,00),B(boot ,13),I(3d,02,09,22),I(01,22,02,22),I(01,22,02,36),I(04,38,02,01),I(45,03,00,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 37 binding: (method-hash-table?) */
  static const void *G008111[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 38 binding: (method-hash-table?) */
  static const void *G008113[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 4 is_init: 0 index: 39 binding: (setter-table-comparator) */
  static const void *G008115[] = {I(ab,1c,83,1d),I(24,00,00,00),B(table1 ,13),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 40 binding: (setter-table-hash-function) */
  static const void *G008117[] = {I(ab,1c,82,1d),I(24,00,00,00),B(table1 ,13),I(09,45,02,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 41 binding: (method-simple-hash-table?) */
  static const void *G008119[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 42 binding: (method-simple-hash-table?) */
  static const void *G008121[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 43 binding: (method-table?) */
  static const void *G008123[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 44 binding: (method-table?) */
  static const void *G008125[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 4 is_init: 0 index: 45 binding: (setter-table-entries) */
  static const void *G008127[] = {I(ab,1c,8a,03),I(1d,24,00,00),B(table1 ,3),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 46 binding: (setter-table-size) */
  static const void *G008129[] = {I(ab,1c,83,1d),I(24,00,00,00),B(table1 ,3),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 47 binding: (setter-table-threshold) */
  static const void *G008131[] = {I(ab,1c,82,1d),I(24,00,00,00),B(table1 ,3),I(09,45,02,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 48 binding: anonymous */
  static const void *G008133[] = {I(a9,24,00,00),B(table1 ,10),I(45,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 49 binding: anonymous */
  static const void *G008135[] = {I(a9,24,00,00),B(boot1 ,45),I(45,00,00,00)};

  /* Byte-vector with size: 2 is_init: 0 index: 50 binding: anonymous */
  static const void *G008137[] = {I(a9,8a,10,8a),I(04,15,45,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 51 binding: anonymous */
  static const void *G008139[] = {I(a9,82,45,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 52 binding: anonymous */
  static const void *G008141[] = {I(aa,41,00,00),B(table1 ,21),I(45,01,00,00)};

  eul_allocate_static_cons(cons_8157, NULL, NULL);
  eul_allocate_static_cons(cons_8162, NULL, NULL);
  eul_allocate_static_cons(cons_8161, NULL, eul_as_static(cons_8162));
  /* Byte-vector with size: 691 is_init: 0 index: 91 binding: top-level */
  static const void *G008143[] = {I(a9,8a,10,89),B(table1 ,17),I(2a,84,89,00),B(table1 ,11),I(2a,8a,04,89),B(table1 ,4),I(2a,23,00,00),B(table1 ,25),I(23,00,00,00),B(table1 ,52),I(3b,01,89,00),B(table1 ,10),I(2a,24,00,00),B(collect ,22),I(24,00,00,00),B(boot1 ,26),I(3c,01,23,00),B(table1 ,53),I(23,00,00,00),B(table1 ,54),I(24,00,00,00),B(boot1 ,26),I(3c,02,23,00),B(table1 ,53),I(23,00,00,00),B(table1 ,55),I(23,00,00,00),B(table1 ,56),I(23,00,00,00),B(table1 ,57),I(24,00,00,00),B(boot1 ,26),I(3c,04,23,00),B(table1 ,53),I(23,00,00,00),B(table1 ,58),I(23,00,00,00),B(table1 ,59),I(23,00,00,00),B(table1 ,25),I(23,00,00,00),B(table1 ,51),I(3b,00,24,00),B(boot1 ,26),I(3c,04,23,00),B(table1 ,53),I(23,00,00,00),B(table1 ,60),I(23,00,00,00),B(table1 ,59),I(23,00,00,00),B(table1 ,25),I(23,00,00,00),B(table1 ,50),I(3b,00,24,00),B(boot1 ,26),I(3c,04,1f,03),I(1f,03,1f,03),I(1f,03,24,00),B(boot1 ,26),I(3c,04,24,00),B(mop_class ,81),I(23,00,00,00),B(table1 ,53),I(23,00,00,00),B(table1 ,61),I(23,00,00,00),B(table1 ,62),I(1f,09,23,00),B(table1 ,63),I(1f,06,23,00),B(table1 ,64),I(23,00,00,00),B(table1 ,65),I(24,00,00,00),B(mop_gf ,2),I(3c,09,1b,89),B(table1 ,3),I(2a,28,1c,1b),I(89,00,00,00),B(table1 ,18),I(2a,24,00,00),B(table1 ,3),I(24,00,00,00),B(boot1 ,26),I(3c,01,23,00),B(table1 ,53),I(23,00,00,00),B(table1 ,66),I(23,00,00,00),B(table1 ,59),I(23,00,00,00),B(table1 ,25),I(23,00,00,00),B(table1 ,49),I(3b,00,23,00),B(table1 ,56),I(23,00,00,00),B(table1 ,32),I(24,00,00,00),B(boot1 ,26),I(3c,06,23,00),B(table1 ,53),I(23,00,00,00),B(table1 ,67),I(23,00,00,00),B(table1 ,59),I(23,00,00,00),B(table1 ,25),I(23,00,00,00),B(table1 ,48),I(3b,00,23,00),B(table1 ,56),I(23,00,00,00),B(table1 ,33),I(24,00,00,00),B(boot1 ,26),I(3c,06,1c,1c),I(24,00,00,00),B(boot1 ,26),I(3c,02,24,00),B(mop_class ,81),I(23,00,00,00),B(table1 ,53),I(23,00,00,00),B(table1 ,68),I(23,00,00,00),B(table1 ,62),I(1f,07,23,00),B(table1 ,63),I(1f,06,23,00),B(table1 ,64),I(23,00,00,00),B(table1 ,69),I(24,00,00,00),B(mop_gf ,2),I(3c,09,1b,89),B(table1 ,13),I(2a,83,24,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(table1 ,70),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,63),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(table1 ,7),I(2a,83,24,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(table1 ,71),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,63),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(table1 ,6),I(2a,83,24,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(table1 ,72),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,63),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(table1 ,8),I(2a,83,24,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(table1 ,73),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,63),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(table1 ,14),I(2a,83,24,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(table1 ,74),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,63),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(table1 ,9),I(2a,83,24,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(table1 ,75),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,63),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(table1 ,2),I(2a,24,00,00),B(boot1 ,42),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(table1 ,16),I(23,00,00,00),B(table1 ,76),I(23,00,00,00),B(table1 ,47),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,42),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(table1 ,5),I(23,00,00,00),B(table1 ,77),I(23,00,00,00),B(table1 ,46),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,42),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(table1 ,19),I(23,00,00,00),B(table1 ,78),I(23,00,00,00),B(table1 ,45),I(3b,02,1d,3c),I(02,2a,24,00),B(table1 ,7),I(8a,03,02,83),I(24,00,00,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,7),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,79),I(23,00,00,00),B(table1 ,44),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,7),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,7),I(8a,03,02,83),I(24,00,00,00),B(table1 ,3),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,7),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,79),I(23,00,00,00),B(table1 ,43),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,7),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,7),I(2a,24,00,00),B(table1 ,3),I(2a,24,00,00),B(table1 ,18),I(24,00,00,00),B(mop_class ,81),I(05,2a,24,00),B(table1 ,3),I(24,00,00,00),B(boot1 ,26),I(3c,01,24,00),B(boot1 ,26),I(3c,00,23,00),B(table1 ,53),I(23,00,00,00),B(table1 ,80),I(23,00,00,00),B(table1 ,62),I(1f,04,23,00),B(table1 ,63),I(1f,05,23,00),B(table1 ,64),I(86,24,00,00),B(boot1 ,26),I(3c,08,24,00),B(table1 ,18),I(1c,24,00,00),B(mop_gf ,12),I(3c,02,2a,24),B(table1 ,6),I(8a,03,02,83),I(24,00,00,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,6),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,81),I(23,00,00,00),B(table1 ,42),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,6),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,6),I(8a,03,02,83),I(24,00,00,00),B(table1 ,18),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,6),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,81),I(23,00,00,00),B(table1 ,41),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,6),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,6),I(2a,24,00,00),B(table1 ,18),I(2a,24,00,00),B(boot1 ,42),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(table1 ,15),I(23,00,00,00),B(table1 ,82),I(23,00,00,00),B(table1 ,40),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,42),I(24,00,00,00),B(boot1 ,42),I(3c,01,24,00),B(table1 ,12),I(23,00,00,00),B(table1 ,83),I(23,00,00,00),B(table1 ,39),I(3b,02,1d,3c),I(02,2a,24,00),B(table1 ,8),I(8a,03,02,83),I(24,00,00,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,8),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,84),I(23,00,00,00),B(table1 ,38),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,8),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,8),I(8a,03,02,83),I(24,00,00,00),B(table1 ,13),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,8),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,84),I(23,00,00,00),B(table1 ,37),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,8),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,8),I(2a,24,00,00),B(table1 ,13),I(2a,24,00,00),B(mop_gf ,12),I(8a,03,02,84),I(24,00,00,00),B(table1 ,3),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(mop_gf ,12),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,85),I(23,00,00,00),B(table1 ,36),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(mop_gf ,12),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,14),I(2a,24,00,00),B(table1 ,14),I(8a,03,02,83),I(24,00,00,00),B(table1 ,3),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,14),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,86),I(23,00,00,00),B(table1 ,31),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,14),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,18),I(8a,03,02,83),I(24,00,00,00),B(table1 ,3),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(collect ,18),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,87),I(23,00,00,00),B(table1 ,30),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,18),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,9),I(8a,03,02,83),I(24,00,00,00),B(table1 ,3),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(collect ,9),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,88),I(23,00,00,00),B(table1 ,29),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,9),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,9),I(2a,24,00,00),B(table1 ,9),I(8a,03,02,83),I(24,00,00,00),B(table1 ,3),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,9),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,89),I(23,00,00,00),B(table1 ,28),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,9),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,2),I(2a,24,00,00),B(table1 ,2),I(8a,03,02,83),I(24,00,00,00),B(table1 ,3),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,2),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,90),I(23,00,00,00),B(table1 ,26),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,2),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,2),I(8a,03,02,83),I(24,00,00,00),B(table1 ,18),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,2),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,90),I(23,00,00,00),B(table1 ,23),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,2),I(1c,24,00,00),B(mop_meth ,5),I(3d,02,7b,45),I(7b,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 92 binding: table-size */
  static const void *G008184[] = {I(aa,83,24,00),B(table1 ,3),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 93 binding: table-comparator */
  static const void *G008186[] = {I(aa,83,24,00),B(table1 ,13),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 94 binding: table-hash-function */
  static const void *G008188[] = {I(aa,82,24,00),B(table1 ,13),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 95 binding: table-threshold */
  static const void *G008190[] = {I(aa,82,24,00),B(table1 ,3),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 96 binding: table-entries */
  static const void *G008192[] = {I(aa,8a,03,24),B(table1 ,3),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 97 binding: table-fill-value */
  static const void *G008194[] = {I(aa,84,24,00),B(table1 ,3),I(08,45,00,00)};

  /* Byte-vector with size: 119 is_init: 1 index: 0 binding: initialize-table1 */
  static const void *G008196[] = {I(87,25,00,00),B(table1 ,1),I(24,00,00,00),B(vector ,1),I(3e,0b,24,00),B(vector ,0),I(3c,00,21,01),I(24,00,00,00),B(string ,1),I(3e,0b,24,00),B(string ,0),I(3c,00,21,01),I(24,00,00,00),B(fpi ,1),I(3e,0b,24,00),B(fpi ,0),I(3c,00,21,01),I(24,00,00,00),B(format ,1),I(3e,0b,24,00),B(format ,0),I(3c,00,21,01),I(24,00,00,00),B(list ,1),I(3e,0b,24,00),B(list ,0),I(3c,00,21,01),I(24,00,00,00),B(compare ,1),I(3e,0b,24,00),B(compare ,0),I(3c,00,21,01),I(24,00,00,00),B(collect ,1),I(3e,0b,24,00),B(collect ,0),I(3c,00,21,01),I(24,00,00,00),B(copy ,1),I(3e,0b,24,00),B(copy ,0),I(3c,00,21,01),I(24,00,00,00),B(convert ,1),I(3e,0b,24,00),B(convert ,0),I(3c,00,21,01),I(24,00,00,00),B(condition ,1),I(3e,0b,24,00),B(condition ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(23,00,00,00),B(table1 ,98),I(23,00,00,00),B(table1 ,97),I(3b,01,25,00),B(table1 ,20),I(23,00,00,00),B(table1 ,99),I(23,00,00,00),B(table1 ,96),I(3b,01,25,00),B(table1 ,19),I(86,25,00,00),B(table1 ,18),I(23,00,00,00),B(table1 ,100),I(23,00,00,00),B(table1 ,95),I(3b,01,25,00),B(table1 ,16),I(23,00,00,00),B(table1 ,101),I(23,00,00,00),B(table1 ,94),I(3b,01,25,00),B(table1 ,15),I(86,25,00,00),B(table1 ,14),I(86,25,00,00),B(table1 ,13),I(23,00,00,00),B(table1 ,102),I(23,00,00,00),B(table1 ,93),I(3b,01,25,00),B(table1 ,12),I(86,25,00,00),B(table1 ,10),I(86,25,00,00),B(table1 ,9),I(86,25,00,00),B(table1 ,8),I(86,25,00,00),B(table1 ,7),I(86,25,00,00),B(table1 ,6),I(23,00,00,00),B(table1 ,103),I(23,00,00,00),B(table1 ,92),I(3b,01,25,00),B(table1 ,5),I(86,25,00,00),B(table1 ,3),I(86,25,00,00),B(table1 ,2),I(23,00,00,00),B(table1 ,104),I(23,00,00,00),B(table1 ,91),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  object_class(str_8088) = eul_static_string_class;
  eul_allocate_bytevector( G008087,G008086);
  eul_allocate_bytevector( G008090,G008089);
  eul_intern_symbol(sym_8093,"anonymous");
  eul_allocate_bytevector( G008092,G008091);
  eul_allocate_bytevector( G008095,G008094);
  eul_allocate_bytevector( G008097,G008096);
  eul_allocate_bytevector( G008099,G008098);
  eul_allocate_bytevector( G008101,G008100);
  eul_allocate_bytevector( G008103,G008102);
  eul_intern_keyword(key_8106,"comparator");
  eul_intern_keyword(key_8107,"hash-function");
  object_class(cons_8109) = eul_static_cons_class;
  eul_car(cons_8109) = key_8107;
  eul_cdr(cons_8109) = eul_nil;
  object_class(cons_8108) = eul_static_cons_class;
  eul_car(cons_8108) = key_8106;
  object_class(str_8110) = eul_static_string_class;
  eul_allocate_bytevector( G008105,G008104);
  eul_allocate_bytevector( G008112,G008111);
  eul_allocate_bytevector( G008114,G008113);
  eul_allocate_bytevector( G008116,G008115);
  eul_allocate_bytevector( G008118,G008117);
  eul_allocate_bytevector( G008120,G008119);
  eul_allocate_bytevector( G008122,G008121);
  eul_allocate_bytevector( G008124,G008123);
  eul_allocate_bytevector( G008126,G008125);
  eul_allocate_bytevector( G008128,G008127);
  eul_allocate_bytevector( G008130,G008129);
  eul_allocate_bytevector( G008132,G008131);
  eul_allocate_bytevector( G008134,G008133);
  eul_allocate_bytevector( G008136,G008135);
  eul_allocate_bytevector( G008138,G008137);
  eul_allocate_bytevector( G008140,G008139);
  eul_allocate_bytevector( G008142,G008141);
  eul_intern_keyword(key_8145,"name");
  eul_intern_symbol(sym_8146,"entries");
  eul_intern_symbol(sym_8147,"fill-value");
  eul_intern_keyword(key_8148,"keyword");
  eul_intern_keyword(key_8149,"fill-value");
  eul_intern_symbol(sym_8150,"size");
  eul_intern_keyword(key_8151,"default");
  eul_intern_symbol(sym_8152,"threshold");
  eul_intern_symbol(sym_8153,"table");
  eul_intern_keyword(key_8154,"direct-superclasses");
  eul_intern_keyword(key_8155,"direct-slots");
  eul_intern_keyword(key_8156,"direct-keywords");
  object_class(cons_8157) = eul_static_cons_class;
  eul_car(cons_8157) = key_8149;
  eul_cdr(cons_8157) = eul_nil;
  eul_intern_symbol(sym_8158,"comparator");
  eul_intern_symbol(sym_8159,"hash-function");
  eul_intern_symbol(sym_8160,"hash-table");
  object_class(cons_8162) = eul_static_cons_class;
  eul_car(cons_8162) = key_8106;
  eul_cdr(cons_8162) = eul_nil;
  object_class(cons_8161) = eul_static_cons_class;
  eul_car(cons_8161) = key_8107;
  eul_intern_symbol(sym_8163,"table?");
  eul_intern_symbol(sym_8164,"simple-hash-table?");
  eul_intern_symbol(sym_8165,"hash-table?");
  eul_intern_symbol(sym_8166,"clear-table");
  eul_intern_symbol(sym_8167,"table-values");
  eul_intern_symbol(sym_8168,"table-keys");
  eul_intern_symbol(sym_8169,"(setter table-threshold)");
  eul_intern_symbol(sym_8170,"(setter table-size)");
  eul_intern_symbol(sym_8171,"(setter table-entries)");
  eul_intern_symbol(sym_8172,"(method table?)");
  eul_intern_symbol(sym_8173,"simple-hash-table");
  eul_intern_symbol(sym_8174,"(method simple-hash-table?)");
  eul_intern_symbol(sym_8175,"(setter table-hash-function)");
  eul_intern_symbol(sym_8176,"(setter table-comparator)");
  eul_intern_symbol(sym_8177,"(method hash-table?)");
  eul_intern_symbol(sym_8178,"(method initialize)");
  eul_intern_symbol(sym_8179,"(method clear-table)");
  eul_intern_symbol(sym_8180,"(method reset)");
  eul_intern_symbol(sym_8181,"(method size)");
  eul_intern_symbol(sym_8182,"(method table-values)");
  eul_intern_symbol(sym_8183,"(method table-keys)");
  eul_allocate_bytevector( G008144,G008143);
  eul_allocate_bytevector( G008185,G008184);
  eul_allocate_bytevector( G008187,G008186);
  eul_allocate_bytevector( G008189,G008188);
  eul_allocate_bytevector( G008191,G008190);
  eul_allocate_bytevector( G008193,G008192);
  eul_allocate_bytevector( G008195,G008194);
  eul_intern_symbol(sym_8198,"table-fill-value");
  eul_intern_symbol(sym_8199,"table-entries");
  eul_intern_symbol(sym_8200,"table-threshold");
  eul_intern_symbol(sym_8201,"table-hash-function");
  eul_intern_symbol(sym_8202,"table-comparator");
  eul_intern_symbol(sym_8203,"table-size");
  eul_intern_symbol(sym_8204,"top-level");
  eul_allocate_bytevector( G008197,G008196);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 21; i++)
      table1_bindings[i] = eul_nil;
  }

  table1_bindings[ 21] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_hash_object8085;
  table1_bindings[ 22] = str_8088;
  table1_bindings[ 23] = G008087;
  table1_bindings[ 24] = G008090;
  table1_bindings[ 25] = sym_8093;
  table1_bindings[ 26] = G008092;
  table1_bindings[ 27] = G008095;
  table1_bindings[ 28] = G008097;
  table1_bindings[ 29] = G008099;
  table1_bindings[ 30] = G008101;
  table1_bindings[ 31] = G008103;
  table1_bindings[ 32] = key_8106;
  table1_bindings[ 33] = key_8107;
  table1_bindings[ 34] = cons_8108;
  table1_bindings[ 35] = str_8110;
  table1_bindings[ 36] = G008105;
  table1_bindings[ 37] = G008112;
  table1_bindings[ 38] = G008114;
  table1_bindings[ 39] = G008116;
  table1_bindings[ 40] = G008118;
  table1_bindings[ 41] = G008120;
  table1_bindings[ 42] = G008122;
  table1_bindings[ 43] = G008124;
  table1_bindings[ 44] = G008126;
  table1_bindings[ 45] = G008128;
  table1_bindings[ 46] = G008130;
  table1_bindings[ 47] = G008132;
  table1_bindings[ 48] = G008134;
  table1_bindings[ 49] = G008136;
  table1_bindings[ 50] = G008138;
  table1_bindings[ 51] = G008140;
  table1_bindings[ 52] = G008142;
  table1_bindings[ 53] = key_8145;
  table1_bindings[ 54] = sym_8146;
  table1_bindings[ 55] = sym_8147;
  table1_bindings[ 56] = key_8148;
  table1_bindings[ 57] = key_8149;
  table1_bindings[ 58] = sym_8150;
  table1_bindings[ 59] = key_8151;
  table1_bindings[ 60] = sym_8152;
  table1_bindings[ 61] = sym_8153;
  table1_bindings[ 62] = key_8154;
  table1_bindings[ 63] = key_8155;
  table1_bindings[ 64] = key_8156;
  table1_bindings[ 65] = cons_8157;
  table1_bindings[ 66] = sym_8158;
  table1_bindings[ 67] = sym_8159;
  table1_bindings[ 68] = sym_8160;
  table1_bindings[ 69] = cons_8161;
  table1_bindings[ 70] = sym_8163;
  table1_bindings[ 71] = sym_8164;
  table1_bindings[ 72] = sym_8165;
  table1_bindings[ 73] = sym_8166;
  table1_bindings[ 74] = sym_8167;
  table1_bindings[ 75] = sym_8168;
  table1_bindings[ 76] = sym_8169;
  table1_bindings[ 77] = sym_8170;
  table1_bindings[ 78] = sym_8171;
  table1_bindings[ 79] = sym_8172;
  table1_bindings[ 80] = sym_8173;
  table1_bindings[ 81] = sym_8174;
  table1_bindings[ 82] = sym_8175;
  table1_bindings[ 83] = sym_8176;
  table1_bindings[ 84] = sym_8177;
  table1_bindings[ 85] = sym_8178;
  table1_bindings[ 86] = sym_8179;
  table1_bindings[ 87] = sym_8180;
  table1_bindings[ 88] = sym_8181;
  table1_bindings[ 89] = sym_8182;
  table1_bindings[ 90] = sym_8183;
  table1_bindings[ 91] = G008144;
  table1_bindings[ 92] = G008185;
  table1_bindings[ 93] = G008187;
  table1_bindings[ 94] = G008189;
  table1_bindings[ 95] = G008191;
  table1_bindings[ 96] = G008193;
  table1_bindings[ 97] = G008195;
  table1_bindings[ 1] = eul_nil;
  table1_bindings[ 98] = sym_8198;
  table1_bindings[ 99] = sym_8199;
  table1_bindings[ 100] = sym_8200;
  table1_bindings[ 101] = sym_8201;
  table1_bindings[ 102] = sym_8202;
  table1_bindings[ 103] = sym_8203;
  table1_bindings[ 104] = sym_8204;
  eul_allocate_lambda( table1_bindings[0], "initialize-table1", 0, G008197);

  }
}


/* eof */
