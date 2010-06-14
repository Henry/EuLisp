/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module table
 **  Copyright: See file table.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern void initialize_module_convert();
extern void initialize_module_copy();
extern void initialize_module_collect();
extern void initialize_module_compare();
extern void initialize_module_list();
extern void initialize_module_fpi();
extern void initialize_module_string();
extern void initialize_module_vector();
extern void initialize_module_table1();
extern LispRef fpi_bindings[];
extern LispRef compare_bindings[];
extern LispRef copy_bindings[];
extern LispRef convert_bindings[];
extern LispRef telos_bindings[];
extern LispRef list_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef string_bindings[];
extern LispRef boot1_bindings[];
extern LispRef vector_bindings[];
extern LispRef boot_bindings[];
extern LispRef collect_bindings[];
extern LispRef table1_bindings[];

/* Module bindings with size 84 */
LispRef table_bindings[84];

/* Foreign functions */
static LispRef ff_stub_eul_table_ref8065 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G008178, G008179, res;

  POPVAL1(G008179);
  POPVAL1(G008178);
  FF_RES_CONVERT6(res,eul_table_ref(FF_ARG_CONVERT8(G008178), FF_ARG_CONVERT3(G008179)));
  return res;
}

static LispRef ff_stub_eul_addr_str8066 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G008180, res;

  POPVAL1(G008180);
  FF_RES_CONVERT3(res,eul_addr_str(FF_ARG_CONVERT8(G008180)));
  return res;
}

static LispRef ff_stub_eul_table_set8067 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G008181, G008182, G008183, res;

  POPVAL1(G008183);
  POPVAL1(G008182);
  POPVAL1(G008181);
  FF_RES_CONVERT6(res,eul_table_set(FF_ARG_CONVERT8(G008181), FF_ARG_CONVERT3(G008182), FF_ARG_CONVERT8(G008183)));
  return res;
}


/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module table */
void initialize_module_table()
{
  if (is_initialized) return;
  initialize_module_telos();
  initialize_module_convert();
  initialize_module_copy();
  initialize_module_collect();
  initialize_module_compare();
  initialize_module_list();
  initialize_module_fpi();
  initialize_module_string();
  initialize_module_vector();
  initialize_module_table1();
  eul_fast_table_set(eul_modules,"table",(LispRef) table_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_8177, sym_8176, sym_8175, sym_8174, sym_8173, sym_8172, sym_8171, sym_8170, sym_8169, G008168, G008166, G008164, G008162, G008160, G008158, G008156, G008154, G008152, G008150, G008148, G008146, G008144, G008142, G008140, sym_8138, sym_8136, sym_8134, sym_8133, sym_8132, sym_8131, sym_8130, sym_8129, sym_8128, sym_8127, sym_8126, sym_8124, sym_8122, sym_8121, sym_8120, sym_8119, G008118, G008116, G008114, G008111, G008109, G008107, G008105, G008103, G008101, G008098, G008096, G008094, G008092, G008090, G008087, G008084, G008081, G008078, sym_8076, G008075, G008073, G008071, G008069;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 3 is_init: 0 index: 14 binding: (method-accumulate1) */
  static const void *G008068[] = {I(ab,24,00,00),B(table ,10),I(3d,02,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 15 binding: (method-accumulate) */
  static const void *G008070[] = {I(43,03,24,00),B(table ,2),I(3d,03,00,00)};

  /* Byte-vector with size: 10 is_init: 0 index: 16 binding: anonymous */
  static const void *G008072[] = {I(aa,1b,7a,1b),I(34,00,00,00),I(00,00,00,20),I(1c,10,1d,11),I(47,00,00,1d),I(1d,47,00,01),I(3d,03,04,22),I(02,32,00,00),I(00,00,00,08),I(86,45,02,00)};

  /* Byte-vector with size: 24 is_init: 0 index: 18 binding: (method-member) */
  static const void *G008074[] = {I(43,fd,46,02),I(1d,48,00,00),I(1b,12,1b,34),I(00,00,00,19),I(1d,47,00,00),I(24,00,00,00),B(table ,5),I(3d,02,04,32),I(00,00,00,3e),I(1c,10,1b,48),I(00,01,1f,03),I(26,00,00,00),I(00,00,00,03),I(24,00,00,00),B(table1 ,4),I(08,47,00,00),I(1c,23,00,00),B(table ,17),I(23,00,00,00),B(table ,16),I(3b,01,24,00),B(collect ,13),I(3d,03,06,22),I(02,45,04,00)};

  eul_allocate_static_string(str_8079, "all? on multiple tables not yet implemented", 43);
  /* Byte-vector with size: 13 is_init: 0 index: 20 binding: (method-all?) */
  static const void *G008077[] = {I(43,fd,12,1b),I(34,00,00,00),I(00,00,00,18),I(1d,1d,24,00),B(table ,3),I(3d,02,03,32),I(00,00,00,18),I(23,00,00,00),B(table ,19),I(24,00,00,00),B(boot ,22),I(3d,01,03,45),I(03,00,00,00)};

  eul_allocate_static_string(str_8082, "anyp on multiple tables not yet implemented", 43);
  /* Byte-vector with size: 13 is_init: 0 index: 22 binding: (method-anyp) */
  static const void *G008080[] = {I(43,fd,12,1b),I(34,00,00,00),I(00,00,00,18),I(1d,1d,24,00),B(table ,8),I(3d,02,03,32),I(00,00,00,18),I(23,00,00,00),B(table ,21),I(24,00,00,00),B(boot ,22),I(3d,01,03,45),I(03,00,00,00)};

  eul_allocate_static_string(str_8085, "map on multiple tables not yet implemented", 42);
  /* Byte-vector with size: 13 is_init: 0 index: 24 binding: (method-map) */
  static const void *G008083[] = {I(43,fd,12,1b),I(34,00,00,00),I(00,00,00,18),I(1d,1d,24,00),B(table ,6),I(3d,02,03,32),I(00,00,00,18),I(23,00,00,00),B(table ,23),I(24,00,00,00),B(boot ,22),I(3d,01,03,45),I(03,00,00,00)};

  eul_allocate_static_string(str_8088, "do on multiple tables not yet implemented", 41);
  /* Byte-vector with size: 13 is_init: 0 index: 26 binding: (method-do) */
  static const void *G008086[] = {I(43,fd,12,1b),I(34,00,00,00),I(00,00,00,18),I(1d,1d,24,00),B(table ,9),I(3d,02,03,32),I(00,00,00,18),I(23,00,00,00),B(table ,25),I(24,00,00,00),B(boot ,22),I(3d,01,03,45),I(03,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 27 binding: (method-emptyp) */
  static const void *G008089[] = {I(aa,24,00,00),B(table ,4),I(3d,01,00,00)};

  /* Byte-vector with size: 52 is_init: 0 index: 28 binding: anonymous */
  static const void *G008091[] = {I(aa,47,00,03),I(1c,02,1b,7a),I(1b,34,00,00),I(00,00,00,5b),I(1c,10,1b,47),I(00,01,47,00),I(04,3c,02,1b),I(34,00,00,00),I(00,00,00,1c),I(1f,03,11,1f),I(04,47,00,02),I(90,2a,1b,22),I(01,32,00,00),I(00,00,00,2c),I(1f,04,2b,1b),I(47,00,06,1a),I(1b,34,00,00),I(00,00,00,13),I(1c,47,00,05),I(3d,01,07,32),I(00,00,00,0c),I(82,47,00,05),I(3d,01,07,22),I(02,22,02,32),I(00,00,00,6c),I(47,00,00,83),I(24,00,00,00),B(table1 ,4),I(08,2b,47,00),I(01,47,00,02),I(0f,47,00,03),I(1f,05,1d,03),I(2a,47,00,00),I(1d,1c,83,1d),I(24,00,00,00),B(table1 ,4),I(09,22,02,2a),I(47,00,00,82),I(24,00,00,00),B(table1 ,4),I(08,1d,1c,1a),I(1b,34,00,00),I(00,00,00,0f),I(86,32,00,00),I(00,00,00,11),I(47,00,00,24),B(table ,7),I(3c,01,2a,47),I(00,00,84,24),B(table1 ,4),I(08,22,04,45),I(03,00,00,00)};

  /* Byte-vector with size: 48 is_init: 0 index: 29 binding: (method-(setter table-ref)) */
  static const void *G008093[] = {I(43,03,46,0a),I(1d,48,00,00),I(1c,48,00,01),I(1b,48,00,02),I(47,00,00,26),I(00,00,00,03),I(24,00,00,00),B(table1 ,4),I(08,47,00,00),I(83,24,00,00),B(table1 ,8),I(08,47,00,00),I(82,24,00,00),B(table1 ,8),I(08,1d,48,00),I(03,1c,48,00),I(04,47,00,03),I(24,00,00,00),B(vector ,5),I(3c,01,1b,34),I(00,00,00,0d),I(86,32,00,00),I(00,00,00,36),I(26,00,00,00),I(00,00,00,10),I(24,00,00,00),B(boot1 ,38),I(3c,01,1b,48),I(00,03,47,00),I(03,47,00,00),I(1c,1c,26,00),I(00,00,00,03),I(1d,24,00,00),B(table1 ,4),I(09,22,04,2a),I(47,00,03,06),I(1b,48,00,06),I(47,00,01,1f),I(03,3c,01,1b),I(47,00,06,18),I(86,1b,48,00),I(05,23,00,00),B(table ,17),I(23,00,00,00),B(table ,28),I(3b,01,48,00),I(05,1c,47,00),I(05,3d,01,0b)};

  /* Byte-vector with size: 6 is_init: 0 index: 30 binding: (method-(setter table-ref)) */
  static const void *G008095[] = {I(43,03,1c,24),B(collect ,8),I(3c,01,1f,03),I(1c,1f,03,41),B(table ,13),I(45,07,00,00)};

  eul_allocate_static_string(str_8099, "", 1);
  /* Byte-vector with size: 8 is_init: 0 index: 32 binding: (method-(setter table-ref)) */
  static const void *G008097[] = {I(43,03,1c,23),B(table ,31),I(24,00,00,00),B(string ,11),I(3c,02,1f,03),I(1c,1f,03,41),B(table ,13),I(45,07,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 33 binding: (method-(setter table-ref)) */
  static const void *G008100[] = {I(43,03,1c,41),B(table ,12),I(22,01,1f,03),I(1c,1f,03,41),B(table ,13),I(45,07,00,00)};

  /* Byte-vector with size: 8 is_init: 0 index: 34 binding: (method-(setter element)) */
  static const void *G008102[] = {I(43,03,24,00),B(table ,5),I(24,00,00,00),B(boot1 ,40),I(3c,01,1f,03),I(1f,03,1f,03),I(1f,03,3d,03),I(04,45,04,00)};

  /* Byte-vector with size: 26 is_init: 0 index: 35 binding: anonymous */
  static const void *G008104[] = {I(aa,47,00,02),I(1c,02,1b,7a),I(1b,34,00,00),I(00,00,00,4f),I(1c,10,1b,47),I(00,01,47,00),I(03,3c,02,1b),I(34,00,00,00),I(00,00,00,10),I(1f,03,11,32),I(00,00,00,2a),I(1f,04,2b,1b),I(47,00,05,1a),I(1b,34,00,00),I(00,00,00,13),I(1c,47,00,04),I(3d,01,07,32),I(00,00,00,0c),I(82,47,00,04),I(3d,01,07,22),I(02,22,02,32),I(00,00,00,12),I(47,00,00,84),I(24,00,00,00),B(table1 ,4),I(08,45,03,00)};

  /* Byte-vector with size: 39 is_init: 0 index: 36 binding: (method-table-ref) */
  static const void *G008106[] = {I(ab,46,07,1c),I(48,00,00,1b),I(48,00,01,47),I(00,00,26,00),I(00,00,00,03),I(24,00,00,00),B(table1 ,4),I(08,47,00,00),I(83,24,00,00),B(table1 ,8),I(08,47,00,00),I(82,24,00,00),B(table1 ,8),I(08,1d,48,00),I(02,1c,48,00),I(03,47,00,02),I(24,00,00,00),B(vector ,5),I(3c,01,1b,34),I(00,00,00,41),I(47,00,02,06),I(1b,48,00,05),I(47,00,01,1f),I(03,3c,01,1b),I(47,00,05,18),I(86,1b,48,00),I(04,23,00,00),B(table ,17),I(23,00,00,00),B(table ,35),I(3b,01,48,00),I(04,1c,47,00),I(04,3d,01,0a),I(22,04,32,00),I(00,00,00,13),I(47,00,00,84),I(24,00,00,00),B(table1 ,4),I(08,45,06,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 37 binding: (method-table-ref) */
  static const void *G008108[] = {I(ab,24,00,00),B(collect ,8),I(3c,01,41,00),B(table ,11),I(45,02,00,00)};

  eul_allocate_static_string(str_8112, "", 1);
  /* Byte-vector with size: 7 is_init: 0 index: 39 binding: (method-table-ref) */
  static const void *G008110[] = {I(ab,23,00,00),B(table ,38),I(24,00,00,00),B(string ,11),I(3c,02,41,00),B(table ,11),I(45,02,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 40 binding: (method-table-ref) */
  static const void *G008113[] = {I(ab,41,00,00),B(table ,12),I(22,01,41,00),B(table ,11),I(45,02,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 41 binding: (method-element) */
  static const void *G008115[] = {I(ab,24,00,00),B(table ,5),I(3d,02,00,00)};

  eul_allocate_static_cons(cons_8125, NULL, NULL);
  eul_allocate_static_cons(cons_8123, NULL, eul_as_static(cons_8125));
  eul_allocate_static_cons(cons_8137, NULL, NULL);
  eul_allocate_static_cons(cons_8135, NULL, eul_as_static(cons_8137));
  /* Byte-vector with size: 658 is_init: 0 index: 60 binding: top-level */
  static const void *G008117[] = {I(a9,84,24,00),B(mop_class ,20),I(24,00,00,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,38),I(3c,03,24,00),B(boot1 ,23),I(3c,00,24,00),B(boot1 ,23),I(3c,00,23,00),B(table ,42),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,54),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(table ,5),I(2a,24,00,00),B(collect ,23),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(table1 ,4),I(86,24,00,00),B(boot1 ,38),I(3c,03,24,00),B(boot1 ,23),I(3c,00,24,00),B(collect ,23),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table ,43),I(23,00,00,00),B(table ,41),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,23),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table ,5),I(2a,24,00,00),B(table ,5),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(table1 ,14),I(86,24,00,00),B(boot1 ,38),I(3c,03,24,00),B(boot1 ,23),I(3c,00,24,00),B(table ,5),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table ,44),I(23,00,00,00),B(table ,40),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table ,5),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table ,5),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(table1 ,14),I(24,00,00,00),B(string ,13),I(24,00,00,00),B(boot1 ,38),I(3c,03,24,00),B(boot1 ,23),I(3c,00,24,00),B(table ,5),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table ,44),I(23,00,00,00),B(table ,39),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table ,5),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table ,5),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(table1 ,14),I(24,00,00,00),B(mop_class ,33),I(24,00,00,00),B(boot1 ,38),I(3c,03,24,00),B(boot1 ,23),I(3c,00,24,00),B(table ,5),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table ,44),I(23,00,00,00),B(table ,37),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table ,5),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table ,5),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(table1 ,8),I(86,24,00,00),B(boot1 ,38),I(3c,03,24,00),B(boot1 ,23),I(3c,00,24,00),B(table ,5),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table ,44),I(23,00,00,00),B(table ,36),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table ,5),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,23),I(24,00,00,00),B(boot1 ,40),I(3c,01,24,00),B(collect ,23),I(24,00,00,00),B(boot1 ,40),I(3c,01,1b,26),I(00,00,00,03),I(02,26,00,00),I(00,00,00,03),I(24,00,00,00),B(table1 ,4),I(86,86,24,00),B(boot1 ,38),I(3c,04,24,00),B(boot1 ,23),I(3c,00,24,00),B(collect ,23),I(24,00,00,00),B(boot1 ,40),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(table ,45),I(23,00,00,00),B(table ,34),I(3b,03,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(boot1 ,40),I(24,00,00,00),B(boot1 ,40),I(3c,01,26,00),I(00,00,00,03),I(24,00,00,00),B(mop_class ,20),I(24,00,00,00),B(mop_class ,20),I(24,00,00,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,38),I(3c,04,24,00),B(boot1 ,23),I(3c,00,24,00),B(boot1 ,23),I(3c,00,23,00),B(table ,47),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,54),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,24,00),B(table ,5),I(1c,1f,06,3c),I(02,2a,24,00),B(table ,5),I(24,00,00,00),B(boot1 ,40),I(3c,01,2a,24),B(table ,5),I(24,00,00,00),B(boot1 ,40),I(3c,01,24,00),B(table ,5),I(24,00,00,00),B(boot1 ,40),I(3c,01,1b,26),I(00,00,00,03),I(02,26,00,00),I(00,00,00,03),I(24,00,00,00),B(table1 ,14),I(86,86,24,00),B(boot1 ,38),I(3c,04,24,00),B(boot1 ,23),I(3c,00,24,00),B(table ,5),I(24,00,00,00),B(boot1 ,40),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(table ,48),I(23,00,00,00),B(table ,33),I(3b,03,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table ,5),I(24,00,00,00),B(boot1 ,40),I(3c,01,24,00),B(table ,5),I(24,00,00,00),B(boot1 ,40),I(3c,01,1b,26),I(00,00,00,03),I(02,26,00,00),I(00,00,00,03),I(24,00,00,00),B(table1 ,14),I(24,00,00,00),B(string ,13),I(86,24,00,00),B(boot1 ,38),I(3c,04,24,00),B(boot1 ,23),I(3c,00,24,00),B(table ,5),I(24,00,00,00),B(boot1 ,40),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(table ,48),I(23,00,00,00),B(table ,32),I(3b,03,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table ,5),I(24,00,00,00),B(boot1 ,40),I(3c,01,24,00),B(table ,5),I(24,00,00,00),B(boot1 ,40),I(3c,01,1b,26),I(00,00,00,03),I(02,26,00,00),I(00,00,00,03),I(24,00,00,00),B(table1 ,14),I(24,00,00,00),B(mop_class ,33),I(86,24,00,00),B(boot1 ,38),I(3c,04,24,00),B(boot1 ,23),I(3c,00,24,00),B(table ,5),I(24,00,00,00),B(boot1 ,40),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(table ,48),I(23,00,00,00),B(table ,30),I(3b,03,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table ,5),I(24,00,00,00),B(boot1 ,40),I(3c,01,24,00),B(table ,5),I(24,00,00,00),B(boot1 ,40),I(3c,01,1b,26),I(00,00,00,03),I(02,26,00,00),I(00,00,00,03),I(24,00,00,00),B(table1 ,8),I(86,86,24,00),B(boot1 ,38),I(3c,04,24,00),B(boot1 ,23),I(3c,00,24,00),B(table ,5),I(24,00,00,00),B(boot1 ,40),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(table ,48),I(23,00,00,00),B(table ,29),I(3b,03,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,3),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(table1 ,4),I(24,00,00,00),B(boot1 ,38),I(3c,02,24,00),B(boot1 ,23),I(3c,00,24,00),B(collect ,3),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table ,49),I(23,00,00,00),B(table ,27),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,6),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,27),I(24,00,00,00),B(table1 ,8),I(24,00,00,00),B(boot1 ,38),I(3c,03,24,00),B(boot1 ,23),I(3c,00,24,00),B(collect ,6),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table ,50),I(23,00,00,00),B(table ,26),I(3b,fd,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,6),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,2),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,27),I(24,00,00,00),B(table1 ,8),I(24,00,00,00),B(boot1 ,38),I(3c,03,24,00),B(boot1 ,23),I(3c,00,24,00),B(collect ,2),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table ,51),I(23,00,00,00),B(table ,24),I(3b,fd,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,2),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,16),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,27),I(24,00,00,00),B(table1 ,8),I(24,00,00,00),B(boot1 ,38),I(3c,03,24,00),B(boot1 ,23),I(3c,00,24,00),B(collect ,16),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table ,52),I(23,00,00,00),B(table ,22),I(3b,fd,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,16),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,19),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,27),I(24,00,00,00),B(table1 ,8),I(24,00,00,00),B(boot1 ,38),I(3c,03,24,00),B(boot1 ,23),I(3c,00,24,00),B(collect ,19),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table ,53),I(23,00,00,00),B(table ,20),I(3b,fd,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,19),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,13),I(26,00,00,00),I(00,00,00,03),I(02,84,86,24),B(table1 ,4),I(24,00,00,00),B(boot1 ,38),I(3c,03,24,00),B(boot1 ,23),I(3c,00,24,00),B(collect ,13),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table ,54),I(23,00,00,00),B(table ,18),I(3b,fd,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,13),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,26),I(26,00,00,00),I(00,00,00,03),I(02,26,00,00),I(00,00,00,03),I(24,00,00,00),B(mop_class ,27),I(86,24,00,00),B(table1 ,4),I(24,00,00,00),B(boot1 ,38),I(3c,04,24,00),B(boot1 ,23),I(3c,00,24,00),B(collect ,26),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table ,55),I(23,00,00,00),B(table ,15),I(3b,03,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,26),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,14),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,27),I(24,00,00,00),B(table1 ,4),I(24,00,00,00),B(boot1 ,38),I(3c,03,24,00),B(boot1 ,23),I(3c,00,24,00),B(collect ,14),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table ,56),I(23,00,00,00),B(table ,14),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,14),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(mop_class ,13),I(24,00,00,00),B(boot1 ,40),I(3c,01,83,24),B(mop_class ,20),I(24,00,00,00),B(boot1 ,38),I(3c,02,24,00),B(boot1 ,23),I(3c,00,24,00),B(boot1 ,23),I(3c,00,23,00),B(table ,59),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,54),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,24,00),B(table1 ,4),I(1c,1f,06,3c),I(02,2a,24,00),B(table1 ,4),I(24,00,00,00),B(mop_class ,13),I(3d,01,89,45),I(89,00,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 61 binding: accumulate-table */
  static const void *G008139[] = {I(43,03,24,00),B(table1 ,16),I(3c,01,24,00),B(list ,17),I(3d,03,00,00)};

  /* Byte-vector with size: 8 is_init: 0 index: 62 binding: anonymous */
  static const void *G008141[] = {I(aa,1b,7a,1b),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,11),I(1c,10,1d,11),I(47,00,00,3d),I(02,02,45,02)};

  /* Byte-vector with size: 12 is_init: 0 index: 63 binding: all?1-table */
  static const void *G008143[] = {I(ab,46,01,1c),I(48,00,00,26),I(00,00,00,03),I(24,00,00,00),B(table1 ,4),I(08,23,00,00),B(table ,17),I(23,00,00,00),B(table ,62),I(3b,01,1c,24),B(vector ,12),I(3d,02,02,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 64 binding: table-empty? */
  static const void *G008145[] = {I(aa,83,24,00),B(table1 ,4),I(08,2d,45,00)};

  /* Byte-vector with size: 24 is_init: 0 index: 65 binding: anonymous */
  static const void *G008147[] = {I(ab,1c,47,00),I(03,1a,1b,34),I(00,00,00,49),I(47,00,02,1f),I(03,02,1f,03),I(2b,1c,7a,1b),I(34,00,00,00),I(00,00,00,28),I(1d,10,1f,03),I(11,1c,1c,47),I(00,00,3c,02),I(1b,1f,08,0f),I(1f,05,1c,47),I(00,01,3d,02),I(0a,22,04,32),I(00,00,00,0e),I(1c,1f,05,47),I(00,01,3d,02),I(06,22,03,32),I(00,00,00,10),I(1c,24,00,00),B(boot ,24),I(3d,01,03,45),I(03,00,00,00)};

  /* Byte-vector with size: 24 is_init: 0 index: 66 binding: map1-table */
  static const void *G008149[] = {I(ab,46,04,1c),I(48,00,00,26),I(00,00,00,03),I(24,00,00,00),B(table1 ,4),I(08,1b,48,00),I(02,47,00,02),I(06,1b,48,00),I(03,47,00,02),I(24,00,00,00),B(vector ,5),I(3c,01,1b,34),I(00,00,00,2d),I(86,1b,48,00),I(01,23,00,00),B(table ,17),I(23,00,00,00),B(table ,65),I(3b,02,48,00),I(01,82,86,47),I(00,01,3d,02),I(05,22,01,32),I(00,00,00,06),I(86,45,04,00)};

  /* Byte-vector with size: 21 is_init: 0 index: 67 binding: anonymous */
  static const void *G008151[] = {I(aa,1b,47,00),I(05,1a,1b,34),I(00,00,00,45),I(47,00,01,1d),I(02,1b,7a,1b),I(34,00,00,00),I(00,00,00,24),I(1c,10,47,00),I(02,3c,01,1b),I(47,00,06,18),I(1b,1f,04,47),I(00,03,3c,02),I(22,02,32,00),I(00,00,00,07),I(86,2a,1f,03),I(2b,47,00,04),I(3d,01,04,22),I(02,32,00,00),I(00,00,00,0a),I(47,00,00,45),I(02,00,00,00)};

  /* Byte-vector with size: 18 is_init: 0 index: 68 binding: anonymous */
  static const void *G008153[] = {I(ab,1c,47,00),I(06,1a,1b,34),I(00,00,00,35),I(47,00,07,1f),I(03,02,1b,34),I(00,00,00,19),I(1f,03,2b,1b),I(1f,04,47,00),I(03,3d,02,05),I(22,01,32,00),I(00,00,00,0e),I(47,00,07,1f),I(04,1f,04,03),I(22,01,32,00),I(00,00,00,0e),I(82,1d,47,00),I(03,3d,02,03),I(45,03,00,00)};

  /* Byte-vector with size: 56 is_init: 0 index: 69 binding: table-rehash */
  static const void *G008155[] = {I(aa,46,08,1b),I(48,00,00,47),I(00,00,26,00),I(00,00,00,03),I(24,00,00,00),B(table1 ,4),I(08,47,00,00),I(82,24,00,00),B(table1 ,8),I(08,1c,48,00),I(01,1b,48,00),I(02,47,00,01),I(24,00,00,00),B(vector ,5),I(3c,01,1b,34),I(00,00,00,9d),I(47,00,01,06),I(1b,48,00,05),I(47,00,05,84),I(16,1b,48,00),I(06,47,00,06),I(24,00,00,00),B(boot1 ,38),I(3c,01,1b,48),I(00,07,47,00),I(00,82,24,00),B(table1 ,4),I(08,84,16,47),I(00,00,47,00),I(07,1c,26,00),I(00,00,00,03),I(1d,24,00,00),B(table1 ,4),I(09,22,02,2a),I(47,00,00,1c),I(1c,82,1d,24),B(table1 ,4),I(09,22,02,2a),I(86,86,1c,48),I(00,03,1b,48),I(00,04,23,00),B(table ,17),I(23,00,00,00),B(table ,68),I(3b,02,48,00),I(03,23,00,00),B(table ,17),I(23,00,00,00),B(table ,67),I(3b,01,48,00),I(04,82,47,00),I(04,3d,01,0a),I(22,06,32,00),I(00,00,00,09),I(47,00,00,45),I(04,00,00,00)};

  /* Byte-vector with size: 8 is_init: 0 index: 70 binding: anonymous */
  static const void *G008157[] = {I(aa,1b,7a,1b),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,11),I(1c,10,1d,11),I(47,00,00,3d),I(02,02,45,02)};

  /* Byte-vector with size: 12 is_init: 0 index: 71 binding: anyp1-table */
  static const void *G008159[] = {I(ab,46,01,1c),I(48,00,00,26),I(00,00,00,03),I(24,00,00,00),B(table1 ,4),I(08,23,00,00),B(table ,17),I(23,00,00,00),B(table ,70),I(3b,01,1c,24),B(vector ,4),I(3d,02,02,00)};

  /* Byte-vector with size: 8 is_init: 0 index: 72 binding: anonymous */
  static const void *G008161[] = {I(aa,1b,7a,1b),I(34,00,00,00),I(00,00,00,18),I(1c,10,1d,11),I(47,00,00,3d),I(02,02,32,00),I(00,00,00,07),I(86,45,02,00)};

  /* Byte-vector with size: 12 is_init: 0 index: 73 binding: do1-table */
  static const void *G008163[] = {I(ab,46,01,1c),I(48,00,00,26),I(00,00,00,03),I(24,00,00,00),B(table1 ,4),I(08,23,00,00),B(table ,17),I(23,00,00,00),B(table ,72),I(3b,01,1c,24),B(vector ,13),I(3d,02,02,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 74 binding: accumulate1-table */
  static const void *G008165[] = {I(ab,24,00,00),B(table1 ,16),I(3c,01,24,00),B(list ,15),I(3d,02,00,00)};

  /* Byte-vector with size: 108 is_init: 1 index: 0 binding: initialize-table */
  static const void *G008167[] = {I(87,25,00,00),B(table ,1),I(24,00,00,00),B(table1 ,1),I(3e,0b,24,00),B(table1 ,0),I(3c,00,21,01),I(24,00,00,00),B(vector ,1),I(3e,0b,24,00),B(vector ,0),I(3c,00,21,01),I(24,00,00,00),B(string ,1),I(3e,0b,24,00),B(string ,0),I(3c,00,21,01),I(24,00,00,00),B(fpi ,1),I(3e,0b,24,00),B(fpi ,0),I(3c,00,21,01),I(24,00,00,00),B(list ,1),I(3e,0b,24,00),B(list ,0),I(3c,00,21,01),I(24,00,00,00),B(compare ,1),I(3e,0b,24,00),B(compare ,0),I(3c,00,21,01),I(24,00,00,00),B(collect ,1),I(3e,0b,24,00),B(collect ,0),I(3c,00,21,01),I(24,00,00,00),B(copy ,1),I(3e,0b,24,00),B(copy ,0),I(3c,00,21,01),I(24,00,00,00),B(convert ,1),I(3e,0b,24,00),B(convert ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(23,00,00,00),B(table ,75),I(23,00,00,00),B(table ,74),I(3b,02,25,00),B(table ,10),I(23,00,00,00),B(table ,76),I(23,00,00,00),B(table ,73),I(3b,02,25,00),B(table ,9),I(23,00,00,00),B(table ,77),I(23,00,00,00),B(table ,71),I(3b,02,25,00),B(table ,8),I(23,00,00,00),B(table ,78),I(23,00,00,00),B(table ,69),I(3b,01,25,00),B(table ,7),I(23,00,00,00),B(table ,79),I(23,00,00,00),B(table ,66),I(3b,02,25,00),B(table ,6),I(86,25,00,00),B(table ,5),I(23,00,00,00),B(table ,80),I(23,00,00,00),B(table ,64),I(3b,01,25,00),B(table ,4),I(23,00,00,00),B(table ,81),I(23,00,00,00),B(table ,63),I(3b,02,25,00),B(table ,3),I(23,00,00,00),B(table ,82),I(23,00,00,00),B(table ,61),I(3b,03,25,00),B(table ,2),I(23,00,00,00),B(table ,83),I(23,00,00,00),B(table ,60),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G008069,G008068);
  eul_allocate_bytevector( G008071,G008070);
  eul_allocate_bytevector( G008073,G008072);
  eul_intern_symbol(sym_8076,"anonymous");
  eul_allocate_bytevector( G008075,G008074);
  object_class(str_8079) = eul_static_string_class;
  eul_allocate_bytevector( G008078,G008077);
  object_class(str_8082) = eul_static_string_class;
  eul_allocate_bytevector( G008081,G008080);
  object_class(str_8085) = eul_static_string_class;
  eul_allocate_bytevector( G008084,G008083);
  object_class(str_8088) = eul_static_string_class;
  eul_allocate_bytevector( G008087,G008086);
  eul_allocate_bytevector( G008090,G008089);
  eul_allocate_bytevector( G008092,G008091);
  eul_allocate_bytevector( G008094,G008093);
  eul_allocate_bytevector( G008096,G008095);
  object_class(str_8099) = eul_static_string_class;
  eul_allocate_bytevector( G008098,G008097);
  eul_allocate_bytevector( G008101,G008100);
  eul_allocate_bytevector( G008103,G008102);
  eul_allocate_bytevector( G008105,G008104);
  eul_allocate_bytevector( G008107,G008106);
  eul_allocate_bytevector( G008109,G008108);
  object_class(str_8112) = eul_static_string_class;
  eul_allocate_bytevector( G008111,G008110);
  eul_allocate_bytevector( G008114,G008113);
  eul_allocate_bytevector( G008116,G008115);
  eul_intern_symbol(sym_8119,"table-ref");
  eul_intern_symbol(sym_8120,"(method element)");
  eul_intern_symbol(sym_8121,"(method table-ref)");
  eul_intern_symbol(sym_8122,"(method (setter element))");
  eul_intern_symbol(sym_8124,"setter");
  object_class(cons_8125) = eul_static_cons_class;
  eul_car(cons_8125) = sym_8119;
  eul_cdr(cons_8125) = eul_nil;
  object_class(cons_8123) = eul_static_cons_class;
  eul_car(cons_8123) = sym_8124;
  eul_intern_symbol(sym_8126,"(method (setter table-ref))");
  eul_intern_symbol(sym_8127,"(method emptyp)");
  eul_intern_symbol(sym_8128,"(method do)");
  eul_intern_symbol(sym_8129,"(method map)");
  eul_intern_symbol(sym_8130,"(method anyp)");
  eul_intern_symbol(sym_8131,"(method all?)");
  eul_intern_symbol(sym_8132,"(method member)");
  eul_intern_symbol(sym_8133,"(method accumulate)");
  eul_intern_symbol(sym_8134,"(method accumulate1)");
  eul_intern_symbol(sym_8136,"converter");
  eul_intern_symbol(sym_8138,"<table>");
  object_class(cons_8137) = eul_static_cons_class;
  eul_car(cons_8137) = sym_8138;
  eul_cdr(cons_8137) = eul_nil;
  object_class(cons_8135) = eul_static_cons_class;
  eul_car(cons_8135) = sym_8136;
  eul_allocate_bytevector( G008118,G008117);
  eul_allocate_bytevector( G008140,G008139);
  eul_allocate_bytevector( G008142,G008141);
  eul_allocate_bytevector( G008144,G008143);
  eul_allocate_bytevector( G008146,G008145);
  eul_allocate_bytevector( G008148,G008147);
  eul_allocate_bytevector( G008150,G008149);
  eul_allocate_bytevector( G008152,G008151);
  eul_allocate_bytevector( G008154,G008153);
  eul_allocate_bytevector( G008156,G008155);
  eul_allocate_bytevector( G008158,G008157);
  eul_allocate_bytevector( G008160,G008159);
  eul_allocate_bytevector( G008162,G008161);
  eul_allocate_bytevector( G008164,G008163);
  eul_allocate_bytevector( G008166,G008165);
  eul_intern_symbol(sym_8169,"accumulate1-table");
  eul_intern_symbol(sym_8170,"do1-table");
  eul_intern_symbol(sym_8171,"anyp1-table");
  eul_intern_symbol(sym_8172,"table-rehash");
  eul_intern_symbol(sym_8173,"map1-table");
  eul_intern_symbol(sym_8174,"table-empty?");
  eul_intern_symbol(sym_8175,"all?1-table");
  eul_intern_symbol(sym_8176,"accumulate-table");
  eul_intern_symbol(sym_8177,"top-level");
  eul_allocate_bytevector( G008168,G008167);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 11; i++)
      table_bindings[i] = eul_nil;
  }

  table_bindings[ 11] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_table_ref8065;
  table_bindings[ 12] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_addr_str8066;
  table_bindings[ 13] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_table_set8067;
  table_bindings[ 14] = G008069;
  table_bindings[ 15] = G008071;
  table_bindings[ 16] = G008073;
  table_bindings[ 17] = sym_8076;
  table_bindings[ 18] = G008075;
  table_bindings[ 19] = str_8079;
  table_bindings[ 20] = G008078;
  table_bindings[ 21] = str_8082;
  table_bindings[ 22] = G008081;
  table_bindings[ 23] = str_8085;
  table_bindings[ 24] = G008084;
  table_bindings[ 25] = str_8088;
  table_bindings[ 26] = G008087;
  table_bindings[ 27] = G008090;
  table_bindings[ 28] = G008092;
  table_bindings[ 29] = G008094;
  table_bindings[ 30] = G008096;
  table_bindings[ 31] = str_8099;
  table_bindings[ 32] = G008098;
  table_bindings[ 33] = G008101;
  table_bindings[ 34] = G008103;
  table_bindings[ 35] = G008105;
  table_bindings[ 36] = G008107;
  table_bindings[ 37] = G008109;
  table_bindings[ 38] = str_8112;
  table_bindings[ 39] = G008111;
  table_bindings[ 40] = G008114;
  table_bindings[ 41] = G008116;
  table_bindings[ 42] = sym_8119;
  table_bindings[ 43] = sym_8120;
  table_bindings[ 44] = sym_8121;
  table_bindings[ 45] = sym_8122;
  table_bindings[ 46] = sym_8124;
  table_bindings[ 47] = cons_8123;
  table_bindings[ 48] = sym_8126;
  table_bindings[ 49] = sym_8127;
  table_bindings[ 50] = sym_8128;
  table_bindings[ 51] = sym_8129;
  table_bindings[ 52] = sym_8130;
  table_bindings[ 53] = sym_8131;
  table_bindings[ 54] = sym_8132;
  table_bindings[ 55] = sym_8133;
  table_bindings[ 56] = sym_8134;
  table_bindings[ 57] = sym_8136;
  table_bindings[ 58] = sym_8138;
  table_bindings[ 59] = cons_8135;
  table_bindings[ 60] = G008118;
  table_bindings[ 61] = G008140;
  table_bindings[ 62] = G008142;
  table_bindings[ 63] = G008144;
  table_bindings[ 64] = G008146;
  table_bindings[ 65] = G008148;
  table_bindings[ 66] = G008150;
  table_bindings[ 67] = G008152;
  table_bindings[ 68] = G008154;
  table_bindings[ 69] = G008156;
  table_bindings[ 70] = G008158;
  table_bindings[ 71] = G008160;
  table_bindings[ 72] = G008162;
  table_bindings[ 73] = G008164;
  table_bindings[ 74] = G008166;
  table_bindings[ 1] = eul_nil;
  table_bindings[ 75] = sym_8169;
  table_bindings[ 76] = sym_8170;
  table_bindings[ 77] = sym_8171;
  table_bindings[ 78] = sym_8172;
  table_bindings[ 79] = sym_8173;
  table_bindings[ 80] = sym_8174;
  table_bindings[ 81] = sym_8175;
  table_bindings[ 82] = sym_8176;
  table_bindings[ 83] = sym_8177;
  eul_allocate_lambda( table_bindings[0], "initialize-table", 0, G008168);

  }
}


/* eof */
