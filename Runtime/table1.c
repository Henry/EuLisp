/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module table1
 **  Copyright: See file table1.em
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

/* Module bindings with size 102 */
LispRef table1_bindings[102];

/* Foreign functions */
static LispRef ff_stub_eul_hash_object481 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G00601, res;

  POPVAL1(G00601);
  FF_RES_CONVERT0(res,eul_hash_object(FF_ARG_CONVERT8(G00601)));
  return res;
}


/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module table1 */
void initialize_module_table1()
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
  eul_fast_table_set(eul_modules,"table1",(LispRef) table1_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_600, sym_599, sym_598, sym_597, sym_596, sym_595, sym_594, G00593, G00591, G00589, G00587, G00585, G00583, G00581, sym_579, sym_578, sym_577, sym_576, sym_575, sym_574, sym_573, sym_572, sym_571, sym_570, sym_569, sym_568, sym_567, sym_566, sym_565, sym_564, sym_563, sym_562, sym_561, sym_560, sym_559, sym_556, sym_555, sym_554, key_552, key_551, key_550, sym_549, sym_548, key_547, sym_546, key_545, key_544, sym_543, sym_542, key_541, G00540, G00538, G00536, G00534, G00532, G00530, G00528, G00526, G00524, G00522, G00520, G00518, G00516, G00514, G00512, G00510, G00508, key_503, key_502, G00501, G00499, G00497, G00495, G00493, G00491, sym_489, G00488, G00486, G00483;

  /* Code vector and literal definitions */
  eul_allocate_static_string(str_484, "keys of table ~a not accessable", 31);
  /* Byte-vector with size: 5 is_init: 0 index: 20 binding: (method-table-keys) */
  static const void *G00482[] = {I(aa,23,00,00),B(table1 ,19),I(1c,24,00,00),B(boot ,21),I(3d,02,01,00)};

  /* Byte-vector with size: 10 is_init: 0 index: 21 binding: anonymous */
  static const void *G00485[] = {I(aa,1b,12,1b),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,17),I(1c,10,1b,47),I(00,00,0f,1b),I(48,00,00,47),I(00,00,22,02),I(45,02,00,00)};

  /* Byte-vector with size: 20 is_init: 0 index: 23 binding: (method-table-keys) */
  static const void *G00487[] = {I(aa,46,01,26),I(00,00,00,03),I(24,00,00,00),B(table1 ,4),I(08,86,1b,48),I(00,00,1c,24),B(vector ,5),I(3c,01,1b,34),I(00,00,00,2d),I(23,00,00,00),B(table1 ,22),I(23,00,00,00),B(table1 ,21),I(3b,01,1f,03),I(24,00,00,00),B(vector ,13),I(3c,02,2a,47),I(00,00,32,00),I(00,00,00,07),I(86,45,03,00)};

  /* Byte-vector with size: 10 is_init: 0 index: 24 binding: anonymous */
  static const void *G00490[] = {I(aa,1b,12,1b),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,17),I(1c,11,1b,47),I(00,00,0f,1b),I(48,00,00,47),I(00,00,22,02),I(45,02,00,00)};

  /* Byte-vector with size: 20 is_init: 0 index: 25 binding: (method-table-values) */
  static const void *G00492[] = {I(aa,46,01,26),I(00,00,00,03),I(24,00,00,00),B(table1 ,4),I(08,86,1b,48),I(00,00,1c,24),B(vector ,5),I(3c,01,1b,34),I(00,00,00,2d),I(23,00,00,00),B(table1 ,22),I(23,00,00,00),B(table1 ,24),I(3b,01,1f,03),I(24,00,00,00),B(vector ,13),I(3c,02,2a,47),I(00,00,32,00),I(00,00,00,07),I(86,45,03,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 26 binding: (method-size) */
  static const void *G00494[] = {I(aa,83,24,00),B(table1 ,4),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 27 binding: (method-reset) */
  static const void *G00496[] = {I(aa,24,00,00),B(table1 ,9),I(3d,01,00,00)};

  /* Byte-vector with size: 19 is_init: 0 index: 28 binding: (method-clear-table) */
  static const void *G00498[] = {I(aa,1b,86,1c),I(26,00,00,00),I(00,00,00,03),I(1d,24,00,00),B(table1 ,4),I(09,22,02,2a),I(1b,82,1c,83),I(1d,24,00,00),B(table1 ,4),I(09,22,02,2a),I(26,00,00,00),I(00,00,00,10),I(26,00,00,00),I(00,00,00,04),I(15,1c,1c,1c),I(82,1d,24,00),B(table1 ,4),I(09,22,02,2a),I(1c,45,02,00)};

  eul_allocate_static_cons(cons_505, NULL, NULL);
  eul_allocate_static_cons(cons_504, NULL, eul_as_static(cons_505));
  eul_allocate_static_string(str_506, "table initialization of ~a without hash function", 48);
  /* Byte-vector with size: 69 is_init: 0 index: 33 binding: (method-initialize) */
  static const void *G00500[] = {I(ab,1c,04,24),B(table1 ,4),I(50,1b,34,00),I(00,00,01,02),I(1c,23,00,00),B(table1 ,29),I(24,00,00,00),B(boot ,27),I(3c,02,1d,23),B(table1 ,30),I(24,00,00,00),B(boot ,27),I(3c,02,24,00),B(mop_inspect ,6),I(3c,01,1b,34),I(00,00,00,25),I(24,00,00,00),B(mop_gf ,2),I(24,00,00,00),B(table1 ,8),I(1f,05,24,00),B(boot ,9),I(3d,03,05,32),I(00,00,00,a8),I(1c,24,00,00),B(compare ,7),I(50,1b,34,00),I(00,00,00,26),I(24,00,00,00),B(mop_gf ,2),I(24,00,00,00),B(table1 ,8),I(1f,06,24,00),B(boot ,9),I(3d,03,06,32),I(00,00,00,76),I(86,24,00,00),B(boot1 ,24),I(24,00,00,00),B(boot1 ,44),I(24,00,00,00),B(boot1 ,26),I(3c,03,1f,03),I(1c,86,6c,1b),I(34,00,00,00),I(00,00,00,3c),I(1f,06,23,00),B(table1 ,31),I(24,00,00,00),B(mop_key ,3),I(3c,02,24,00),B(mop_gf ,2),I(24,00,00,00),B(table1 ,14),I(1d,24,00,00),B(boot ,9),I(3d,03,09,22),I(01,32,00,00),I(00,00,00,1a),I(23,00,00,00),B(table1 ,32),I(1f,08,24,00),B(boot ,21),I(3d,02,08,22),I(02,22,01,22),I(02,32,00,00),I(00,00,00,0a),I(38,02,01,45),I(03,00,00,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 34 binding: (method-hash-table-p) */
  static const void *G00507[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 35 binding: (method-hash-table-p) */
  static const void *G00509[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 4 is_init: 0 index: 36 binding: (setter-table-comparator) */
  static const void *G00511[] = {I(ab,1c,83,1d),I(24,00,00,00),B(table1 ,8),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 37 binding: (setter-table-hash-function) */
  static const void *G00513[] = {I(ab,1c,82,1d),I(24,00,00,00),B(table1 ,8),I(09,45,02,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 38 binding: (method-simple-hash-table-p) */
  static const void *G00515[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 39 binding: (method-simple-hash-table-p) */
  static const void *G00517[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 40 binding: (method-tablep) */
  static const void *G00519[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 41 binding: (method-tablep) */
  static const void *G00521[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 5 is_init: 0 index: 42 binding: (setter-table-entries) */
  static const void *G00523[] = {I(ab,1c,26,00),I(00,00,00,03),I(1d,24,00,00),B(table1 ,4),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 43 binding: (setter-table-size) */
  static const void *G00525[] = {I(ab,1c,83,1d),I(24,00,00,00),B(table1 ,4),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 44 binding: (setter-table-threshold) */
  static const void *G00527[] = {I(ab,1c,82,1d),I(24,00,00,00),B(table1 ,4),I(09,45,02,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 45 binding: anonymous */
  static const void *G00529[] = {I(a9,24,00,00),B(table1 ,3),I(45,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 46 binding: anonymous */
  static const void *G00531[] = {I(a9,24,00,00),B(boot1 ,44),I(45,00,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 47 binding: anonymous */
  static const void *G00533[] = {I(a9,26,00,00),I(00,00,00,10),I(26,00,00,00),I(00,00,00,04),I(15,45,00,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 48 binding: anonymous */
  static const void *G00535[] = {I(a9,82,45,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 49 binding: anonymous */
  static const void *G00537[] = {I(aa,41,00,00),B(table1 ,18),I(45,01,00,00)};

  eul_allocate_static_cons(cons_553, NULL, NULL);
  eul_allocate_static_cons(cons_558, NULL, NULL);
  eul_allocate_static_cons(cons_557, NULL, eul_as_static(cons_558));
  /* Byte-vector with size: 711 is_init: 0 index: 88 binding: top-level */
  static const void *G00539[] = {I(a9,23,00,00),B(table1 ,22),I(23,00,00,00),B(table1 ,49),I(3b,01,89,00),B(table1 ,3),I(2a,24,00,00),B(collect ,24),I(24,00,00,00),B(boot1 ,26),I(3c,01,23,00),B(table1 ,50),I(23,00,00,00),B(table1 ,51),I(24,00,00,00),B(boot1 ,26),I(3c,02,23,00),B(table1 ,50),I(23,00,00,00),B(table1 ,52),I(23,00,00,00),B(table1 ,53),I(23,00,00,00),B(table1 ,54),I(24,00,00,00),B(boot1 ,26),I(3c,04,23,00),B(table1 ,50),I(23,00,00,00),B(table1 ,55),I(23,00,00,00),B(table1 ,56),I(23,00,00,00),B(table1 ,22),I(23,00,00,00),B(table1 ,48),I(3b,00,24,00),B(boot1 ,26),I(3c,04,23,00),B(table1 ,50),I(23,00,00,00),B(table1 ,57),I(23,00,00,00),B(table1 ,56),I(23,00,00,00),B(table1 ,22),I(23,00,00,00),B(table1 ,47),I(3b,00,24,00),B(boot1 ,26),I(3c,04,1f,03),I(1f,03,1f,03),I(1f,03,24,00),B(boot1 ,26),I(3c,04,24,00),B(mop_class ,71),I(23,00,00,00),B(table1 ,50),I(23,00,00,00),B(table1 ,58),I(23,00,00,00),B(table1 ,59),I(1f,09,23,00),B(table1 ,60),I(1f,06,23,00),B(table1 ,61),I(23,00,00,00),B(table1 ,62),I(24,00,00,00),B(mop_gf ,2),I(3c,09,1b,89),B(table1 ,4),I(2a,28,1c,1b),I(89,00,00,00),B(table1 ,14),I(2a,24,00,00),B(table1 ,4),I(24,00,00,00),B(boot1 ,26),I(3c,01,23,00),B(table1 ,50),I(23,00,00,00),B(table1 ,63),I(23,00,00,00),B(table1 ,56),I(23,00,00,00),B(table1 ,22),I(23,00,00,00),B(table1 ,46),I(3b,00,23,00),B(table1 ,53),I(23,00,00,00),B(table1 ,29),I(24,00,00,00),B(boot1 ,26),I(3c,06,23,00),B(table1 ,50),I(23,00,00,00),B(table1 ,64),I(23,00,00,00),B(table1 ,56),I(23,00,00,00),B(table1 ,22),I(23,00,00,00),B(table1 ,45),I(3b,00,23,00),B(table1 ,53),I(23,00,00,00),B(table1 ,30),I(24,00,00,00),B(boot1 ,26),I(3c,06,1c,1c),I(24,00,00,00),B(boot1 ,26),I(3c,02,24,00),B(mop_class ,71),I(23,00,00,00),B(table1 ,50),I(23,00,00,00),B(table1 ,65),I(23,00,00,00),B(table1 ,59),I(1f,07,23,00),B(table1 ,60),I(1f,06,23,00),B(table1 ,61),I(23,00,00,00),B(table1 ,66),I(24,00,00,00),B(mop_gf ,2),I(3c,09,1b,89),B(table1 ,8),I(2a,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(table1 ,67),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(table1 ,12),I(2a,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(table1 ,68),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(table1 ,6),I(2a,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(table1 ,69),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(table1 ,13),I(2a,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(table1 ,70),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(table1 ,9),I(2a,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(table1 ,71),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(table1 ,16),I(2a,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(table1 ,72),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(table1 ,2),I(2a,24,00,00),B(boot1 ,41),I(24,00,00,00),B(boot1 ,41),I(3c,01,24,00),B(table1 ,11),I(23,00,00,00),B(table1 ,73),I(23,00,00,00),B(table1 ,44),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,41),I(24,00,00,00),B(boot1 ,41),I(3c,01,24,00),B(table1 ,5),I(23,00,00,00),B(table1 ,74),I(23,00,00,00),B(table1 ,43),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,41),I(24,00,00,00),B(boot1 ,41),I(3c,01,24,00),B(table1 ,15),I(23,00,00,00),B(table1 ,75),I(23,00,00,00),B(table1 ,42),I(3b,02,1d,3c),I(02,2a,24,00),B(table1 ,12),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,12),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,76),I(23,00,00,00),B(table1 ,41),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,12),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,12),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(table1 ,4),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,12),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,76),I(23,00,00,00),B(table1 ,40),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,12),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,12),I(2a,24,00,00),B(table1 ,4),I(2a,24,00,00),B(table1 ,14),I(24,00,00,00),B(mop_class ,71),I(05,2a,24,00),B(table1 ,4),I(24,00,00,00),B(boot1 ,26),I(3c,01,24,00),B(boot1 ,26),I(3c,00,23,00),B(table1 ,50),I(23,00,00,00),B(table1 ,77),I(23,00,00,00),B(table1 ,59),I(1f,04,23,00),B(table1 ,60),I(1f,05,23,00),B(table1 ,61),I(86,24,00,00),B(boot1 ,26),I(3c,08,24,00),B(table1 ,14),I(1c,24,00,00),B(mop_gf ,12),I(3c,02,2a,24),B(table1 ,6),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,6),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,78),I(23,00,00,00),B(table1 ,39),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,6),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,6),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(table1 ,14),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,6),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,78),I(23,00,00,00),B(table1 ,38),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,6),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,6),I(2a,24,00,00),B(table1 ,14),I(2a,24,00,00),B(boot1 ,41),I(24,00,00,00),B(boot1 ,41),I(3c,01,24,00),B(table1 ,10),I(23,00,00,00),B(table1 ,79),I(23,00,00,00),B(table1 ,37),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,41),I(24,00,00,00),B(boot1 ,41),I(3c,01,24,00),B(table1 ,7),I(23,00,00,00),B(table1 ,80),I(23,00,00,00),B(table1 ,36),I(3b,02,1d,3c),I(02,2a,24,00),B(table1 ,13),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,13),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,81),I(23,00,00,00),B(table1 ,35),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,13),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,13),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(table1 ,8),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,13),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,81),I(23,00,00,00),B(table1 ,34),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,13),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,13),I(2a,24,00,00),B(table1 ,8),I(2a,24,00,00),B(mop_gf ,12),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(table1 ,4),I(86,24,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(mop_gf ,12),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,82),I(23,00,00,00),B(table1 ,33),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(mop_gf ,12),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,9),I(2a,24,00,00),B(table1 ,9),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(table1 ,4),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,9),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,83),I(23,00,00,00),B(table1 ,28),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,9),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,19),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(table1 ,4),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(collect ,19),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,84),I(23,00,00,00),B(table1 ,27),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,19),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,8),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(table1 ,4),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(collect ,8),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,85),I(23,00,00,00),B(table1 ,26),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,8),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,16),I(2a,24,00,00),B(table1 ,16),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(table1 ,4),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,16),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,86),I(23,00,00,00),B(table1 ,25),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,16),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,2),I(2a,24,00,00),B(table1 ,2),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(table1 ,4),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,2),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,87),I(23,00,00,00),B(table1 ,23),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,2),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,2),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(table1 ,14),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,2),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(table1 ,87),I(23,00,00,00),B(table1 ,20),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(table1 ,2),I(1c,24,00,00),B(mop_meth ,5),I(3d,02,7b,45),I(7b,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 89 binding: table-size */
  static const void *G00580[] = {I(aa,83,24,00),B(table1 ,4),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 90 binding: table-comparator */
  static const void *G00582[] = {I(aa,83,24,00),B(table1 ,8),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 91 binding: table-hash-function */
  static const void *G00584[] = {I(aa,82,24,00),B(table1 ,8),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 92 binding: table-threshold */
  static const void *G00586[] = {I(aa,82,24,00),B(table1 ,4),I(08,45,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 93 binding: table-entries */
  static const void *G00588[] = {I(aa,26,00,00),I(00,00,00,03),I(24,00,00,00),B(table1 ,4),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 94 binding: table-fill-value */
  static const void *G00590[] = {I(aa,84,24,00),B(table1 ,4),I(08,45,00,00)};

  /* Byte-vector with size: 109 is_init: 1 index: 0 binding: initialize-table1 */
  static const void *G00592[] = {I(87,25,00,00),B(table1 ,1),I(24,00,00,00),B(vector ,1),I(3e,0b,24,00),B(vector ,0),I(3c,00,21,01),I(24,00,00,00),B(string ,1),I(3e,0b,24,00),B(string ,0),I(3c,00,21,01),I(24,00,00,00),B(fpi ,1),I(3e,0b,24,00),B(fpi ,0),I(3c,00,21,01),I(24,00,00,00),B(list ,1),I(3e,0b,24,00),B(list ,0),I(3c,00,21,01),I(24,00,00,00),B(compare ,1),I(3e,0b,24,00),B(compare ,0),I(3c,00,21,01),I(24,00,00,00),B(collect ,1),I(3e,0b,24,00),B(collect ,0),I(3c,00,21,01),I(24,00,00,00),B(copy ,1),I(3e,0b,24,00),B(copy ,0),I(3c,00,21,01),I(24,00,00,00),B(convert ,1),I(3e,0b,24,00),B(convert ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(23,00,00,00),B(table1 ,95),I(23,00,00,00),B(table1 ,94),I(3b,01,25,00),B(table1 ,17),I(86,25,00,00),B(table1 ,16),I(23,00,00,00),B(table1 ,96),I(23,00,00,00),B(table1 ,93),I(3b,01,25,00),B(table1 ,15),I(86,25,00,00),B(table1 ,14),I(86,25,00,00),B(table1 ,13),I(86,25,00,00),B(table1 ,12),I(23,00,00,00),B(table1 ,97),I(23,00,00,00),B(table1 ,92),I(3b,01,25,00),B(table1 ,11),I(23,00,00,00),B(table1 ,98),I(23,00,00,00),B(table1 ,91),I(3b,01,25,00),B(table1 ,10),I(86,25,00,00),B(table1 ,9),I(86,25,00,00),B(table1 ,8),I(23,00,00,00),B(table1 ,99),I(23,00,00,00),B(table1 ,90),I(3b,01,25,00),B(table1 ,7),I(86,25,00,00),B(table1 ,6),I(23,00,00,00),B(table1 ,100),I(23,00,00,00),B(table1 ,89),I(3b,01,25,00),B(table1 ,5),I(86,25,00,00),B(table1 ,4),I(86,25,00,00),B(table1 ,3),I(86,25,00,00),B(table1 ,2),I(23,00,00,00),B(table1 ,101),I(23,00,00,00),B(table1 ,88),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  object_class(str_484) = eul_static_string_class;
  eul_allocate_bytevector( G00483,G00482);
  eul_allocate_bytevector( G00486,G00485);
  eul_intern_symbol(sym_489,"anonymous");
  eul_allocate_bytevector( G00488,G00487);
  eul_allocate_bytevector( G00491,G00490);
  eul_allocate_bytevector( G00493,G00492);
  eul_allocate_bytevector( G00495,G00494);
  eul_allocate_bytevector( G00497,G00496);
  eul_allocate_bytevector( G00499,G00498);
  eul_intern_keyword(key_502,"comparator");
  eul_intern_keyword(key_503,"hash-function");
  object_class(cons_505) = eul_static_cons_class;
  eul_car(cons_505) = key_503;
  eul_cdr(cons_505) = eul_nil;
  object_class(cons_504) = eul_static_cons_class;
  eul_car(cons_504) = key_502;
  object_class(str_506) = eul_static_string_class;
  eul_allocate_bytevector( G00501,G00500);
  eul_allocate_bytevector( G00508,G00507);
  eul_allocate_bytevector( G00510,G00509);
  eul_allocate_bytevector( G00512,G00511);
  eul_allocate_bytevector( G00514,G00513);
  eul_allocate_bytevector( G00516,G00515);
  eul_allocate_bytevector( G00518,G00517);
  eul_allocate_bytevector( G00520,G00519);
  eul_allocate_bytevector( G00522,G00521);
  eul_allocate_bytevector( G00524,G00523);
  eul_allocate_bytevector( G00526,G00525);
  eul_allocate_bytevector( G00528,G00527);
  eul_allocate_bytevector( G00530,G00529);
  eul_allocate_bytevector( G00532,G00531);
  eul_allocate_bytevector( G00534,G00533);
  eul_allocate_bytevector( G00536,G00535);
  eul_allocate_bytevector( G00538,G00537);
  eul_intern_keyword(key_541,"name");
  eul_intern_symbol(sym_542,"entries");
  eul_intern_symbol(sym_543,"fill-value");
  eul_intern_keyword(key_544,"keyword");
  eul_intern_keyword(key_545,"fill-value");
  eul_intern_symbol(sym_546,"size");
  eul_intern_keyword(key_547,"default");
  eul_intern_symbol(sym_548,"threshold");
  eul_intern_symbol(sym_549,"table");
  eul_intern_keyword(key_550,"direct-superclasses");
  eul_intern_keyword(key_551,"direct-slots");
  eul_intern_keyword(key_552,"direct-keywords");
  object_class(cons_553) = eul_static_cons_class;
  eul_car(cons_553) = key_545;
  eul_cdr(cons_553) = eul_nil;
  eul_intern_symbol(sym_554,"comparator");
  eul_intern_symbol(sym_555,"hash-function");
  eul_intern_symbol(sym_556,"hash-table");
  object_class(cons_558) = eul_static_cons_class;
  eul_car(cons_558) = key_502;
  eul_cdr(cons_558) = eul_nil;
  object_class(cons_557) = eul_static_cons_class;
  eul_car(cons_557) = key_503;
  eul_intern_symbol(sym_559,"tablep");
  eul_intern_symbol(sym_560,"simple-hash-table-p");
  eul_intern_symbol(sym_561,"hash-table-p");
  eul_intern_symbol(sym_562,"clear-table");
  eul_intern_symbol(sym_563,"table-values");
  eul_intern_symbol(sym_564,"table-keys");
  eul_intern_symbol(sym_565,"(setter table-threshold)");
  eul_intern_symbol(sym_566,"(setter table-size)");
  eul_intern_symbol(sym_567,"(setter table-entries)");
  eul_intern_symbol(sym_568,"(method tablep)");
  eul_intern_symbol(sym_569,"simple-hash-table");
  eul_intern_symbol(sym_570,"(method simple-hash-table-p)");
  eul_intern_symbol(sym_571,"(setter table-hash-function)");
  eul_intern_symbol(sym_572,"(setter table-comparator)");
  eul_intern_symbol(sym_573,"(method hash-table-p)");
  eul_intern_symbol(sym_574,"(method initialize)");
  eul_intern_symbol(sym_575,"(method clear-table)");
  eul_intern_symbol(sym_576,"(method reset)");
  eul_intern_symbol(sym_577,"(method size)");
  eul_intern_symbol(sym_578,"(method table-values)");
  eul_intern_symbol(sym_579,"(method table-keys)");
  eul_allocate_bytevector( G00540,G00539);
  eul_allocate_bytevector( G00581,G00580);
  eul_allocate_bytevector( G00583,G00582);
  eul_allocate_bytevector( G00585,G00584);
  eul_allocate_bytevector( G00587,G00586);
  eul_allocate_bytevector( G00589,G00588);
  eul_allocate_bytevector( G00591,G00590);
  eul_intern_symbol(sym_594,"table-fill-value");
  eul_intern_symbol(sym_595,"table-entries");
  eul_intern_symbol(sym_596,"table-threshold");
  eul_intern_symbol(sym_597,"table-hash-function");
  eul_intern_symbol(sym_598,"table-comparator");
  eul_intern_symbol(sym_599,"table-size");
  eul_intern_symbol(sym_600,"top-level");
  eul_allocate_bytevector( G00593,G00592);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 18; i++)
      table1_bindings[i] = eul_nil;
  }

  table1_bindings[ 18] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_hash_object481;
  table1_bindings[ 19] = str_484;
  table1_bindings[ 20] = G00483;
  table1_bindings[ 21] = G00486;
  table1_bindings[ 22] = sym_489;
  table1_bindings[ 23] = G00488;
  table1_bindings[ 24] = G00491;
  table1_bindings[ 25] = G00493;
  table1_bindings[ 26] = G00495;
  table1_bindings[ 27] = G00497;
  table1_bindings[ 28] = G00499;
  table1_bindings[ 29] = key_502;
  table1_bindings[ 30] = key_503;
  table1_bindings[ 31] = cons_504;
  table1_bindings[ 32] = str_506;
  table1_bindings[ 33] = G00501;
  table1_bindings[ 34] = G00508;
  table1_bindings[ 35] = G00510;
  table1_bindings[ 36] = G00512;
  table1_bindings[ 37] = G00514;
  table1_bindings[ 38] = G00516;
  table1_bindings[ 39] = G00518;
  table1_bindings[ 40] = G00520;
  table1_bindings[ 41] = G00522;
  table1_bindings[ 42] = G00524;
  table1_bindings[ 43] = G00526;
  table1_bindings[ 44] = G00528;
  table1_bindings[ 45] = G00530;
  table1_bindings[ 46] = G00532;
  table1_bindings[ 47] = G00534;
  table1_bindings[ 48] = G00536;
  table1_bindings[ 49] = G00538;
  table1_bindings[ 50] = key_541;
  table1_bindings[ 51] = sym_542;
  table1_bindings[ 52] = sym_543;
  table1_bindings[ 53] = key_544;
  table1_bindings[ 54] = key_545;
  table1_bindings[ 55] = sym_546;
  table1_bindings[ 56] = key_547;
  table1_bindings[ 57] = sym_548;
  table1_bindings[ 58] = sym_549;
  table1_bindings[ 59] = key_550;
  table1_bindings[ 60] = key_551;
  table1_bindings[ 61] = key_552;
  table1_bindings[ 62] = cons_553;
  table1_bindings[ 63] = sym_554;
  table1_bindings[ 64] = sym_555;
  table1_bindings[ 65] = sym_556;
  table1_bindings[ 66] = cons_557;
  table1_bindings[ 67] = sym_559;
  table1_bindings[ 68] = sym_560;
  table1_bindings[ 69] = sym_561;
  table1_bindings[ 70] = sym_562;
  table1_bindings[ 71] = sym_563;
  table1_bindings[ 72] = sym_564;
  table1_bindings[ 73] = sym_565;
  table1_bindings[ 74] = sym_566;
  table1_bindings[ 75] = sym_567;
  table1_bindings[ 76] = sym_568;
  table1_bindings[ 77] = sym_569;
  table1_bindings[ 78] = sym_570;
  table1_bindings[ 79] = sym_571;
  table1_bindings[ 80] = sym_572;
  table1_bindings[ 81] = sym_573;
  table1_bindings[ 82] = sym_574;
  table1_bindings[ 83] = sym_575;
  table1_bindings[ 84] = sym_576;
  table1_bindings[ 85] = sym_577;
  table1_bindings[ 86] = sym_578;
  table1_bindings[ 87] = sym_579;
  table1_bindings[ 88] = G00540;
  table1_bindings[ 89] = G00581;
  table1_bindings[ 90] = G00583;
  table1_bindings[ 91] = G00585;
  table1_bindings[ 92] = G00587;
  table1_bindings[ 93] = G00589;
  table1_bindings[ 94] = G00591;
  table1_bindings[ 1] = eul_nil;
  table1_bindings[ 95] = sym_594;
  table1_bindings[ 96] = sym_595;
  table1_bindings[ 97] = sym_596;
  table1_bindings[ 98] = sym_597;
  table1_bindings[ 99] = sym_598;
  table1_bindings[ 100] = sym_599;
  table1_bindings[ 101] = sym_600;
  eul_allocate_lambda( table1_bindings[0], "initialize-table1", 0, G00593);

  }
}


/* eof */
