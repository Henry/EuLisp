/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module ex-import
 **  Copyright: See file ex-import.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_i_all();
extern void initialize_module_sx_obj();
extern void initialize_module_sx_node();
extern void initialize_module_p_env();
extern void initialize_module_cg_interf();
extern LispRef i_all_bindings[];
extern LispRef convert_bindings[];
extern LispRef collect_bindings[];
extern LispRef i_notify_bindings[];
extern LispRef thread_bindings[];
extern LispRef number_bindings[];
extern LispRef sx_obj_bindings[];
extern LispRef aux_table_bindings[];
extern LispRef cg_interf_bindings[];
extern LispRef dynamic_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef boot1_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef boot_bindings[];
extern LispRef i_error_bindings[];
extern LispRef i_param_bindings[];
extern LispRef mop_access_bindings[];
extern LispRef format_bindings[];
extern LispRef stream2_bindings[];
extern LispRef p_env_bindings[];
extern LispRef sx_obj1_bindings[];
extern LispRef sx_node_bindings[];

/* Module bindings with size 79 */
LispRef ex_import_bindings[79];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module ex-import */
void initialize_module_ex_import()
{
  if (is_initialized) return;
  initialize_module_i_all();
  initialize_module_sx_obj();
  initialize_module_sx_node();
  initialize_module_p_env();
  initialize_module_cg_interf();
  eul_fast_table_set(eul_modules,"ex_import",(LispRef) ex_import_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_4619, sym_4618, sym_4617, sym_4616, sym_4615, sym_4614, sym_4613, sym_4612, sym_4611, G004610, G004608, G004604, G004602, G004600, G004597, sym_4595, G004593, G004589, G004587, G004585, G004583, G004581, G004578, G004576, sym_4574, sym_4573, sym_4572, sym_4571, G004570, sym_4568, G004567, G004563, G004561, G004559, sym_4557, G004556, G004552, G004550, G004548, sym_4546, G004545, G004541, G004539, G004537, G004535, sym_4533, sym_4532, sym_4531, G004530, key_4528, G004525, G004523, G004521, G004519;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 18 is_init: 0 index: 11 binding: anonymous */
  static const void *G004518[] = {I(aa,1b,47,00),I(01,24,00,00),B(ex_import ,4),I(3c,02,24,00),B(sx_node ,21),I(3c,01,47,00),I(02,1d,24,00),B(ex_import ,10),I(3c,02,1c,1c),I(1c,26,00,00),I(00,00,00,06),I(1d,24,00,00),B(sx_obj1 ,42),I(09,22,02,2a),I(1c,24,00,00),B(p_env ,5),I(3d,01,03,45),I(03,00,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 12 binding: anonymous */
  static const void *G004520[] = {I(aa,1b,47,00),I(01,24,00,00),B(ex_import ,4),I(3c,02,24,00),B(p_env ,5),I(3d,01,01,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 13 binding: (method-G004357) */
  static const void *G004522[] = {I(ab,86,45,02)};

  eul_allocate_static_string(str_4526, "compile time error condition: ", 30);
  eul_allocate_static_string(str_4527, "bad prefix import syntax", 24);
  /* Byte-vector with size: 26 is_init: 0 index: 17 binding: (method-G004357) */
  static const void *G004524[] = {I(ab,24,00,00),B(stream2 ,10),I(23,00,00,00),B(ex_import ,14),I(24,00,00,00),B(format ,4),I(3c,02,2a,24),B(stream2 ,10),I(1d,24,00,00),B(mop_access ,8),I(3c,02,2a,24),B(i_param ,55),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,2a),I(23,00,00,00),B(ex_import ,15),I(24,00,00,00),B(i_error ,5),I(23,00,00,00),B(ex_import ,16),I(47,00,00,24),B(boot ,12),I(3d,04,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 127 is_init: 0 index: 21 binding: anonymous */
  static const void *G004529[] = {I(ab,46,07,1c),I(48,00,00,84),I(24,00,00,00),B(mop_class ,20),I(24,00,00,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,24,00),B(boot1 ,25),I(3c,00,23,00),B(ex_import ,18),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,26),I(00,00,00,03),I(02,84,86,86),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,1f,03),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,7),I(3c,02,1f,04),I(1f,04,23,00),B(ex_import ,19),I(23,00,00,00),B(ex_import ,17),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,06),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,07),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(i_error ,5),I(86,24,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,1f,0a),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,7),I(3c,02,1f,04),I(1f,04,23,00),B(ex_import ,19),I(23,00,00,00),B(ex_import ,13),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,0d),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,0e),I(24,00,00,00),B(dynamic ,5),I(3c,01,2a,47),I(00,00,77,1b),I(24,00,00,00),B(cg_interf ,24),I(3c,01,1b,48),I(00,01,47,00),I(01,26,00,00),I(00,00,00,0f),I(24,00,00,00),B(sx_obj1 ,59),I(08,47,00,00),I(73,1b,48,00),I(02,1c,24,00),B(aux_table ,8),I(3c,01,24,00),B(sx_obj ,25),I(1c,24,00,00),B(boot ,10),I(3c,02,47,00),I(00,76,24,00),B(number ,9),I(3c,02,23,00),B(ex_import ,18),I(23,00,00,00),B(ex_import ,12),I(3b,01,1c,24),B(boot ,16),I(3c,02,2a,47),I(00,00,76,23),B(ex_import ,18),I(23,00,00,00),B(ex_import ,11),I(3b,01,1c,24),B(boot ,16),I(3c,02,2a,23),B(ex_import ,20),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,26),I(00,00,00,03),I(24,00,00,00),B(sx_obj1 ,59),I(08,1b,1f,09),I(24,00,00,00),B(thread ,28),I(3c,02,1d,1c),I(1c,26,00,00),I(00,00,00,03),I(1d,24,00,00),B(sx_obj1 ,59),I(09,22,02,83),I(24,00,00,00),B(dynamic ,6),I(3c,01,2a,1b),I(45,1f,00,00)};

  /* Byte-vector with size: 15 is_init: 0 index: 22 binding: anonymous */
  static const void *G004534[] = {I(aa,1b,10,1b),I(47,00,01,24),B(ex_import ,4),I(3c,02,24,00),B(sx_node ,21),I(3c,01,1d,73),I(1c,1c,1c,26),I(00,00,00,06),I(1d,24,00,00),B(sx_obj1 ,42),I(09,22,02,2a),I(1c,24,00,00),B(p_env ,5),I(3d,01,04,45),I(04,00,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 23 binding: anonymous */
  static const void *G004536[] = {I(aa,1b,47,00),I(01,24,00,00),B(ex_import ,4),I(3c,02,24,00),B(p_env ,5),I(3d,01,01,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 24 binding: (method-G004325) */
  static const void *G004538[] = {I(ab,86,45,02)};

  eul_allocate_static_string(str_4542, "compile time error condition: ", 30);
  eul_allocate_static_string(str_4543, "bad rename import syntax", 24);
  /* Byte-vector with size: 26 is_init: 0 index: 27 binding: (method-G004325) */
  static const void *G004540[] = {I(ab,24,00,00),B(stream2 ,10),I(23,00,00,00),B(ex_import ,25),I(24,00,00,00),B(format ,4),I(3c,02,2a,24),B(stream2 ,10),I(1d,24,00,00),B(mop_access ,8),I(3c,02,2a,24),B(i_param ,55),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,2a),I(23,00,00,00),B(ex_import ,26),I(24,00,00,00),B(i_error ,5),I(23,00,00,00),B(ex_import ,16),I(47,00,00,24),B(boot ,12),I(3d,04,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 130 is_init: 0 index: 29 binding: anonymous */
  static const void *G004544[] = {I(ab,46,04,1c),I(48,00,00,84),I(24,00,00,00),B(mop_class ,20),I(24,00,00,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,24,00),B(boot1 ,25),I(3c,00,23,00),B(ex_import ,18),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,26),I(00,00,00,03),I(02,84,86,86),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,1f,03),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,7),I(3c,02,1f,04),I(1f,04,23,00),B(ex_import ,28),I(23,00,00,00),B(ex_import ,27),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,06),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,07),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(i_error ,5),I(86,24,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,1f,0a),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,7),I(3c,02,1f,04),I(1f,04,23,00),B(ex_import ,28),I(23,00,00,00),B(ex_import ,24),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,0d),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,0e),I(24,00,00,00),B(dynamic ,5),I(3c,01,2a,47),I(00,00,76,1b),I(24,00,00,00),B(cg_interf ,24),I(3c,01,1b,48),I(00,01,47,00),I(01,26,00,00),I(00,00,00,0f),I(24,00,00,00),B(sx_obj1 ,59),I(08,24,00,00),B(aux_table ,8),I(3c,01,24,00),B(sx_obj ,25),I(1c,24,00,00),B(boot ,10),I(3c,02,47,00),I(00,73,24,00),B(boot1 ,29),I(1c,24,00,00),B(boot ,10),I(3c,02,1d,1c),I(24,00,00,00),B(number ,9),I(3c,02,23,00),B(ex_import ,18),I(23,00,00,00),B(ex_import ,23),I(3b,01,1c,24),B(boot ,16),I(3c,02,2a,47),I(00,00,73,23),B(ex_import ,18),I(23,00,00,00),B(ex_import ,22),I(3b,01,1c,24),B(boot ,16),I(3c,02,2a,23),B(ex_import ,20),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,26),I(00,00,00,03),I(24,00,00,00),B(sx_obj1 ,59),I(08,1b,1f,0a),I(24,00,00,00),B(thread ,28),I(3c,02,1d,1c),I(1c,26,00,00),I(00,00,00,03),I(1d,24,00,00),B(sx_obj1 ,59),I(09,22,02,83),I(24,00,00,00),B(dynamic ,6),I(3c,01,2a,1b),I(45,20,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 30 binding: anonymous */
  static const void *G004547[] = {I(aa,1b,47,00),I(01,24,00,00),B(ex_import ,4),I(3c,02,24,00),B(p_env ,5),I(3d,01,01,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 31 binding: (method-G004297) */
  static const void *G004549[] = {I(ab,86,45,02)};

  eul_allocate_static_string(str_4553, "compile time error condition: ", 30);
  eul_allocate_static_string(str_4554, "bad except import syntax", 24);
  /* Byte-vector with size: 26 is_init: 0 index: 34 binding: (method-G004297) */
  static const void *G004551[] = {I(ab,24,00,00),B(stream2 ,10),I(23,00,00,00),B(ex_import ,32),I(24,00,00,00),B(format ,4),I(3c,02,2a,24),B(stream2 ,10),I(1d,24,00,00),B(mop_access ,8),I(3c,02,2a,24),B(i_param ,55),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,2a),I(23,00,00,00),B(ex_import ,33),I(24,00,00,00),B(i_error ,5),I(23,00,00,00),B(ex_import ,16),I(47,00,00,24),B(boot ,12),I(3d,04,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 118 is_init: 0 index: 36 binding: anonymous */
  static const void *G004555[] = {I(ab,46,04,1c),I(48,00,00,84),I(24,00,00,00),B(mop_class ,20),I(24,00,00,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,24,00),B(boot1 ,25),I(3c,00,23,00),B(ex_import ,18),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,26),I(00,00,00,03),I(02,84,86,86),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,1f,03),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,7),I(3c,02,1f,04),I(1f,04,23,00),B(ex_import ,35),I(23,00,00,00),B(ex_import ,34),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,06),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,07),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(i_error ,5),I(86,24,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,1f,0a),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,7),I(3c,02,1f,04),I(1f,04,23,00),B(ex_import ,35),I(23,00,00,00),B(ex_import ,31),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,0d),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,0e),I(24,00,00,00),B(dynamic ,5),I(3c,01,2a,47),I(00,00,76,1b),I(24,00,00,00),B(cg_interf ,24),I(3c,01,1b,48),I(00,01,47,00),I(01,26,00,00),I(00,00,00,0f),I(24,00,00,00),B(sx_obj1 ,59),I(08,1b,24,00),B(aux_table ,8),I(3c,01,24,00),B(sx_obj ,25),I(1c,24,00,00),B(boot ,10),I(3c,02,47,00),I(00,73,24,00),B(number ,9),I(3c,02,23,00),B(ex_import ,18),I(23,00,00,00),B(ex_import ,30),I(3b,01,1c,24),B(boot ,16),I(3c,02,2a,23),B(ex_import ,20),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,26),I(00,00,00,03),I(24,00,00,00),B(sx_obj1 ,59),I(08,1b,1f,07),I(24,00,00,00),B(thread ,28),I(3c,02,1d,1c),I(1c,26,00,00),I(00,00,00,03),I(1d,24,00,00),B(sx_obj1 ,59),I(09,22,02,83),I(24,00,00,00),B(dynamic ,6),I(3c,01,2a,1b),I(45,1d,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 37 binding: anonymous */
  static const void *G004558[] = {I(aa,1b,47,00),I(01,24,00,00),B(ex_import ,4),I(3c,02,24,00),B(p_env ,5),I(3d,01,01,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 38 binding: (method-G004271) */
  static const void *G004560[] = {I(ab,86,45,02)};

  eul_allocate_static_string(str_4564, "compile time error condition: ", 30);
  eul_allocate_static_string(str_4565, "bad only import syntax", 22);
  /* Byte-vector with size: 26 is_init: 0 index: 41 binding: (method-G004271) */
  static const void *G004562[] = {I(ab,24,00,00),B(stream2 ,10),I(23,00,00,00),B(ex_import ,39),I(24,00,00,00),B(format ,4),I(3c,02,2a,24),B(stream2 ,10),I(1d,24,00,00),B(mop_access ,8),I(3c,02,2a,24),B(i_param ,55),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,2a),I(23,00,00,00),B(ex_import ,40),I(24,00,00,00),B(i_error ,5),I(23,00,00,00),B(ex_import ,16),I(47,00,00,24),B(boot ,12),I(3d,04,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 105 is_init: 0 index: 43 binding: anonymous */
  static const void *G004566[] = {I(ab,46,04,1c),I(48,00,00,84),I(24,00,00,00),B(mop_class ,20),I(24,00,00,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,24,00),B(boot1 ,25),I(3c,00,23,00),B(ex_import ,18),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,26),I(00,00,00,03),I(02,84,86,86),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,1f,03),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,7),I(3c,02,1f,04),I(1f,04,23,00),B(ex_import ,42),I(23,00,00,00),B(ex_import ,41),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,06),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,07),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(i_error ,5),I(86,24,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,1f,0a),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,7),I(3c,02,1f,04),I(1f,04,23,00),B(ex_import ,42),I(23,00,00,00),B(ex_import ,38),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,0d),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,0e),I(24,00,00,00),B(dynamic ,5),I(3c,01,2a,47),I(00,00,76,1b),I(24,00,00,00),B(cg_interf ,24),I(3c,01,1b,48),I(00,01,47,00),I(00,73,23,00),B(ex_import ,18),I(23,00,00,00),B(ex_import ,37),I(3b,01,1c,24),B(boot ,16),I(3c,02,2a,23),B(ex_import ,20),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,26),I(00,00,00,03),I(24,00,00,00),B(sx_obj1 ,59),I(08,1b,1f,05),I(24,00,00,00),B(thread ,28),I(3c,02,1d,1c),I(1c,26,00,00),I(00,00,00,03),I(1d,24,00,00),B(sx_obj1 ,59),I(09,22,02,83),I(24,00,00,00),B(dynamic ,6),I(3c,01,2a,1b),I(45,1b,00,00)};

  /* Byte-vector with size: 38 is_init: 0 index: 48 binding: top-level */
  static const void *G004569[] = {I(a9,24,00,00),B(aux_table ,4),I(3c,00,1b,89),B(ex_import ,3),I(2a,23,00,00),B(ex_import ,44),I(23,00,00,00),B(ex_import ,18),I(23,00,00,00),B(ex_import ,43),I(3b,02,24,00),B(ex_import ,8),I(3c,02,2a,23),B(ex_import ,45),I(23,00,00,00),B(ex_import ,18),I(23,00,00,00),B(ex_import ,36),I(3b,02,24,00),B(ex_import ,8),I(3c,02,2a,23),B(ex_import ,46),I(23,00,00,00),B(ex_import ,18),I(23,00,00,00),B(ex_import ,29),I(3b,02,24,00),B(ex_import ,8),I(3c,02,2a,23),B(ex_import ,47),I(23,00,00,00),B(ex_import ,18),I(23,00,00,00),B(ex_import ,21),I(3b,02,24,00),B(ex_import ,8),I(3d,02,01,45),I(01,00,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 49 binding: expand-old-imports */
  static const void *G004575[] = {I(aa,24,00,00),B(ex_import ,6),I(1c,24,00,00),B(boot ,10),I(3d,02,01,00)};

  eul_allocate_static_string(str_4579, "external binding ~a not available in module ~a", 46);
  /* Byte-vector with size: 23 is_init: 0 index: 51 binding: import-binding */
  static const void *G004577[] = {I(ab,1c,1c,24),B(p_env ,19),I(3c,02,1b,34),I(00,00,00,0d),I(1b,32,00,00),I(00,00,00,27),I(1d,24,00,00),B(sx_node ,5),I(3c,01,1b,23),B(ex_import ,50),I(1f,05,1f,05),I(24,00,00,00),B(i_notify ,6),I(3c,04,22,01),I(1b,87,1c,26),I(00,00,00,03),I(1d,24,00,00),B(sx_obj1 ,42),I(09,22,02,2a),I(1d,24,00,00),B(ex_import ,5),I(3c,01,2a,1b),I(45,04,00,00)};

  /* Byte-vector with size: 59 is_init: 0 index: 52 binding: register-imported-module */
  static const void *G004580[] = {I(aa,23,00,00),B(ex_import ,20),I(24,00,00,00),B(dynamic ,3),I(3c,01,1c,26),I(00,00,00,15),I(24,00,00,00),B(sx_obj1 ,59),I(08,1c,26,00),I(00,00,00,03),I(24,00,00,00),B(sx_obj1 ,59),I(08,1c,1c,86),I(6c,1b,34,00),I(00,00,00,0e),I(86,32,00,00),I(00,00,00,aa),I(1f,03,83,24),B(sx_obj1 ,59),I(08,1f,05,83),I(24,00,00,00),B(sx_obj1 ,59),I(08,24,00,00),B(cg_interf ,6),I(3c,00,1f,05),I(1c,86,6c,1b),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,13),I(1f,06,1f,03),I(0f,1b,20,04),I(1f,03,22,01),I(2a,23,00,00),B(ex_import ,20),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,26),I(00,00,00,03),I(24,00,00,00),B(sx_obj1 ,59),I(08,1b,1f,09),I(24,00,00,00),B(thread ,28),I(3c,02,1d,1c),I(1c,26,00,00),I(00,00,00,03),I(1d,24,00,00),B(sx_obj1 ,59),I(09,22,02,2a),I(1f,06,1f,06),I(24,00,00,00),B(number ,14),I(3c,02,1f,0b),I(1c,1c,83,1d),I(24,00,00,00),B(sx_obj1 ,59),I(09,22,0a,45),I(05,00,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 53 binding: expand-import */
  static const void *G004582[] = {I(aa,24,00,00),B(ex_import ,9),I(24,00,00,00),B(ex_import ,9),I(3d,02,00,00)};

  /* Byte-vector with size: 20 is_init: 0 index: 54 binding: anonymous */
  static const void *G004584[] = {I(ab,1b,7a,1b),I(34,00,00,00),I(00,00,00,28),I(1d,41,00,00),B(aux_table ,10),I(22,01,1d,1c),I(24,00,00,00),B(p_env ,5),I(3d,02,04,22),I(01,32,00,00),I(00,00,00,26),I(1c,24,00,00),B(p_env ,5),I(3c,01,2a,1c),I(87,1c,26,00),I(00,00,00,03),I(1d,24,00,00),B(sx_obj1 ,42),I(09,22,02,45),I(03,00,00,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 55 binding: (method-G004221) */
  static const void *G004586[] = {I(ab,86,45,02)};

  eul_allocate_static_string(str_4590, "compile time error condition: ", 30);
  eul_allocate_static_string(str_4591, "cannot import lexical module ~a", 31);
  /* Byte-vector with size: 32 is_init: 0 index: 58 binding: (method-G004221) */
  static const void *G004588[] = {I(ab,24,00,00),B(stream2 ,10),I(23,00,00,00),B(ex_import ,56),I(24,00,00,00),B(format ,4),I(3c,02,2a,24),B(stream2 ,10),I(1d,24,00,00),B(mop_access ,8),I(3c,02,2a,24),B(i_param ,55),I(34,00,00,00),I(00,00,00,10),I(86,32,00,00),I(00,00,00,44),I(23,00,00,00),B(ex_import ,57),I(47,00,00,24),B(format ,2),I(3c,02,23,00),B(ex_import ,20),I(24,00,00,00),B(dynamic ,3),I(3c,01,1c,24),B(i_error ,5),I(23,00,00,00),B(ex_import ,16),I(1f,03,24,00),B(boot ,12),I(3d,04,04,22),I(02,45,02,00)};

  eul_allocate_static_string(str_4594, "  Import module ~a ...", 22);
  /* Byte-vector with size: 109 is_init: 0 index: 61 binding: import-module */
  static const void *G004592[] = {I(aa,46,01,1b),I(48,00,00,23),B(ex_import ,59),I(47,00,00,24),B(i_notify ,4),I(3c,02,2a,84),I(24,00,00,00),B(mop_class ,20),I(24,00,00,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,24,00),B(boot1 ,25),I(3c,00,23,00),B(ex_import ,18),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,26),I(00,00,00,03),I(02,84,86,86),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,1f,03),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,7),I(3c,02,1f,04),I(1f,04,23,00),B(ex_import ,60),I(23,00,00,00),B(ex_import ,58),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,06),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,07),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(i_error ,5),I(86,24,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,1f,0a),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,7),I(3c,02,1f,04),I(1f,04,23,00),B(ex_import ,60),I(23,00,00,00),B(ex_import ,55),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,0d),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,0e),I(24,00,00,00),B(dynamic ,5),I(3c,01,2a,47),I(00,00,24,00),B(sx_obj1 ,45),I(3c,01,1b,34),I(00,00,00,0d),I(1b,32,00,00),I(00,00,00,11),I(47,00,00,24),B(cg_interf ,24),I(3c,01,1b,26),I(00,00,00,0f),I(24,00,00,00),B(sx_obj1 ,59),I(08,23,00,00),B(ex_import ,18),I(23,00,00,00),B(ex_import ,54),I(3b,02,1c,24),B(aux_table ,7),I(3c,02,2a,47),I(00,00,24,00),B(sx_obj1 ,45),I(3c,01,1b,34),I(00,00,00,0d),I(86,32,00,00),I(00,00,00,11),I(1d,24,00,00),B(ex_import ,5),I(3c,01,2a,1d),I(83,24,00,00),B(dynamic ,6),I(3c,01,2a,1b),I(45,18,00,00)};

  eul_allocate_static_string(str_4598, "redefinition of expander ~a", 27);
  /* Byte-vector with size: 17 is_init: 0 index: 63 binding: install-import-expander */
  static const void *G004596[] = {I(ab,1c,24,00),B(ex_import ,3),I(3c,01,1b,34),I(00,00,00,1d),I(86,23,00,00),B(ex_import ,62),I(1f,04,24,00),B(i_notify ,5),I(3c,03,32,00),I(00,00,00,07),I(86,2a,24,00),B(ex_import ,3),I(24,00,00,00),B(boot1 ,41),I(3c,01,1f,03),I(1f,03,1d,3d),I(02,04,00,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 64 binding: anonymous */
  static const void *G004599[] = {I(ab,1c,45,02)};

  /* Byte-vector with size: 3 is_init: 0 index: 65 binding: anonymous */
  static const void *G004601[] = {I(ab,1c,24,00),B(ex_import ,7),I(3d,01,02,00)};

  eul_allocate_static_string(str_4605, "no import expander ~a available", 31);
  eul_allocate_static_string(str_4606, "no import expander ~a available", 31);
  /* Byte-vector with size: 42 is_init: 0 index: 68 binding: import-expander */
  static const void *G004603[] = {I(ab,1c,7c,1b),I(34,00,00,00),I(00,00,00,20),I(23,00,00,00),B(ex_import ,18),I(23,00,00,00),B(ex_import ,65),I(3b,02,32,00),I(00,00,00,80),I(1d,7a,12,1b),I(34,00,00,00),I(00,00,00,20),I(23,00,00,00),B(ex_import ,18),I(23,00,00,00),B(ex_import ,64),I(3b,02,32,00),I(00,00,00,5a),I(1f,03,10,7c),I(1b,34,00,00),I(00,00,00,3b),I(1f,04,10,24),B(ex_import ,3),I(3c,01,1b,34),I(00,00,00,0d),I(1b,32,00,00),I(00,00,00,19),I(23,00,00,00),B(ex_import ,66),I(1f,06,24,00),B(boot ,12),I(3c,02,22,01),I(32,00,00,00),I(00,00,00,1a),I(23,00,00,00),B(ex_import ,67),I(1f,05,24,00),B(boot ,12),I(3c,02,22,01),I(22,01,1f,03),I(1f,03,1d,3d),I(02,04,00,00)};

  /* Byte-vector with size: 10 is_init: 0 index: 69 binding: make-prefix */
  static const void *G004607[] = {I(ab,1c,82,02),I(1c,82,02,1c),I(1c,24,00,00),B(collect ,17),I(3c,02,24,00),B(mop_class ,5),I(24,00,00,00),B(convert ,2),I(3d,02,04,45),I(04,00,00,00)};

  /* Byte-vector with size: 83 is_init: 1 index: 0 binding: initialize-ex-import */
  static const void *G004609[] = {I(87,25,00,00),B(ex_import ,1),I(24,00,00,00),B(cg_interf ,1),I(3e,0b,24,00),B(cg_interf ,0),I(3c,00,21,01),I(24,00,00,00),B(p_env ,1),I(3e,0b,24,00),B(p_env ,0),I(3c,00,21,01),I(24,00,00,00),B(sx_node ,1),I(3e,0b,24,00),B(sx_node ,0),I(3c,00,21,01),I(24,00,00,00),B(sx_obj ,1),I(3e,0b,24,00),B(sx_obj ,0),I(3c,00,21,01),I(24,00,00,00),B(i_all ,1),I(3e,0b,24,00),B(i_all ,0),I(3c,00,21,01),I(23,00,00,00),B(ex_import ,70),I(23,00,00,00),B(ex_import ,69),I(3b,02,25,00),B(ex_import ,10),I(23,00,00,00),B(ex_import ,71),I(23,00,00,00),B(ex_import ,68),I(3b,02,25,00),B(ex_import ,9),I(23,00,00,00),B(ex_import ,72),I(23,00,00,00),B(ex_import ,63),I(3b,02,25,00),B(ex_import ,8),I(23,00,00,00),B(ex_import ,73),I(23,00,00,00),B(ex_import ,61),I(3b,01,25,00),B(ex_import ,7),I(23,00,00,00),B(ex_import ,74),I(23,00,00,00),B(ex_import ,53),I(3b,01,25,00),B(ex_import ,6),I(23,00,00,00),B(ex_import ,75),I(23,00,00,00),B(ex_import ,52),I(3b,01,25,00),B(ex_import ,5),I(23,00,00,00),B(ex_import ,76),I(23,00,00,00),B(ex_import ,51),I(3b,02,25,00),B(ex_import ,4),I(86,25,00,00),B(ex_import ,3),I(23,00,00,00),B(ex_import ,77),I(23,00,00,00),B(ex_import ,49),I(3b,01,25,00),B(ex_import ,2),I(23,00,00,00),B(ex_import ,78),I(23,00,00,00),B(ex_import ,48),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G004519,G004518);
  eul_allocate_bytevector( G004521,G004520);
  eul_allocate_bytevector( G004523,G004522);
  object_class(str_4526) = eul_static_string_class;
  object_class(str_4527) = eul_static_string_class;
  eul_intern_keyword(key_4528,"ct-error-value");
  eul_allocate_bytevector( G004525,G004524);
  eul_intern_symbol(sym_4531,"anonymous");
  eul_intern_symbol(sym_4532,"(method G004357)");
  eul_intern_symbol(sym_4533,"*actual-module*");
  eul_allocate_bytevector( G004530,G004529);
  eul_allocate_bytevector( G004535,G004534);
  eul_allocate_bytevector( G004537,G004536);
  eul_allocate_bytevector( G004539,G004538);
  object_class(str_4542) = eul_static_string_class;
  object_class(str_4543) = eul_static_string_class;
  eul_allocate_bytevector( G004541,G004540);
  eul_intern_symbol(sym_4546,"(method G004325)");
  eul_allocate_bytevector( G004545,G004544);
  eul_allocate_bytevector( G004548,G004547);
  eul_allocate_bytevector( G004550,G004549);
  object_class(str_4553) = eul_static_string_class;
  object_class(str_4554) = eul_static_string_class;
  eul_allocate_bytevector( G004552,G004551);
  eul_intern_symbol(sym_4557,"(method G004297)");
  eul_allocate_bytevector( G004556,G004555);
  eul_allocate_bytevector( G004559,G004558);
  eul_allocate_bytevector( G004561,G004560);
  object_class(str_4564) = eul_static_string_class;
  object_class(str_4565) = eul_static_string_class;
  eul_allocate_bytevector( G004563,G004562);
  eul_intern_symbol(sym_4568,"(method G004271)");
  eul_allocate_bytevector( G004567,G004566);
  eul_intern_symbol(sym_4571,"only");
  eul_intern_symbol(sym_4572,"except");
  eul_intern_symbol(sym_4573,"rename");
  eul_intern_symbol(sym_4574,"prefix");
  eul_allocate_bytevector( G004570,G004569);
  eul_allocate_bytevector( G004576,G004575);
  object_class(str_4579) = eul_static_string_class;
  eul_allocate_bytevector( G004578,G004577);
  eul_allocate_bytevector( G004581,G004580);
  eul_allocate_bytevector( G004583,G004582);
  eul_allocate_bytevector( G004585,G004584);
  eul_allocate_bytevector( G004587,G004586);
  object_class(str_4590) = eul_static_string_class;
  object_class(str_4591) = eul_static_string_class;
  eul_allocate_bytevector( G004589,G004588);
  object_class(str_4594) = eul_static_string_class;
  eul_intern_symbol(sym_4595,"(method G004221)");
  eul_allocate_bytevector( G004593,G004592);
  object_class(str_4598) = eul_static_string_class;
  eul_allocate_bytevector( G004597,G004596);
  eul_allocate_bytevector( G004600,G004599);
  eul_allocate_bytevector( G004602,G004601);
  object_class(str_4605) = eul_static_string_class;
  object_class(str_4606) = eul_static_string_class;
  eul_allocate_bytevector( G004604,G004603);
  eul_allocate_bytevector( G004608,G004607);
  eul_intern_symbol(sym_4611,"make-prefix");
  eul_intern_symbol(sym_4612,"import-expander");
  eul_intern_symbol(sym_4613,"install-import-expander");
  eul_intern_symbol(sym_4614,"import-module");
  eul_intern_symbol(sym_4615,"expand-import");
  eul_intern_symbol(sym_4616,"register-imported-module");
  eul_intern_symbol(sym_4617,"import-binding");
  eul_intern_symbol(sym_4618,"expand-old-imports");
  eul_intern_symbol(sym_4619,"top-level");
  eul_allocate_bytevector( G004610,G004609);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 11; i++)
      ex_import_bindings[i] = eul_nil;
  }

  ex_import_bindings[ 11] = G004519;
  ex_import_bindings[ 12] = G004521;
  ex_import_bindings[ 13] = G004523;
  ex_import_bindings[ 14] = str_4526;
  ex_import_bindings[ 15] = str_4527;
  ex_import_bindings[ 16] = key_4528;
  ex_import_bindings[ 17] = G004525;
  ex_import_bindings[ 18] = sym_4531;
  ex_import_bindings[ 19] = sym_4532;
  ex_import_bindings[ 20] = sym_4533;
  ex_import_bindings[ 21] = G004530;
  ex_import_bindings[ 22] = G004535;
  ex_import_bindings[ 23] = G004537;
  ex_import_bindings[ 24] = G004539;
  ex_import_bindings[ 25] = str_4542;
  ex_import_bindings[ 26] = str_4543;
  ex_import_bindings[ 27] = G004541;
  ex_import_bindings[ 28] = sym_4546;
  ex_import_bindings[ 29] = G004545;
  ex_import_bindings[ 30] = G004548;
  ex_import_bindings[ 31] = G004550;
  ex_import_bindings[ 32] = str_4553;
  ex_import_bindings[ 33] = str_4554;
  ex_import_bindings[ 34] = G004552;
  ex_import_bindings[ 35] = sym_4557;
  ex_import_bindings[ 36] = G004556;
  ex_import_bindings[ 37] = G004559;
  ex_import_bindings[ 38] = G004561;
  ex_import_bindings[ 39] = str_4564;
  ex_import_bindings[ 40] = str_4565;
  ex_import_bindings[ 41] = G004563;
  ex_import_bindings[ 42] = sym_4568;
  ex_import_bindings[ 43] = G004567;
  ex_import_bindings[ 44] = sym_4571;
  ex_import_bindings[ 45] = sym_4572;
  ex_import_bindings[ 46] = sym_4573;
  ex_import_bindings[ 47] = sym_4574;
  ex_import_bindings[ 48] = G004570;
  ex_import_bindings[ 49] = G004576;
  ex_import_bindings[ 50] = str_4579;
  ex_import_bindings[ 51] = G004578;
  ex_import_bindings[ 52] = G004581;
  ex_import_bindings[ 53] = G004583;
  ex_import_bindings[ 54] = G004585;
  ex_import_bindings[ 55] = G004587;
  ex_import_bindings[ 56] = str_4590;
  ex_import_bindings[ 57] = str_4591;
  ex_import_bindings[ 58] = G004589;
  ex_import_bindings[ 59] = str_4594;
  ex_import_bindings[ 60] = sym_4595;
  ex_import_bindings[ 61] = G004593;
  ex_import_bindings[ 62] = str_4598;
  ex_import_bindings[ 63] = G004597;
  ex_import_bindings[ 64] = G004600;
  ex_import_bindings[ 65] = G004602;
  ex_import_bindings[ 66] = str_4605;
  ex_import_bindings[ 67] = str_4606;
  ex_import_bindings[ 68] = G004604;
  ex_import_bindings[ 69] = G004608;
  ex_import_bindings[ 1] = eul_nil;
  ex_import_bindings[ 70] = sym_4611;
  ex_import_bindings[ 71] = sym_4612;
  ex_import_bindings[ 72] = sym_4613;
  ex_import_bindings[ 73] = sym_4614;
  ex_import_bindings[ 74] = sym_4615;
  ex_import_bindings[ 75] = sym_4616;
  ex_import_bindings[ 76] = sym_4617;
  ex_import_bindings[ 77] = sym_4618;
  ex_import_bindings[ 78] = sym_4619;
  eul_allocate_lambda( ex_import_bindings[0], "initialize-ex-import", 0, G004610);

  }
}


/* eof */
