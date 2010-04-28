/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module convert1
 **  Copyright: See file convert1.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern void initialize_module_condition();
extern void initialize_module_convert();
extern void initialize_module_collect();
extern void initialize_module_fpi();
extern void initialize_module_character();
extern void initialize_module_list();
extern void initialize_module_string();
extern void initialize_module_vector();
extern void initialize_module_table();
extern LispRef list_bindings[];
extern LispRef telos_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef condition_bindings[];
extern LispRef boot_bindings[];
extern LispRef convert_bindings[];
extern LispRef vector_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef table1_bindings[];
extern LispRef boot1_bindings[];
extern LispRef table_bindings[];
extern LispRef fpi_bindings[];
extern LispRef character_bindings[];
extern LispRef collect_bindings[];
extern LispRef string_bindings[];

/* Module bindings with size 41 */
LispRef convert1_bindings[41];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module convert1 */
void initialize_module_convert1()
{
  if (is_initialized) return;
  initialize_module_telos();
  initialize_module_condition();
  initialize_module_convert();
  initialize_module_collect();
  initialize_module_fpi();
  initialize_module_character();
  initialize_module_list();
  initialize_module_string();
  initialize_module_vector();
  initialize_module_table();
  eul_fast_table_set(eul_modules,"convert1",(LispRef) convert1_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_1587, G001586, sym_1584, sym_1583, sym_1582, sym_1581, sym_1580, sym_1579, G001578, G001576, G001574, G001572, G001570, G001568, G001566, G001564, G001562, key_1560, G001559, G001556, G001554, G001552, G001550, G001548, G001546, G001544, G001542, G001540, sym_1538, G001537, G001535, G001533, G001531, G001529, G001527, G001525, G001523, G001521, G001519;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 1 is_init: 0 index: 2 binding: (method-(converter <character>)) */
  static const void *G001518[] = {I(aa,63,45,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 3 binding: (method-(converter <int>)) */
  static const void *G001520[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 4 binding: (method-(converter <int>)) */
  static const void *G001522[] = {I(aa,62,45,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 5 binding: (method-(converter <int>)) */
  static const void *G001524[] = {I(aa,41,00,00),B(string ,22),I(45,01,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 6 binding: (method-(converter <string>)) */
  static const void *G001526[] = {I(aa,24,00,00),B(collect ,7),I(3d,01,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 7 binding: (method-(converter <string>)) */
  static const void *G001528[] = {I(aa,24,00,00),B(character ,11),I(3d,01,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 8 binding: (method-(converter <string>)) */
  static const void *G001530[] = {I(aa,41,00,00),B(fpi ,8),I(45,01,00,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 9 binding: (method-(converter <table>)) */
  static const void *G001532[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 19 is_init: 0 index: 10 binding: anonymous */
  static const void *G001534[] = {I(a9,47,00,01),I(82,1a,1b,34),I(00,00,00,0d),I(47,00,02,32),I(00,00,00,38),I(47,00,00,47),I(00,01,02,24),B(table ,3),I(24,00,00,00),B(boot1 ,41),I(3c,01,47,00),I(02,47,00,01),I(1f,03,1f,03),I(3c,03,2a,47),I(00,01,2c,1b),I(48,00,01,47),I(00,03,3d,00),I(04,22,03,45),I(01,00,00,00)};

  /* Byte-vector with size: 19 is_init: 0 index: 12 binding: (method-(converter <table>)) */
  static const void *G001536[] = {I(aa,46,05,1b),I(48,00,00,47),I(00,00,06,24),B(table1 ,4),I(24,00,00,00),B(mop_gf ,2),I(3c,01,1c,48),I(00,01,1b,48),I(00,02,47,00),I(01,2c,1b,48),I(00,01,86,1b),I(48,00,03,23),B(convert1 ,11),I(23,00,00,00),B(convert1 ,10),I(3b,00,48,00),I(03,47,00,03),I(3d,00,05,45),I(05,00,00,00)};

  /* Byte-vector with size: 18 is_init: 0 index: 13 binding: anonymous */
  static const void *G001539[] = {I(aa,1b,12,1b),I(34,00,00,00),I(00,00,00,10),I(47,00,00,32),I(00,00,00,37),I(1c,10,24,00),B(table ,3),I(24,00,00,00),B(boot1 ,41),I(3c,01,47,00),I(00,47,00,01),I(1f,03,1f,03),I(3c,03,2a,47),I(00,01,2b,1b),I(48,00,01,1f),I(04,11,47,00),I(02,3d,01,05),I(22,03,45,02)};

  /* Byte-vector with size: 16 is_init: 0 index: 14 binding: (method-(converter <table>)) */
  static const void *G001541[] = {I(aa,46,03,24),B(table1 ,4),I(24,00,00,00),B(mop_gf ,2),I(3c,01,82,1c),I(48,00,00,1b),I(48,00,01,86),I(1b,48,00,02),I(23,00,00,00),B(convert1 ,11),I(23,00,00,00),B(convert1 ,13),I(3b,01,48,00),I(02,1f,03,47),I(00,02,3d,01),I(04,45,04,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 15 binding: (method-(converter <vector>)) */
  static const void *G001543[] = {I(aa,24,00,00),B(table1 ,16),I(3c,01,24,00),B(vector ,8),I(24,00,00,00),B(convert ,2),I(3d,02,00,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 16 binding: (method-(converter <vector>)) */
  static const void *G001545[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 14 is_init: 0 index: 17 binding: anonymous */
  static const void *G001547[] = {I(a9,47,00,03),I(82,1a,1b,34),I(00,00,00,0d),I(47,00,04,32),I(00,00,00,25),I(47,00,00,47),I(00,03,0b,47),I(00,04,47,00),I(03,1d,03,2a),I(47,00,03,2c),I(1b,48,00,03),I(47,00,01,3d),I(00,03,22,02),I(45,01,00,00)};

  /* Byte-vector with size: 18 is_init: 0 index: 18 binding: (method-(converter <vector>)) */
  static const void *G001549[] = {I(aa,46,06,1b),I(48,00,00,47),I(00,00,06,1b),I(48,00,03,47),I(00,03,24,00),B(boot1 ,39),I(3c,01,1b,48),I(00,04,47,00),I(03,2c,1b,48),I(00,03,86,1b),I(48,00,01,23),B(convert1 ,11),I(23,00,00,00),B(convert1 ,17),I(3b,00,48,00),I(01,47,00,01),I(3d,00,05,45),I(05,00,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 19 binding: (method-(converter <vector>)) */
  static const void *G001551[] = {I(aa,1b,24,00),B(boot ,8),I(3c,01,1b,1d),I(41,00,00,00),B(boot1 ,54),I(45,04,00,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 20 binding: (method-(converter <string>)) */
  static const void *G001553[] = {I(aa,24,00,00),B(table1 ,16),I(3c,01,24,00),B(string ,13),I(24,00,00,00),B(convert ,2),I(3d,02,00,00)};

  eul_allocate_static_string(str_1557, "conversion to string with non character element ", 48);
  /* Byte-vector with size: 25 is_init: 0 index: 22 binding: anonymous */
  static const void *G001555[] = {I(a9,47,00,03),I(82,1a,1b,34),I(00,00,00,0d),I(47,00,04,32),I(00,00,00,52),I(47,00,00,47),I(00,03,02,1b),I(81,1b,34,00),I(00,00,00,0e),I(86,32,00,00),I(00,00,00,21),I(23,00,00,00),B(convert1 ,21),I(24,00,00,00),B(condition ,9),I(24,00,00,00),B(boot ,21),I(3c,02,2a,47),I(00,04,47,00),I(03,1f,03,0c),I(2a,47,00,03),I(2c,1b,48,00),I(03,47,00,01),I(3d,00,04,22),I(03,45,01,00)};

  /* Byte-vector with size: 21 is_init: 0 index: 24 binding: (method-(converter <string>)) */
  static const void *G001558[] = {I(aa,46,06,1b),I(48,00,00,47),I(00,00,06,1b),I(48,00,03,24),B(string ,13),I(23,00,00,00),B(convert1 ,23),I(47,00,03,24),B(mop_gf ,2),I(3c,03,1b,48),I(00,04,47,00),I(03,2c,1b,48),I(00,03,86,1b),I(48,00,01,23),B(convert1 ,11),I(23,00,00,00),B(convert1 ,22),I(3b,00,48,00),I(01,47,00,01),I(3d,00,05,45),I(05,00,00,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 25 binding: (method-(converter <string>)) */
  static const void *G001561[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 3 is_init: 0 index: 26 binding: (method-(converter <string>)) */
  static const void *G001563[] = {I(aa,41,00,00),B(string ,18),I(45,01,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 27 binding: (method-(converter <list>)) */
  static const void *G001565[] = {I(aa,24,00,00),B(table1 ,16),I(3d,01,00,00)};

  /* Byte-vector with size: 13 is_init: 0 index: 28 binding: anonymous */
  static const void *G001567[] = {I(aa,47,00,02),I(82,1a,1b,34),I(00,00,00,0d),I(1c,32,00,00),I(00,00,00,22),I(47,00,00,47),I(00,02,02,47),I(00,02,2c,1b),I(48,00,02,1c),I(1f,04,0f,47),I(00,01,3d,01),I(04,22,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 13 is_init: 0 index: 29 binding: (method-(converter <list>)) */
  static const void *G001569[] = {I(aa,46,04,1b),I(48,00,00,47),I(00,00,06,2c),I(1b,48,00,02),I(86,1b,48,00),I(01,23,00,00),B(convert1 ,11),I(23,00,00,00),B(convert1 ,28),I(3b,01,48,00),I(01,86,47,00),I(01,3d,01,03),I(45,03,00,00)};

  /* Byte-vector with size: 13 is_init: 0 index: 30 binding: anonymous */
  static const void *G001571[] = {I(aa,47,00,02),I(82,1a,1b,34),I(00,00,00,0d),I(1c,32,00,00),I(00,00,00,22),I(47,00,00,47),I(00,02,0b,47),I(00,02,2c,1b),I(48,00,02,1c),I(1f,04,0f,47),I(00,01,3d,01),I(04,22,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 13 is_init: 0 index: 31 binding: (method-(converter <list>)) */
  static const void *G001573[] = {I(aa,46,04,1b),I(48,00,00,47),I(00,00,06,2c),I(1b,48,00,02),I(86,1b,48,00),I(01,23,00,00),B(convert1 ,11),I(23,00,00,00),B(convert1 ,30),I(3b,01,48,00),I(01,86,47,00),I(01,3d,01,03),I(45,03,00,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 32 binding: (method-(converter <list>)) */
  static const void *G001575[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 772 is_init: 0 index: 39 binding: top-level */
  static const void *G001577[] = {I(a9,24,00,00),B(mop_class ,64),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(mop_class ,64),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(mop_class ,64),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(mop_class ,64),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,33),I(23,00,00,00),B(convert1 ,32),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(mop_class ,64),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(mop_class ,64),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(string ,13),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(mop_class ,64),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,33),I(23,00,00,00),B(convert1 ,31),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(mop_class ,64),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(mop_class ,64),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(vector ,8),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(mop_class ,64),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,33),I(23,00,00,00),B(convert1 ,29),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(mop_class ,64),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(mop_class ,64),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(table1 ,4),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(mop_class ,64),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,33),I(23,00,00,00),B(convert1 ,27),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(mop_class ,64),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,34),I(23,00,00,00),B(convert1 ,26),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(string ,13),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,34),I(23,00,00,00),B(convert1 ,25),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(vector ,8),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,34),I(23,00,00,00),B(convert1 ,24),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(table1 ,4),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,34),I(23,00,00,00),B(convert1 ,20),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(vector ,8),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(vector ,8),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(mop_class ,64),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(vector ,8),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,35),I(23,00,00,00),B(convert1 ,19),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(vector ,8),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(vector ,8),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(string ,13),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(vector ,8),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,35),I(23,00,00,00),B(convert1 ,18),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(vector ,8),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(vector ,8),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(vector ,8),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(vector ,8),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,35),I(23,00,00,00),B(convert1 ,16),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(vector ,8),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(vector ,8),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(table1 ,4),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(vector ,8),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,35),I(23,00,00,00),B(convert1 ,15),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,4),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(table1 ,4),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(mop_class ,64),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,4),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,36),I(23,00,00,00),B(convert1 ,14),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,4),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(table1 ,4),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(vector ,8),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,4),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,36),I(23,00,00,00),B(convert1 ,12),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(table1 ,4),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(table1 ,4),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(table1 ,4),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(table1 ,4),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,36),I(23,00,00,00),B(convert1 ,9),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(fpi ,5),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,34),I(23,00,00,00),B(convert1 ,8),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(character ,7),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,34),I(23,00,00,00),B(convert1 ,7),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(mop_class ,34),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(string ,13),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,34),I(23,00,00,00),B(convert1 ,6),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(fpi ,5),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(fpi ,5),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(string ,13),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(fpi ,5),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,37),I(23,00,00,00),B(convert1 ,5),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(fpi ,5),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(fpi ,5),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(character ,7),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(fpi ,5),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,37),I(23,00,00,00),B(convert1 ,4),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(fpi ,5),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(fpi ,5),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(fpi ,5),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(fpi ,5),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,37),I(23,00,00,00),B(convert1 ,3),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(character ,7),I(24,00,00,00),B(mop_class ,13),I(3c,01,24,00),B(character ,7),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,03),I(02,83,24,00),B(fpi ,5),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(character ,7),I(24,00,00,00),B(mop_class ,13),I(3c,01,1b,26),I(00,00,00,04),I(02,1d,1c,24),B(boot ,11),I(3c,02,1f,05),I(1f,05,23,00),B(convert1 ,38),I(23,00,00,00),B(convert1 ,2),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,08),I(1c,24,00,00),B(mop_meth ,5),I(3d,02,c6,45),I(c6,00,00,00)};

  /* Byte-vector with size: 58 is_init: 1 index: 0 binding: initialize-convert1 */
  static const void *G001585[] = {I(87,25,00,00),B(convert1 ,1),I(24,00,00,00),B(table ,1),I(3e,0b,24,00),B(table ,0),I(3c,00,21,01),I(24,00,00,00),B(vector ,1),I(3e,0b,24,00),B(vector ,0),I(3c,00,21,01),I(24,00,00,00),B(string ,1),I(3e,0b,24,00),B(string ,0),I(3c,00,21,01),I(24,00,00,00),B(list ,1),I(3e,0b,24,00),B(list ,0),I(3c,00,21,01),I(24,00,00,00),B(character ,1),I(3e,0b,24,00),B(character ,0),I(3c,00,21,01),I(24,00,00,00),B(fpi ,1),I(3e,0b,24,00),B(fpi ,0),I(3c,00,21,01),I(24,00,00,00),B(collect ,1),I(3e,0b,24,00),B(collect ,0),I(3c,00,21,01),I(24,00,00,00),B(convert ,1),I(3e,0b,24,00),B(convert ,0),I(3c,00,21,01),I(24,00,00,00),B(condition ,1),I(3e,0b,24,00),B(condition ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(23,00,00,00),B(convert1 ,40),I(23,00,00,00),B(convert1 ,39),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G001519,G001518);
  eul_allocate_bytevector( G001521,G001520);
  eul_allocate_bytevector( G001523,G001522);
  eul_allocate_bytevector( G001525,G001524);
  eul_allocate_bytevector( G001527,G001526);
  eul_allocate_bytevector( G001529,G001528);
  eul_allocate_bytevector( G001531,G001530);
  eul_allocate_bytevector( G001533,G001532);
  eul_allocate_bytevector( G001535,G001534);
  eul_intern_symbol(sym_1538,"anonymous");
  eul_allocate_bytevector( G001537,G001536);
  eul_allocate_bytevector( G001540,G001539);
  eul_allocate_bytevector( G001542,G001541);
  eul_allocate_bytevector( G001544,G001543);
  eul_allocate_bytevector( G001546,G001545);
  eul_allocate_bytevector( G001548,G001547);
  eul_allocate_bytevector( G001550,G001549);
  eul_allocate_bytevector( G001552,G001551);
  eul_allocate_bytevector( G001554,G001553);
  object_class(str_1557) = eul_static_string_class;
  eul_allocate_bytevector( G001556,G001555);
  eul_intern_keyword(key_1560,"size");
  eul_allocate_bytevector( G001559,G001558);
  eul_allocate_bytevector( G001562,G001561);
  eul_allocate_bytevector( G001564,G001563);
  eul_allocate_bytevector( G001566,G001565);
  eul_allocate_bytevector( G001568,G001567);
  eul_allocate_bytevector( G001570,G001569);
  eul_allocate_bytevector( G001572,G001571);
  eul_allocate_bytevector( G001574,G001573);
  eul_allocate_bytevector( G001576,G001575);
  eul_intern_symbol(sym_1579,"(method (converter <list>))");
  eul_intern_symbol(sym_1580,"(method (converter <string>))");
  eul_intern_symbol(sym_1581,"(method (converter <vector>))");
  eul_intern_symbol(sym_1582,"(method (converter <table>))");
  eul_intern_symbol(sym_1583,"(method (converter <int>))");
  eul_intern_symbol(sym_1584,"(method (converter <character>))");
  eul_allocate_bytevector( G001578,G001577);
  eul_intern_symbol(sym_1587,"top-level");
  eul_allocate_bytevector( G001586,G001585);

  /* Set local bindings */
  convert1_bindings[ 2] = G001519;
  convert1_bindings[ 3] = G001521;
  convert1_bindings[ 4] = G001523;
  convert1_bindings[ 5] = G001525;
  convert1_bindings[ 6] = G001527;
  convert1_bindings[ 7] = G001529;
  convert1_bindings[ 8] = G001531;
  convert1_bindings[ 9] = G001533;
  convert1_bindings[ 10] = G001535;
  convert1_bindings[ 11] = sym_1538;
  convert1_bindings[ 12] = G001537;
  convert1_bindings[ 13] = G001540;
  convert1_bindings[ 14] = G001542;
  convert1_bindings[ 15] = G001544;
  convert1_bindings[ 16] = G001546;
  convert1_bindings[ 17] = G001548;
  convert1_bindings[ 18] = G001550;
  convert1_bindings[ 19] = G001552;
  convert1_bindings[ 20] = G001554;
  convert1_bindings[ 21] = str_1557;
  convert1_bindings[ 22] = G001556;
  convert1_bindings[ 23] = key_1560;
  convert1_bindings[ 24] = G001559;
  convert1_bindings[ 25] = G001562;
  convert1_bindings[ 26] = G001564;
  convert1_bindings[ 27] = G001566;
  convert1_bindings[ 28] = G001568;
  convert1_bindings[ 29] = G001570;
  convert1_bindings[ 30] = G001572;
  convert1_bindings[ 31] = G001574;
  convert1_bindings[ 32] = G001576;
  convert1_bindings[ 33] = sym_1579;
  convert1_bindings[ 34] = sym_1580;
  convert1_bindings[ 35] = sym_1581;
  convert1_bindings[ 36] = sym_1582;
  convert1_bindings[ 37] = sym_1583;
  convert1_bindings[ 38] = sym_1584;
  convert1_bindings[ 39] = G001578;
  convert1_bindings[ 1] = eul_nil;
  convert1_bindings[ 40] = sym_1587;
  eul_allocate_lambda( convert1_bindings[0], "initialize-convert1", 0, G001586);

  }
}


/* eof */
