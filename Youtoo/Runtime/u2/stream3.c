/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module stream3
 **  Copyright: See file stream3.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern void initialize_module_integer();
extern void initialize_module_collect();
extern void initialize_module_list();
extern void initialize_module_character();
extern void initialize_module_string();
extern void initialize_module_vector();
extern void initialize_module_float();
extern void initialize_module_stream1();
extern void initialize_module_stream();
extern void initialize_module_format();
extern LispRef stream1_bindings[];
extern LispRef list_bindings[];
extern LispRef telos_bindings[];
extern LispRef string_bindings[];
extern LispRef vector_bindings[];
extern LispRef float_bindings[];
extern LispRef integer_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef boot_bindings[];
extern LispRef boot1_bindings[];
extern LispRef collect_bindings[];
extern LispRef character_bindings[];
extern LispRef format_bindings[];
extern LispRef socket_bindings[];
extern LispRef stream2_bindings[];
extern LispRef stream_bindings[];

/* Module bindings with size 52 */
LispRef stream3_bindings[52];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module stream3 */
void initialize_module_stream3()
{
  if (is_initialized) return;
  initialize_module_telos();
  initialize_module_integer();
  initialize_module_collect();
  initialize_module_list();
  initialize_module_character();
  initialize_module_string();
  initialize_module_vector();
  initialize_module_float();
  initialize_module_stream1();
  initialize_module_stream();
  initialize_module_format();
  eul_fast_table_set(eul_modules,"stream3",(LispRef) stream3_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_448, G00447, sym_445, sym_444, G00443, G00441, G00439, G00435, G00432, G00430, G00428, G00425, G00422, G00419, G00414, G00412, G00409, G00406, G00404, sym_399, G00398, G00395, G00392, G00387, G00385, G00382, G00380, G00378, G00376, sym_374, G00373;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 19 is_init: 0 index: 3 binding: (method-generic-print) */
  static const void *G00372[] = {I(ab,1c,82,02),I(1b,34,00,00),I(00,00,00,0f),I(1b,32,00,00),I(00,00,00,0f),I(23,00,00,00),B(stream3 ,2),I(82,02,1d,27),I(3c,24,00,00),B(stream ,23),I(3c,02,2a,1b),I(06,1c,1c,1f),I(05,24,00,00),B(stream ,11),I(3c,03,2a,1f),I(03,27,3e,24),B(stream ,23),I(3c,02,2a,1f),I(04,45,05,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 4 binding: (method-generic-print) */
  static const void *G00375[] = {I(ab,1c,06,1d),I(1c,1f,03,24),B(stream ,11),I(3c,03,2a,1d),I(45,03,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 5 binding: (method-generic-print) */
  static const void *G00377[] = {I(ab,1c,82,02),I(1b,1d,24,00),B(stream2 ,30),I(3c,02,2a,1d),I(45,03,00,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 6 binding: (method-generic-print) */
  static const void *G00379[] = {I(ab,1b,1d,24),B(stream ,23),I(3c,02,2a,1c),I(45,02,00,00)};

  eul_allocate_static_string(str_383, " . ", 3);
  /* Byte-vector with size: 27 is_init: 0 index: 8 binding: (method-generic-print) */
  static const void *G00381[] = {I(ab,1b,27,28),I(24,00,00,00),B(stream ,23),I(3c,02,2a,1c),I(24,00,00,00),B(stream2 ,30),I(1d,24,00,00),B(stream ,18),I(3c,03,1b,12),I(1b,34,00,00),I(00,00,00,0f),I(86,32,00,00),I(00,00,00,2d),I(23,00,00,00),B(stream3 ,7),I(26,00,00,00),I(00,00,00,03),I(1f,04,24,00),B(stream ,11),I(3c,03,2a,1d),I(1d,24,00,00),B(stream ,8),I(3c,02,2a,1d),I(27,29,24,00),B(stream ,23),I(3c,02,2a,1f),I(03,45,04,00)};

  /* Byte-vector with size: 19 is_init: 0 index: 9 binding: anonymous */
  static const void *G00384[] = {I(a9,47,00,03),I(47,00,02,1a),I(1b,34,00,00),I(00,00,00,3f),I(47,00,00,47),I(00,03,02,47),I(00,01,1c,24),B(stream ,8),I(3c,02,2a,47),I(00,01,27,20),I(24,00,00,00),B(stream ,23),I(3c,02,2a,47),I(00,03,2b,1b),I(48,00,03,47),I(00,04,3d,00),I(03,22,02,32),I(00,00,00,06),I(86,45,01,00)};

  eul_allocate_static_string(str_388, "#()", 3);
  eul_allocate_static_string(str_389, "#(", 2);
  eul_allocate_static_string(str_390, "#(", 2);
  /* Byte-vector with size: 61 is_init: 0 index: 13 binding: (method-generic-print) */
  static const void *G00386[] = {I(ab,46,06,1c),I(48,00,00,1b),I(48,00,01,47),I(00,00,06,1b),I(2d,1b,34,00),I(00,00,00,26),I(23,00,00,00),B(stream3 ,10),I(26,00,00,00),I(00,00,00,03),I(47,00,01,24),B(stream ,11),I(3c,03,32,00),I(00,00,00,bc),I(1c,83,19,1b),I(34,00,00,00),I(00,00,00,4c),I(23,00,00,00),B(stream3 ,11),I(84,47,00,01),I(24,00,00,00),B(stream ,11),I(3c,03,2a,47),I(00,00,82,02),I(47,00,01,1c),I(24,00,00,00),B(stream ,8),I(3c,02,2a,47),I(00,01,27,29),I(24,00,00,00),B(stream ,23),I(3c,02,22,01),I(32,00,00,00),I(00,00,00,6c),I(1d,2c,82,1c),I(48,00,02,1b),I(48,00,03,23),B(stream3 ,12),I(84,47,00,01),I(24,00,00,00),B(stream ,11),I(3c,03,2a,86),I(1b,48,00,04),I(23,00,00,00),B(stream3 ,2),I(23,00,00,00),B(stream3 ,9),I(3b,00,48,00),I(04,47,00,04),I(3c,00,2a,47),I(00,00,47,00),I(02,02,47,00),I(01,1c,24,00),B(stream ,8),I(3c,02,2a,47),I(00,01,27,29),I(24,00,00,00),B(stream ,23),I(3c,02,22,04),I(22,01,2a,47),I(00,00,45,04)};

  eul_allocate_static_string(str_393, "#<~a: ~a:~a>", 12);
  /* Byte-vector with size: 15 is_init: 0 index: 15 binding: (method-generic-write) */
  static const void *G00391[] = {I(ab,1c,04,1b),I(82,02,1f,03),I(84,24,00,00),B(socket ,8),I(08,1f,04,26),I(00,00,00,03),I(24,00,00,00),B(socket ,8),I(08,1f,04,23),B(stream3 ,14),I(1f,04,1f,04),I(1f,04,24,00),B(format ,4),I(3c,05,2a,1f),I(05,45,06,00)};

  eul_allocate_static_string(str_396, "#<~a: ~a:~a>", 12);
  /* Byte-vector with size: 14 is_init: 0 index: 17 binding: (method-generic-write) */
  static const void *G00394[] = {I(ab,1c,04,1b),I(82,02,1f,03),I(83,24,00,00),B(socket ,2),I(08,1f,04,82),I(24,00,00,00),B(socket ,2),I(08,1f,04,23),B(stream3 ,16),I(1f,04,1f,04),I(1f,04,24,00),B(format ,4),I(3c,05,2a,1f),I(05,45,06,00)};

  eul_allocate_static_string(str_400, "*unconnected*", 13);
  eul_allocate_static_string(str_401, "*unconnected*", 13);
  eul_allocate_static_string(str_402, "#<~a: ~a>", 9);
  /* Byte-vector with size: 47 is_init: 0 index: 22 binding: (method-generic-write) */
  static const void *G00397[] = {I(ab,1c,26,00),I(00,00,00,05),I(24,00,00,00),B(stream2 ,20),I(08,1d,26,00),I(00,00,00,04),I(24,00,00,00),B(stream2 ,20),I(08,1f,03,82),I(24,00,00,00),B(stream2 ,20),I(08,1f,04,04),I(1b,82,02,1d),I(23,00,00,00),B(stream3 ,18),I(50,1b,34,00),I(00,00,00,36),I(1f,04,24,00),B(stream2 ,24),I(3c,01,1b,34),I(00,00,00,15),I(1f,05,84,24),B(stream2 ,3),I(08,32,00,00),I(00,00,00,0f),I(23,00,00,00),B(stream3 ,19),I(22,01,32,00),I(00,00,00,30),I(1f,05,24,00),B(stream2 ,24),I(3c,01,1b,34),I(00,00,00,15),I(1f,06,84,24),B(stream2 ,3),I(08,32,00,00),I(00,00,00,0f),I(23,00,00,00),B(stream3 ,20),I(22,01,1f,07),I(23,00,00,00),B(stream3 ,21),I(1f,04,1f,03),I(24,00,00,00),B(format ,4),I(3c,04,2a,1f),I(08,45,09,00)};

  /* Byte-vector with size: 12 is_init: 0 index: 23 binding: (method-generic-write) */
  static const void *G00403[] = {I(ab,1b,27,22),I(24,00,00,00),B(stream ,23),I(3c,02,2a,1c),I(06,1d,1c,1f),I(03,24,00,00),B(stream ,11),I(3c,03,2a,1c),I(27,22,24,00),B(stream ,23),I(3c,02,2a,1d),I(45,03,00,00)};

  eul_allocate_static_string(str_407, "#\\\\n", 4);
  /* Byte-vector with size: 21 is_init: 0 index: 25 binding: (method-generic-write) */
  static const void *G00405[] = {I(ab,1c,27,0a),I(50,1b,34,00),I(00,00,00,26),I(23,00,00,00),B(stream3 ,24),I(26,00,00,00),I(00,00,00,04),I(1f,03,24,00),B(stream ,11),I(3c,03,32,00),I(00,00,00,28),I(1c,27,23,24),B(stream ,23),I(3c,02,2a,1c),I(27,5c,24,00),B(stream ,23),I(3c,02,2a,1c),I(1f,03,24,00),B(stream ,23),I(3c,02,2a,1d),I(45,03,00,00)};

  eul_allocate_static_string(str_410, " . ", 3);
  /* Byte-vector with size: 27 is_init: 0 index: 27 binding: (method-generic-write) */
  static const void *G00408[] = {I(ab,1b,27,28),I(24,00,00,00),B(stream ,23),I(3c,02,2a,1c),I(24,00,00,00),B(stream2 ,27),I(1d,24,00,00),B(stream ,18),I(3c,03,1b,12),I(1b,34,00,00),I(00,00,00,0f),I(86,32,00,00),I(00,00,00,2d),I(23,00,00,00),B(stream3 ,26),I(26,00,00,00),I(00,00,00,03),I(1f,04,24,00),B(stream ,11),I(3c,03,2a,1d),I(1d,24,00,00),B(stream ,4),I(3c,02,2a,1d),I(27,29,24,00),B(stream ,23),I(3c,02,2a,1f),I(03,45,04,00)};

  /* Byte-vector with size: 19 is_init: 0 index: 28 binding: anonymous */
  static const void *G00411[] = {I(a9,47,00,03),I(47,00,02,1a),I(1b,34,00,00),I(00,00,00,3f),I(47,00,00,47),I(00,03,02,47),I(00,01,1c,24),B(stream ,4),I(3c,02,2a,47),I(00,01,27,20),I(24,00,00,00),B(stream ,23),I(3c,02,2a,47),I(00,03,2b,1b),I(48,00,03,47),I(00,04,3d,00),I(03,22,02,32),I(00,00,00,06),I(86,45,01,00)};

  eul_allocate_static_string(str_415, "#()", 3);
  eul_allocate_static_string(str_416, "#(", 2);
  eul_allocate_static_string(str_417, "#(", 2);
  /* Byte-vector with size: 61 is_init: 0 index: 32 binding: (method-generic-write) */
  static const void *G00413[] = {I(ab,46,06,1c),I(48,00,00,1b),I(48,00,01,47),I(00,00,06,1b),I(2d,1b,34,00),I(00,00,00,26),I(23,00,00,00),B(stream3 ,29),I(26,00,00,00),I(00,00,00,03),I(47,00,01,24),B(stream ,11),I(3c,03,32,00),I(00,00,00,bc),I(1c,83,19,1b),I(34,00,00,00),I(00,00,00,4c),I(23,00,00,00),B(stream3 ,30),I(84,47,00,01),I(24,00,00,00),B(stream ,11),I(3c,03,2a,47),I(00,00,82,02),I(47,00,01,1c),I(24,00,00,00),B(stream ,4),I(3c,02,2a,47),I(00,01,27,29),I(24,00,00,00),B(stream ,23),I(3c,02,22,01),I(32,00,00,00),I(00,00,00,6c),I(1d,2c,82,1c),I(48,00,02,1b),I(48,00,03,23),B(stream3 ,31),I(84,47,00,01),I(24,00,00,00),B(stream ,11),I(3c,03,2a,86),I(1b,48,00,04),I(23,00,00,00),B(stream3 ,2),I(23,00,00,00),B(stream3 ,28),I(3b,00,48,00),I(04,47,00,04),I(3c,00,2a,47),I(00,00,47,00),I(02,02,47,00),I(01,1c,24,00),B(stream ,4),I(3c,02,2a,47),I(00,01,27,29),I(24,00,00,00),B(stream ,23),I(3c,02,22,04),I(22,01,2a,47),I(00,00,45,04)};

  eul_allocate_static_string(str_420, "#<~a: ~a>", 9);
  /* Byte-vector with size: 10 is_init: 0 index: 34 binding: (method-generic-write) */
  static const void *G00418[] = {I(ab,1c,04,1b),I(82,02,1f,03),I(82,02,1f,03),I(23,00,00,00),B(stream3 ,33),I(1f,03,1f,03),I(24,00,00,00),B(format ,4),I(3d,04,05,45),I(05,00,00,00)};

  eul_allocate_static_string(str_423, "%f", 2);
  /* Byte-vector with size: 6 is_init: 0 index: 36 binding: (method-generic-write) */
  static const void *G00421[] = {I(ab,1b,23,00),B(stream3 ,35),I(1f,03,24,00),B(stream ,21),I(3c,03,2a,1b),I(45,02,00,00)};

  eul_allocate_static_string(str_426, "%i", 2);
  /* Byte-vector with size: 6 is_init: 0 index: 38 binding: (method-generic-write) */
  static const void *G00424[] = {I(ab,1b,23,00),B(stream3 ,37),I(1f,03,24,00),B(stream ,21),I(3c,03,2a,1b),I(45,02,00,00)};

  /* Byte-vector with size: 9 is_init: 0 index: 39 binding: (method-generic-write) */
  static const void *G00427[] = {I(ab,1c,82,02),I(1b,06,1c,1c),I(1f,04,24,00),B(stream ,11),I(3c,03,2a,1d),I(27,3a,24,00),B(stream ,23),I(3c,02,2a,1f),I(03,45,04,00)};

  /* Byte-vector with size: 21 is_init: 0 index: 40 binding: (method-generic-write) */
  static const void *G00429[] = {I(ab,1c,82,02),I(24,00,00,00),B(character ,10),I(1c,24,00,00),B(collect ,16),I(3c,02,1b,34),I(00,00,00,15),I(1c,1f,03,24),B(stream2 ,30),I(3c,02,32,00),I(00,00,00,28),I(1d,27,7c,24),B(stream ,23),I(3c,02,2a,1c),I(1f,03,24,00),B(stream2 ,30),I(3c,02,2a,1d),I(27,7c,24,00),B(stream ,23),I(3c,02,2a,1f),I(03,45,04,00)};

  eul_allocate_static_string(str_433, "()", 2);
  /* Byte-vector with size: 6 is_init: 0 index: 42 binding: (method-generic-write) */
  static const void *G00431[] = {I(ab,23,00,00),B(stream3 ,41),I(84,1d,24,00),B(stream ,11),I(3c,03,2a,86),I(45,02,00,00)};

  eul_allocate_static_string(str_436, "#<", 2);
  eul_allocate_static_string(str_437, ": ", 2);
  /* Byte-vector with size: 22 is_init: 0 index: 45 binding: (method-generic-write) */
  static const void *G00434[] = {I(ab,23,00,00),B(stream3 ,43),I(84,1d,24,00),B(stream ,11),I(3c,03,2a,1c),I(04,1b,82,02),I(82,02,1b,06),I(1c,1c,1f,05),I(24,00,00,00),B(stream ,11),I(3c,03,2a,23),B(stream3 ,44),I(84,1f,05,24),B(stream ,11),I(3c,03,2a,1f),I(04,1f,04,24),B(stream ,5),I(3c,02,2a,1f),I(03,27,3e,24),B(stream ,23),I(3c,02,2a,1f),I(04,45,05,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 46 binding: (method-generic-print) */
  static const void *G00438[] = {I(ab,24,00,00),B(stream2 ,27),I(3d,02,00,00)};

  /* Byte-vector with size: 11 is_init: 0 index: 47 binding: (method-generic-write) */
  static const void *G00440[] = {I(ab,1b,84,24),B(stream2 ,20),I(08,1b,34,00),I(00,00,00,1e),I(1c,84,24,00),B(stream2 ,20),I(08,1f,03,1f),I(03,1d,3d,02),I(04,22,01,32),I(00,00,00,06),I(86,45,03,00)};

  /* Byte-vector with size: 656 is_init: 0 index: 50 binding: top-level */
  static const void *G00442[] = {I(a9,24,00,00),B(stream2 ,27),I(26,00,00,00),I(00,00,00,03),I(02,84,86,24),B(stream2 ,20),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,27),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,48),I(23,00,00,00),B(stream3 ,47),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,27),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,30),I(26,00,00,00),I(00,00,00,03),I(02,84,86,24),B(stream2 ,20),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,30),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,49),I(23,00,00,00),B(stream3 ,46),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,30),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,27),I(26,00,00,00),I(00,00,00,03),I(02,84,86,24),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,27),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,48),I(23,00,00,00),B(stream3 ,45),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,27),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,27),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,28),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,27),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,48),I(23,00,00,00),B(stream3 ,42),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,27),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,27),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,5),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,27),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,48),I(23,00,00,00),B(stream3 ,40),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,27),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,27),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,92),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,27),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,48),I(23,00,00,00),B(stream3 ,39),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,27),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,27),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(integer ,2),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,27),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,48),I(23,00,00,00),B(stream3 ,38),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,27),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,27),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(float ,8),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,27),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,48),I(23,00,00,00),B(stream3 ,36),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,27),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,27),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,32),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,27),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,48),I(23,00,00,00),B(stream3 ,34),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,27),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,27),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(vector ,9),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,27),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,48),I(23,00,00,00),B(stream3 ,32),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,27),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,27),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,82),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,27),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,48),I(23,00,00,00),B(stream3 ,27),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,27),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,27),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(character ,5),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,27),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,48),I(23,00,00,00),B(stream3 ,25),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,27),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,27),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(string ,13),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,27),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,48),I(23,00,00,00),B(stream3 ,23),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,27),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,27),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(stream2 ,4),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,27),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,48),I(23,00,00,00),B(stream3 ,22),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,27),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,27),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(socket ,2),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,27),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,48),I(23,00,00,00),B(stream3 ,17),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,27),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,27),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(socket ,8),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,27),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,48),I(23,00,00,00),B(stream3 ,15),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,27),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,30),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(vector ,9),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,30),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,49),I(23,00,00,00),B(stream3 ,13),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,30),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,30),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,82),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,30),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,49),I(23,00,00,00),B(stream3 ,8),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,30),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,30),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(character ,5),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,30),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,49),I(23,00,00,00),B(stream3 ,6),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,30),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,30),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,5),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,30),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,49),I(23,00,00,00),B(stream3 ,5),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,30),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,30),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(string ,13),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,30),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,49),I(23,00,00,00),B(stream3 ,4),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,30),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,30),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,86),I(24,00,00,00),B(stream2 ,19),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,30),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(stream3 ,49),I(23,00,00,00),B(stream3 ,3),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,30),I(1c,24,00,00),B(mop_meth ,5),I(3d,02,84,45),I(84,00,00,00)};

  /* Byte-vector with size: 63 is_init: 1 index: 0 binding: initialize-stream3 */
  static const void *G00446[] = {I(87,25,00,00),B(stream3 ,1),I(24,00,00,00),B(format ,1),I(3e,0b,24,00),B(format ,0),I(3c,00,21,01),I(24,00,00,00),B(stream ,1),I(3e,0b,24,00),B(stream ,0),I(3c,00,21,01),I(24,00,00,00),B(stream1 ,1),I(3e,0b,24,00),B(stream1 ,0),I(3c,00,21,01),I(24,00,00,00),B(float ,1),I(3e,0b,24,00),B(float ,0),I(3c,00,21,01),I(24,00,00,00),B(vector ,1),I(3e,0b,24,00),B(vector ,0),I(3c,00,21,01),I(24,00,00,00),B(string ,1),I(3e,0b,24,00),B(string ,0),I(3c,00,21,01),I(24,00,00,00),B(character ,1),I(3e,0b,24,00),B(character ,0),I(3c,00,21,01),I(24,00,00,00),B(list ,1),I(3e,0b,24,00),B(list ,0),I(3c,00,21,01),I(24,00,00,00),B(collect ,1),I(3e,0b,24,00),B(collect ,0),I(3c,00,21,01),I(24,00,00,00),B(integer ,1),I(3e,0b,24,00),B(integer ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(23,00,00,00),B(stream3 ,51),I(23,00,00,00),B(stream3 ,50),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_intern_symbol(sym_374,"anonymous");
  eul_allocate_bytevector( G00373,G00372);
  eul_allocate_bytevector( G00376,G00375);
  eul_allocate_bytevector( G00378,G00377);
  eul_allocate_bytevector( G00380,G00379);
  object_class(str_383) = eul_static_string_class;
  eul_allocate_bytevector( G00382,G00381);
  eul_allocate_bytevector( G00385,G00384);
  object_class(str_388) = eul_static_string_class;
  object_class(str_389) = eul_static_string_class;
  object_class(str_390) = eul_static_string_class;
  eul_allocate_bytevector( G00387,G00386);
  object_class(str_393) = eul_static_string_class;
  eul_allocate_bytevector( G00392,G00391);
  object_class(str_396) = eul_static_string_class;
  eul_allocate_bytevector( G00395,G00394);
  eul_intern_symbol(sym_399,"r");
  object_class(str_400) = eul_static_string_class;
  object_class(str_401) = eul_static_string_class;
  object_class(str_402) = eul_static_string_class;
  eul_allocate_bytevector( G00398,G00397);
  eul_allocate_bytevector( G00404,G00403);
  object_class(str_407) = eul_static_string_class;
  eul_allocate_bytevector( G00406,G00405);
  object_class(str_410) = eul_static_string_class;
  eul_allocate_bytevector( G00409,G00408);
  eul_allocate_bytevector( G00412,G00411);
  object_class(str_415) = eul_static_string_class;
  object_class(str_416) = eul_static_string_class;
  object_class(str_417) = eul_static_string_class;
  eul_allocate_bytevector( G00414,G00413);
  object_class(str_420) = eul_static_string_class;
  eul_allocate_bytevector( G00419,G00418);
  object_class(str_423) = eul_static_string_class;
  eul_allocate_bytevector( G00422,G00421);
  object_class(str_426) = eul_static_string_class;
  eul_allocate_bytevector( G00425,G00424);
  eul_allocate_bytevector( G00428,G00427);
  eul_allocate_bytevector( G00430,G00429);
  object_class(str_433) = eul_static_string_class;
  eul_allocate_bytevector( G00432,G00431);
  object_class(str_436) = eul_static_string_class;
  object_class(str_437) = eul_static_string_class;
  eul_allocate_bytevector( G00435,G00434);
  eul_allocate_bytevector( G00439,G00438);
  eul_allocate_bytevector( G00441,G00440);
  eul_intern_symbol(sym_444,"(method generic-write)");
  eul_intern_symbol(sym_445,"(method generic-print)");
  eul_allocate_bytevector( G00443,G00442);
  eul_intern_symbol(sym_448,"top-level");
  eul_allocate_bytevector( G00447,G00446);

  /* Set local bindings */
  stream3_bindings[ 2] = sym_374;
  stream3_bindings[ 3] = G00373;
  stream3_bindings[ 4] = G00376;
  stream3_bindings[ 5] = G00378;
  stream3_bindings[ 6] = G00380;
  stream3_bindings[ 7] = str_383;
  stream3_bindings[ 8] = G00382;
  stream3_bindings[ 9] = G00385;
  stream3_bindings[ 10] = str_388;
  stream3_bindings[ 11] = str_389;
  stream3_bindings[ 12] = str_390;
  stream3_bindings[ 13] = G00387;
  stream3_bindings[ 14] = str_393;
  stream3_bindings[ 15] = G00392;
  stream3_bindings[ 16] = str_396;
  stream3_bindings[ 17] = G00395;
  stream3_bindings[ 18] = sym_399;
  stream3_bindings[ 19] = str_400;
  stream3_bindings[ 20] = str_401;
  stream3_bindings[ 21] = str_402;
  stream3_bindings[ 22] = G00398;
  stream3_bindings[ 23] = G00404;
  stream3_bindings[ 24] = str_407;
  stream3_bindings[ 25] = G00406;
  stream3_bindings[ 26] = str_410;
  stream3_bindings[ 27] = G00409;
  stream3_bindings[ 28] = G00412;
  stream3_bindings[ 29] = str_415;
  stream3_bindings[ 30] = str_416;
  stream3_bindings[ 31] = str_417;
  stream3_bindings[ 32] = G00414;
  stream3_bindings[ 33] = str_420;
  stream3_bindings[ 34] = G00419;
  stream3_bindings[ 35] = str_423;
  stream3_bindings[ 36] = G00422;
  stream3_bindings[ 37] = str_426;
  stream3_bindings[ 38] = G00425;
  stream3_bindings[ 39] = G00428;
  stream3_bindings[ 40] = G00430;
  stream3_bindings[ 41] = str_433;
  stream3_bindings[ 42] = G00432;
  stream3_bindings[ 43] = str_436;
  stream3_bindings[ 44] = str_437;
  stream3_bindings[ 45] = G00435;
  stream3_bindings[ 46] = G00439;
  stream3_bindings[ 47] = G00441;
  stream3_bindings[ 48] = sym_444;
  stream3_bindings[ 49] = sym_445;
  stream3_bindings[ 50] = G00443;
  stream3_bindings[ 1] = eul_nil;
  stream3_bindings[ 51] = sym_448;
  eul_allocate_lambda( stream3_bindings[0], "initialize-stream3", 0, G00447);

  }
}


/* eof */