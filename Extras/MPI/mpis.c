///-----------------------------------------------------------------------------
///                 Generated by EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Description: C source file of EuLisp module mpis
/// Copyright: See file mpis.em
///-----------------------------------------------------------------------------

#include "eulisp.h"

/* Imported modules */
extern void initialize_module_level1();
extern void initialize_module_serial();
extern LispRef level1_bindings[];
extern LispRef string_bindings[];
extern LispRef character_bindings[];
extern LispRef float_bindings[];
extern LispRef fpi_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef boot1_bindings[];
extern LispRef condition_bindings[];
extern LispRef stream_bindings[];
extern LispRef serial_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef collect_bindings[];
extern LispRef boot_bindings[];
extern LispRef stream2_bindings[];

/* Module bindings with size 77 */
LispRef mpis_bindings[77];

/* Foreign functions */
static LispRef ff_stub_eul_mpi_send229 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G00316, G00317, G00318, res;

  POPVAL1(G00318);
  POPVAL1(G00317);
  POPVAL1(G00316);
  FF_RES_CONVERT6(res,eul_mpi_send(FF_ARG_CONVERT8(G00316), FF_ARG_CONVERT0(G00317), FF_ARG_CONVERT8(G00318)));
  return res;
}

static LispRef ff_stub_eul_mpi_receive230 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G00319, G00320, G00321, res;

  POPVAL1(G00321);
  POPVAL1(G00320);
  POPVAL1(G00319);
  FF_RES_CONVERT6(res,eul_mpi_receive(FF_ARG_CONVERT8(G00319), FF_ARG_CONVERT8(G00320), FF_ARG_CONVERT0(G00321)));
  return res;
}

static LispRef ff_stub_eul_mpi_probe231 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G00322, G00323, G00324, res;

  POPVAL1(G00324);
  POPVAL1(G00323);
  POPVAL1(G00322);
  FF_RES_CONVERT6(res,eul_mpi_probe(FF_ARG_CONVERT8(G00322), FF_ARG_CONVERT8(G00323), FF_ARG_CONVERT8(G00324)));
  return res;
}

static LispRef ff_stub_MPI_Finalize232 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef res;

  FF_RES_CONVERT0(res,MPI_Finalize());
  return res;
}

static LispRef ff_stub_eul_mpi_initialize233 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef res;

  FF_RES_CONVERT6(res,eul_mpi_initialize());
  return res;
}


/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module mpis */
void initialize_module_mpis()
{
  if (is_initialized) return;
  initialize_module_level1();
  initialize_module_serial();
  eul_fast_table_set(eul_modules,"mpis",(LispRef) mpis_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_315, sym_314, sym_313, sym_312, sym_311, sym_310, G00309, G00307, G00305, G00303, G00301, G00299, sym_297, sym_296, sym_295, sym_294, sym_293, sym_292, sym_291, sym_290, sym_289, sym_288, sym_287, sym_286, key_285, key_284, key_283, sym_282, sym_281, sym_280, sym_279, sym_278, key_277, G00276, G00274, G00272, G00270, G00268, G00266, G00264, sym_261, sym_260, key_259, G00258, G00255, G00253, G00251, G00248, G00245, G00242, G00239, G00237, G00235;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 13 is_init: 0 index: 19 binding: (method-generic-read) */
  static const void *G00234[] = {I(43,03,1d,84),I(24,00,00,00),B(mpis ,9),I(08,1f,03,82),I(24,00,00,00),B(mpis ,9),I(08,2a,1b,86),I(1f,04,41,00),B(mpis ,16),I(22,03,24,00),B(stream2 ,10),I(2a,12,2a,86),I(45,04,00,00)};

  /* Byte-vector with size: 14 is_init: 0 index: 20 binding: (method-generic-write) */
  static const void *G00236[] = {I(ab,1b,83,24),B(mpis ,9),I(08,1c,87,1c),I(82,1d,24,00),B(mpis ,9),I(09,22,02,2a),I(1d,1c,24,00),B(stream2 ,29),I(3c,02,2a,1c),I(86,1c,82,1d),I(24,00,00,00),B(mpis ,9),I(09,22,02,2a),I(1c,45,03,00)};

  eul_allocate_static_string(str_240, "mpi write error with ~a", 23);
  /* Byte-vector with size: 17 is_init: 0 index: 22 binding: (method-generic-write) */
  static const void *G00238[] = {I(ab,1b,84,24),B(mpis ,9),I(08,1c,82,24),B(mpis ,9),I(08,1f,03,1d),I(1d,41,00,00),B(mpis ,14),I(22,03,12,1b),I(34,00,00,00),I(00,00,00,20),I(23,00,00,00),B(mpis ,21),I(1f,05,24,00),B(boot ,21),I(3d,02,05,32),I(00,00,00,07),I(1f,03,45,05)};

  eul_allocate_static_string(str_243, "mpi write error with ~a", 23);
  /* Byte-vector with size: 17 is_init: 0 index: 24 binding: (method-generic-write) */
  static const void *G00241[] = {I(ab,1b,84,24),B(mpis ,9),I(08,1c,82,24),B(mpis ,9),I(08,1f,03,1d),I(1d,41,00,00),B(mpis ,14),I(22,03,12,1b),I(34,00,00,00),I(00,00,00,20),I(23,00,00,00),B(mpis ,23),I(1f,05,24,00),B(boot ,21),I(3d,02,05,32),I(00,00,00,07),I(1f,03,45,05)};

  eul_allocate_static_string(str_246, "mpi write error with ~a", 23);
  /* Byte-vector with size: 17 is_init: 0 index: 26 binding: (method-generic-write) */
  static const void *G00244[] = {I(ab,1b,84,24),B(mpis ,9),I(08,1c,82,24),B(mpis ,9),I(08,1f,03,1d),I(1d,41,00,00),B(mpis ,14),I(22,03,12,1b),I(34,00,00,00),I(00,00,00,20),I(23,00,00,00),B(mpis ,25),I(1f,05,24,00),B(boot ,21),I(3d,02,05,32),I(00,00,00,07),I(1f,03,45,05)};

  eul_allocate_static_string(str_249, "mpi write error with ~a", 23);
  /* Byte-vector with size: 17 is_init: 0 index: 28 binding: (method-generic-write) */
  static const void *G00247[] = {I(ab,1b,84,24),B(mpis ,9),I(08,1c,82,24),B(mpis ,9),I(08,1f,03,1d),I(1d,41,00,00),B(mpis ,14),I(22,03,12,1b),I(34,00,00,00),I(00,00,00,20),I(23,00,00,00),B(mpis ,27),I(1f,05,24,00),B(boot ,21),I(3d,02,05,32),I(00,00,00,07),I(1f,03,45,05)};

  /* Byte-vector with size: 5 is_init: 0 index: 29 binding: (method-reset) */
  static const void *G00250[] = {I(aa,83,24,00),B(mpis ,9),I(08,24,00,00),B(collect ,19),I(3d,01,00,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 30 binding: (method-disconnect) */
  static const void *G00252[] = {I(aa,41,00,00),B(mpis ,17),I(2a,38,01,00),I(45,01,00,00)};

  eul_allocate_static_string(str_256, "#<~a: ~a ~a>", 12);
  /* Byte-vector with size: 16 is_init: 0 index: 32 binding: (method-generic-prin) */
  static const void *G00254[] = {I(ab,1c,04,1b),I(82,02,1f,03),I(26,00,00,00),I(00,00,00,03),I(24,00,00,00),B(mpis ,9),I(08,1f,04,84),I(24,00,00,00),B(mpis ,9),I(08,1f,04,23),B(mpis ,31),I(1f,04,1f,04),I(1f,04,24,00),B(mop_gf ,17),I(3d,05,06,45),I(06,00,00,00)};

  eul_allocate_static_string(str_262, "exceeded specified number of mpi stream", 39);
  /* Byte-vector with size: 70 is_init: 0 index: 37 binding: (method-initialize) */
  static const void *G00257[] = {I(ab,1c,1c,37),I(02,2a,24,00),B(mpis ,10),I(24,00,00,00),B(mpis ,11),I(1a,1b,34,00),I(00,00,00,e2),I(24,00,00,00),B(serial ,23),I(23,00,00,00),B(mpis ,33),I(23,00,00,00),B(mpis ,34),I(24,00,00,00),B(mop_gf ,2),I(3c,03,24,00),B(mpis ,10),I(24,00,00,00),B(mpis ,8),I(19,1b,34,00),I(00,00,00,26),I(1f,04,24,00),B(mpis ,7),I(1c,26,00,00),I(00,00,00,03),I(1d,24,00,00),B(mpis ,9),I(09,22,02,32),I(00,00,00,20),I(1f,04,23,00),B(mpis ,35),I(1c,26,00,00),I(00,00,00,03),I(1d,24,00,00),B(mpis ,9),I(09,22,02,2a),I(1f,04,24,00),B(mpis ,10),I(1c,84,1d,24),B(mpis ,9),I(09,22,02,2a),I(24,00,00,00),B(mpis ,10),I(2b,1b,89,00),B(mpis ,10),I(2a,1f,05,1f),I(03,1c,83,1d),I(24,00,00,00),B(mpis ,9),I(09,22,02,2a),I(1f,05,87,1c),I(82,1d,24,00),B(mpis ,9),I(09,22,02,2a),I(1d,1f,06,24),B(stream ,14),I(3c,02,2a,1f),I(05,86,1c,82),I(1d,24,00,00),B(mpis ,9),I(09,22,05,32),I(00,00,00,1f),I(23,00,00,00),B(mpis ,36),I(24,00,00,00),B(condition ,9),I(24,00,00,00),B(boot ,21),I(3c,02,2a,1d),I(45,03,00,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 38 binding: (method-mpi-stream-p) */
  static const void *G00263[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 39 binding: (method-mpi-stream-p) */
  static const void *G00265[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 5 is_init: 0 index: 40 binding: (setter-mpi-stream-host) */
  static const void *G00267[] = {I(ab,1c,26,00),I(00,00,00,03),I(1d,24,00,00),B(mpis ,9),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 41 binding: (setter-mpi-stream-rank) */
  static const void *G00269[] = {I(ab,1c,84,1d),I(24,00,00,00),B(mpis ,9),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 42 binding: (setter-mpi-stream-object-stream) */
  static const void *G00271[] = {I(ab,1c,83,1d),I(24,00,00,00),B(mpis ,9),I(09,45,02,00)};

  /* Byte-vector with size: 4 is_init: 0 index: 43 binding: (setter-mpi-stream-tag) */
  static const void *G00273[] = {I(ab,1c,82,1d),I(24,00,00,00),B(mpis ,9),I(09,45,02,00)};

  /* Byte-vector with size: 485 is_init: 0 index: 65 binding: top-level */
  static const void *G00275[] = {I(a9,24,00,00),B(stream2 ,21),I(24,00,00,00),B(boot1 ,26),I(3c,01,23,00),B(mpis ,44),I(23,00,00,00),B(mpis ,45),I(24,00,00,00),B(boot1 ,26),I(3c,02,23,00),B(mpis ,44),I(23,00,00,00),B(mpis ,46),I(24,00,00,00),B(boot1 ,26),I(3c,02,23,00),B(mpis ,44),I(23,00,00,00),B(mpis ,47),I(24,00,00,00),B(boot1 ,26),I(3c,02,23,00),B(mpis ,44),I(23,00,00,00),B(mpis ,48),I(24,00,00,00),B(boot1 ,26),I(3c,02,1f,03),I(1f,03,1f,03),I(1f,03,24,00),B(boot1 ,26),I(3c,04,24,00),B(mop_class ,71),I(23,00,00,00),B(mpis ,44),I(23,00,00,00),B(mpis ,49),I(23,00,00,00),B(mpis ,50),I(1f,09,23,00),B(mpis ,51),I(1f,06,23,00),B(mpis ,52),I(86,24,00,00),B(mop_gf ,2),I(3c,09,1b,89),B(mpis ,9),I(2a,41,00,00),B(mpis ,18),I(1b,89,00,00),B(mpis ,12),I(2a,24,00,00),B(mpis ,12),I(82,02,1b,89),B(mpis ,8),I(2a,24,00,00),B(mpis ,12),I(83,02,1b,89),B(mpis ,11),I(2a,24,00,00),B(mpis ,12),I(84,02,1b,89),B(mpis ,7),I(2a,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(mpis ,53),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(mpis ,6),I(2a,82,89,00),B(mpis ,10),I(2a,24,00,00),B(boot1 ,41),I(24,00,00,00),B(boot1 ,41),I(3c,01,24,00),B(mpis ,5),I(23,00,00,00),B(mpis ,54),I(23,00,00,00),B(mpis ,43),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,41),I(24,00,00,00),B(boot1 ,41),I(3c,01,24,00),B(mpis ,4),I(23,00,00,00),B(mpis ,55),I(23,00,00,00),B(mpis ,42),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,41),I(24,00,00,00),B(boot1 ,41),I(3c,01,24,00),B(mpis ,3),I(23,00,00,00),B(mpis ,56),I(23,00,00,00),B(mpis ,41),I(3b,02,1d,3c),I(02,2a,24,00),B(boot1 ,41),I(24,00,00,00),B(boot1 ,41),I(3c,01,24,00),B(mpis ,2),I(23,00,00,00),B(mpis ,57),I(23,00,00,00),B(mpis ,40),I(3b,02,1d,3c),I(02,2a,24,00),B(mpis ,6),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(mpis ,6),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(mpis ,58),I(23,00,00,00),B(mpis ,39),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(mpis ,6),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(mpis ,6),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mpis ,9),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(mpis ,6),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(mpis ,58),I(23,00,00,00),B(mpis ,38),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(mpis ,6),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(mpis ,6),I(2a,24,00,00),B(mpis ,9),I(2a,24,00,00),B(mop_gf ,12),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mpis ,9),I(86,24,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(mop_gf ,12),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(mpis ,59),I(23,00,00,00),B(mpis ,37),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(mop_gf ,12),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,2),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mpis ,9),I(24,00,00,00),B(stream2 ,21),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,2),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(mpis ,60),I(23,00,00,00),B(mpis ,32),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,2),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,41),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mpis ,9),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,41),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(mpis ,61),I(23,00,00,00),B(mpis ,30),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,41),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(collect ,19),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mpis ,9),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(collect ,19),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(mpis ,62),I(23,00,00,00),B(mpis ,29),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(collect ,19),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,29),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(fpi ,5),I(24,00,00,00),B(mpis ,9),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,29),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(mpis ,63),I(23,00,00,00),B(mpis ,28),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,29),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,29),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(float ,6),I(24,00,00,00),B(mpis ,9),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,29),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(mpis ,63),I(23,00,00,00),B(mpis ,26),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,29),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,29),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(character ,7),I(24,00,00,00),B(mpis ,9),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,29),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(mpis ,63),I(23,00,00,00),B(mpis ,24),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,29),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,29),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(string ,13),I(24,00,00,00),B(mpis ,9),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,29),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(mpis ,63),I(23,00,00,00),B(mpis ,22),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,29),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,29),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,21),I(24,00,00,00),B(mpis ,9),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,29),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(mpis ,63),I(23,00,00,00),B(mpis ,20),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,29),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(stream2 ,44),I(26,00,00,00),I(00,00,00,03),I(02,26,00,00),I(00,00,00,03),I(24,00,00,00),B(mpis ,9),I(86,86,24,00),B(boot1 ,39),I(3c,04,24,00),B(boot1 ,26),I(3c,00,24,00),B(stream2 ,44),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(mpis ,64),I(23,00,00,00),B(mpis ,19),I(3b,03,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(stream2 ,44),I(1c,24,00,00),B(mop_meth ,5),I(3d,02,5b,45),I(5b,00,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 66 binding: mpi-stream-host */
  static const void *G00298[] = {I(aa,26,00,00),I(00,00,00,03),I(24,00,00,00),B(mpis ,9),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 67 binding: mpi-stream-rank */
  static const void *G00300[] = {I(aa,84,24,00),B(mpis ,9),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 68 binding: mpi-stream-object-stream */
  static const void *G00302[] = {I(aa,83,24,00),B(mpis ,9),I(08,45,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 69 binding: mpi-stream-tag */
  static const void *G00304[] = {I(aa,82,24,00),B(mpis ,9),I(08,45,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 70 binding: local-mpi-stream? */
  static const void *G00306[] = {I(aa,84,24,00),B(mpis ,9),I(08,24,00,00),B(mpis ,8),I(19,45,00,00)};

  /* Byte-vector with size: 62 is_init: 1 index: 0 binding: initialize-mpis */
  static const void *G00308[] = {I(87,25,00,00),B(mpis ,1),I(24,00,00,00),B(serial ,1),I(3e,0b,24,00),B(serial ,0),I(3c,00,21,01),I(24,00,00,00),B(level1 ,1),I(3e,0b,24,00),B(level1 ,0),I(3c,00,21,01),I(23,00,00,00),B(mpis ,71),I(23,00,00,00),B(mpis ,70),I(3b,01,25,00),B(mpis ,13),I(86,25,00,00),B(mpis ,12),I(86,25,00,00),B(mpis ,11),I(86,25,00,00),B(mpis ,10),I(86,25,00,00),B(mpis ,9),I(86,25,00,00),B(mpis ,8),I(86,25,00,00),B(mpis ,7),I(86,25,00,00),B(mpis ,6),I(23,00,00,00),B(mpis ,72),I(23,00,00,00),B(mpis ,69),I(3b,01,25,00),B(mpis ,5),I(23,00,00,00),B(mpis ,73),I(23,00,00,00),B(mpis ,68),I(3b,01,25,00),B(mpis ,4),I(23,00,00,00),B(mpis ,74),I(23,00,00,00),B(mpis ,67),I(3b,01,25,00),B(mpis ,3),I(23,00,00,00),B(mpis ,75),I(23,00,00,00),B(mpis ,66),I(3b,01,25,00),B(mpis ,2),I(23,00,00,00),B(mpis ,76),I(23,00,00,00),B(mpis ,65),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G00235,G00234);
  eul_allocate_bytevector( G00237,G00236);
  object_class(str_240) = eul_static_string_class;
  eul_allocate_bytevector( G00239,G00238);
  object_class(str_243) = eul_static_string_class;
  eul_allocate_bytevector( G00242,G00241);
  object_class(str_246) = eul_static_string_class;
  eul_allocate_bytevector( G00245,G00244);
  object_class(str_249) = eul_static_string_class;
  eul_allocate_bytevector( G00248,G00247);
  eul_allocate_bytevector( G00251,G00250);
  eul_allocate_bytevector( G00253,G00252);
  object_class(str_256) = eul_static_string_class;
  eul_allocate_bytevector( G00255,G00254);
  eul_intern_keyword(key_259,"mode");
  eul_intern_symbol(sym_260,"rw");
  eul_intern_symbol(sym_261,"*remote*");
  object_class(str_262) = eul_static_string_class;
  eul_allocate_bytevector( G00258,G00257);
  eul_allocate_bytevector( G00264,G00263);
  eul_allocate_bytevector( G00266,G00265);
  eul_allocate_bytevector( G00268,G00267);
  eul_allocate_bytevector( G00270,G00269);
  eul_allocate_bytevector( G00272,G00271);
  eul_allocate_bytevector( G00274,G00273);
  eul_intern_keyword(key_277,"name");
  eul_intern_symbol(sym_278,"host");
  eul_intern_symbol(sym_279,"rank");
  eul_intern_symbol(sym_280,"object-stream");
  eul_intern_symbol(sym_281,"tag");
  eul_intern_symbol(sym_282,"mpi-stream");
  eul_intern_keyword(key_283,"direct-superclasses");
  eul_intern_keyword(key_284,"direct-slots");
  eul_intern_keyword(key_285,"direct-keywords");
  eul_intern_symbol(sym_286,"mpi-stream-p");
  eul_intern_symbol(sym_287,"(setter mpi-stream-tag)");
  eul_intern_symbol(sym_288,"(setter mpi-stream-object-stream)");
  eul_intern_symbol(sym_289,"(setter mpi-stream-rank)");
  eul_intern_symbol(sym_290,"(setter mpi-stream-host)");
  eul_intern_symbol(sym_291,"(method mpi-stream-p)");
  eul_intern_symbol(sym_292,"(method initialize)");
  eul_intern_symbol(sym_293,"(method generic-prin)");
  eul_intern_symbol(sym_294,"(method disconnect)");
  eul_intern_symbol(sym_295,"(method reset)");
  eul_intern_symbol(sym_296,"(method generic-write)");
  eul_intern_symbol(sym_297,"(method generic-read)");
  eul_allocate_bytevector( G00276,G00275);
  eul_allocate_bytevector( G00299,G00298);
  eul_allocate_bytevector( G00301,G00300);
  eul_allocate_bytevector( G00303,G00302);
  eul_allocate_bytevector( G00305,G00304);
  eul_allocate_bytevector( G00307,G00306);
  eul_intern_symbol(sym_310,"local-mpi-stream?");
  eul_intern_symbol(sym_311,"mpi-stream-tag");
  eul_intern_symbol(sym_312,"mpi-stream-object-stream");
  eul_intern_symbol(sym_313,"mpi-stream-rank");
  eul_intern_symbol(sym_314,"mpi-stream-host");
  eul_intern_symbol(sym_315,"top-level");
  eul_allocate_bytevector( G00309,G00308);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 14; i++)
      mpis_bindings[i] = eul_nil;
  }

  mpis_bindings[ 14] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_mpi_send229;
  mpis_bindings[ 15] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_mpi_receive230;
  mpis_bindings[ 16] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_mpi_probe231;
  mpis_bindings[ 17] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_MPI_Finalize232;
  mpis_bindings[ 18] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_mpi_initialize233;
  mpis_bindings[ 19] = G00235;
  mpis_bindings[ 20] = G00237;
  mpis_bindings[ 21] = str_240;
  mpis_bindings[ 22] = G00239;
  mpis_bindings[ 23] = str_243;
  mpis_bindings[ 24] = G00242;
  mpis_bindings[ 25] = str_246;
  mpis_bindings[ 26] = G00245;
  mpis_bindings[ 27] = str_249;
  mpis_bindings[ 28] = G00248;
  mpis_bindings[ 29] = G00251;
  mpis_bindings[ 30] = G00253;
  mpis_bindings[ 31] = str_256;
  mpis_bindings[ 32] = G00255;
  mpis_bindings[ 33] = key_259;
  mpis_bindings[ 34] = sym_260;
  mpis_bindings[ 35] = sym_261;
  mpis_bindings[ 36] = str_262;
  mpis_bindings[ 37] = G00258;
  mpis_bindings[ 38] = G00264;
  mpis_bindings[ 39] = G00266;
  mpis_bindings[ 40] = G00268;
  mpis_bindings[ 41] = G00270;
  mpis_bindings[ 42] = G00272;
  mpis_bindings[ 43] = G00274;
  mpis_bindings[ 44] = key_277;
  mpis_bindings[ 45] = sym_278;
  mpis_bindings[ 46] = sym_279;
  mpis_bindings[ 47] = sym_280;
  mpis_bindings[ 48] = sym_281;
  mpis_bindings[ 49] = sym_282;
  mpis_bindings[ 50] = key_283;
  mpis_bindings[ 51] = key_284;
  mpis_bindings[ 52] = key_285;
  mpis_bindings[ 53] = sym_286;
  mpis_bindings[ 54] = sym_287;
  mpis_bindings[ 55] = sym_288;
  mpis_bindings[ 56] = sym_289;
  mpis_bindings[ 57] = sym_290;
  mpis_bindings[ 58] = sym_291;
  mpis_bindings[ 59] = sym_292;
  mpis_bindings[ 60] = sym_293;
  mpis_bindings[ 61] = sym_294;
  mpis_bindings[ 62] = sym_295;
  mpis_bindings[ 63] = sym_296;
  mpis_bindings[ 64] = sym_297;
  mpis_bindings[ 65] = G00276;
  mpis_bindings[ 66] = G00299;
  mpis_bindings[ 67] = G00301;
  mpis_bindings[ 68] = G00303;
  mpis_bindings[ 69] = G00305;
  mpis_bindings[ 70] = G00307;
  mpis_bindings[ 1] = eul_nil;
  mpis_bindings[ 71] = sym_310;
  mpis_bindings[ 72] = sym_311;
  mpis_bindings[ 73] = sym_312;
  mpis_bindings[ 74] = sym_313;
  mpis_bindings[ 75] = sym_314;
  mpis_bindings[ 76] = sym_315;
  eul_allocate_lambda( mpis_bindings[0], "initialize-mpis", 0, G00309);

  }
}


/* eof */
