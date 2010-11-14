/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module double
 **  Copyright: See file double.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern void initialize_module_compare();
extern void initialize_module_number();
extern void initialize_module_integer();
extern void initialize_module_fpi();
extern void initialize_module_string();
extern void initialize_module_float();
extern void initialize_module_double1();
extern LispRef string_bindings[];
extern LispRef integer_bindings[];
extern LispRef telos_bindings[];
extern LispRef number_bindings[];
extern LispRef fpi_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef boot_bindings[];
extern LispRef boot1_bindings[];
extern LispRef float_bindings[];
extern LispRef compare_bindings[];
extern LispRef double1_bindings[];

/* Module bindings with size 47 */
LispRef double_bindings[47];

/* Foreign functions */
static LispRef ff_stub_eul_get_dbl_max619 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef res;

  FF_RES_CONVERT2(res,eul_get_dbl_max());
  return res;
}

static LispRef ff_stub_eul_get_dbl_min620 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef res;

  FF_RES_CONVERT2(res,eul_get_dbl_min());
  return res;
}

static LispRef ff_stub_eul_get_dbl_epsilon621 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef res;

  FF_RES_CONVERT2(res,eul_get_dbl_epsilon());
  return res;
}

static LispRef ff_stub_eul_get_neg_dbl_epsilon622 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef res;

  FF_RES_CONVERT2(res,eul_get_neg_dbl_epsilon());
  return res;
}

static LispRef ff_stub_eul_dbl_equal623 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G00680, G00681, res;

  POPVAL1(G00681);
  POPVAL1(G00680);
  FF_RES_CONVERT6(res,eul_dbl_equal(FF_ARG_CONVERT2(G00680), FF_ARG_CONVERT2(G00681)));
  return res;
}

static LispRef ff_stub_eul_dbl_less624 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G00682, G00683, res;

  POPVAL1(G00683);
  POPVAL1(G00682);
  FF_RES_CONVERT6(res,eul_dbl_less(FF_ARG_CONVERT2(G00682), FF_ARG_CONVERT2(G00683)));
  return res;
}


/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module double */
void initialize_module_double()
{
  if (is_initialized) return;
  initialize_module_telos();
  initialize_module_compare();
  initialize_module_number();
  initialize_module_integer();
  initialize_module_fpi();
  initialize_module_string();
  initialize_module_float();
  initialize_module_double1();
  eul_fast_table_set(eul_modules,"double",(LispRef) double_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_679, sym_678, sym_677, G00676, G00674, G00672, sym_670, sym_669, sym_668, sym_667, sym_666, sym_665, sym_664, sym_663, sym_662, G00661, G00659, G00657, G00655, G00653, G00651, G00649, G00647, G00645, G00643, G00641, G00639, G00637, G00635, G00633, G00631, G00629, dbl_627, G00626;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 5 is_init: 0 index: 15 binding: (method-zero?) */
  static const void *G00625[] = {I(aa,23,00,00),B(double ,14),I(41,00,00,00),B(double ,12),I(45,02,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 16 binding: (method-binary-lcm) */
  static const void *G00628[] = {I(ab,41,00,00),B(double1 ,13),I(22,01,24,00),B(double ,7),I(3d,02,00,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 17 binding: (method-binary-lcm) */
  static const void *G00630[] = {I(ab,1c,41,00),B(double1 ,13),I(22,01,1b,1d),I(24,00,00,00),B(double ,7),I(3d,02,03,45),I(03,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 18 binding: (method-binary-lcm) */
  static const void *G00632[] = {I(ab,24,00,00),B(double ,7),I(3d,02,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 19 binding: (method-binary-gcd) */
  static const void *G00634[] = {I(ab,41,00,00),B(double1 ,13),I(22,01,24,00),B(double ,2),I(3d,02,00,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 20 binding: (method-binary-gcd) */
  static const void *G00636[] = {I(ab,1c,41,00),B(double1 ,13),I(22,01,1b,1d),I(24,00,00,00),B(double ,2),I(3d,02,03,45),I(03,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 21 binding: (method-binary-gcd) */
  static const void *G00638[] = {I(ab,24,00,00),B(double ,2),I(3d,02,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 22 binding: (method-truncate) */
  static const void *G00640[] = {I(aa,41,00,00),B(double1 ,11),I(45,01,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 23 binding: (method-round) */
  static const void *G00642[] = {I(aa,41,00,00),B(double1 ,10),I(45,01,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 24 binding: (method-floor) */
  static const void *G00644[] = {I(aa,41,00,00),B(double1 ,9),I(45,01,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 25 binding: (method-ceiling) */
  static const void *G00646[] = {I(aa,41,00,00),B(double1 ,8),I(45,01,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 26 binding: (method-binary<) */
  static const void *G00648[] = {I(ab,41,00,00),B(double1 ,13),I(22,01,41,00),B(double ,13),I(45,02,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 27 binding: (method-binary<) */
  static const void *G00650[] = {I(ab,1c,41,00),B(double1 ,13),I(22,01,1b,1d),I(41,00,00,00),B(double ,13),I(45,05,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 28 binding: (method-binary<) */
  static const void *G00652[] = {I(ab,41,00,00),B(double ,13),I(45,02,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 29 binding: (method-binary=) */
  static const void *G00654[] = {I(ab,41,00,00),B(double1 ,13),I(22,01,41,00),B(double ,12),I(45,02,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 30 binding: (method-binary=) */
  static const void *G00656[] = {I(ab,1c,41,00),B(double1 ,13),I(22,01,1b,1d),I(41,00,00,00),B(double ,12),I(45,05,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 31 binding: (method-binary=) */
  static const void *G00658[] = {I(ab,41,00,00),B(double ,12),I(45,02,00,00)};

  /* Byte-vector with size: 518 is_init: 0 index: 41 binding: top-level */
  static const void *G00660[] = {I(a9,41,00,00),B(double ,8),I(1b,89,00,00),B(double ,6),I(2a,41,00,00),B(double ,10),I(1b,89,00,00),B(double ,4),I(2a,41,00,00),B(double ,9),I(1b,89,00,00),B(double ,3),I(2a,41,00,00),B(double ,11),I(1b,89,00,00),B(double ,5),I(2a,24,00,00),B(compare ,9),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(float ,8),I(24,00,00,00),B(float ,8),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(compare ,9),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,32),I(23,00,00,00),B(double ,31),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(compare ,9),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(compare ,9),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(fpi ,6),I(24,00,00,00),B(float ,8),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(compare ,9),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,32),I(23,00,00,00),B(double ,30),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(compare ,9),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(compare ,9),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(float ,8),I(24,00,00,00),B(fpi ,6),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(compare ,9),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,32),I(23,00,00,00),B(double ,29),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(compare ,9),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(compare ,6),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(float ,8),I(24,00,00,00),B(float ,8),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(compare ,6),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,33),I(23,00,00,00),B(double ,28),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(compare ,6),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(compare ,6),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(fpi ,6),I(24,00,00,00),B(float ,8),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(compare ,6),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,33),I(23,00,00,00),B(double ,27),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(compare ,6),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(compare ,6),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(float ,8),I(24,00,00,00),B(fpi ,6),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(compare ,6),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,33),I(23,00,00,00),B(double ,26),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(compare ,6),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(float ,9),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(float ,8),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(float ,9),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,34),I(23,00,00,00),B(double ,25),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(float ,9),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(float ,7),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(float ,8),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(float ,7),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,35),I(23,00,00,00),B(double ,24),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(float ,7),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(float ,5),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(float ,8),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(float ,5),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,36),I(23,00,00,00),B(double ,23),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(float ,5),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(float ,2),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(float ,8),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(float ,2),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,37),I(23,00,00,00),B(double ,22),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(float ,2),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(number ,25),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(float ,8),I(24,00,00,00),B(float ,8),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(number ,25),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,38),I(23,00,00,00),B(double ,21),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(number ,25),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(number ,25),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(fpi ,6),I(24,00,00,00),B(float ,8),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(number ,25),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,38),I(23,00,00,00),B(double ,20),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(number ,25),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(number ,25),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(float ,8),I(24,00,00,00),B(fpi ,6),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(number ,25),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,38),I(23,00,00,00),B(double ,19),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(number ,25),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(number ,19),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(float ,8),I(24,00,00,00),B(float ,8),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(number ,19),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,39),I(23,00,00,00),B(double ,18),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(number ,19),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(number ,19),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(fpi ,6),I(24,00,00,00),B(float ,8),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(number ,19),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,39),I(23,00,00,00),B(double ,17),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(number ,19),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(number ,19),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(float ,8),I(24,00,00,00),B(fpi ,6),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(number ,19),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,39),I(23,00,00,00),B(double ,16),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(number ,19),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(number ,16),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(float ,8),I(24,00,00,00),B(boot1 ,40),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(number ,16),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(double ,40),I(23,00,00,00),B(double ,15),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(number ,16),I(1c,24,00,00),B(mop_meth ,5),I(3d,02,6a,45),I(6a,00,00,00)};

  /* Byte-vector with size: 51 is_init: 0 index: 42 binding: double-binary-gcd */
  static const void *G00671[] = {I(ab,1c,1c,41),B(double ,13),I(22,02,1b,34),I(00,00,00,55),I(1c,1f,03,41),B(double1 ,5),I(22,02,1f,03),I(1c,41,00,00),B(double1 ,4),I(22,02,1b,1f),I(04,41,00,00),B(double ,12),I(22,02,1b,34),I(00,00,00,0d),I(1f,05,32,00),I(00,00,00,1f),I(1f,04,1d,41),B(double1 ,3),I(22,02,1b,1f),I(07,24,00,00),B(double ,2),I(3d,02,07,22),I(01,22,03,32),I(00,00,00,6d),I(1c,1f,03,41),B(double ,13),I(22,02,1b,34),I(00,00,00,59),I(1f,03,1f,03),I(41,00,00,00),B(double1 ,5),I(22,02,1f,03),I(1c,41,00,00),B(double1 ,4),I(22,02,1b,1f),I(06,41,00,00),B(double ,12),I(22,02,1b,34),I(00,00,00,0d),I(1f,05,32,00),I(00,00,00,1f),I(1f,06,1d,41),B(double1 ,3),I(22,02,1b,1f),I(07,24,00,00),B(double ,2),I(3d,02,08,22),I(01,22,03,32),I(00,00,00,07),I(1f,03,22,01),I(45,03,00,00)};

  /* Byte-vector with size: 18 is_init: 0 index: 43 binding: double-binary-lcm */
  static const void *G00673[] = {I(ab,1c,2d,1b),I(34,00,00,00),I(00,00,00,10),I(1b,32,00,00),I(00,00,00,09),I(1c,2d,1b,34),I(00,00,00,0d),I(82,32,00,00),I(00,00,00,29),I(1f,03,1f,03),I(41,00,00,00),B(double1 ,4),I(22,02,1f,04),I(1f,04,24,00),B(double ,2),I(3c,02,41,00),B(double1 ,5),I(22,02,45,04)};

  /* Byte-vector with size: 68 is_init: 1 index: 0 binding: initialize-double */
  static const void *G00675[] = {I(87,25,00,00),B(double ,1),I(24,00,00,00),B(double1 ,1),I(3e,0b,24,00),B(double1 ,0),I(3c,00,21,01),I(24,00,00,00),B(float ,1),I(3e,0b,24,00),B(float ,0),I(3c,00,21,01),I(24,00,00,00),B(string ,1),I(3e,0b,24,00),B(string ,0),I(3c,00,21,01),I(24,00,00,00),B(fpi ,1),I(3e,0b,24,00),B(fpi ,0),I(3c,00,21,01),I(24,00,00,00),B(integer ,1),I(3e,0b,24,00),B(integer ,0),I(3c,00,21,01),I(24,00,00,00),B(number ,1),I(3e,0b,24,00),B(number ,0),I(3c,00,21,01),I(24,00,00,00),B(compare ,1),I(3e,0b,24,00),B(compare ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(23,00,00,00),B(double ,44),I(23,00,00,00),B(double ,43),I(3b,02,25,00),B(double ,7),I(86,25,00,00),B(double ,6),I(86,25,00,00),B(double ,5),I(86,25,00,00),B(double ,4),I(86,25,00,00),B(double ,3),I(23,00,00,00),B(double ,45),I(23,00,00,00),B(double ,42),I(3b,02,25,00),B(double ,2),I(23,00,00,00),B(double ,46),I(23,00,00,00),B(double ,41),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_double(dbl_627,0.000000);
  eul_allocate_bytevector( G00626,G00625);
  eul_allocate_bytevector( G00629,G00628);
  eul_allocate_bytevector( G00631,G00630);
  eul_allocate_bytevector( G00633,G00632);
  eul_allocate_bytevector( G00635,G00634);
  eul_allocate_bytevector( G00637,G00636);
  eul_allocate_bytevector( G00639,G00638);
  eul_allocate_bytevector( G00641,G00640);
  eul_allocate_bytevector( G00643,G00642);
  eul_allocate_bytevector( G00645,G00644);
  eul_allocate_bytevector( G00647,G00646);
  eul_allocate_bytevector( G00649,G00648);
  eul_allocate_bytevector( G00651,G00650);
  eul_allocate_bytevector( G00653,G00652);
  eul_allocate_bytevector( G00655,G00654);
  eul_allocate_bytevector( G00657,G00656);
  eul_allocate_bytevector( G00659,G00658);
  eul_intern_symbol(sym_662,"(method binary=)");
  eul_intern_symbol(sym_663,"(method binary<)");
  eul_intern_symbol(sym_664,"(method ceiling)");
  eul_intern_symbol(sym_665,"(method floor)");
  eul_intern_symbol(sym_666,"(method round)");
  eul_intern_symbol(sym_667,"(method truncate)");
  eul_intern_symbol(sym_668,"(method binary-gcd)");
  eul_intern_symbol(sym_669,"(method binary-lcm)");
  eul_intern_symbol(sym_670,"(method zero?)");
  eul_allocate_bytevector( G00661,G00660);
  eul_allocate_bytevector( G00672,G00671);
  eul_allocate_bytevector( G00674,G00673);
  eul_intern_symbol(sym_677,"double-binary-lcm");
  eul_intern_symbol(sym_678,"double-binary-gcd");
  eul_intern_symbol(sym_679,"top-level");
  eul_allocate_bytevector( G00676,G00675);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 8; i++)
      double_bindings[i] = eul_nil;
  }

  double_bindings[ 8] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_get_dbl_max619;
  double_bindings[ 9] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_get_dbl_min620;
  double_bindings[ 10] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_get_dbl_epsilon621;
  double_bindings[ 11] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_get_neg_dbl_epsilon622;
  double_bindings[ 12] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_dbl_equal623;
  double_bindings[ 13] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_dbl_less624;
  double_bindings[ 14] = dbl_627;
  double_bindings[ 15] = G00626;
  double_bindings[ 16] = G00629;
  double_bindings[ 17] = G00631;
  double_bindings[ 18] = G00633;
  double_bindings[ 19] = G00635;
  double_bindings[ 20] = G00637;
  double_bindings[ 21] = G00639;
  double_bindings[ 22] = G00641;
  double_bindings[ 23] = G00643;
  double_bindings[ 24] = G00645;
  double_bindings[ 25] = G00647;
  double_bindings[ 26] = G00649;
  double_bindings[ 27] = G00651;
  double_bindings[ 28] = G00653;
  double_bindings[ 29] = G00655;
  double_bindings[ 30] = G00657;
  double_bindings[ 31] = G00659;
  double_bindings[ 32] = sym_662;
  double_bindings[ 33] = sym_663;
  double_bindings[ 34] = sym_664;
  double_bindings[ 35] = sym_665;
  double_bindings[ 36] = sym_666;
  double_bindings[ 37] = sym_667;
  double_bindings[ 38] = sym_668;
  double_bindings[ 39] = sym_669;
  double_bindings[ 40] = sym_670;
  double_bindings[ 41] = G00661;
  double_bindings[ 42] = G00672;
  double_bindings[ 43] = G00674;
  double_bindings[ 1] = eul_nil;
  double_bindings[ 44] = sym_677;
  double_bindings[ 45] = sym_678;
  double_bindings[ 46] = sym_679;
  eul_allocate_lambda( double_bindings[0], "initialize-double", 0, G00676);

  }
}


/* eof */