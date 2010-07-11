/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module stream1
 **  Copyright: See file stream1.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern LispRef telos_bindings[];
extern LispRef boot1_bindings[];

/* Module bindings with size 41 */
LispRef stream1_bindings[41];

/* Foreign functions */
static LispRef ff_stub_eul_posix_codes4742 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef res;

  FF_RES_CONVERT6(res,eul_posix_codes());
  return res;
}

static LispRef ff_stub_open4743 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004774, G004775, G004776, res;

  POPVAL1(G004776);
  POPVAL1(G004775);
  POPVAL1(G004774);
  FF_RES_CONVERT0(res,open(FF_ARG_CONVERT3(G004774), FF_ARG_CONVERT0(G004775), FF_ARG_CONVERT0(G004776)));
  return res;
}

static LispRef ff_stub_close4744 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004777, res;

  POPVAL1(G004777);
  FF_RES_CONVERT0(res,close(FF_ARG_CONVERT0(G004777)));
  return res;
}

static LispRef ff_stub_read4745 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004778, G004779, G004780, res;

  POPVAL1(G004780);
  POPVAL1(G004779);
  POPVAL1(G004778);
  FF_RES_CONVERT0(res,read(FF_ARG_CONVERT0(G004778), FF_ARG_CONVERT3(G004779), FF_ARG_CONVERT0(G004780)));
  return res;
}

static LispRef ff_stub_write4746 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004781, G004782, G004783, res;

  POPVAL1(G004783);
  POPVAL1(G004782);
  POPVAL1(G004781);
  FF_RES_CONVERT0(res,write(FF_ARG_CONVERT0(G004781), FF_ARG_CONVERT3(G004782), FF_ARG_CONVERT0(G004783)));
  return res;
}

static LispRef ff_stub_eul_sprintf4747 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004784, G004785, G004786, G004787, res;

  POPVAL1(G004787);
  POPVAL1(G004786);
  POPVAL1(G004785);
  POPVAL1(G004784);
  FF_RES_CONVERT0(res,eul_sprintf(FF_ARG_CONVERT3(G004784), FF_ARG_CONVERT0(G004785), FF_ARG_CONVERT3(G004786), FF_ARG_CONVERT8(G004787)));
  return res;
}

static LispRef ff_stub_eul_sprintf_string4748 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004788, G004789, G004790, G004791, G004792, G004793, res;

  POPVAL1(G004793);
  POPVAL1(G004792);
  POPVAL1(G004791);
  POPVAL1(G004790);
  POPVAL1(G004789);
  POPVAL1(G004788);
  FF_RES_CONVERT0(res,eul_sprintf_string(FF_ARG_CONVERT3(G004788), FF_ARG_CONVERT0(G004789), FF_ARG_CONVERT0(G004790), FF_ARG_CONVERT0(G004791), FF_ARG_CONVERT3(G004792), FF_ARG_CONVERT3(G004793)));
  return res;
}

static LispRef ff_stub_eul_make_socket4749 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004794, G004795, G004796, res;

  POPVAL1(G004796);
  POPVAL1(G004795);
  POPVAL1(G004794);
  FF_RES_CONVERT0(res,eul_make_socket(FF_ARG_CONVERT3(G004794), FF_ARG_CONVERT3(G004795), FF_ARG_CONVERT0(G004796)));
  return res;
}

static LispRef ff_stub_eul_socket_accept4750 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004797, res;

  POPVAL1(G004797);
  FF_RES_CONVERT0(res,eul_socket_accept(FF_ARG_CONVERT0(G004797)));
  return res;
}

static LispRef ff_stub_eul_make_connection4751 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004798, G004799, G004800, res;

  POPVAL1(G004800);
  POPVAL1(G004799);
  POPVAL1(G004798);
  FF_RES_CONVERT0(res,eul_make_connection(FF_ARG_CONVERT3(G004798), FF_ARG_CONVERT3(G004799), FF_ARG_CONVERT3(G004800)));
  return res;
}

static LispRef ff_stub_eul_socket_strerror4752 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004801, res;

  POPVAL1(G004801);
  FF_RES_CONVERT3(res,eul_socket_strerror(FF_ARG_CONVERT0(G004801)));
  return res;
}

static LispRef ff_stub_eul_strerror4753 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef res;

  FF_RES_CONVERT3(res,eul_strerror());
  return res;
}

static LispRef ff_stub_ntok4754 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004802, G004803, res;

  POPVAL1(G004803);
  POPVAL1(G004802);
  FF_RES_CONVERT6(res,ntok(FF_ARG_CONVERT8(G004802), FF_ARG_CONVERT8(G004803)));
  return res;
}

static LispRef ff_stub_read_into_buffer4755 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004804, G004805, G004806, res;

  POPVAL1(G004806);
  POPVAL1(G004805);
  POPVAL1(G004804);
  FF_RES_CONVERT0(res,read_into_buffer(FF_ARG_CONVERT0(G004804), FF_ARG_CONVERT3(G004805), FF_ARG_CONVERT0(G004806)));
  return res;
}

static LispRef ff_stub_eul_hostname4756 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef res;

  FF_RES_CONVERT3(res,eul_hostname());
  return res;
}


/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module stream1 */
void initialize_module_stream1()
{
  if (is_initialized) return;
  initialize_module_telos();
  eul_fast_table_set(eul_modules,"stream1",(LispRef) stream1_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_4773, sym_4772, sym_4771, G004770, G004768, G004766, sym_4764, sym_4763, sym_4762, sym_4761, sym_4760, sym_4759, G004758;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 83 is_init: 0 index: 35 binding: top-level */
  static const void *G004757[] = {I(a9,41,00,00),B(stream1 ,14),I(1b,89,00,00),B(stream1 ,9),I(2a,24,00,00),B(stream1 ,9),I(82,02,1b,89),B(stream1 ,4),I(2a,24,00,00),B(stream1 ,9),I(83,02,1b,89),B(stream1 ,11),I(2a,24,00,00),B(stream1 ,9),I(84,02,1b,89),B(stream1 ,2),I(2a,24,00,00),B(stream1 ,9),I(26,00,00,00),I(00,00,00,03),I(02,1b,89,00),B(stream1 ,7),I(2a,24,00,00),B(stream1 ,9),I(26,00,00,00),I(00,00,00,04),I(02,1b,89,00),B(stream1 ,10),I(2a,24,00,00),B(stream1 ,9),I(26,00,00,00),I(00,00,00,05),I(02,1b,89,00),B(stream1 ,8),I(2a,24,00,00),B(stream1 ,9),I(26,00,00,00),I(00,00,00,06),I(02,1b,89,00),B(stream1 ,13),I(2a,24,00,00),B(stream1 ,9),I(26,00,00,00),I(00,00,00,07),I(02,1b,89,00),B(stream1 ,12),I(2a,24,00,00),B(stream1 ,8),I(24,00,00,00),B(stream1 ,13),I(14,24,00,00),B(stream1 ,11),I(1c,14,24,00),B(stream1 ,11),I(24,00,00,00),B(stream1 ,7),I(14,24,00,00),B(stream1 ,2),I(24,00,00,00),B(stream1 ,7),I(14,23,00,00),B(stream1 ,29),I(24,00,00,00),B(stream1 ,4),I(23,00,00,00),B(stream1 ,30),I(1f,05,23,00),B(stream1 ,31),I(1f,06,23,00),B(stream1 ,32),I(24,00,00,00),B(stream1 ,2),I(23,00,00,00),B(stream1 ,33),I(24,00,00,00),B(stream1 ,2),I(23,00,00,00),B(stream1 ,34),I(1f,0b,24,00),B(boot1 ,25),I(3c,0c,1b,89),B(stream1 ,5),I(45,0e,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 36 binding: hostname */
  static const void *G004765[] = {I(a9,41,00,00),B(stream1 ,28),I(45,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 37 binding: strerror */
  static const void *G004767[] = {I(a9,41,00,00),B(stream1 ,25),I(45,00,00,00)};

  /* Byte-vector with size: 45 is_init: 1 index: 0 binding: initialize-stream1 */
  static const void *G004769[] = {I(87,25,00,00),B(stream1 ,1),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(86,25,00,00),B(stream1 ,13),I(86,25,00,00),B(stream1 ,12),I(86,25,00,00),B(stream1 ,11),I(86,25,00,00),B(stream1 ,10),I(86,25,00,00),B(stream1 ,9),I(86,25,00,00),B(stream1 ,8),I(86,25,00,00),B(stream1 ,7),I(23,00,00,00),B(stream1 ,38),I(23,00,00,00),B(stream1 ,37),I(3b,00,25,00),B(stream1 ,6),I(86,25,00,00),B(stream1 ,5),I(86,25,00,00),B(stream1 ,4),I(23,00,00,00),B(stream1 ,39),I(23,00,00,00),B(stream1 ,36),I(3b,00,25,00),B(stream1 ,3),I(86,25,00,00),B(stream1 ,2),I(23,00,00,00),B(stream1 ,40),I(23,00,00,00),B(stream1 ,35),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_intern_symbol(sym_4759,"r");
  eul_intern_symbol(sym_4760,"w");
  eul_intern_symbol(sym_4761,"a");
  eul_intern_symbol(sym_4762,"r+");
  eul_intern_symbol(sym_4763,"w+");
  eul_intern_symbol(sym_4764,"a+");
  eul_allocate_bytevector( G004758,G004757);
  eul_allocate_bytevector( G004766,G004765);
  eul_allocate_bytevector( G004768,G004767);
  eul_intern_symbol(sym_4771,"strerror");
  eul_intern_symbol(sym_4772,"hostname");
  eul_intern_symbol(sym_4773,"top-level");
  eul_allocate_bytevector( G004770,G004769);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 14; i++)
      stream1_bindings[i] = eul_nil;
  }

  stream1_bindings[ 14] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_posix_codes4742;
  stream1_bindings[ 15] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_open4743;
  stream1_bindings[ 16] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_close4744;
  stream1_bindings[ 17] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_read4745;
  stream1_bindings[ 18] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_write4746;
  stream1_bindings[ 19] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_sprintf4747;
  stream1_bindings[ 20] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_sprintf_string4748;
  stream1_bindings[ 21] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_make_socket4749;
  stream1_bindings[ 22] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_socket_accept4750;
  stream1_bindings[ 23] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_make_connection4751;
  stream1_bindings[ 24] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_socket_strerror4752;
  stream1_bindings[ 25] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_strerror4753;
  stream1_bindings[ 26] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_ntok4754;
  stream1_bindings[ 27] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_read_into_buffer4755;
  stream1_bindings[ 28] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_hostname4756;
  stream1_bindings[ 29] = sym_4759;
  stream1_bindings[ 30] = sym_4760;
  stream1_bindings[ 31] = sym_4761;
  stream1_bindings[ 32] = sym_4762;
  stream1_bindings[ 33] = sym_4763;
  stream1_bindings[ 34] = sym_4764;
  stream1_bindings[ 35] = G004758;
  stream1_bindings[ 36] = G004766;
  stream1_bindings[ 37] = G004768;
  stream1_bindings[ 1] = eul_nil;
  stream1_bindings[ 38] = sym_4771;
  stream1_bindings[ 39] = sym_4772;
  stream1_bindings[ 40] = sym_4773;
  eul_allocate_lambda( stream1_bindings[0], "initialize-stream1", 0, G004770);

  }
}


/* eof */
