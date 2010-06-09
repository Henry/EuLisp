/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module event
 **  Copyright: See file event.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern LispRef telos_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef boot1_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef boot_bindings[];

/* Module bindings with size 11 */
LispRef event_bindings[11];

/* Foreign functions */
static LispRef ff_stub_eul_ticks_per_second54 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef res;

  FF_RES_CONVERT2(res,eul_ticks_per_second());
  return res;
}


/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module event */
void initialize_module_event()
{
  if (is_initialized) return;
  initialize_module_telos();
  eul_fast_table_set(eul_modules,"event",(LispRef) event_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_64, G0063, sym_61, sym_60, G0059, G0056;

  /* Code vector and literal definitions */
  eul_allocate_static_string(str_57, "wait not yet implemented", 24);
  /* Byte-vector with size: 5 is_init: 0 index: 6 binding: (method-wait) */
  static const void *G0055[] = {I(ab,23,00,00),B(event ,5),I(24,00,00,00),B(boot ,12),I(3d,01,02,00)};

  /* Byte-vector with size: 59 is_init: 0 index: 9 binding: top-level */
  static const void *G0058[] = {I(a9,41,00,00),B(event ,4),I(1b,89,00,00),B(event ,3),I(2a,84,24,00),B(mop_class ,20),I(24,00,00,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,24,00),B(boot1 ,25),I(3c,00,23,00),B(event ,7),I(1f,03,24,00),B(mop_class ,15),I(24,00,00,00),B(mop_class ,54),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(event ,2),I(2a,24,00,00),B(event ,2),I(2a,24,00,00),B(event ,2),I(26,00,00,00),I(00,00,00,03),I(02,84,24,00),B(mop_class ,20),I(24,00,00,00),B(mop_class ,20),I(24,00,00,00),B(boot1 ,39),I(3c,03,24,00),B(boot1 ,25),I(3c,00,24,00),B(event ,2),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,7),I(3c,02,1f,04),I(1f,04,23,00),B(event ,8),I(23,00,00,00),B(event ,6),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(event ,2),I(1c,24,00,00),B(mop_meth ,5),I(3d,02,0b,45),I(0b,00,00,00)};

  /* Byte-vector with size: 17 is_init: 1 index: 0 binding: initialize-event */
  static const void *G0062[] = {I(87,25,00,00),B(event ,1),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(86,25,00,00),B(event ,3),I(86,25,00,00),B(event ,2),I(23,00,00,00),B(event ,10),I(23,00,00,00),B(event ,9),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  object_class(str_57) = eul_static_string_class;
  eul_allocate_bytevector( G0056,G0055);
  eul_intern_symbol(sym_60,"wait");
  eul_intern_symbol(sym_61,"(method wait)");
  eul_allocate_bytevector( G0059,G0058);
  eul_intern_symbol(sym_64,"top-level");
  eul_allocate_bytevector( G0063,G0062);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 4; i++)
      event_bindings[i] = eul_nil;
  }

  event_bindings[ 4] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_ticks_per_second54;
  event_bindings[ 5] = str_57;
  event_bindings[ 6] = G0056;
  event_bindings[ 7] = sym_60;
  event_bindings[ 8] = sym_61;
  event_bindings[ 9] = G0059;
  event_bindings[ 1] = eul_nil;
  event_bindings[ 10] = sym_64;
  eul_allocate_lambda( event_bindings[0], "initialize-event", 0, G0063);

  }
}


/* eof */
