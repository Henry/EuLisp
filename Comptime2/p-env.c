/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module p-env
 **  Copyright: See file p-env.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_i_all();
extern void initialize_module_sx_obj();
extern LispRef i_all_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef sx_obj2_bindings[];
extern LispRef i_error_bindings[];
extern LispRef boot1_bindings[];
extern LispRef i_notify_bindings[];
extern LispRef sx_obj_bindings[];
extern LispRef dynamic_bindings[];
extern LispRef boot_bindings[];
extern LispRef sx_obj1_bindings[];
extern LispRef aux_table_bindings[];
extern LispRef i_param_bindings[];

/* Module bindings with size 77 */
LispRef p_env_bindings[77];

/* Foreign functions */
static LispRef ff_stub_eul_dyn_level1_syntax_binding_info2669 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G002750, res;

  POPVAL1(G002750);
  FF_RES_CONVERT6(res,eul_dyn_level1_syntax_binding_info(FF_ARG_CONVERT8(G002750)));
  return res;
}


/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module p-env */
void initialize_module_p_env()
{
  if (is_initialized) return;
  initialize_module_i_all();
  initialize_module_sx_obj();
  eul_fast_table_set(eul_modules,"p_env",(LispRef) p_env_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_2749, sym_2748, sym_2747, sym_2746, sym_2745, sym_2744, sym_2743, sym_2742, sym_2741, sym_2740, sym_2739, sym_2738, sym_2737, sym_2736, sym_2735, sym_2734, sym_2733, sym_2732, G002731, G002729, G002727, G002725, G002723, G002721, G002716, G002714, G002712, G002710, G002706, G002704, G002702, G002699, key_2697, key_2696, key_2695, key_2694, key_2693, G002692, G002690, G002688, G002686, sym_2681, G002680, G002678, sym_2676, G002675, G002673, G002671;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 5 is_init: 0 index: 21 binding: loaded-syntax-modules */
  static const void *G002670[] = {I(a9,24,00,00),B(i_param ,31),I(24,00,00,00),B(aux_table ,8),I(3d,01,00,00)};

  /* Byte-vector with size: 6 is_init: 0 index: 22 binding: anonymous */
  static const void *G002672[] = {I(aa,1b,26,00),I(00,00,00,06),I(24,00,00,00),B(sx_obj1 ,43),I(08,1b,1d,0f),I(45,02,00,00)};

  /* Byte-vector with size: 11 is_init: 0 index: 24 binding: add-local-static-bindings */
  static const void *G002674[] = {I(ab,23,00,00),B(p_env ,23),I(23,00,00,00),B(p_env ,22),I(3b,01,1d,24),B(boot ,10),I(3c,02,1b,1d),I(24,00,00,00),B(boot ,7),I(3d,02,03,45),I(03,00,00,00)};

  /* Byte-vector with size: 10 is_init: 0 index: 25 binding: find-module */
  static const void *G002677[] = {I(aa,1b,24,00),B(p_env ,14),I(3c,01,1b,34),I(00,00,00,0d),I(1b,32,00,00),I(00,00,00,12),I(1c,24,00,00),B(i_param ,19),I(3d,01,02,45),I(02,00,00,00)};

  eul_allocate_static_string(str_2682, "new lexical binding ~a shadows lexical binding ~a", 49);
  eul_allocate_static_string(str_2683, "new lexical binding ~a is shadowed by syntax binding ~a", 55);
  eul_allocate_static_string(str_2684, "no actual module", 16);
  /* Byte-vector with size: 114 is_init: 0 index: 30 binding: set-lexical-binding */
  static const void *G002679[] = {I(43,fe,23,00),B(p_env ,26),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,34),I(00,00,01,95),I(1b,26,00,00),I(00,00,00,10),I(24,00,00,00),B(sx_obj1 ,59),I(08,1f,03,1f),I(03,1d,24,00),B(p_env ,8),I(3c,03,1b,26),I(00,00,00,06),I(24,00,00,00),B(sx_obj1 ,43),I(08,1c,24,00),B(sx_obj ,17),I(3c,01,1c,24),B(p_env ,7),I(3c,01,1d,24),B(p_env ,10),I(3c,01,1c,24),B(sx_obj1 ,30),I(3c,01,1b,34),I(00,00,00,89),I(1d,24,00,00),B(sx_obj ,17),I(3c,01,1f,03),I(83,24,00,00),B(sx_obj1 ,43),I(08,24,00,00),B(i_param ,33),I(1b,34,00,00),I(00,00,00,0f),I(1b,32,00,00),I(00,00,00,0b),I(1d,1f,07,50),I(1b,34,00,00),I(00,00,00,33),I(1f,09,1f,04),I(1c,26,00,00),I(00,00,00,05),I(1d,24,00,00),B(sx_obj1 ,43),I(09,22,02,2a),I(1f,09,1f,03),I(1c,83,1d,24),B(sx_obj1 ,43),I(09,22,02,32),I(00,00,00,1b),I(86,23,00,00),B(p_env ,27),I(1f,0b,1f,09),I(24,00,00,00),B(i_notify ,5),I(3c,04,22,04),I(32,00,00,00),I(00,00,00,63),I(1c,24,00,00),B(sx_obj1 ,30),I(3c,01,1b,34),I(00,00,00,29),I(1f,03,1f,03),I(50,12,1b,34),I(00,00,00,15),I(24,00,00,00),B(i_param ,10),I(12,32,00,00),I(00,00,00,08),I(86,22,01,32),I(00,00,00,06),I(86,1b,34,00),I(00,00,00,22),I(86,23,00,00),B(p_env ,28),I(1f,09,1f,06),I(24,00,00,00),B(i_notify ,5),I(3c,04,32,00),I(00,00,00,07),I(86,22,02,2a),I(1f,07,26,00),I(00,00,00,10),I(24,00,00,00),B(sx_obj1 ,59),I(08,1b,24,00),B(boot1 ,41),I(3c,01,1f,06),I(1f,08,1d,3c),I(02,2a,24,00),B(i_param ,10),I(12,1b,34,00),I(00,00,00,0e),I(86,32,00,00),I(00,00,00,23),I(1f,0a,82,24),B(sx_obj1 ,59),I(08,1b,24,00),B(boot1 ,41),I(3c,01,1f,09),I(1f,0b,1d,3d),I(02,0f,22,02),I(22,0a,32,00),I(00,00,00,21),I(23,00,00,00),B(p_env ,29),I(24,00,00,00),B(i_error ,5),I(24,00,00,00),B(boot ,12),I(3d,02,03,45),I(03,00,00,00)};

  /* Byte-vector with size: 5 is_init: 0 index: 31 binding: loaded-modules */
  static const void *G002685[] = {I(a9,24,00,00),B(i_param ,20),I(24,00,00,00),B(aux_table ,8),I(3d,01,00,00)};

  /* Byte-vector with size: 15 is_init: 0 index: 32 binding: anonymous */
  static const void *G002687[] = {I(ab,26,00,00),I(00,00,00,10),I(24,00,00,00),B(sx_obj1 ,59),I(08,1c,1c,3c),I(01,1b,34,00),I(00,00,00,22),I(1b,1f,03,1f),I(03,24,00,00),B(p_env ,8),I(3c,03,1b,48),I(00,00,47,00),I(00,22,01,32),I(00,00,00,06),I(86,45,03,00)};

  /* Byte-vector with size: 49 is_init: 0 index: 33 binding: get-lexical-binding */
  static const void *G002689[] = {I(43,fe,46,01),I(1b,1b,34,00),I(00,00,00,0e),I(1b,32,00,00),I(00,00,00,42),I(23,00,00,00),B(p_env ,26),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,34),I(00,00,00,25),I(23,00,00,00),B(p_env ,26),I(24,00,00,00),B(dynamic ,3),I(3c,01,24,00),B(boot1 ,25),I(3c,01,32,00),I(00,00,00,07),I(86,22,01,86),I(1b,48,00,00),I(1f,04,1d,23),B(p_env ,23),I(23,00,00,00),B(p_env ,32),I(3b,02,24,00),B(boot ,24),I(3c,03,2a,47),I(00,00,34,00),I(00,00,00,4a),I(47,00,00,84),I(24,00,00,00),B(sx_obj1 ,43),I(08,1b,24,00),B(sx_obj2 ,10),I(3c,01,1b,34),I(00,00,00,25),I(1c,82,24,00),B(sx_obj2 ,11),I(08,1b,83,14),I(1f,03,1c,1c),I(82,1d,24,00),B(sx_obj2 ,11),I(09,22,04,32),I(00,00,00,06),I(86,22,02,32),I(00,00,00,06),I(86,2a,47,00),I(00,45,05,00)};

  /* Byte-vector with size: 36 is_init: 0 index: 39 binding: expand-hard-coded-level1-binding */
  static const void *G002691[] = {I(43,03,1d,7a),I(1b,34,00,00),I(00,00,00,87),I(1f,03,10,1f),I(04,73,1f,05),I(75,1f,05,7a),I(1b,34,00,00),I(00,00,00,0f),I(1f,06,10,32),I(00,00,00,18),I(1f,06,1b,34),I(00,00,00,0d),I(1b,32,00,00),I(00,00,00,08),I(1d,22,01,24),B(sx_obj1 ,72),I(23,00,00,00),B(p_env ,34),I(1d,23,00,00),B(p_env ,35),I(1f,07,23,00),B(p_env ,36),I(1f,08,23,00),B(p_env ,37),I(87,23,00,00),B(p_env ,38),I(1f,0e,24,00),B(mop_gf ,2),I(3c,0b,1f,07),I(24,00,00,00),B(boot1 ,41),I(3c,01,1d,1d),I(1d,3c,02,2a),I(1c,22,07,32),I(00,00,00,07),I(1f,03,45,04)};

  eul_allocate_static_string(str_2700, "reloading of syntax module ~a", 29);
  /* Byte-vector with size: 18 is_init: 0 index: 41 binding: new-syntax-module */
  static const void *G002698[] = {I(ab,1c,24,00),B(p_env ,17),I(3c,01,1b,34),I(00,00,00,1d),I(86,23,00,00),B(p_env ,40),I(1f,04,24,00),B(i_notify ,5),I(3d,03,03,32),I(00,00,00,21),I(24,00,00,00),B(i_param ,31),I(24,00,00,00),B(boot1 ,41),I(3c,01,1f,03),I(1f,03,1d,3d),I(02,04,22,01),I(45,03,00,00)};

  /* Byte-vector with size: 18 is_init: 0 index: 42 binding: anonymous */
  static const void *G002701[] = {I(ab,1b,26,00),I(00,00,00,0e),I(24,00,00,00),B(sx_obj1 ,59),I(08,1d,1c,3c),I(01,1b,34,00),I(00,00,00,2e),I(1b,1f,04,1f),I(03,24,00,00),B(p_env ,8),I(3c,03,1b,1f),I(04,24,00,00),B(p_env ,12),I(3c,02,1b,48),I(00,00,47,00),I(00,22,02,32),I(00,00,00,06),I(86,45,04,00)};

  /* Byte-vector with size: 29 is_init: 0 index: 43 binding: get-syntax-binding */
  static const void *G002703[] = {I(43,fe,46,01),I(1b,1b,34,00),I(00,00,00,0e),I(1b,32,00,00),I(00,00,00,42),I(23,00,00,00),B(p_env ,26),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,34),I(00,00,00,25),I(23,00,00,00),B(p_env ,26),I(24,00,00,00),B(dynamic ,3),I(3c,01,24,00),B(boot1 ,25),I(3c,01,32,00),I(00,00,00,07),I(86,22,01,86),I(1b,48,00,00),I(1f,04,1d,23),B(p_env ,23),I(23,00,00,00),B(p_env ,42),I(3b,02,24,00),B(boot ,24),I(3c,03,2a,47),I(00,00,45,05)};

  eul_allocate_static_string(str_2707, "conflicting exported bindings ~a and ~a", 39);
  eul_allocate_static_string(str_2708, "no actual module specified", 26);
  /* Byte-vector with size: 72 is_init: 0 index: 46 binding: set-external-binding */
  static const void *G002705[] = {I(43,fe,23,00),B(p_env ,26),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,34),I(00,00,00,ed),I(1b,26,00,00),I(00,00,00,0f),I(24,00,00,00),B(sx_obj1 ,59),I(08,1f,03,1f),I(03,1d,24,00),B(p_env ,8),I(3c,03,1b,26),I(00,00,00,06),I(24,00,00,00),B(sx_obj1 ,43),I(08,1c,24,00),B(sx_obj ,17),I(3c,01,1c,24),B(p_env ,19),I(3c,01,1b,24),B(sx_obj1 ,30),I(3c,01,1b,34),I(00,00,00,71),I(1c,24,00,00),B(sx_obj ,17),I(3c,01,1d,83),I(24,00,00,00),B(sx_obj1 ,43),I(08,1c,1f,05),I(50,12,1b,34),I(00,00,00,0d),I(1b,32,00,00),I(00,00,00,1e),I(1f,09,26,00),I(00,00,00,15),I(24,00,00,00),B(sx_obj1 ,59),I(08,1f,03,1c),I(50,22,01,1b),I(34,00,00,00),I(00,00,00,24),I(86,23,00,00),B(p_env ,44),I(1f,0a,1f,08),I(24,00,00,00),B(i_notify ,5),I(3c,04,32,00),I(00,00,00,07),I(86,22,04,32),I(00,00,00,06),I(86,2a,1f,06),I(26,00,00,00),I(00,00,00,0f),I(24,00,00,00),B(sx_obj1 ,59),I(08,1b,24,00),B(boot1 ,41),I(3c,01,1f,05),I(1f,07,1d,3d),I(02,0b,22,08),I(32,00,00,00),I(00,00,00,23),I(23,00,00,00),B(p_env ,45),I(24,00,00,00),B(i_error ,5),I(24,00,00,00),B(boot ,12),I(3d,02,03,45),I(03,00,00,00)};

  /* Byte-vector with size: 17 is_init: 0 index: 47 binding: expand-interpreter-defined-syntax-binding */
  static const void *G002709[] = {I(ab,24,00,00),B(i_param ,10),I(34,00,00,00),I(00,00,00,10),I(1c,7c,32,00),I(00,00,00,07),I(86,1b,34,00),I(00,00,00,26),I(1c,26,00,00),I(00,00,00,10),I(24,00,00,00),B(sx_obj1 ,59),I(08,1f,03,1c),I(3d,01,04,22),I(01,32,00,00),I(00,00,00,08),I(1d,45,03,00)};

  /* Byte-vector with size: 16 is_init: 0 index: 48 binding: find-syntax-module */
  static const void *G002711[] = {I(aa,1b,24,00),B(p_env ,14),I(3c,01,1b,34),I(00,00,00,0d),I(1b,32,00,00),I(00,00,00,2c),I(1c,24,00,00),B(p_env ,17),I(3c,01,1b,34),I(00,00,00,0d),I(1b,32,00,00),I(00,00,00,12),I(1d,24,00,00),B(i_param ,5),I(3d,01,03,22),I(01,45,02,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 49 binding: get-module */
  static const void *G002713[] = {I(aa,24,00,00),B(i_param ,20),I(3d,01,00,00)};

  eul_allocate_static_string(str_2717, "new syntax binding ~a shadows syntax binding ~a", 47);
  eul_allocate_static_string(str_2718, "new syntax binding ~a shadows lexical binding ~a", 48);
  eul_allocate_static_string(str_2719, "no actual module specified", 26);
  /* Byte-vector with size: 99 is_init: 0 index: 53 binding: set-syntax-binding */
  static const void *G002715[] = {I(43,fe,23,00),B(p_env ,26),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,34),I(00,00,01,59),I(1b,26,00,00),I(00,00,00,0e),I(24,00,00,00),B(sx_obj1 ,59),I(08,1f,03,1f),I(03,1d,24,00),B(p_env ,8),I(3c,03,1b,1f),I(03,24,00,00),B(p_env ,12),I(3c,02,1b,24),B(sx_obj ,17),I(3c,01,1c,26),I(00,00,00,06),I(24,00,00,00),B(sx_obj1 ,43),I(08,1b,24,00),B(p_env ,7),I(3c,01,1c,24),B(p_env ,10),I(3c,01,1b,24),B(sx_obj1 ,30),I(3c,01,1b,34),I(00,00,00,89),I(1c,24,00,00),B(sx_obj ,17),I(3c,01,1d,83),I(24,00,00,00),B(sx_obj1 ,43),I(08,24,00,00),B(i_param ,33),I(1b,34,00,00),I(00,00,00,0f),I(1b,32,00,00),I(00,00,00,0b),I(1d,1f,08,50),I(1b,34,00,00),I(00,00,00,33),I(1f,09,1f,04),I(1c,26,00,00),I(00,00,00,05),I(1d,24,00,00),B(sx_obj1 ,43),I(09,22,02,2a),I(1f,09,1f,03),I(1c,83,1d,24),B(sx_obj1 ,43),I(09,22,02,32),I(00,00,00,1b),I(86,23,00,00),B(p_env ,50),I(1f,0b,1f,08),I(24,00,00,00),B(i_notify ,5),I(3c,04,22,04),I(32,00,00,00),I(00,00,00,4b),I(1d,24,00,00),B(sx_obj1 ,30),I(3c,01,1b,34),I(00,00,00,11),I(1f,03,1f,0c),I(50,12,32,00),I(00,00,00,07),I(86,1b,34,00),I(00,00,00,22),I(86,23,00,00),B(p_env ,51),I(1f,09,1f,07),I(24,00,00,00),B(i_notify ,5),I(3c,04,32,00),I(00,00,00,07),I(86,22,02,2a),I(1f,08,26,00),I(00,00,00,0e),I(24,00,00,00),B(sx_obj1 ,59),I(08,1b,24,00),B(boot1 ,41),I(3c,01,1f,05),I(1f,08,1d,3d),I(02,0d,22,0a),I(32,00,00,00),I(00,00,00,23),I(23,00,00,00),B(p_env ,52),I(24,00,00,00),B(i_error ,5),I(24,00,00,00),B(boot ,12),I(3d,02,03,45),I(03,00,00,00)};

  /* Byte-vector with size: 7 is_init: 0 index: 54 binding: new-module */
  static const void *G002720[] = {I(ab,24,00,00),B(i_param ,20),I(24,00,00,00),B(boot1 ,41),I(3c,01,1d,1d),I(1d,3d,02,03),I(45,03,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 55 binding: get-syntax-module */
  static const void *G002722[] = {I(aa,24,00,00),B(i_param ,31),I(3d,01,00,00)};

  /* Byte-vector with size: 23 is_init: 0 index: 56 binding: get-local-static-binding */
  static const void *G002724[] = {I(ab,1b,1d,24),B(boot ,20),I(3c,02,1b,34),I(00,00,00,4d),I(1b,11,1b,84),I(24,00,00,00),B(sx_obj1 ,43),I(08,1b,24,00),B(sx_obj2 ,10),I(3c,01,1b,34),I(00,00,00,25),I(1c,82,24,00),B(sx_obj2 ,11),I(08,1b,83,14),I(1f,03,1c,1c),I(82,1d,24,00),B(sx_obj2 ,11),I(09,22,04,32),I(00,00,00,06),I(86,2a,1d,22),I(03,32,00,00),I(00,00,00,08),I(86,45,03,00)};

  /* Byte-vector with size: 15 is_init: 0 index: 57 binding: anonymous */
  static const void *G002726[] = {I(ab,26,00,00),I(00,00,00,0f),I(24,00,00,00),B(sx_obj1 ,59),I(08,1c,1c,3c),I(01,1b,34,00),I(00,00,00,22),I(1b,1f,03,1f),I(03,24,00,00),B(p_env ,8),I(3c,03,1b,48),I(00,00,47,00),I(00,22,01,32),I(00,00,00,06),I(86,45,03,00)};

  /* Byte-vector with size: 29 is_init: 0 index: 58 binding: get-external-binding */
  static const void *G002728[] = {I(43,fe,46,01),I(1b,1b,34,00),I(00,00,00,0e),I(1b,32,00,00),I(00,00,00,42),I(23,00,00,00),B(p_env ,26),I(24,00,00,00),B(dynamic ,3),I(3c,01,1b,34),I(00,00,00,25),I(23,00,00,00),B(p_env ,26),I(24,00,00,00),B(dynamic ,3),I(3c,01,24,00),B(boot1 ,25),I(3c,01,32,00),I(00,00,00,07),I(86,22,01,86),I(1b,48,00,00),I(1f,04,1d,23),B(p_env ,23),I(23,00,00,00),B(p_env ,57),I(3b,02,24,00),B(boot ,24),I(3c,03,2a,47),I(00,00,45,05)};

  /* Byte-vector with size: 121 is_init: 1 index: 0 binding: initialize-p-env */
  static const void *G002730[] = {I(87,25,00,00),B(p_env ,1),I(24,00,00,00),B(sx_obj ,1),I(3e,0b,24,00),B(sx_obj ,0),I(3c,00,21,01),I(24,00,00,00),B(i_all ,1),I(3e,0b,24,00),B(i_all ,0),I(3c,00,21,01),I(23,00,00,00),B(p_env ,59),I(23,00,00,00),B(p_env ,58),I(3b,fe,25,00),B(p_env ,19),I(23,00,00,00),B(p_env ,60),I(23,00,00,00),B(p_env ,56),I(3b,02,25,00),B(p_env ,18),I(23,00,00,00),B(p_env ,61),I(23,00,00,00),B(p_env ,55),I(3b,01,25,00),B(p_env ,17),I(23,00,00,00),B(p_env ,62),I(23,00,00,00),B(p_env ,54),I(3b,02,25,00),B(p_env ,16),I(23,00,00,00),B(p_env ,63),I(23,00,00,00),B(p_env ,53),I(3b,fe,25,00),B(p_env ,15),I(23,00,00,00),B(p_env ,64),I(23,00,00,00),B(p_env ,49),I(3b,01,25,00),B(p_env ,14),I(23,00,00,00),B(p_env ,65),I(23,00,00,00),B(p_env ,48),I(3b,01,25,00),B(p_env ,13),I(23,00,00,00),B(p_env ,66),I(23,00,00,00),B(p_env ,47),I(3b,02,25,00),B(p_env ,12),I(23,00,00,00),B(p_env ,67),I(23,00,00,00),B(p_env ,46),I(3b,fe,25,00),B(p_env ,11),I(23,00,00,00),B(p_env ,68),I(23,00,00,00),B(p_env ,43),I(3b,fe,25,00),B(p_env ,10),I(23,00,00,00),B(p_env ,69),I(23,00,00,00),B(p_env ,41),I(3b,02,25,00),B(p_env ,9),I(23,00,00,00),B(p_env ,70),I(23,00,00,00),B(p_env ,39),I(3b,03,25,00),B(p_env ,8),I(23,00,00,00),B(p_env ,71),I(23,00,00,00),B(p_env ,33),I(3b,fe,25,00),B(p_env ,7),I(23,00,00,00),B(p_env ,72),I(23,00,00,00),B(p_env ,31),I(3b,00,25,00),B(p_env ,6),I(23,00,00,00),B(p_env ,73),I(23,00,00,00),B(p_env ,30),I(3b,fe,25,00),B(p_env ,5),I(23,00,00,00),B(p_env ,74),I(23,00,00,00),B(p_env ,25),I(3b,01,25,00),B(p_env ,4),I(23,00,00,00),B(p_env ,75),I(23,00,00,00),B(p_env ,24),I(3b,02,25,00),B(p_env ,3),I(23,00,00,00),B(p_env ,76),I(23,00,00,00),B(p_env ,21),I(3b,00,25,00),B(p_env ,2),I(86,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G002671,G002670);
  eul_allocate_bytevector( G002673,G002672);
  eul_intern_symbol(sym_2676,"anonymous");
  eul_allocate_bytevector( G002675,G002674);
  eul_allocate_bytevector( G002678,G002677);
  eul_intern_symbol(sym_2681,"*actual-module*");
  object_class(str_2682) = eul_static_string_class;
  object_class(str_2683) = eul_static_string_class;
  object_class(str_2684) = eul_static_string_class;
  eul_allocate_bytevector( G002680,G002679);
  eul_allocate_bytevector( G002686,G002685);
  eul_allocate_bytevector( G002688,G002687);
  eul_allocate_bytevector( G002690,G002689);
  eul_intern_keyword(key_2693,"local-name");
  eul_intern_keyword(key_2694,"module");
  eul_intern_keyword(key_2695,"obj");
  eul_intern_keyword(key_2696,"imported");
  eul_intern_keyword(key_2697,"local-index");
  eul_allocate_bytevector( G002692,G002691);
  object_class(str_2700) = eul_static_string_class;
  eul_allocate_bytevector( G002699,G002698);
  eul_allocate_bytevector( G002702,G002701);
  eul_allocate_bytevector( G002704,G002703);
  object_class(str_2707) = eul_static_string_class;
  object_class(str_2708) = eul_static_string_class;
  eul_allocate_bytevector( G002706,G002705);
  eul_allocate_bytevector( G002710,G002709);
  eul_allocate_bytevector( G002712,G002711);
  eul_allocate_bytevector( G002714,G002713);
  object_class(str_2717) = eul_static_string_class;
  object_class(str_2718) = eul_static_string_class;
  object_class(str_2719) = eul_static_string_class;
  eul_allocate_bytevector( G002716,G002715);
  eul_allocate_bytevector( G002721,G002720);
  eul_allocate_bytevector( G002723,G002722);
  eul_allocate_bytevector( G002725,G002724);
  eul_allocate_bytevector( G002727,G002726);
  eul_allocate_bytevector( G002729,G002728);
  eul_intern_symbol(sym_2732,"get-external-binding");
  eul_intern_symbol(sym_2733,"get-local-static-binding");
  eul_intern_symbol(sym_2734,"get-syntax-module");
  eul_intern_symbol(sym_2735,"new-module");
  eul_intern_symbol(sym_2736,"set-syntax-binding");
  eul_intern_symbol(sym_2737,"get-module");
  eul_intern_symbol(sym_2738,"find-syntax-module");
  eul_intern_symbol(sym_2739,"expand-interpreter-defined-syntax-binding");
  eul_intern_symbol(sym_2740,"set-external-binding");
  eul_intern_symbol(sym_2741,"get-syntax-binding");
  eul_intern_symbol(sym_2742,"new-syntax-module");
  eul_intern_symbol(sym_2743,"expand-hard-coded-level1-binding");
  eul_intern_symbol(sym_2744,"get-lexical-binding");
  eul_intern_symbol(sym_2745,"loaded-modules");
  eul_intern_symbol(sym_2746,"set-lexical-binding");
  eul_intern_symbol(sym_2747,"find-module");
  eul_intern_symbol(sym_2748,"add-local-static-bindings");
  eul_intern_symbol(sym_2749,"loaded-syntax-modules");
  eul_allocate_bytevector( G002731,G002730);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 20; i++)
      p_env_bindings[i] = eul_nil;
  }

  p_env_bindings[ 20] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_dyn_level1_syntax_binding_info2669;
  p_env_bindings[ 21] = G002671;
  p_env_bindings[ 22] = G002673;
  p_env_bindings[ 23] = sym_2676;
  p_env_bindings[ 24] = G002675;
  p_env_bindings[ 25] = G002678;
  p_env_bindings[ 26] = sym_2681;
  p_env_bindings[ 27] = str_2682;
  p_env_bindings[ 28] = str_2683;
  p_env_bindings[ 29] = str_2684;
  p_env_bindings[ 30] = G002680;
  p_env_bindings[ 31] = G002686;
  p_env_bindings[ 32] = G002688;
  p_env_bindings[ 33] = G002690;
  p_env_bindings[ 34] = key_2693;
  p_env_bindings[ 35] = key_2694;
  p_env_bindings[ 36] = key_2695;
  p_env_bindings[ 37] = key_2696;
  p_env_bindings[ 38] = key_2697;
  p_env_bindings[ 39] = G002692;
  p_env_bindings[ 40] = str_2700;
  p_env_bindings[ 41] = G002699;
  p_env_bindings[ 42] = G002702;
  p_env_bindings[ 43] = G002704;
  p_env_bindings[ 44] = str_2707;
  p_env_bindings[ 45] = str_2708;
  p_env_bindings[ 46] = G002706;
  p_env_bindings[ 47] = G002710;
  p_env_bindings[ 48] = G002712;
  p_env_bindings[ 49] = G002714;
  p_env_bindings[ 50] = str_2717;
  p_env_bindings[ 51] = str_2718;
  p_env_bindings[ 52] = str_2719;
  p_env_bindings[ 53] = G002716;
  p_env_bindings[ 54] = G002721;
  p_env_bindings[ 55] = G002723;
  p_env_bindings[ 56] = G002725;
  p_env_bindings[ 57] = G002727;
  p_env_bindings[ 58] = G002729;
  p_env_bindings[ 1] = eul_nil;
  p_env_bindings[ 59] = sym_2732;
  p_env_bindings[ 60] = sym_2733;
  p_env_bindings[ 61] = sym_2734;
  p_env_bindings[ 62] = sym_2735;
  p_env_bindings[ 63] = sym_2736;
  p_env_bindings[ 64] = sym_2737;
  p_env_bindings[ 65] = sym_2738;
  p_env_bindings[ 66] = sym_2739;
  p_env_bindings[ 67] = sym_2740;
  p_env_bindings[ 68] = sym_2741;
  p_env_bindings[ 69] = sym_2742;
  p_env_bindings[ 70] = sym_2743;
  p_env_bindings[ 71] = sym_2744;
  p_env_bindings[ 72] = sym_2745;
  p_env_bindings[ 73] = sym_2746;
  p_env_bindings[ 74] = sym_2747;
  p_env_bindings[ 75] = sym_2748;
  p_env_bindings[ 76] = sym_2749;
  eul_allocate_lambda( p_env_bindings[0], "initialize-p-env", 0, G002731);

  }
}


/* eof */
