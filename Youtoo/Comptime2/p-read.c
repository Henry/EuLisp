/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module p-read
 **  Copyright: See file p-read.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_i_all();
extern void initialize_module_sx_obj();
extern LispRef sx_obj_bindings[];
extern LispRef i_all_bindings[];
extern LispRef read_bindings[];
extern LispRef i_notify_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef condition_bindings[];
extern LispRef format_bindings[];
extern LispRef boot_bindings[];
extern LispRef stream_bindings[];
extern LispRef string_bindings[];
extern LispRef i_param_bindings[];
extern LispRef dynamic_bindings[];
extern LispRef stream2_bindings[];

/* Module bindings with size 13 */
LispRef p_read_bindings[13];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module p-read */
void initialize_module_p_read()
{
  if (is_initialized) return;
  initialize_module_i_all();
  initialize_module_sx_obj();
  eul_fast_table_set(eul_modules,"p_read",(LispRef) p_read_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_624, G00623, sym_620, sym_619, key_618, sym_615, G00614, G00612;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 7 is_init: 0 index: 3 binding: anonymous */
  static const void *G00611[] = {I(a9,47,00,00),I(24,00,00,00),B(stream2 ,39),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3d,01,00,00)};

  eul_allocate_static_string(str_616, ".em", 3);
  eul_allocate_static_string(str_617, "No such file or directory ~a in ~a", 34);
  eul_allocate_static_string(str_621, "  Reading sources from ~a~a~a.em ...", 36);
  /* Byte-vector with size: 78 is_init: 0 index: 11 binding: read-source-file */
  static const void *G00613[] = {I(aa,46,01,23),B(p_read ,4),I(89,00,00,00),B(i_param ,59),I(2a,1b,7d,1b),I(34,00,00,00),I(00,00,00,10),I(1b,32,00,00),I(00,00,00,0a),I(1c,82,02,23),B(p_read ,5),I(24,00,00,00),B(string ,11),I(3c,02,24,00),B(stream ,17),I(1c,24,00,00),B(i_param ,16),I(24,00,00,00),B(boot ,5),I(3c,03,1b,12),I(1b,34,00,00),I(00,00,00,3b),I(23,00,00,00),B(p_read ,6),I(1f,03,24,00),B(i_param ,16),I(24,00,00,00),B(format ,2),I(3c,03,24,00),B(condition ,8),I(1c,24,00,00),B(boot ,12),I(3d,02,06,22),I(01,32,00,00),I(00,00,00,ae),I(1c,10,1d,11),I(24,00,00,00),B(stream2 ,4),I(23,00,00,00),B(p_read ,7),I(1f,03,24,00),B(mop_gf ,2),I(3c,03,86,1c),I(48,00,00,23),B(p_read ,8),I(24,00,00,00),B(dynamic ,3),I(3c,01,23,00),B(p_read ,9),I(23,00,00,00),B(p_read ,3),I(3b,00,1c,0f),I(23,00,00,00),B(p_read ,8),I(1c,24,00,00),B(dynamic ,2),I(3c,02,2a,23),B(p_read ,10),I(1f,05,24,00),B(i_param ,22),I(1f,0d,24,00),B(i_notify ,4),I(3c,04,2a,1f),I(04,89,00,00),B(i_param ,36),I(2a,47,00,00),I(24,00,00,00),B(read ,12),I(3c,01,1b,20),I(04,1f,03,47),I(00,00,24,00),B(stream2 ,39),I(3c,01,2a,83),I(24,00,00,00),B(dynamic ,8),I(3c,01,2a,1f),I(04,22,08,45),I(05,00,00,00)};

  /* Byte-vector with size: 19 is_init: 1 index: 0 binding: initialize-p-read */
  static const void *G00622[] = {I(87,25,00,00),B(p_read ,1),I(24,00,00,00),B(sx_obj ,1),I(3e,0b,24,00),B(sx_obj ,0),I(3c,00,21,01),I(24,00,00,00),B(i_all ,1),I(3e,0b,24,00),B(i_all ,0),I(3c,00,21,01),I(23,00,00,00),B(p_read ,12),I(23,00,00,00),B(p_read ,11),I(3b,01,25,00),B(p_read ,2),I(86,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G00612,G00611);
  eul_intern_symbol(sym_615,"read");
  object_class(str_616) = eul_static_string_class;
  object_class(str_617) = eul_static_string_class;
  eul_intern_keyword(key_618,"file-name");
  eul_intern_symbol(sym_619,"*clean-ups*");
  eul_intern_symbol(sym_620,"anonymous");
  object_class(str_621) = eul_static_string_class;
  eul_allocate_bytevector( G00614,G00613);
  eul_intern_symbol(sym_624,"read-source-file");
  eul_allocate_bytevector( G00623,G00622);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 3; i++)
      p_read_bindings[i] = eul_nil;
  }

  p_read_bindings[ 3] = G00612;
  p_read_bindings[ 4] = sym_615;
  p_read_bindings[ 5] = str_616;
  p_read_bindings[ 6] = str_617;
  p_read_bindings[ 7] = key_618;
  p_read_bindings[ 8] = sym_619;
  p_read_bindings[ 9] = sym_620;
  p_read_bindings[ 10] = str_621;
  p_read_bindings[ 11] = G00614;
  p_read_bindings[ 1] = eul_nil;
  p_read_bindings[ 12] = sym_624;
  eul_allocate_lambda( p_read_bindings[0], "initialize-p-read", 0, G00623);

  }
}


/* eof */
