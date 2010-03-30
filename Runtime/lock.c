/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module lock
 **  Copyright: See file lock.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern void initialize_module_thread();
extern LispRef telos_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef boot_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef boot1_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef thread_bindings[];

/* Module bindings with size 42 */
LispRef lock_bindings[42];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module lock */
void initialize_module_lock()
{
  if (is_initialized) return;
  initialize_module_telos();
  initialize_module_thread();
  eul_fast_table_set(eul_modules,"lock",(LispRef) lock_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_4398, sym_4397, G004396, G004394, sym_4392, sym_4391, sym_4390, sym_4389, sym_4388, sym_4387, sym_4386, sym_4385, sym_4384, key_4382, key_4381, key_4380, sym_4379, key_4378, key_4377, key_4376, sym_4375, key_4374, G004373, G004371, G004369, G004367, G004365, G004363, G004361, sym_4359, G004358, G004356, G004354;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 5 is_init: 0 index: 9 binding: (method-unlock) */
  static const void *G004353[] = {I(aa,1b,82,1c),I(82,1d,24,00),B(lock ,8),I(09,22,02,2a),I(1b,45,01,00)};

  /* Byte-vector with size: 10 is_init: 0 index: 10 binding: anonymous */
  static const void *G004355[] = {I(a9,47,00,00),I(07,1b,34,00),I(00,00,00,0e),I(1b,32,00,00),I(00,00,00,18),I(24,00,00,00),B(thread ,6),I(3c,00,2a,47),I(00,01,3d,00),I(01,45,01,00)};

  /* Byte-vector with size: 11 is_init: 0 index: 12 binding: (method-lock) */
  static const void *G004357[] = {I(aa,46,02,1b),I(48,00,00,86),I(1b,48,00,01),I(23,00,00,00),B(lock ,11),I(23,00,00,00),B(lock ,10),I(3b,00,48,00),I(01,47,00,01),I(3d,00,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 13 binding: (method-lockp) */
  static const void *G004360[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 14 binding: (method-lockp) */
  static const void *G004362[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 15 binding: (method-semaphorep) */
  static const void *G004364[] = {I(aa,1b,45,01)};

  /* Byte-vector with size: 1 is_init: 0 index: 16 binding: (method-semaphorep) */
  static const void *G004366[] = {I(aa,86,45,01)};

  /* Byte-vector with size: 4 is_init: 0 index: 17 binding: (setter-semaphore-counter) */
  static const void *G004368[] = {I(ab,1c,82,1d),I(24,00,00,00),B(lock ,8),I(09,45,02,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 18 binding: anonymous */
  static const void *G004370[] = {I(a9,82,45,00)};

  eul_allocate_static_cons(cons_4383, NULL, NULL);
  /* Byte-vector with size: 311 is_init: 0 index: 38 binding: top-level */
  static const void *G004372[] = {I(a9,24,00,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,26),I(3c,01,23,00),B(lock ,19),I(23,00,00,00),B(lock ,20),I(23,00,00,00),B(lock ,21),I(23,00,00,00),B(lock ,11),I(23,00,00,00),B(lock ,18),I(3b,00,23,00),B(lock ,22),I(23,00,00,00),B(lock ,23),I(24,00,00,00),B(boot1 ,26),I(3c,06,1b,24),B(boot1 ,26),I(3c,01,24,00),B(mop_class ,71),I(23,00,00,00),B(lock ,19),I(23,00,00,00),B(lock ,24),I(23,00,00,00),B(lock ,25),I(1f,06,23,00),B(lock ,26),I(1f,06,23,00),B(lock ,27),I(23,00,00,00),B(lock ,28),I(24,00,00,00),B(mop_gf ,2),I(3c,09,1b,89),B(lock ,8),I(2a,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(lock ,29),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(lock ,7),I(2a,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(lock ,30),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(lock ,6),I(2a,24,00,00),B(lock ,8),I(89,00,00,00),B(lock ,2),I(2a,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(lock ,31),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(lock ,3),I(2a,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(lock ,32),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,55),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,89),B(lock ,4),I(2a,24,00,00),B(boot1 ,41),I(24,00,00,00),B(boot1 ,41),I(3c,01,24,00),B(lock ,5),I(23,00,00,00),B(lock ,33),I(23,00,00,00),B(lock ,17),I(3b,02,1d,3c),I(02,2a,24,00),B(lock ,7),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(lock ,7),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(lock ,34),I(23,00,00,00),B(lock ,16),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(lock ,7),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(lock ,7),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(lock ,8),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(lock ,7),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(lock ,34),I(23,00,00,00),B(lock ,15),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(lock ,7),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(lock ,7),I(2a,24,00,00),B(lock ,6),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(mop_class ,21),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(lock ,6),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(lock ,35),I(23,00,00,00),B(lock ,14),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(lock ,6),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(lock ,6),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(lock ,8),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(lock ,6),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(lock ,35),I(23,00,00,00),B(lock ,13),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(lock ,6),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(lock ,6),I(2a,24,00,00),B(lock ,8),I(2a,24,00,00),B(lock ,3),I(2a,24,00,00),B(lock ,3),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(lock ,2),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(lock ,3),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(lock ,36),I(23,00,00,00),B(lock ,12),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(lock ,3),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,2a,24),B(lock ,4),I(2a,24,00,00),B(lock ,4),I(26,00,00,00),I(00,00,00,03),I(02,83,24,00),B(lock ,2),I(24,00,00,00),B(boot1 ,39),I(3c,02,24,00),B(boot1 ,26),I(3c,00,24,00),B(lock ,4),I(26,00,00,00),I(00,00,00,04),I(02,1c,1c,24),B(boot ,11),I(3c,02,1f,04),I(1f,04,23,00),B(lock ,37),I(23,00,00,00),B(lock ,9),I(3b,01,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,24,00),B(lock ,4),I(1c,24,00,00),B(mop_meth ,5),I(3d,02,39,45),I(39,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 39 binding: semaphore-counter */
  static const void *G004393[] = {I(aa,82,24,00),B(lock ,8),I(08,45,00,00)};

  /* Byte-vector with size: 36 is_init: 1 index: 0 binding: initialize-lock */
  static const void *G004395[] = {I(87,25,00,00),B(lock ,1),I(24,00,00,00),B(thread ,1),I(3e,0b,24,00),B(thread ,0),I(3c,00,21,01),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(86,25,00,00),B(lock ,8),I(86,25,00,00),B(lock ,7),I(86,25,00,00),B(lock ,6),I(23,00,00,00),B(lock ,40),I(23,00,00,00),B(lock ,39),I(3b,01,25,00),B(lock ,5),I(86,25,00,00),B(lock ,4),I(86,25,00,00),B(lock ,3),I(86,25,00,00),B(lock ,2),I(23,00,00,00),B(lock ,41),I(23,00,00,00),B(lock ,38),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G004354,G004353);
  eul_allocate_bytevector( G004356,G004355);
  eul_intern_symbol(sym_4359,"anonymous");
  eul_allocate_bytevector( G004358,G004357);
  eul_allocate_bytevector( G004361,G004360);
  eul_allocate_bytevector( G004363,G004362);
  eul_allocate_bytevector( G004365,G004364);
  eul_allocate_bytevector( G004367,G004366);
  eul_allocate_bytevector( G004369,G004368);
  eul_allocate_bytevector( G004371,G004370);
  eul_intern_keyword(key_4374,"name");
  eul_intern_symbol(sym_4375,"counter");
  eul_intern_keyword(key_4376,"default");
  eul_intern_keyword(key_4377,"keyword");
  eul_intern_keyword(key_4378,"counter");
  eul_intern_symbol(sym_4379,"semaphore");
  eul_intern_keyword(key_4380,"direct-superclasses");
  eul_intern_keyword(key_4381,"direct-slots");
  eul_intern_keyword(key_4382,"direct-keywords");
  object_class(cons_4383) = eul_static_cons_class;
  eul_car(cons_4383) = key_4378;
  eul_cdr(cons_4383) = eul_nil;
  eul_intern_symbol(sym_4384,"semaphorep");
  eul_intern_symbol(sym_4385,"lockp");
  eul_intern_symbol(sym_4386,"lock");
  eul_intern_symbol(sym_4387,"unlock");
  eul_intern_symbol(sym_4388,"(setter semaphore-counter)");
  eul_intern_symbol(sym_4389,"(method semaphorep)");
  eul_intern_symbol(sym_4390,"(method lockp)");
  eul_intern_symbol(sym_4391,"(method lock)");
  eul_intern_symbol(sym_4392,"(method unlock)");
  eul_allocate_bytevector( G004373,G004372);
  eul_allocate_bytevector( G004394,G004393);
  eul_intern_symbol(sym_4397,"semaphore-counter");
  eul_intern_symbol(sym_4398,"top-level");
  eul_allocate_bytevector( G004396,G004395);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 9; i++)
      lock_bindings[i] = eul_nil;
  }

  lock_bindings[ 9] = G004354;
  lock_bindings[ 10] = G004356;
  lock_bindings[ 11] = sym_4359;
  lock_bindings[ 12] = G004358;
  lock_bindings[ 13] = G004361;
  lock_bindings[ 14] = G004363;
  lock_bindings[ 15] = G004365;
  lock_bindings[ 16] = G004367;
  lock_bindings[ 17] = G004369;
  lock_bindings[ 18] = G004371;
  lock_bindings[ 19] = key_4374;
  lock_bindings[ 20] = sym_4375;
  lock_bindings[ 21] = key_4376;
  lock_bindings[ 22] = key_4377;
  lock_bindings[ 23] = key_4378;
  lock_bindings[ 24] = sym_4379;
  lock_bindings[ 25] = key_4380;
  lock_bindings[ 26] = key_4381;
  lock_bindings[ 27] = key_4382;
  lock_bindings[ 28] = cons_4383;
  lock_bindings[ 29] = sym_4384;
  lock_bindings[ 30] = sym_4385;
  lock_bindings[ 31] = sym_4386;
  lock_bindings[ 32] = sym_4387;
  lock_bindings[ 33] = sym_4388;
  lock_bindings[ 34] = sym_4389;
  lock_bindings[ 35] = sym_4390;
  lock_bindings[ 36] = sym_4391;
  lock_bindings[ 37] = sym_4392;
  lock_bindings[ 38] = G004373;
  lock_bindings[ 39] = G004394;
  lock_bindings[ 1] = eul_nil;
  lock_bindings[ 40] = sym_4397;
  lock_bindings[ 41] = sym_4398;
  eul_allocate_lambda( lock_bindings[0], "initialize-lock", 0, G004396);

  }
}


/* eof */