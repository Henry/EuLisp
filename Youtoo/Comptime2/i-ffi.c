/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module i-ffi
 **  Copyright: See file i-ffi.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_i_all();
extern LispRef i_all_bindings[];
extern LispRef i_notify_bindings[];
extern LispRef collect_bindings[];

/* Module bindings with size 40 */
LispRef i_ffi_bindings[40];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module i-ffi */
void initialize_module_i_ffi()
{
  if (is_initialized) return;
  initialize_module_i_all();
  eul_fast_table_set(eul_modules,"i_ffi",(LispRef) i_ffi_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_2873, sym_2872, sym_2871, sym_2870, sym_2869, G002868, G002866, G002863, G002861, G002858, vec_2831, sym_2854, vec_2818, sym_2830, vec_2793, vec_2768, sym_2792, sym_2790, sym_2788, sym_2786, sym_2784, sym_2782, sym_2780, sym_2778, sym_2776, sym_2774, sym_2772, sym_2770, G002767;

  /* Code vector and literal definitions */
  eul_allocate_static_cons(cons_2791, NULL, NULL);
  eul_allocate_static_cons(cons_2789, NULL, eul_as_static(cons_2791));
  eul_allocate_static_cons(cons_2787, NULL, eul_as_static(cons_2789));
  eul_allocate_static_cons(cons_2785, NULL, eul_as_static(cons_2787));
  eul_allocate_static_cons(cons_2783, NULL, eul_as_static(cons_2785));
  eul_allocate_static_cons(cons_2781, NULL, eul_as_static(cons_2783));
  eul_allocate_static_cons(cons_2779, NULL, eul_as_static(cons_2781));
  eul_allocate_static_cons(cons_2777, NULL, eul_as_static(cons_2779));
  eul_allocate_static_cons(cons_2775, NULL, eul_as_static(cons_2777));
  eul_allocate_static_cons(cons_2773, NULL, eul_as_static(cons_2775));
  eul_allocate_static_cons(cons_2771, NULL, eul_as_static(cons_2773));
  eul_allocate_static_cons(cons_2769, NULL, eul_as_static(cons_2771));
  eul_allocate_static_string(str_2795, "int", 3);
  eul_allocate_static_string(str_2797, "char", 4);
  eul_allocate_static_string(str_2799, "double", 6);
  eul_allocate_static_string(str_2801, "const char *", 12);
  eul_allocate_static_string(str_2803, "void **", 7);
  eul_allocate_static_string(str_2805, "int", 3);
  eul_allocate_static_string(str_2807, "const char *", 12);
  eul_allocate_static_string(str_2809, "int", 3);
  eul_allocate_static_string(str_2811, "void *", 6);
  eul_allocate_static_string(str_2813, "int *", 5);
  eul_allocate_static_string(str_2815, "double *", 8);
  eul_allocate_static_string(str_2817, "char **", 7);
  eul_allocate_static_cons(cons_2816, eul_as_static(str_2817), NULL);
  eul_allocate_static_cons(cons_2814, eul_as_static(str_2815), eul_as_static(cons_2816));
  eul_allocate_static_cons(cons_2812, eul_as_static(str_2813), eul_as_static(cons_2814));
  eul_allocate_static_cons(cons_2810, eul_as_static(str_2811), eul_as_static(cons_2812));
  eul_allocate_static_cons(cons_2808, eul_as_static(str_2809), eul_as_static(cons_2810));
  eul_allocate_static_cons(cons_2806, eul_as_static(str_2807), eul_as_static(cons_2808));
  eul_allocate_static_cons(cons_2804, eul_as_static(str_2805), eul_as_static(cons_2806));
  eul_allocate_static_cons(cons_2802, eul_as_static(str_2803), eul_as_static(cons_2804));
  eul_allocate_static_cons(cons_2800, eul_as_static(str_2801), eul_as_static(cons_2802));
  eul_allocate_static_cons(cons_2798, eul_as_static(str_2799), eul_as_static(cons_2800));
  eul_allocate_static_cons(cons_2796, eul_as_static(str_2797), eul_as_static(cons_2798));
  eul_allocate_static_cons(cons_2794, eul_as_static(str_2795), eul_as_static(cons_2796));
  eul_allocate_static_cons(cons_2829, NULL, NULL);
  eul_allocate_static_cons(cons_2828, NULL, eul_as_static(cons_2829));
  eul_allocate_static_cons(cons_2827, NULL, eul_as_static(cons_2828));
  eul_allocate_static_cons(cons_2826, NULL, eul_as_static(cons_2827));
  eul_allocate_static_cons(cons_2825, NULL, eul_as_static(cons_2826));
  eul_allocate_static_cons(cons_2824, NULL, eul_as_static(cons_2825));
  eul_allocate_static_cons(cons_2823, NULL, eul_as_static(cons_2824));
  eul_allocate_static_cons(cons_2822, NULL, eul_as_static(cons_2823));
  eul_allocate_static_cons(cons_2821, NULL, eul_as_static(cons_2822));
  eul_allocate_static_cons(cons_2820, NULL, eul_as_static(cons_2821));
  eul_allocate_static_cons(cons_2819, NULL, eul_as_static(cons_2820));
  eul_allocate_static_string(str_2833, "int", 3);
  eul_allocate_static_string(str_2835, "char", 4);
  eul_allocate_static_string(str_2837, "double", 6);
  eul_allocate_static_string(str_2839, "char *", 6);
  eul_allocate_static_string(str_2841, "char *", 6);
  eul_allocate_static_string(str_2843, "int", 3);
  eul_allocate_static_string(str_2845, "void *", 6);
  eul_allocate_static_string(str_2847, "int *", 5);
  eul_allocate_static_string(str_2849, "double *", 8);
  eul_allocate_static_string(str_2851, "char ** ", 8);
  eul_allocate_static_string(str_2856, "", 0);
  eul_allocate_static_cons(cons_2855, eul_as_static(str_2856), NULL);
  eul_allocate_static_cons(cons_2853, NULL, eul_as_static(cons_2855));
  eul_allocate_static_cons(cons_2852, NULL, eul_as_static(cons_2853));
  eul_allocate_static_cons(cons_2850, eul_as_static(str_2851), eul_as_static(cons_2852));
  eul_allocate_static_cons(cons_2848, eul_as_static(str_2849), eul_as_static(cons_2850));
  eul_allocate_static_cons(cons_2846, eul_as_static(str_2847), eul_as_static(cons_2848));
  eul_allocate_static_cons(cons_2844, eul_as_static(str_2845), eul_as_static(cons_2846));
  eul_allocate_static_cons(cons_2842, eul_as_static(str_2843), eul_as_static(cons_2844));
  eul_allocate_static_cons(cons_2840, eul_as_static(str_2841), eul_as_static(cons_2842));
  eul_allocate_static_cons(cons_2838, eul_as_static(str_2839), eul_as_static(cons_2840));
  eul_allocate_static_cons(cons_2836, eul_as_static(str_2837), eul_as_static(cons_2838));
  eul_allocate_static_cons(cons_2834, eul_as_static(str_2835), eul_as_static(cons_2836));
  eul_allocate_static_cons(cons_2832, eul_as_static(str_2833), eul_as_static(cons_2834));
  /* Byte-vector with size: 17 is_init: 0 index: 28 binding: top-level */
  static const void *G002766[] = {I(a9,23,00,00),B(i_ffi ,22),I(89,00,00,00),B(i_ffi ,9),I(2a,23,00,00),B(i_ffi ,23),I(89,00,00,00),B(i_ffi ,2),I(2a,23,00,00),B(i_ffi ,25),I(89,00,00,00),B(i_ffi ,5),I(2a,23,00,00),B(i_ffi ,27),I(89,00,00,00),B(i_ffi ,4),I(45,00,00,00)};

  eul_allocate_static_string(str_2859, "bad defextern result converter ~a", 33);
  /* Byte-vector with size: 14 is_init: 0 index: 30 binding: res-converter-index */
  static const void *G002857[] = {I(aa,1b,24,00),B(i_ffi ,5),I(24,00,00,00),B(collect ,17),I(3c,02,1b,34),I(00,00,00,0d),I(1b,32,00,00),I(00,00,00,1a),I(82,23,00,00),B(i_ffi ,29),I(1f,03,24,00),B(i_notify ,6),I(3d,03,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 31 binding: res-converter-as-C-type */
  static const void *G002860[] = {I(aa,24,00,00),B(i_ffi ,4),I(1c,02,45,01)};

  eul_allocate_static_string(str_2864, "bad defextern argument converter ~a", 35);
  /* Byte-vector with size: 14 is_init: 0 index: 33 binding: arg-converter-index */
  static const void *G002862[] = {I(aa,1b,24,00),B(i_ffi ,9),I(24,00,00,00),B(collect ,17),I(3c,02,1b,34),I(00,00,00,0d),I(1b,32,00,00),I(00,00,00,1a),I(82,23,00,00),B(i_ffi ,32),I(1f,03,24,00),B(i_notify ,6),I(3d,03,02,45),I(02,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 34 binding: arg-converter-as-C-type */
  static const void *G002865[] = {I(aa,24,00,00),B(i_ffi ,2),I(1c,02,45,01)};

  /* Byte-vector with size: 45 is_init: 1 index: 0 binding: initialize-i-ffi */
  static const void *G002867[] = {I(87,25,00,00),B(i_ffi ,1),I(24,00,00,00),B(i_all ,1),I(3e,0b,24,00),B(i_all ,0),I(3c,00,21,01),I(86,25,00,00),B(i_ffi ,9),I(23,00,00,00),B(i_ffi ,35),I(23,00,00,00),B(i_ffi ,34),I(3b,01,25,00),B(i_ffi ,8),I(23,00,00,00),B(i_ffi ,36),I(23,00,00,00),B(i_ffi ,33),I(3b,01,25,00),B(i_ffi ,7),I(23,00,00,00),B(i_ffi ,37),I(23,00,00,00),B(i_ffi ,31),I(3b,01,25,00),B(i_ffi ,6),I(86,25,00,00),B(i_ffi ,5),I(86,25,00,00),B(i_ffi ,4),I(23,00,00,00),B(i_ffi ,38),I(23,00,00,00),B(i_ffi ,30),I(3b,01,25,00),B(i_ffi ,3),I(86,25,00,00),B(i_ffi ,2),I(23,00,00,00),B(i_ffi ,39),I(23,00,00,00),B(i_ffi ,28),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_intern_symbol(sym_2770,"<int>");
  eul_intern_symbol(sym_2772,"<character>");
  eul_intern_symbol(sym_2774,"<double>");
  eul_intern_symbol(sym_2776,"<string>");
  eul_intern_symbol(sym_2778,"<vector>");
  eul_intern_symbol(sym_2780,"<file-stream>");
  eul_intern_symbol(sym_2782,"<symbol>");
  eul_intern_symbol(sym_2784,"boolean");
  eul_intern_symbol(sym_2786,"ptr");
  eul_intern_symbol(sym_2788,"<int*>");
  eul_intern_symbol(sym_2790,"<double*>");
  eul_intern_symbol(sym_2792,"<string*>");
  object_class(cons_2791) = eul_static_cons_class;
  eul_car(cons_2791) = sym_2792;
  eul_cdr(cons_2791) = eul_nil;
  object_class(cons_2789) = eul_static_cons_class;
  eul_car(cons_2789) = sym_2790;
  object_class(cons_2787) = eul_static_cons_class;
  eul_car(cons_2787) = sym_2788;
  object_class(cons_2785) = eul_static_cons_class;
  eul_car(cons_2785) = sym_2786;
  object_class(cons_2783) = eul_static_cons_class;
  eul_car(cons_2783) = sym_2784;
  object_class(cons_2781) = eul_static_cons_class;
  eul_car(cons_2781) = sym_2782;
  object_class(cons_2779) = eul_static_cons_class;
  eul_car(cons_2779) = sym_2780;
  object_class(cons_2777) = eul_static_cons_class;
  eul_car(cons_2777) = sym_2778;
  object_class(cons_2775) = eul_static_cons_class;
  eul_car(cons_2775) = sym_2776;
  object_class(cons_2773) = eul_static_cons_class;
  eul_car(cons_2773) = sym_2774;
  object_class(cons_2771) = eul_static_cons_class;
  eul_car(cons_2771) = sym_2772;
  object_class(cons_2769) = eul_static_cons_class;
  eul_car(cons_2769) = sym_2770;
  eul_allocate_vector(vec_2768,12,cons_2769);
  object_class(str_2795) = eul_static_string_class;
  object_class(str_2797) = eul_static_string_class;
  object_class(str_2799) = eul_static_string_class;
  object_class(str_2801) = eul_static_string_class;
  object_class(str_2803) = eul_static_string_class;
  object_class(str_2805) = eul_static_string_class;
  object_class(str_2807) = eul_static_string_class;
  object_class(str_2809) = eul_static_string_class;
  object_class(str_2811) = eul_static_string_class;
  object_class(str_2813) = eul_static_string_class;
  object_class(str_2815) = eul_static_string_class;
  object_class(str_2817) = eul_static_string_class;
  object_class(cons_2816) = eul_static_cons_class;
  eul_cdr(cons_2816) = eul_nil;
  object_class(cons_2814) = eul_static_cons_class;
  object_class(cons_2812) = eul_static_cons_class;
  object_class(cons_2810) = eul_static_cons_class;
  object_class(cons_2808) = eul_static_cons_class;
  object_class(cons_2806) = eul_static_cons_class;
  object_class(cons_2804) = eul_static_cons_class;
  object_class(cons_2802) = eul_static_cons_class;
  object_class(cons_2800) = eul_static_cons_class;
  object_class(cons_2798) = eul_static_cons_class;
  object_class(cons_2796) = eul_static_cons_class;
  object_class(cons_2794) = eul_static_cons_class;
  eul_allocate_vector(vec_2793,12,cons_2794);
  eul_intern_symbol(sym_2830,"void");
  object_class(cons_2829) = eul_static_cons_class;
  eul_car(cons_2829) = sym_2830;
  eul_cdr(cons_2829) = eul_nil;
  object_class(cons_2828) = eul_static_cons_class;
  eul_car(cons_2828) = sym_2792;
  object_class(cons_2827) = eul_static_cons_class;
  eul_car(cons_2827) = sym_2790;
  object_class(cons_2826) = eul_static_cons_class;
  eul_car(cons_2826) = sym_2788;
  object_class(cons_2825) = eul_static_cons_class;
  eul_car(cons_2825) = sym_2786;
  object_class(cons_2824) = eul_static_cons_class;
  eul_car(cons_2824) = sym_2784;
  object_class(cons_2823) = eul_static_cons_class;
  eul_car(cons_2823) = sym_2782;
  object_class(cons_2822) = eul_static_cons_class;
  eul_car(cons_2822) = sym_2776;
  object_class(cons_2821) = eul_static_cons_class;
  eul_car(cons_2821) = sym_2774;
  object_class(cons_2820) = eul_static_cons_class;
  eul_car(cons_2820) = sym_2772;
  object_class(cons_2819) = eul_static_cons_class;
  eul_car(cons_2819) = sym_2770;
  eul_allocate_vector(vec_2818,11,cons_2819);
  object_class(str_2833) = eul_static_string_class;
  object_class(str_2835) = eul_static_string_class;
  object_class(str_2837) = eul_static_string_class;
  object_class(str_2839) = eul_static_string_class;
  object_class(str_2841) = eul_static_string_class;
  object_class(str_2843) = eul_static_string_class;
  object_class(str_2845) = eul_static_string_class;
  object_class(str_2847) = eul_static_string_class;
  object_class(str_2849) = eul_static_string_class;
  object_class(str_2851) = eul_static_string_class;
  eul_intern_symbol(sym_2854,"*");
  object_class(str_2856) = eul_static_string_class;
  object_class(cons_2855) = eul_static_cons_class;
  eul_cdr(cons_2855) = eul_nil;
  object_class(cons_2853) = eul_static_cons_class;
  eul_car(cons_2853) = sym_2854;
  object_class(cons_2852) = eul_static_cons_class;
  eul_car(cons_2852) = sym_2830;
  object_class(cons_2850) = eul_static_cons_class;
  object_class(cons_2848) = eul_static_cons_class;
  object_class(cons_2846) = eul_static_cons_class;
  object_class(cons_2844) = eul_static_cons_class;
  object_class(cons_2842) = eul_static_cons_class;
  object_class(cons_2840) = eul_static_cons_class;
  object_class(cons_2838) = eul_static_cons_class;
  object_class(cons_2836) = eul_static_cons_class;
  object_class(cons_2834) = eul_static_cons_class;
  object_class(cons_2832) = eul_static_cons_class;
  eul_allocate_vector(vec_2831,13,cons_2832);
  eul_allocate_bytevector( G002767,G002766);
  object_class(str_2859) = eul_static_string_class;
  eul_allocate_bytevector( G002858,G002857);
  eul_allocate_bytevector( G002861,G002860);
  object_class(str_2864) = eul_static_string_class;
  eul_allocate_bytevector( G002863,G002862);
  eul_allocate_bytevector( G002866,G002865);
  eul_intern_symbol(sym_2869,"arg-converter-as-C-type");
  eul_intern_symbol(sym_2870,"arg-converter-index");
  eul_intern_symbol(sym_2871,"res-converter-as-C-type");
  eul_intern_symbol(sym_2872,"res-converter-index");
  eul_intern_symbol(sym_2873,"top-level");
  eul_allocate_bytevector( G002868,G002867);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 10; i++)
      i_ffi_bindings[i] = eul_nil;
  }

  i_ffi_bindings[ 10] = sym_2770;
  i_ffi_bindings[ 11] = sym_2772;
  i_ffi_bindings[ 12] = sym_2774;
  i_ffi_bindings[ 13] = sym_2776;
  i_ffi_bindings[ 14] = sym_2778;
  i_ffi_bindings[ 15] = sym_2780;
  i_ffi_bindings[ 16] = sym_2782;
  i_ffi_bindings[ 17] = sym_2784;
  i_ffi_bindings[ 18] = sym_2786;
  i_ffi_bindings[ 19] = sym_2788;
  i_ffi_bindings[ 20] = sym_2790;
  i_ffi_bindings[ 21] = sym_2792;
  i_ffi_bindings[ 22] = vec_2768;
  i_ffi_bindings[ 23] = vec_2793;
  i_ffi_bindings[ 24] = sym_2830;
  i_ffi_bindings[ 25] = vec_2818;
  i_ffi_bindings[ 26] = sym_2854;
  i_ffi_bindings[ 27] = vec_2831;
  i_ffi_bindings[ 28] = G002767;
  i_ffi_bindings[ 29] = str_2859;
  i_ffi_bindings[ 30] = G002858;
  i_ffi_bindings[ 31] = G002861;
  i_ffi_bindings[ 32] = str_2864;
  i_ffi_bindings[ 33] = G002863;
  i_ffi_bindings[ 34] = G002866;
  i_ffi_bindings[ 1] = eul_nil;
  i_ffi_bindings[ 35] = sym_2869;
  i_ffi_bindings[ 36] = sym_2870;
  i_ffi_bindings[ 37] = sym_2871;
  i_ffi_bindings[ 38] = sym_2872;
  i_ffi_bindings[ 39] = sym_2873;
  eul_allocate_lambda( i_ffi_bindings[0], "initialize-i-ffi", 0, G002868);

  }
}


/* eof */
