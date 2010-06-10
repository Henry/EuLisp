/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: level1, eval, youtoo
///  Authos: Andreas Kind
///  Description: forein function support for double precision floats
///-----------------------------------------------------------------------------
#ifndef EUL_DBL_H
#define EUL_DBL_H
///-----------------------------------------------------------------------------

extern double eul_get_dbl_max();
extern double eul_get_dbl_min();
extern double eul_get_dbl_epsilon();
extern double eul_get_neg_dbl_epsilon();
extern double eul_dbl_sum(double, double);
extern double eul_dbl_difference(double, double);
extern double eul_dbl_product(double, double);
extern double eul_dbl_quotient(double, double);
extern double eul_dbl_remainder(double, double);
extern int eul_dbl_mod(double, double);
extern LispRef eul_dbl_less(double, double);
extern LispRef eul_dbl_equal(double, double);
extern double eul_dbl_ceiling(double);
extern double eul_dbl_floor(double);
extern int eul_dbl_round(double);
extern int eul_dbl_truncate(double);
extern char *eul_dbl_as_str(double);
extern LispRef write_next_ref(void **, int, LispRef);

#define eul_fpi_as_dbl(x) ((double) x)

///-----------------------------------------------------------------------------
#endif // EUL_DBL_H
///-----------------------------------------------------------------------------
