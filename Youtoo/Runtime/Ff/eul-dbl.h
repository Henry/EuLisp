/// Copyright 1997 A. Kind & University of Bath
/// Copyright 2010 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                         EuLisp System 'Youtoo'
///-----------------------------------------------------------------------------
//
//  Youtoo is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
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
