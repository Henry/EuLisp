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
/// Title: Foreign function interface to POSIX and PCRE libraries
///  Library: string-perls
///  Authors: Henry G. Weller
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------

#ifndef EUL_STRING_PERLS_H
#define EUL_STRING_PERLS_H

#include "eulisp.h"

///-----------------------------------------------------------------------------

// Split the string str at the delimiters and return a list of the sub-strings
// Note: the list returned is in reverse order
extern LispRef eul_split_string(const char *str, const char *delimiters);

extern LispRef eul_match_string(const char *str, const char *regex);
extern LispRef eul_match_all_string(const char *str, const char *regex);

///-----------------------------------------------------------------------------
#endif // EUL_STRING_PERLS_H
///-----------------------------------------------------------------------------
