/// Copyright 2003 T. Kurt Bond
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
///  Title: Interface to POSIX time
///  Library: Tools
///  Authors: T. Kurt Bond
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include <eulisp.h>
#include <time.h>

char *eul_strftime (char *str)
{
    char buffer[1024];

    time_t t = time((time_t *)0);
    struct tm *lt = localtime (&t);
    size_t n = strftime (buffer, 1024, (char *)str, lt);
    buffer[n] = '\0';
    char *res = (char *) gc_malloc (n + 1);
    strcpy (res, buffer);

    // Should I be doing something using c_strn_as_eul_str() here???
    return res;
}


///-----------------------------------------------------------------------------
