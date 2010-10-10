/// Copyright 1997 J. Garcia & University of Bath
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
/// Title: String operations
///  Library: tcltk
///  Authors: J. Garcia
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#ifndef STR_OPERATIONS_H
#define STR_OPERATIONS_H

#include "eulisp.h"

struct infoargs
{
    int argc;
    const char **argv;
};

extern int ParseArguments2
(
    struct infoargs *,
    const char *,
    const char *,
    LispRef
);

extern int ParseArguments3
(
    struct infoargs *,
    const char *,
    const char *,
    const char *,
    LispRef
);

extern int ParseArguments4
(
    struct infoargs *,
    const char *,
    const char *,
    const char *,
    const char *
);

///-----------------------------------------------------------------------------
#endif
///-----------------------------------------------------------------------------
