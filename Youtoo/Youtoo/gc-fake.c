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
/// Title: used if no gc is used
///  Library: boot, telos, level1, eval, youtoo
///  Authors: Andreas Kind
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------

#include <stdc.h>

void *GC_malloc(size_t size)
{
    return (void *)malloc(size);
}

int GC_expand_hp(size_t n)
{
    return 1;
}

void GC_free(void *p)
{
    return;
}

void* GC_realloc(void *p, size_t n)
{
    return NULL;
}

///-----------------------------------------------------------------------------
