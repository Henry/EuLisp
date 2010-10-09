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
/// Title: Foreign function test
///  Authors: Andreas Kind
///  Maintainer: Henry G. Weller
///  Compilation:
//    cc -c ffi-ext.c
///-----------------------------------------------------------------------------

#include <eulisp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct
{
    char *name;
    int value;
} STRUCT;

static int struct_count = 0;

LispRef ext_get_struct()
{
    STRUCT *p = gc_malloc(sizeof(STRUCT));

    char buf[1024];

    #ifdef DEBUG
    fprintf(stderr, "ext_get_struct: %p\n", p);
    #endif

    sprintf(buf, "struct %03d", ++struct_count);
    p->name = gc_malloc(strlen(buf) + 1);
    p->value = struct_count * 10;
    strcpy(p->name, buf);

    return (LispRef) p;
}


LispRef ext_print_struct(STRUCT * p)
{
    #ifdef DEBUG
    fprintf(stderr, "ext_print_struct: %p\n", p);
    #endif

    printf("%s/%d\n", p->name, p->value);
    return eul_true;
}

///-----------------------------------------------------------------------------
