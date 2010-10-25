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
/// Title: List processing
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "stdc.h"
#include "config.h"
#include "notify.h"
#include "shared-mem.h"
#include "tag.h"
#include "object.h"
#include "register.h"
#include "class.h"
#include "list.h"
#include "fpi.h"
#include "util.h"

///-----------------------------------------------------------------------------
/// Printer
///-----------------------------------------------------------------------------
void fprint_cons(FILE *fd, LispRef o)
{
    LispRef rest = o;
    putc('(', fd);
    fprint_ref(fd, eul_car(rest));

    while (!((rest = (eul_cdr(rest))) == eul_nil))
    {
        putc(' ', fd);
        if (eul_is_cons(rest))
        {
            fprint_ref(fd, eul_car(rest));
        }
        else
        {
            putc('.', fd);
            putc(' ', fd);
            fprint_ref(fd, rest);
            break;
        }
    }
    putc(')', fd);
}


void fprint_vector(FILE * fd, LispRef o)
{
    int n = eul_fpi_as_c_int(object_size(o));
    putc('#', fd);
    putc('(', fd);
    for (int i = 0; i < n; i++)
    {
        fprint_ref(fd, slot_ref(o, i));
        if (i != (n - 1))
        {
            putc(' ', fd);
        }
    }
    putc(')', fd);
}


///-----------------------------------------------------------------------------
/// Initialization
///-----------------------------------------------------------------------------

#ifdef WITH_CONS_TAG
void eul_initialize_cons()
{
    define_printer(CONS_TAG, fprint_cons);
}
#endif // WITH_CONS_TAG


///-----------------------------------------------------------------------------
