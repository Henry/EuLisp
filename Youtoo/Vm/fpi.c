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
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Description: fixed precision integer
///-----------------------------------------------------------------------------

#include "stdc.h"
#include "config.h"
#include "notify.h"
#include "shared-mem.h"
#include "tag.h"
#include "object.h"
#include "fpi.h"
#include "util.h"

///-----------------------------------------------------------------------------
/// Fpi printer
///-----------------------------------------------------------------------------
static void fprint_fpi(FILE *fd, LispRef o)
{
    fprintf(fd, "%"ptrIntPM"d", fpi_value(o));
}


///-----------------------------------------------------------------------------
/// Initialization
///-----------------------------------------------------------------------------
void eul_initialize_fpi()
{
    define_printer(FPI_TAG, fprint_fpi);
}


/** ------------------------------------------------------------------------ **/
