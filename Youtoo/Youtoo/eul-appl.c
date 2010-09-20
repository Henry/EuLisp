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
///  Description: stand-alone entry point
///-----------------------------------------------------------------------------

#include "stdc.h"
#include "shared-mem.h"
#include "config.h"
#include "notify.h"
#include "tag.h"
#include "object.h"
#include "register.h"
#include "fpi.h"
#include "eul-string.h"
#include "symbol.h"
#include "list.h"
#include "bytevec.h"
#include "ff.h"
#include "class.h"

extern void run_application();

///-----------------------------------------------------------------------------
/// Main function
///-----------------------------------------------------------------------------

#ifndef WITH_PCR_THREADS

int main(int argc, char **argv)
{
    EUL_INITIALIZE(argc, argv);

    // module initialization
    run_application();

    // Handle return code
    LispRef res;
    EXTERNAL_POPVAL1(res);
    if (eul_is_int(res))
    {
        return eul_int_as_c_int(res);
    }

    return 0;
}

#else

// #include <config/PCR_StdDefs.h>
// #include <io/PCR_IO.h>
#include <base/PCR_Base.h>
// #include <base/PCR_BaseArgs.h>

int PCR_youtoo_main(int argc, char **argv, void *data)
{
    EUL_INITIALIZE(argc, argv);

    // Module initialization
    run_application();

    // Handle return code
    LispRef res;
    EXTERNAL_POPVAL1(res);

    if (eul_is_int(res))
    {
        return eul_int_as_c_int(res);
    }

    return 0;
}

PCR_Base_App runList[] =
{
    {
        "youtoo",
        (PCR_Base_AppProc*) & PCR_youtoo_main,
        (void*)NIL,
        NIL,
        (PCR_Bool) NIL,
        (PCR_Base_AppProc*) NIL,
        (void*)NIL
    },
    {NIL}
};

int main(int argc, const char **argv)
{
    char buffer[100];

    PCR_Base_StartApps(argc, argv, runList, PCR_Bool_true, buffer, 100);
    buffer[99] = '\0';
    printf("\n*** Error [system]: %s\n", buffer);
    exit(-1);
}

///-----------------------------------------------------------------------------
#endif // WITH_PCR_THREADS
///-----------------------------------------------------------------------------
