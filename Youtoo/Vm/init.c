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
///  Title: initialization of the bytecode interpreter
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------

#include "stdc.h"
#include "config.h"
#include "init.h"
#include "notify.h"
#include "shared-mem.h"
#include "tag.h"
#include "io.h"
#include "object.h"
#include "bytevec.h"
#include "fpi.h"
#include "list.h"
#include "character.h"
#include "eul-string.h"
#include "register.h"
#include "signals.h"
#include "util.h"
#include "class.h"

extern long eul_profiling_table[];

///-----------------------------------------------------------------------------
/// Initialise the system
///-----------------------------------------------------------------------------

int EUL_INITIALIZE(int argc, char **argv)
{
    if (eul_initialize_runtime_system() != 1)
    {
        return 0;
    }

    // Set default environment variables if not already set
    char* default_eul_dir = EUL_DIR;
    char* default_eul_arch = EUL_ARCH;

    if (!getenv("EUL_DIR"))
    {
        setenv("EUL_DIR", default_eul_dir, 1);
    }
    else
    {
        default_eul_dir = getenv("EUL_DIR");
    }

    if (!getenv("EUL_ARCH"))
    {
        setenv("EUL_ARCH", default_eul_arch, 1);
    }
    else
    {
        default_eul_arch = getenv("EUL_ARCH");
    }

    char *default_load_path = malloc
    (
        strlen(default_eul_dir)
      + strlen(default_eul_arch)
      + 10
    );
    strcpy(default_load_path, ".:");
    strcat(default_load_path, default_eul_dir);
    strcat(default_load_path, "/Lib.");
    strcat(default_load_path, default_eul_arch);

    if (!getenv("EUL_LOAD_PATH"))
    {
        setenv("EUL_LOAD_PATH", default_load_path, 1);
    }

    if (!getenv("EUL_LIBRARY_LOAD_PATH"))
    {
        setenv("EUL_LIBRARY_LOAD_PATH", default_load_path, 1);
    }

    free(default_load_path);

    eul_allocate_int(eul_argc, argc);
    eul_allocate_string_vector(eul_argv, argc, argv);

    return 1;
}


int eul_initialize_runtime_system()
{
    NOTIFY0("Initialize runtime system ...");

    if (gc_expand_hp(DEFAULT_HEAP) != 1)
    {
        return 0;
    }

    eul_initialize_register();
    eul_initialize_signal();
    eul_initialize_util();
    eul_initialize_fpi();

    #ifdef WITH_CONS_TAG
    eul_initialize_cons();
    #endif // WITH_CONS_TAG

    eul_initialize_char();

    #ifdef INSTRUMENTED
    for (int i = 0; i < HIGHEST_BYTE_CODE + 1; ++i)
    {
        eul_profiling_table[i] = 0;
    }
    #endif // INSTRUMENTED

    return 1;
}


///-----------------------------------------------------------------------------
