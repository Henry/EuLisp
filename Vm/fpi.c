/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: eulvm (Bytecode Interpreter -- Eutopia)
 **  Authors: Keith Playford, Andreas Kind
 **  Description: fixed precision integer
 ** ----------------------------------------------------------------------- **/

#include "stdc.h"
#include "config.h"
#include "notify.h"
#include "shared-mem.h"
#include "tag.h"
#include "object.h"
#include "fpi.h"
#include "util.h"

/** ----------------------------------------------------------------- **
 ** Fpi printer
 ** ----------------------------------------------------------------- **/

static void fprint_fpi(FILE *fd, LispRef o)
{
    fprintf(fd, "%"ptrIntPM"d", fpi_value(o));
}


/** ----------------------------------------------------------------- **
 ** Initialization
 ** ----------------------------------------------------------------- **/

void eul_initialize_fpi()
{
    define_printer(FPI_TAG, fprint_fpi);
}
