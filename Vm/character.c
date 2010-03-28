/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: eulvm (Bytecode Interpreter -- Eutopia)
 **  Authors: Keith Playford, Andreas Kind
 **  Description: characters
 ** ----------------------------------------------------------------------- **/

#include "stdc.h"
#include "config.h"
#include "notify.h"
#include "shared-mem.h"
#include "tag.h"
#include "object.h"
#include "character.h"
#include "util.h"

/** ----------------------------------------------------------------- **
 ** Character printer
 ** ----------------------------------------------------------------- **/

static void fprint_char(FILE *fd, LispRef o)
{
    fprintf(fd, "%c", (char)char_value(o));
}


/** ----------------------------------------------------------------- **
 ** Initialization
 ** ----------------------------------------------------------------- **/

void eul_initialize_char()
{
    NOTIFY0(".Initialize characters");
    define_printer(CHAR_TAG, fprint_char);
}
