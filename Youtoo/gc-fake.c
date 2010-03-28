/** Copyright (c) 1997 by A Kind & University of Bath. All rights reserved. **/

/** ----------------------------------------------------------------------- **
 **                     EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Library: boot, telos, level1, eval, youtoo
 **  Authos: Andreas Kind
 **  Description: used if no gc is used
 ** ----------------------------------------------------------------------- **/

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
