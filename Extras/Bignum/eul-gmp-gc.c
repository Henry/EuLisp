#include "eulisp.h"
#include "gmp.h"

///-----------------------------------------------------------------------------
/// Set the memory funtions
///-----------------------------------------------------------------------------

void *C_gc_realloc(void *p, size_t o, size_t n)
{
    return (gc_realloc(p, n));
}

void C_gc_free(void *p, size_t o)
{
    gc_free(p);
}

void *C_gc_malloc(size_t n)
{
    return (gc_malloc(n));
}

long eul_gmp_init()
{
    mp_set_memory_functions(C_gc_malloc, C_gc_realloc, C_gc_free);
    return (1);
}


///-----------------------------------------------------------------------------
