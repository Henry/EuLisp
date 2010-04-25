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
