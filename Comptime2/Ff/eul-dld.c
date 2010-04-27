/// Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'youtoo'
///-----------------------------------------------------------------------------
///  Library: eval, youtoo
///  Authors: Andreas Kind
///  Description: dynamic linker
///-----------------------------------------------------------------------------

#include "eulisp.h"

#if ptrNBytes==4
#   if BYTE_ORDER == LITTLE_ENDIAN
#       define II(y0, y1, y2, y3)                                              \
            ((y0)+(y1*256)+(y2*256*256)+(y3*256*256*256))
#   else
#       define II(y0, y1, y2, y3)                                              \
            ((y3)+(y2*256)+(y1*256*256)+(y0*256*256*256))
#   endif /* LITTLE_ENDIAN */
#elif ptrNBytes==8
#   if BYTE_ORDER == LITTLE_ENDIAN
#       define II(y0, y1, y2, y3)                                                     \
            ((y0)+(y1*256*256)+(y2*256*256*256*256)+(y3*256*256*256*256*256*256))
#   else
#       define II(y0, y1, y2, y3)                                                     \
            ((y3*256)+(y2*256*256*256)+(y1*256*256*256*256*256)+(y0*256*256*256*256*256*256*256))
#   endif /* LITTLE_ENDIAN */
#else
#   error ptrNBytes set incorrectly
#endif /* ptrNBytes */

static char buf[64];
static int is_init_bv = 0;


///-----------------------------------------------------------------------------
/// Module name
///-----------------------------------------------------------------------------
char *eul_module_name_as_C_module_name_string(LispRef sym)
{
    return as_C_module_name(eul_symbol_as_c_string(sym));
}

char *as_C_module_name(char *str)
{
    char *res = (char *)gc_malloc(32);
    char *res_ptr = res;

    char c;
    while ((c = *str++) != '\0')
    {
        if (c == '-')
        {
            *res_ptr++ = '_';
        }
        else
        {
            *res_ptr++ = c;
        }
    }
    *res_ptr = '\0';

    return res;
}


///-----------------------------------------------------------------------------
/// Create run-time module (i.e. allocate and register binding vector)
///-----------------------------------------------------------------------------
LispRef *eul_dyn_create_module(char *module_name, int size)
{
    // printf(".Create module %s with size %d\n", module_name, size);
    LispRef *binding_vec = (LispRef *) gc_malloc(size * sizeof(LispRef *));

    // Set local bindings
    binding_vec[0] = eul_true;
    for (int i = 1; i < size; i++)
    {
        binding_vec[i] = eul_nil;
    }

    eul_fast_table_set
    (
        eul_modules,
        as_C_module_name(module_name),
        (LispRef)binding_vec
    );

    return binding_vec;
}


///-----------------------------------------------------------------------------
/// Look for "I(%d, %d, %d, %d)" or "B(%s, %d)"
///-----------------------------------------------------------------------------
int fill_bytevector(FILE * fp, LispRef bv, int size)
{
    // Skip " static const void *G000??[] = "
    int var_nr;
    char var_name[6];
    fscanf(fp, "  static const void *G%d[] = ", &var_nr);
    sprintf(var_name, "%d", var_nr);

    void **data = (void **)BYTEVECTOR_DATA(bv);
    int binding_index;
    ptrInt x0, x1, x2, x3;
    LispRef *word;
    LispRef bindings;

    for (int i = 0; i < size; i++)
    {
        // Skip "{" or ", " or "}"
        getc(fp);

        if (getc(fp) == 'I')
        {
            fscanf(fp,
            "(%" ptrIntPM "x, %" ptrIntPM "x, %" ptrIntPM "x, %" ptrIntPM "x)",
            &x0, &x1, &x2, &x3);
            // printf("fill_bytevector (%d, %d, %d, %d)\n", x0, x1, x2, x3);
            // fflush(stdout);
            word = (void *)II(x0, x1, x2, x3);
        }
        else
        {
            fscanf(fp, "(%s ,%d)", buf, &binding_index);
            bindings = eul_fast_table_ref(eul_modules, buf);
            // printf("fill_bytevector B(%s, %d)\n", buf, binding_index);
            // fflush(stdout);
            if (eul_null(bindings))
            {
                fprintf(stderr,
                "\n*** WARNING [eul-dld][fill_bytevector]: module %s not loaded\n",
                buf);
                fflush(stderr);
                return (0);
            }
            word = ((LispRef *) bindings) + binding_index;
        }

        data[i] = (void *)word;
    }

    return 1;
}


///-----------------------------------------------------------------------------
/// Look for "Module bindings with size: %d"
///-----------------------------------------------------------------------------
static int binding_vector_size;

LispRef *create_binding_vector(char *module_name, FILE * fp)
{
    binding_vector_size = 0;
    while (fgets(buf, 20, fp) != NULL &&
    strncmp(buf, "/* Module bindings ", 19));

    if (buf != NULL)
    {
        fscanf(fp, "with size %d]", &binding_vector_size);
    }

    // Make space for 256 additional bindings; see also cg-dld.em
    int size = binding_vector_size + 256;

    // Create run-time module; return it's binding vector
    return eul_dyn_create_module(module_name, size);
}


///-----------------------------------------------------------------------------
/// Load compiled module
///-----------------------------------------------------------------------------
int load_compiled_module(char *module_name, FILE * fp)
{
    LispRef bv;
    LispRef *binding_vec = create_binding_vector(module_name, fp);

    // Get bytevectors
    while (is_init_bv == 0)
    {
        int size = create_next_bytevector(&bv, fp, binding_vec);

        if (!fill_bytevector(fp, bv, size))
        {
            return (0);
        }
    }

    is_init_bv = 0;

    return 1;
}


///-----------------------------------------------------------------------------
/// Look for "Byte-vector with size: %d is_init: %d index: %d binding_name: %s"
///-----------------------------------------------------------------------------
int create_next_bytevector(LispRef * bv_ptr, FILE * fp, LispRef * binding_vec)
{
    while (fgets(buf, 18, fp) != NULL && strncmp(buf, "  /* Byte-vector ", 17));

    if (buf == NULL)
    {
        return 0;
    }

    int size, index;

    fscanf
    (
        fp,
        "with size: %d is_init: %d index: %d binding: %s */\n",
        &size,
        &is_init_bv,
        &index,
        buf
    );

    int byte_size = sizeof(LispRef) * size;
    eul_allocate_bytevector1(*bv_ptr, (char *)gc_malloc(byte_size), byte_size);

    if (index)
    {
        binding_vec[index] = *bv_ptr;
    }
    else
    {
        char *binding_name;

        binding_name = (char *)gc_malloc(strlen(buf) + 1);
        strcpy(binding_name, buf);

        // Change method names like (method-foo) into (method foo)
        if (*binding_name == '(')
        {
            *(binding_name + 7) = ' ';
        }

        eul_allocate_lambda(binding_vec[0], binding_name, 0, *bv_ptr);
    }

    return size;
}


///-----------------------------------------------------------------------------
/// Dynamically load a module (local literals not initialized!)
///-----------------------------------------------------------------------------
int eul_dyn_load_module(char *module_name, char *file_name)
{
    FILE *fp = fopen(file_name, "r");
    int res = load_compiled_module(module_name, fp);
    fclose(fp);

    if (res)
    {
        return (binding_vector_size);
    }
    else
    {
        return (0);
    }
}


///-----------------------------------------------------------------------------
/// Write a binding reference into a bytevector
///-----------------------------------------------------------------------------
LispRef eul_write_next_bv_binding_ref1
(
    LispRef bv,
    int bv_index,
    char *module_name,
    int local_index
)
{
    void **data = (void **)STRING_DATA(bv);

    LispRef bindings = eul_fast_table_ref(eul_modules, module_name);

    if (eul_null(bindings))
    {
        fprintf
        (stderr,
        "\n*** WARNING [eul-dld][eul_write_next_bv_binding_ref1]: module %s not loaded\n",
        module_name);
        fflush(stderr);
    }
    else
    {
        LispRef *word = ((LispRef *) bindings) + local_index;

        data[bv_index / ptrNBytes] = (void *)word;
    }

    return eul_nil;
}


///-----------------------------------------------------------------------------
/// Allocate lambda object
///-----------------------------------------------------------------------------
LispRef eul_allocate_lambda2(LispRef name, LispRef arity, LispRef bv)
{
    LispRef res;
    eul_allocate_lambda1(res, name, arity, bv);

    return res;
}


///-----------------------------------------------------------------------------
