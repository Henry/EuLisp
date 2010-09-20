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
///  Library: eul-serial2
///  Authos: Andreas Kind
///  Description: serializing closures
///-----------------------------------------------------------------------------

#include "eulisp.h"
#include "bytecode.h"
#include "bytecode2.h"
#include "eul-serial2.h"

static LispRef *eul_sorted_modules = NULL;
static int eul_nmodules;


///-----------------------------------------------------------------------------
/// Compare two module vectors
///-----------------------------------------------------------------------------

static int compare_module_vectors(LispRef *x, LispRef *y)
{
    ptrInt addr1 = (ptrInt)TABLE_ENTRY_VALUE(*x);
    ptrInt addr2 = (ptrInt)TABLE_ENTRY_VALUE(*y);

    WITH_DEBUG
    (
        fprintf
        (
            stderr,
            "comparing %"ptrIntPM"d and %"ptrIntPM"d ... ",
            addr1,
            addr2
        )
    );
    fflush(stdout);

    int res = (addr1 < addr2) ? -1 : 1;
    WITH_DEBUG(fprintf(stderr, "%d\n", res));

    return res;
}


///-----------------------------------------------------------------------------
/// Store linked modules in eul_sorted_modules sorted by addresses
///-----------------------------------------------------------------------------

LispRef eul_sort_modules()
{
    LispRef vec = TABLE_ENTRIES(eul_modules);
    int n = eul_int_as_c_int(object_size(vec));
    eul_nmodules = eul_int_as_c_int(TABLE_POPULATION(eul_modules));
    eul_sorted_modules = (LispRef *) gc_malloc(eul_nmodules*sizeof(LispRef));

    // Initialize the new vector of module vectors
    LispRef entry;
    int j = 0;
    for (int i = 0; i < n; i++)
    {
        if (IS_TABLE_ENTRY(entry = slot_ref(vec, i)))
        {
            WITH_DEBUG
            (
                fprintf
                (
                    stderr,
                    "entry: %"ptrIntPM"d %s\n",
                    (ptrInt)TABLE_ENTRY_VALUE(entry),
                    (char *)TABLE_ENTRY_KEY(entry)
                )
            );
            eul_sorted_modules[j++] = entry;
        }
    }

    qsort
    (
        eul_sorted_modules,
        eul_nmodules,
        sizeof(LispRef),
        (int (*)(const void *, const void *))compare_module_vectors
    );

    for (int j = 0; j < eul_nmodules; j++)
    {
        entry = eul_sorted_modules[j];
        WITH_DEBUG
        (
            fprintf
            (
                stderr,
                "new entry: %"ptrIntPM"d %s\n",
                (ptrInt)TABLE_ENTRY_VALUE(entry),
                (char *)TABLE_ENTRY_KEY(entry)
            )
        );
    }

    return eul_nil;
}


///-----------------------------------------------------------------------------
/// Get binding location using a ptr: #(obj module_name index)
/// obj is () if the binding is located in one of the standard modules
/// e.g. contained in level1
///-----------------------------------------------------------------------------

LispRef eul_get_binding_location(LispRef *ptr, LispRef std_modules)
{
    ptrInt addr1 = (ptrInt)ptr;
    LispRef obj = *ptr;

    ptrInt addr2;
    LispRef entry;
    for (int j = eul_nmodules - 1; j >= 0; j--)
    {
        entry = eul_sorted_modules[j];
        addr2 = (ptrInt)TABLE_ENTRY_VALUE(entry);
        WITH_DEBUG
        (
            fprintf
            (
                stderr,
                "if %"ptrIntPM"d <= %"ptrIntPM"d (%s) break\n", addr1, addr2,
                (char *)TABLE_ENTRY_KEY(entry)
            );
        )

        if (addr1 >= addr2)
        {
            break;
        }
    }

    if (addr1 < addr2)
    {
        // binding not valid
        return eul_nil;
    }

    char *module_name = (char *)TABLE_ENTRY_KEY(entry);
    ptrInt index = (addr1 - addr2)/ptrNBytes;

    WITH_DEBUG
    (
        fprintf
        (
            stderr,
            "!!!Revealed binding module: %s index: %"ptrIntPM"d\n",
            module_name,
            index
        )
    );
    WITH_DEBUG(fprintf(stderr, "!!!Revealed binding obj: "));
    WITH_DEBUG(fprint_ref(stderr, obj));
    WITH_DEBUG(fprintf(stderr, "\n"));

    LispRef sym;
    eul_intern_symbol(sym, module_name);
    if (eul_is_object(obj))
    {
        int n = eul_int_as_c_int(eul_size_of(std_modules));
        for (int i = 0; i < n; i++)
        {
            if (eul_slot_ref(std_modules, i) == sym)
            {
                obj = eul_nil;
                break;
            }
        }
    }
    else
    {
        obj = eul_nil;
    }

    LispRef res;
    eul_allocate_vector(res, 3, eul_nil);
    eul_slot_ref(res, 0) = obj;
    eul_slot_ref(res, 1) = sym;
    eul_slot_ref(res, 2) = c_int_as_eul_int(index);

    return res;
}


///-----------------------------------------------------------------------------
/// Reveal the binding references of bytevectors
/// Returns a list of #(obj module_name index) for each reference
/// obj is () if the binding is located in one of the standard modules
/// e.g. contained in level1
///-----------------------------------------------------------------------------

#define rel_pc(cv) ((uPtrInt) (pc - (Instruction *) cv))
#define align(cv, pc) (pc += 3 + (4 - (rel_pc(cv)%4)))
#define binding_ref(pc) (*((LispRef **)(pc - 3)))

LispRef eul_raw_bytevector_refs(Instruction *code, int n, LispRef std_modules)
{
    if (eul_sorted_modules == NULL)
    {
        eul_sort_modules();
    }

    Instruction *pc = code;
    LispRef res = eul_nil;

    while (1)
    {
        if (pc < (code + n))
        {
            Instruction instr = *pc;

            WITH_DEBUG(fprintf(stderr, ">>instr: %i (%x)\n", instr, instr));
            WITH_DEBUG
            (
                fprintf
                (
                    stderr,
                    "pc: %"ptrIntPM"i (%"ptrIntPM"x)\n",
                    (ptrInt)pc,
                    (ptrInt)pc)
            );

            int inlined_arg_size = eul_instr_inlined_arg_size(instr);

            WITH_DEBUG
            (
                fprintf
                (
                    stderr,
                    "inlined_arg_size: %i\n",
                    inlined_arg_size
                )
            );

            if (inlined_arg_size == 4)
            {
                align(code, pc);
                switch (instr)
                {
                    case BC_STATIC_REF:
                    case BC_BINDING_REF:
                    case BC_SET_BINDING_REF:
                    case BC_SET_AND_GET_BINDING_REF:
                    {
                        LispRef ref = eul_get_binding_location
                        (
                            binding_ref(pc),
                            std_modules
                        );
                        eul_allocate_cons(res, ref, res);
                    }
                }
            }
            else
            {
                pc += inlined_arg_size;
            }
        }
        else
        {
            break;
        }

        ++pc;
    }

    return res;
}


LispRef eul_lambda_refs(LispRef fun, LispRef std_modules)
{
    Instruction *code = (Instruction *) LAMBDA_CODE(fun);
    int n = 4*eul_int_as_c_int(object_size(fun))/ptrNBytes;
    return eul_raw_bytevector_refs(code, n, std_modules);
}


LispRef eul_bytevector_refs(LispRef bv, LispRef std_modules)
{
    Instruction *code = (Instruction *) BYTEVECTOR_DATA(bv);
    int n = 4*eul_int_as_c_int(object_size(bv))/ptrNBytes;
    return eul_raw_bytevector_refs(code, n, std_modules);
}


///-----------------------------------------------------------------------------
/// Relink the binding references of bytevectors
///-----------------------------------------------------------------------------

LispRef eul_link_raw_bytevector_refs(Instruction *code, int n, LispRef refs)
{
    Instruction *pc = code;

    while (1)
    {
        if (pc < (code + n))
        {
            Instruction instr = *pc;

            WITH_DEBUG(fprintf(stderr, ">>instr: %i (%x)\n", instr, instr));

            WITH_DEBUG
            (
                fprintf
                (
                    stderr,
                    "pc: %"ptrIntPM"i (%"ptrIntPM"x)\n",
                    (ptrInt)pc,
                    (ptrInt)pc
                )
            );

            int inlined_arg_size = eul_instr_inlined_arg_size(instr);

            WITH_DEBUG
            (
                fprintf
                (
                    stderr,
                    "inlined_arg_size: %i\n",
                    inlined_arg_size
                )
            );

            if (inlined_arg_size == 4)
            {
                align(code, pc);
                switch (instr)
                {
                    case BC_STATIC_REF:
                    case BC_BINDING_REF:
                    case BC_SET_BINDING_REF:
                    case BC_SET_AND_GET_BINDING_REF:
                    {
                        LispRef ref = eul_car(refs);
                        WITH_DEBUG(fprintf(stderr, "!!!Ref: "));
                        WITH_DEBUG(fprint_ref(stderr, ref));
                        WITH_DEBUG(fprintf(stderr, "\n"));
                        if (ref == eul_nil)
                        {
                            // binding not valid
                            return eul_nil;
                        }
                        LispRef obj = eul_slot_ref(ref, 0);
                        char *module_name =
                            eul_symbol_as_c_string(eul_slot_ref(ref, 1));
                        int index = eul_int_as_c_int(eul_slot_ref(ref, 2));
                        WITH_DEBUG
                        (
                            fprintf
                            (
                                stderr,
                                "!!!Link binding module: %s index: %d\n",
                                module_name,
                                index
                            )
                        );
                        if (obj == eul_nil)
                        {
                            obj = eul_dyn_binding_ref(module_name, index);
                        }
                        WITH_DEBUG(fprintf(stderr, "!!!Link binding obj: "));
                        WITH_DEBUG(fprint_ref(stderr, obj));
                        WITH_DEBUG(fprintf(stderr, "\n"));
                        LispRef bindings =
                            eul_fast_table_ref(eul_modules, module_name);
                        LispRef* ptr = ((LispRef *) bindings) + index;
                        binding_ref(pc) = ptr;
                        refs = eul_cdr(refs);
                    }
                }
            }
            else
            {
                pc += inlined_arg_size;
            }
        }
        else
        {
            break;
        }

        ++pc;
    }
    return eul_true;
}


LispRef eul_link_lambda_refs(LispRef fun, LispRef refs)
{
    Instruction *code = (Instruction *) LAMBDA_CODE(fun);
    int n = 4*eul_int_as_c_int(object_size(fun))/ptrNBytes;
    return eul_link_raw_bytevector_refs(code, n, refs);
}


LispRef eul_link_bytevector_refs(LispRef bv, LispRef refs)
{
    Instruction *code = (Instruction *) BYTEVECTOR_DATA(bv);
    int n = 4*eul_int_as_c_int(object_size(bv))/ptrNBytes;
    return eul_link_raw_bytevector_refs(code, n, refs);
}


///-----------------------------------------------------------------------------
/// Test
///-----------------------------------------------------------------------------

extern LispRef compare_bindings[];
extern LispRef stream_bindings[];

LispRef eul_test_migrate()
{
    eul_sort_modules();
    eul_get_binding_location(B(compare, 6), eul_nil);
    eul_get_binding_location(B(stream, 0), eul_nil);
    return eul_nil;
}


///-----------------------------------------------------------------------------
