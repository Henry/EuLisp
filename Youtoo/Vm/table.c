
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
///  Authors: Andreas Kind, Pete Broadbery
///  Description: hash tables
///-----------------------------------------------------------------------------

#include "stdc.h"
#include "config.h"
#include "symbol.h"
#include "tag.h"
#include "shared-mem.h"
#include "register.h"
#include "eul-string.h"
#include "keyword.h"
#include "list.h"
#include "class.h"
#include "operator.h"
#include "fpi.h"
#include "table.h"


///-----------------------------------------------------------------------------
/// Magic numbers from CACM 6/90; extra 1 to avoid arith in hash fn
///-----------------------------------------------------------------------------

const unsigned char hash_table[] =
{
    1, 87, 49, 12, 176, 178, 102, 166, 121, 193, 6, 84, 249, 230, 44, 163,
    14, 197, 213, 181, 161, 85, 218, 80, 64, 239, 24, 226, 236, 142, 38, 200,
    110, 177, 104, 103, 141, 253, 255, 50, 77, 101, 81, 18, 45, 96, 31, 222,
    25, 107, 190, 70, 86, 237, 240, 34, 72, 242, 20, 214, 244, 227, 149, 235,
    97, 234, 57, 22, 60, 250, 82, 175, 208, 5, 127, 199, 111, 62, 135, 248,
    174, 169, 211, 58, 66, 154, 106, 195, 245, 171, 17, 187, 182, 179, 0, 243,
    132, 56, 148, 75, 128, 133, 158, 100, 130, 126, 91, 13, 153, 246, 216, 219,
    119, 68, 223, 78, 83, 88, 201, 99, 122, 11, 92, 32, 136, 114, 52, 10,
    138, 30, 48, 183, 156, 35, 61, 26, 143, 74, 251, 94, 129, 162, 63, 152,
    170, 7, 115, 167, 241, 206, 3, 150, 55, 59, 151, 220, 90, 53, 23, 131,
    125, 173, 15, 238, 79, 95, 89, 16, 105, 137, 225, 224, 217, 160, 37, 123,
    118, 73, 2, 157, 46, 116, 9, 145, 134, 228, 207, 212, 202, 215, 69, 229,
    27, 188, 67, 124, 168, 252, 42, 4, 29, 108, 21, 247, 19, 205, 39, 203,
    233, 40, 186, 147, 198, 192, 155, 33, 164, 191, 98, 204, 165, 180, 117, 76,
    140, 36, 210, 172, 41, 54, 159, 8, 185, 232, 113, 196, 231, 47, 146, 120,
    51, 65, 28, 144, 254, 221, 93, 189, 194, 139, 112, 43, 71, 109, 184, 209,
    // repeat to avoid taking mods.
    1, 87
};


///-----------------------------------------------------------------------------
/// Hash function
///-----------------------------------------------------------------------------

int eul_hash_string(char *key)
{
    unsigned char h1 = 0, h2 = 0, h3 = 0;

    while (*key != '\0')
    {
        h1 = hash_table[h1 ^ (*key)];
        h2 = hash_table[h2 ^ (*key + 1)];
        h3 = hash_table[h3 ^ (*key + 2)];
        key++;
    }
    return (((int)h1 << 16) | ((int)h2 << 8) | (int)h3);
}


int eul_hash_object(LispRef key)
{
    int leaves = 0;
    int res = eul_hash_object_aux(key, &leaves);
    return (res > 0x1fffffff ? res >> 2 : res);
}


#define NUMBER_OF_CONSIDERED_LEAVES (4)

int eul_hash_object_aux(LispRef key, int *leaves)
{
    (*leaves)++;
    if (eul_is_symbol(key))
    {
        return eul_hash_string(eul_symbol_as_c_string(key));
    }
    else if (eul_is_keyword(key))
    {
        return eul_hash_string(eul_keyword_as_c_string(key));
    }
    else if (eul_is_string(key))
    {
        return eul_hash_string(eul_string_as_c_string(key));
    }

    switch (tag_field(key))
    {
        case (FPI_TAG):
            return (ptrInt)key;
        case (CHAR_TAG):
            return (ptrInt)key;
    }

    // () is not a proper leave
    if (eul_null(key))
    {
        (*leaves)--;
        return 0;
    }

    if (eul_is_function(key))
    {
        return (ptrInt)key;
    }

    int n = eul_int_as_c_int(eul_size_of(key));
    if (n == 0)
    {
        return (ptrInt)key;
    }

    // Xor-shift hash results of some leaves in the object structure

    (*leaves)--;

    int m = 0, res = 0, subkey;
    LispRef val;

    while (((*leaves) < NUMBER_OF_CONSIDERED_LEAVES) && (m < n))
    {
        val = eul_slot_ref(key, m++);
        if ((subkey = eul_hash_object_aux(val, leaves)))
        {
            res = (res << 1) ^ ((char)subkey);
        }
    }

    return res;
}


///-----------------------------------------------------------------------------
/// Rehash
///-----------------------------------------------------------------------------

LispRef eul_table_fast_rehash(LispRef tab)
{
    LispRef old_vec = TABLE_ENTRIES(tab);
    if (computed_object_class(old_vec) != PGLOBAL(glob_vector_class))
        return (tab);

    int old_n = eul_int_as_c_int(object_size(old_vec));
    int new_n = old_n * TABLE_FILL_FACTOR;
    int threshold = eul_int_as_c_int(TABLE_THRESHOLD(tab)) * TABLE_FILL_FACTOR;
    TABLE_THRESHOLD(tab) = c_int_as_eul_int(threshold);

    LispRef new_vec;
    eul_allocate_object(new_vec, PGLOBAL(glob_vector_class), new_n, eul_nil);

    for (int j = 0; j < old_n; j++)
    {
        if (IS_TABLE_ENTRY(slot_ref(old_vec, j)))
        {
            LispRef new_entry;

            int i = eul_hash_string((char *)TABLE_ENTRY_KEY(slot_ref(old_vec, j)));
            new_entry = slot_ref(old_vec, j);
            while (slot_ref(new_vec, i % new_n) != eul_nil)
            {
                i++;
            }
            slot_ref(new_vec, i % new_n) = new_entry;
        }
    }

    TABLE_ENTRIES(tab) = new_vec;

    return (tab);
}


///-----------------------------------------------------------------------------
/// Get table entry; key is always a char *
///-----------------------------------------------------------------------------

LispRef eul_fast_table_ref(LispRef tab, char *key)
{
    LispRef vec = TABLE_ENTRIES(tab);

    if (computed_object_class(vec) != PGLOBAL(glob_vector_class))
    {
        return (TABLE_FILL_VALUE(tab));
    }

    int n = eul_int_as_c_int(object_size(vec));
    LispRef *last_ptr = &(slot_ref(vec, n));

    int i = eul_hash_string(key) % n;
    LispRef *ptr = &(slot_ref(vec, i));

    while (1)
    {
        if
        (
            IS_TABLE_ENTRY(*ptr)
         && !strcmp(((char *)TABLE_ENTRY_KEY(*ptr)), key)
        )
        {
            return (TABLE_ENTRY_VALUE(*ptr));
        }
        else if (eul_null(*ptr))
        {
            return (TABLE_FILL_VALUE(tab));
        }
        else if (++ptr == last_ptr)
        {
            ptr = &(slot_ref(vec, 0));
        }
    }
}


///-----------------------------------------------------------------------------
/// Set table entry; key is always a char *
///-----------------------------------------------------------------------------

LispRef eul_fast_table_set(LispRef tab, char *key, LispRef value)
{
    LispRef vec = TABLE_ENTRIES(tab);

    if (computed_object_class(vec) != PGLOBAL(glob_vector_class))
    {
        eul_allocate_object(vec, PGLOBAL(glob_vector_class),
        MIN_TABLE_ENTRIES, eul_nil);
        TABLE_ENTRIES(tab) = vec;
    }

    int n = eul_int_as_c_int(object_size(vec));
    int i = eul_hash_string(key) % n;

    while (1)
    {
        if (!IS_TABLE_ENTRY(slot_ref(vec, i)))
        {
            int population, threshold;

            eul_allocate_table_entry(slot_ref(vec, i), ((LispRef) key), value);
            population = eul_int_as_c_int(TABLE_POPULATION(tab)) + 1;
            threshold = eul_int_as_c_int(TABLE_THRESHOLD(tab));
            TABLE_POPULATION(tab) = c_int_as_eul_int(population);

            if (population == threshold)
            {
                eul_table_fast_rehash(tab);
            }

            return (TABLE_FILL_VALUE(tab));
        }

        /*  else if (((char *) TABLE_ENTRY_KEY(slot_ref(vec, i))) == key) { */
        else if (!strcmp(((char *)TABLE_ENTRY_KEY(slot_ref(vec, i))), key))
        {
            LispRef old = TABLE_ENTRY_VALUE(slot_ref(vec, i));
            TABLE_ENTRY_VALUE(slot_ref(vec, i)) = value;

            return (old);
        }
        else if (++i == n)
        {
            i = 0;
        }
    }
}
