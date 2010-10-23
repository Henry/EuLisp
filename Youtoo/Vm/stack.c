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
/// Title: Stack handling
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "stdc.h"
#include "config.h"
#include "notify.h"
#include "shared-mem.h"
#include "tag.h"
#include "object.h"
#include "register.h"
#include "class.h"
#include "fpi.h"
#include "stack.h"


///-----------------------------------------------------------------------------
/// Allocate stack
///-----------------------------------------------------------------------------
StackRef eul_allocate_stack()
{
    StackRef stk = (StackRef) gc_malloc(sizeof(Stack));

    stk->sp = stk->base;
    stk->segment = eul_nil;
    stk->size = 0;

    return stk;
}


///-----------------------------------------------------------------------------
/// Flush stack buffer
///  Dump the stack buffer's contents to the heap leaving the given
///  amount.
///  Based on Oaklisp code: Copyright (C) 1987, B Pearlmutter & K Lang.
///-----------------------------------------------------------------------------
void flush_stack_buffer(StackRef stk, int hangover)
{
    NOTIFY5
    (
        "{+ sp=%" ptrIntPM "x (base+%" ptrIntPM "d) "
        "*sp=%" ptrIntPM "x hangover=%d pushed=%d ",
        (ptrInt) stk->sp,
        stk->sp - stk->base, (ptrInt) LVPEEK(stk->sp), hangover, stk->size
    );

    int to_flush = (stk->sp - stk->base) - hangover;

    if (to_flush < 0)
    {
        SERIOUS_WARNING0("cannot flush stack buffer");
        return;
    }

    int left_to_flush = to_flush;

    LispRef *src = stk->base;
    LispRef seg;

    while (left_to_flush > 0)
    {
        int size =
            (left_to_flush < MAX_STACK_SEGMENT_SIZE
            ? left_to_flush : MAX_STACK_SEGMENT_SIZE);

        eul_allocate_segment(seg, size);
        STACK_SEGMENT_LAST_SEGMENT(seg) = stk->segment;
        stk->segment = seg;

        for (int i = 0; i < size; ++i)
        {
            slot_ref(seg, STACK_SEGMENT_HEADER_LENGTH + i) = *src++;
        }

        left_to_flush -= size;
    }

    for (int i = 0; i < hangover; ++i)
    {
        stk->base[i] = *src++;
    }

    stk->sp -= to_flush;
    stk->size += to_flush;

    NOTIFY4
    (
        "sp=%" ptrIntPM "x (base+%" ptrIntPM "d) "
        "hangover=%d pushed=%d }\n",
        (ptrInt) stk->sp, stk->sp - stk->base, hangover, stk->size
    );
}


///-----------------------------------------------------------------------------
/// Unflush stack buffer
///  Based on Oaklisp code: Copyright (C) 1987, B Pearlmutter & K Lang.
///-----------------------------------------------------------------------------
void unflush_stack_buffer(StackRef stk, int hangover)
{
    NOTIFY4
    (
        "{- sp=%" ptrIntPM "x (base+%" ptrIntPM "d) "
        "hangover=%d pushed=%d ",
        (ptrInt) stk->sp, stk->sp - stk->base, hangover, stk->size
    );

    int n = STACK_BUFFER_SIZE - hangover;
    int count = stk->sp - stk->base;
    int new_count = count;

    if (stk->size < n - count)
    {
        n = stk->size + count;
    }

    int number_to_pull = 0;
    LispRef seg = stk->segment;

    for
    (   ;
        (
            (new_count < n)
         && (
                new_count
              + fpi_value(object_size(seg))
              - STACK_SEGMENT_HEADER_LENGTH
            ) <= n
        );
        seg = STACK_SEGMENT_LAST_SEGMENT(seg)
    )
    {
        new_count += fpi_value(object_size(seg)) - STACK_SEGMENT_HEADER_LENGTH;
        number_to_pull += 1;
    }

    NOTIFY3("new_count=%d pull=%d n=%d ", new_count, number_to_pull, n);

    LispRef *dest = stk->base + new_count - 1;

    for (int i = count - 1; i >= 0; i--)
    {
        *dest-- = stk->base[i];
    }

    for (seg = stk->segment; number_to_pull > 0; number_to_pull--)
    {
        for
        (
            int i = fpi_value(object_size(seg))
                  - STACK_SEGMENT_HEADER_LENGTH - 1;
            i >= 0;
            i--
        )
        {
            *dest-- = slot_ref(seg, STACK_SEGMENT_HEADER_LENGTH + i);
        }

        seg = STACK_SEGMENT_LAST_SEGMENT(seg);
    }
    stk->segment = seg;
    stk->sp = stk->base + new_count;
    stk->size -= new_count - count;

    NOTIFY5
    (
        " sp=%" ptrIntPM "x (base+%" ptrIntPM "d) "
        "*sp=%" ptrIntPM "x hangover=%d pushed=%d }\n",
        (ptrInt) stk->sp,
        stk->sp - stk->base, (ptrInt) LVPEEK(stk->sp), hangover, stk->size
    );
}


///-----------------------------------------------------------------------------
/// Copy stack segment
///-----------------------------------------------------------------------------
LispRef copy_stack_segment(LispRef seg)
{
    if (seg == eul_nil)
    {
        return eul_nil;
    }

    LispRef tmp_seg = seg;
    int tmp_seg_size =
        fpi_value(object_size(tmp_seg)) - STACK_SEGMENT_HEADER_LENGTH;

    LispRef prev_seg, new_seg;

    eul_allocate_segment(new_seg, tmp_seg_size);

    for
    (
        int i = STACK_SEGMENT_HEADER_LENGTH;
        i < tmp_seg_size + STACK_SEGMENT_HEADER_LENGTH;
        i++
    )
    {
        slot_ref(new_seg, i) = slot_ref(tmp_seg, i);
    }

    LispRef res = new_seg;

    while
    (
        (STACK_SEGMENT_LAST_SEGMENT(tmp_seg) != NULL)
     && (STACK_SEGMENT_LAST_SEGMENT(tmp_seg) != eul_nil)
    )
    {
        prev_seg = new_seg;
        tmp_seg = STACK_SEGMENT_LAST_SEGMENT(tmp_seg);
        tmp_seg_size =
            fpi_value(object_size(tmp_seg)) - STACK_SEGMENT_HEADER_LENGTH;
        eul_allocate_segment(new_seg, tmp_seg_size);

        for
        (
            int i = STACK_SEGMENT_HEADER_LENGTH;
            i < tmp_seg_size + STACK_SEGMENT_HEADER_LENGTH;
            i++
        )
        {
            slot_ref(new_seg, i) = slot_ref(tmp_seg, i);
        }

        STACK_SEGMENT_LAST_SEGMENT(prev_seg) = new_seg;
    }

    return res;
}


///-----------------------------------------------------------------------------
