/// Copyright 1994-2010 Fraunhofer ISST
/// Copyright 2010 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                           EuLisp System 'Eu2C'
///-----------------------------------------------------------------------------
//
//  Eu2C is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
///-----------------------------------------------------------------------------
/// Title: Heap initialization
///  Library: Xalloc
///  Authors: E. Ulrich Kriegel
///  Maintainer: Henry G. Weller
///  Description:
//    Contains only the heap initialization function to be recompiled with
//    -DSTART_NUM_OF_CARDS=nnn if necessary
///-----------------------------------------------------------------------------
#include <stdio.h>
#include <unistd.h>
#include "xalloc.h"

// Free card list , used only here and in heap.c
extern long *FCL;

// get first part of heap
void initialize_heap()
{
    curnumcard = 0;
    hincr = START_NUM_OF_CARDS;
    start_nb_of_cards = START_NUM_OF_CARDS;

    // Ensure that heap starts above 64 K boundary. unfortunately, brk() is not
    // Available on some arch's
    void *current_break = NULL;
    if ((current_break = sbrk(0)) < (void *)MINHEAPBEGIN)
    {
        sbrk(MINHEAPBEGIN - (long)current_break);
    }

    // expanded to inc_heap_size(hincr)
    inc_heap();

    // set HEAPBEGIN to the address of the first Card in
    // the free card list
    HEAPBEGIN = FCL;
}


///-----------------------------------------------------------------------------
