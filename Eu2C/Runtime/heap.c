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
/// Title: Card allocation and reclaiming functions
///  Library: Xalloc
///  Authors: Jens Bimberg
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include <stdio.h>
#include <unistd.h>
#include "xalloc.h"

///-----------------------------------------------------------------------------
/// Global vars for heap
///-----------------------------------------------------------------------------
long *HEAPBEGIN, *HEAPEND;
long hincr, curnumcard;

// Free card list , used only here and in heap-init.c
long *FCL = NULL;


///-----------------------------------------------------------------------------
/// Global vars for statistics
///-----------------------------------------------------------------------------
int start_nb_of_cards = 0;


///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
// increase Heap by noc cards, but at most up to MAX_NUM_OF_CARDS, change hincr
long inc_heap_size(register long noc)
{
    #ifdef PRINTSIZES
    fprintf(stderr, "Heapsize is now %ld byte.\n", curnumcard*CARDSIZE);
    #endif

    // increase heap by one card at least
    noc = max(1, noc);

    // maximum heap size already reached ?
    if (curnumcard >= MAX_NUM_OF_CARDS)
    {
        return 0;
    }

    // less than noc to get ?
    if (curnumcard + noc > MAX_NUM_OF_CARDS)
    {
        noc = MAX_NUM_OF_CARDS - curnumcard;
    }

    // align the current break at a multiple of CARDSIZE
    long offset = ((long)sbrk(0) & CARDMASK);

    if (offset)
    {
        sbrk(CARDSIZE - offset);
    }

    // Try to get the storage ...impossible ?
    register long *s = (long *)sbrk(noc*CARDSIZE);
    if (s == (long *)-1)
    {
        return 0;
    }

    // succesful
    curnumcard += noc;  // increase current number of cards
    s[0] = (long)FCL;   // put new storage into FCL
    s[1] = noc;
    FCL = s;
    HEAPEND = (long *)sbrk(0);  // increase HEAPEND
    hincr = HMULT*curnumcard/HDIFF; // update heap_increment

    #ifdef PRINTSIZES
    fprintf(stderr, "Heapsize is now %ld byte.\n", curnumcard*CARDSIZE);
    #endif

    return noc;
}


long *new_card()
{
    if (FCL == NULL)
    {
        return NULL;  // FCL empty
    }
    if (FCL[1] > 1)
    {
        FCL[1]--;       // leave all but one card there
        long *c = (&FCL[FCL[1]*byte2word(CARDSIZE)]);

        return c;
    }
    else
    {
        long *c = FCL;        // only one card left
        FCL = (long *)FCL[0];   // connect parts of FCL
        return c;
    }
}


#ifdef USE_LARGE

long *new_large_card(register long n)
{
    register long **last = &FCL;
    register long *act = 0;

    while ((act = *last))
    {
        register long df = 0;
        if ((df = act[1] - n) > 0)
        {
            act[1] = df;        // leave some cards there
            return &act[df*byte2word(CARDSIZE)];
        }

        if (df == 0)
        {
            *last = (long *)*act;       // connect parts of FCL
            return act;
        }

        last = (long **)act;    // df < 0, continue search
    }

    return NULL;
}


// Our FCL is a sorted list in that way, that the part with the highest address
// is pointed to by FCL and the part with the lowest one points to nil itself.
// So inc_card_space is allowed to put the new part of our heap onto the begin
// of this list. (We assume, that any newly allocated part of our heap starts
// at a higher address than the one before.)
void reclaim_card(register long *c, register long n)
{
    register long **last = &FCL;
    register long *act = NULL;

    while ((act = *last))
    {
        if (c > act)
        {
            // c has a higher address than the found part, so we are sure to
            // leave it here
            if (&act[act[1]*byte2word(CARDSIZE)] == c)
            {
                act[1] += n;    // onto the end of this part
                return;
            }
            else
            {
                *last = c;      // new part between two old
                c[0] = (long)act;
                c[1] = n;
                return;
            }
        }

        // c starts at a a lower address than the found part; if it can
        // be connected with the found part, we can leave it here, but
        // normally we have to loop

        if (&c[n*byte2word(CARDSIZE)] == act)
        {
            *last = c;  // onto the begin of this part
            c[0] = act[0];
            c[1] = act[1] + n;

            // now check whether it is possible to connect this with the next
            // smaller part
            if ((act = (long *)c[0]))
            {
                // if next part exists
                if (&act[act[1]*byte2word(CARDSIZE)] == c)
                {
                    *last = act;
                    act[1] += c[1];
                }
            }
            return;
        }
        else
        {
            last = (long **)act;        // continue with next part
        }
    }

    // c is smaller than any part of the FCL
    *last = c;
    c[0] = 0;
    c[1] = n;
}


#else  // Not USE_LARGE

// If not defined USE_LARGE there's no reason for building a sorted list,
// just put the returned card onto the begin of the FCL
void reclaim_card(register long *c)
{
    c[0] = (long)FCL;
    c[1] = 1;
    FCL = c;
}

#endif  // USE_LARGE


///-----------------------------------------------------------------------------
