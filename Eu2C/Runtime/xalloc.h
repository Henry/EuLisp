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
/// Title: xalloc internal definitions
///  Library: Runtime
///  Authors: Jens Bimberg
///  Description: xalloc internal definitions
///    contains macros to be used in xalloc functions since they are separeted
///    into several modules. NOT to be used by outside user
///-----------------------------------------------------------------------------
#ifndef XALLOC_MISC_H
#define XALLOC_MISC_H

#include <string.h>

#include "xalloc_arch.h"
#include "xalloc_conf.h"
#include "xalloc_user.h"   // def's of STSS ..., prototypes

#ifndef NOTHREADS
#include "thread.h"
#else
#define ENABLE_SCHEDULE()
#define DISABLE_SCHEDULE()
#endif

#ifdef MULTI_FSL
    // A closer connection between threads and xalloc takes place
    #include "xalloc_th.h"
#endif

///-----------------------------------------------------------------------------
/// Some simple sizes and macros to be found anywhere
///-----------------------------------------------------------------------------

#define BITS_PER_BYTE 8
#define LOG_BITS_PER_BYTE 3
#define LOG_BYTES_PER_WORD (LOG_WORD_SIZE - LOG_BITS_PER_BYTE)

#define byte2word(s) (((s) + (BYTES_PER_WORD - 1)) >> LOG_BYTES_PER_WORD)
#define word2byte(s) ((s) << LOG_BYTES_PER_WORD)

#define isset(w,b)      ((w) & (1<<(b)))
#define set(w,b)        ((w) |= (1<<(b)))
#define reset(w,b)      ((w) &= ~(1<<(b)))

#define max(x,y)        (((x) > (y))? (x) : (y))
#define min(x,y)        (((x) < (y))? (x) : (y))

#ifndef NULL
#define NULL    0
#endif

///-----------------------------------------------------------------------------
/// Type-definitions
///-----------------------------------------------------------------------------
#define CARDMASK        (CARDSIZE - 1)
#define CARD_ADDR(p)    ((card *) ((long)(p) & ~CARDMASK))

#define NB_OF_MARK_WORDS (byte2word(CARDSIZE)/WORD_SIZE)

typedef struct _CardHeader
{
    struct _Card    **card_index;           /* ptr to card reference-list*/
    long            object_size;            /* size in words */
    long            object_byte_size;       /* object-size in bytes */
    long            card_nb;                /* nb of basic cards */
    long            type_descriptor;
    long            object_mask;            /* mask for fast pointer test */
    struct _Card    *next_card;             /* to link cards together */
    long            markbits[NB_OF_MARK_WORDS];
} CardHeader;

#define root            header.card_index
#define size            header.object_size
#define bytesize        header.object_byte_size
#define cardnum         header.card_nb
#define type_dscr       header.type_descriptor
#define mask            header.object_mask
#define next            header.next_card
#define mkarea          header.markbits

#define USERSPACE       (byte2word(CARDSIZE - sizeof(CardHeader)))

typedef struct _Card
{
    CardHeader header;
    long    userspace[USERSPACE];
} card;

#define basiccards(s)   (((s) <= USERSPACE) ? 1                                \
    : (((s) - USERSPACE) / (byte2word(CARDSIZE)) + 2))

///-----------------------------------------------------------------------------
/// Macros to handle information on cards
///-----------------------------------------------------------------------------

#define get_size(c)     ((c)->size)
#define get_mask(c)     ((c)->mask)
#define get_tdscr(c)    ((c)->type_dscr)
#define get_cardnum(c)  ((c)->cardnum)

#define ms_card(c)      (get_size(c) == 0)
#define mt_card(c)      (get_tdscr(c) == 0)

#define get_ms_real_size(p)     (word2byte(*((long *)(p)-1)-1))
#define get_real_size(c)        ((c)->bytesize)

#define put_size(c,s)           ((c)->size = (s))
#define put_tdscr(c,t)          ((c)->type_dscr = (t))
#define put_cardnum(c,n)        ((c)->cardnum = (n))
#define put_real_size(c,s)      ((c)->bytesize = (s))

#define into_card_list(cd,c)    ((c)->next = get_card_d(cd), store_card_d(cd,c))
#define into_full_list(cd,c)    ((c)->next = get_full_d(cd), store_full_d(cd,c))

#define off_card_list(cd,c)     store_card_d((cd), (c)->next)

// we need information about marking in xalloc.c, too,
// because we use markbits another way on STMS

#define markword(c,p)   (((c)->mkarea)[((long)(p) & CARDMASK)>>                \
    (LOG_WORD_SIZE+LOG_BYTES_PER_WORD)])
#define markbit(p)      (((long)(p)>>LOG_BYTES_PER_WORD) & (WORD_SIZE-1))

#define ismarked(c,p)   isset(markword(c,p), markbit(p))
#define mark(c,p)       set(markword(c,p), markbit(p))
#define unmark(c,p)     reset(markword(c,p), markbit(p))

#define large_unmark(c) ((c)->mkarea[0] = 0)    // faster than mask first bit

#ifndef __sparc__
#define bzero(p, n)     memset((p), 0, (n))
#endif

#define clear_marks(c)  bzero((c)->mkarea, NB_OF_MARK_WORDS * sizeof(long))

///-----------------------------------------------------------------------------
/// Macros to test values to be pointers
///-----------------------------------------------------------------------------
#define is_pointer(p)   (((long)(p) > 0) && (((long)(p) & 3) == 0))

#define p_in_heap(p)    (((((long)(p)) & 3) == 0)                              \
    && ((p) > HEAPBEGIN)                                                       \
    && ((p) < HEAPEND))

#define living_card(c)  (((long)((c)->root) >= (long)first_card_index)         \
    && ((long)((c)->root) < (long)last_card_index)                             \
    && (*((c)->root) == (c)))


#ifdef USE_LARGE
#ifdef OPTIMIZED_POINTER_TESTS

#define consistent_pointer(c,p,s,m) ((((long)(p) & (m)) == 0) ||               \
    ((((long *)(c) + byte2word(CARDSIZE) - (p)) % (s)) == 0) ||                \
    (((s) > USERSPACE) && ((p) == ((c)->userspace))))
#define obj_begin(c,p,s,m) (((s) > USERSPACE) ? (c)->userspace :               \
    ((m) != DEFAULT_MASK) ? (long)(p) & ~(m) :                                 \
    (p) + 1 + (((long *)(c) + byte2word(CARDSIZE) - 1 - (p)) % (s)) - (s))

#else   // argument m is never used

#define consistent_pointer(c,p,s,m) (                                          \
        ((((long *)(c) + byte2word(CARDSIZE) - (p)) % (s)) == 0) ||            \
        (((s) > USERSPACE) && ((p) == ((c)->userspace))))
#define obj_begin(c,p,s,m) (((s) > USERSPACE) ? (c)->userspace :               \
    (p) + 1 + (((long *)(c) + byte2word(CARDSIZE) - 1 - (p)) % (s)) - (s))

#endif  // OPTIMIZED_POINTER_TESTS

#else   // no large objects

#ifdef OPTIMIZED_POINTER_TESTS

#define consistent_pointer(c,p,s,m) ((((long)(p) & (m)) == 0) ||               \
    ((((long *)(c) + byte2word(CARDSIZE) - (p)) % (s)) == 0))
#define obj_begin(c,p,s,m) (((m) != DEFAULT_MASK) ? (long)(p) & ~(m) :         \
    (p) + 1 + (((long *)(c) + byte2word(CARDSIZE) - 1 - (p)) % (s)) - (s))

#else   // argument m is never used

#define consistent_pointer(c,p,s,m) (                                          \
        (((long *)(c) + byte2word(CARDSIZE) - (p)) % (s)) == 0)
#define obj_begin(c,p,s,m) (                                                   \
        (p) + 1 + (((long *)(c) + byte2word(CARDSIZE) - 1 - (p)) % (s)) - (s))

#endif  // OPTIMIZED_POINTER_TESTS

#endif  // USE_LARGE

#ifdef  USE_STMS
#define consistent_ms_pointer(c,p)      ismarked((c),(p)-1)
#endif  // USE_STMS


// for importer here the defs of our arrays. don't want to deal with them
// during garbage_collections, so enabling to skip them easily. B/W again...

struct _gcarrays
{
    card           *S_Cards[HIGHEST_CARDDSCR + 1];
    #ifdef SKIP_SWEEP
    card           *S_Fulls[HIGHEST_CARDDSCR + 1];
    #endif
    #ifndef MULTI_FSL
    long           *S_Freepts[HIGHEST_CARDDSCR + 1];
    #endif
    TypeDscr        S_Types[HIGHEST_CARDDSCR + 1];
    long            S_Sizes[HIGHEST_CARDDSCR + 1];
    unsigned long   S_UTypes[HIGHEST_TYPEDSCR + 1];
    void            (*S_MkFcns[HIGHEST_TYPEDSCR + 1]) ();
    card           *S_usedcards[MAX_NUM_OF_CARDS];
} gcarrays;

#define Cards           gcarrays.S_Cards
#define Fulls           gcarrays.S_Fulls
#define Freepts         gcarrays.S_Freepts
#define Types           gcarrays.S_Types
#define Sizes           gcarrays.S_Sizes
#define UTypes          gcarrays.S_UTypes
#define MkFcns          gcarrays.S_MkFcns
#define usedcards       gcarrays.S_usedcards

#define gcarraybegin    (long *)(&gcarrays)
#define gcarrayend      (long *)((char *)(&gcarrays) + sizeof(gcarrays))

card          **used_cards_end;

CardDscr        last_used_card;
TypeDscr        last_used_type;

#define is_tdscr(td)    (((td)>0) && ((td)<last_used_type))
#define is_cdscr(cd)    (((cd)>=0) && ((cd)<last_used_card))

///-----------------------------------------------------------------------------
/// Macros to handle information hold in these arrays
///-----------------------------------------------------------------------------

#ifndef MULTI_FSL
#define get_free_object(cd)     (Freepts[cd])
#define get_freept_addr(cd)     (&Freepts[cd])
#define store_free_object(cd,o) Freepts[cd] = (o)
#endif

#define get_type_d(cd)          (Types[cd])
#define get_size_d(cd)          (Sizes[cd])
#define get_card_d(cd)          (Cards[cd])
#define get_full_d(cd)          (Fulls[cd])

#define ms_card_d(cd)           (! get_size_d(cd))
#define mt_card_d(cd)           (! get_type_d(cd))

#define store_type_d(cd,t)      Types[cd] = (t)
#define store_size_d(cd,s)      Sizes[cd] = (s)
#define store_card_d(cd,c)      Cards[cd] = (c)
#define store_full_d(cd,c)      Fulls[cd] = (c)

#define store_mark_fcn(td,f)    (MkFcns[td] = f)
#define store_user_type(td, t)  (UTypes[td] = t)

#define get_mark_fcn(td)        (MkFcns[td])
#define get_user_type(td)       (UTypes[td])

#define first_card_index        usedcards
#define last_card_index         used_cards_end
#define next_card_index(c)      ++c
#define prev_card_index(c)      --c

#define MT_ALIGN_WORDS          byte2word(MT_ALIGNMENT)
#define MT_AL_MASK              (MT_ALIGN_WORDS - 1)

// increase x up to a multiple of MT_ALIGN_WORDS and by another MT_ALIGN_WORDS
// words to hold type information
#define MT_ALIGN(x)     (((x + (MT_AL_MASK)) & ~(MT_AL_MASK)) + MT_ALIGN_WORDS)

///-----------------------------------------------------------------------------
/// Macros to do some jobs needed in different functions
///-----------------------------------------------------------------------------

#define register_card(c)        (c)->root = last_card_index,                   \
    *last_card_index = (c),                                                    \
    next_card_index(last_card_index)

#define DEFAULT_MASK    -1
#define set_default_mask(c)     ((c)->mask = DEFAULT_MASK)

#ifdef OPTIMIZED_POINTER_TESTS
#define set_mask(c)                                                            \
    {  switch(get_size(c))                                                     \
        { case 1:                                                              \
                c->mask = 0x3;                                                 \
                break;                                                         \
            case 2:                                                            \
                c->mask = 0x7;                                                 \
                break;                                                         \
            case 4:                                                            \
                c->mask = 0xf;                                                 \
                break;                                                         \
            case 8:                                                            \
                c->mask = 0x1f;                                                \
                break;                                                         \
            default:                                                           \
                set_default_mask(c);}                                          \
    }
#else
#define set_mask(c)     set_default_mask(c)
#endif

#ifdef  USE_STMS
// variable spaces contain pointer to next space and their own length.
// variable objects of size 1 are not allowed
typedef struct _vobject {
    struct _vobject *vnext;
    long            vlength;
} vobject;

#endif

///-----------------------------------------------------------------------------
/// Vars and prototypes from heap-module
///-----------------------------------------------------------------------------
long    *HEAPBEGIN, *HEAPEND;
long    hincr, curnumcard;
long    inc_heap_size(int noc);
#define inc_heap()      inc_heap_size(hincr)
void    initialize_heap();
long    *new_card();
long    *new_large_card();
void    reclaim_card();

///-----------------------------------------------------------------------------
/// function that is defined in xalloc.c and used in card.c as well
///-----------------------------------------------------------------------------
long * build_free_list();

///-----------------------------------------------------------------------------
/// Global vars for statistics
///-----------------------------------------------------------------------------
int start_nb_of_cards;

///-----------------------------------------------------------------------------
#endif  // XALLOC_MISC_H
///-----------------------------------------------------------------------------
