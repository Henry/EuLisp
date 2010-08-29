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
///  Title: xalloc configuration file
///  Library: Runtime
///  Authors: Jens Bimberg
///  Description:
///    Configuration of xalloc parameters like number of card descriptors,
///    used types
///-----------------------------------------------------------------------------
#ifndef XALLOC_CONF_H
#define XALLOC_CONF_H

#include <stdio.h>

// Set a boundary for minimal heap address to reduce the chance of a wrong
// pointer identification . the value is only used to set the brk. The actual
// heap begin will be aligned at card boundary
#define MINHEAPBEGIN 0x10000

// Dimensions of some needed arrays and our heap, some constants

// Smallest value to be returned by describe_type. Values
// between 1 and (SMALLEST_TYPEDSCR - 1) may be used for other
// purposes, usage of TypeDscr 0 is not allowed.
#define SMALLEST_TYPEDSCR       1

// Highest value to be returned by describe_type, dimension of
// internal type-array.
#define HIGHEST_TYPEDSCR        4096

// same for cardtypes to be described by describe_card
#define SMALLEST_CARDDSCR       0
#define HIGHEST_CARDDSCR        4096

// There's no reason to set the size of the used Cards
// to the machines page size (as to get by get_page_size()),
// but it must be a power of 2 at least
#define CARDSIZE        0x1000

// Maximum Heap size (in cards of CARDSIZE byte)
// including static cards
#define MAX_NUM_OF_CARDS        1000

// Heapsize at begin of program execution. Negative or zero
// value will be set to 1 by the initialization function
#ifndef START_NUM_OF_CARDS
#define START_NUM_OF_CARDS      16
#endif

// We increase our heap, if - after a garbage collection -
// more than GCMULT/GCDIFF of the current heap is still in use
#define GCMULT  1
#define GCDIFF  3

// When our heap becomes to small we increase it by HMULT/HDIFF
// of the current heap size (but at most up to MAX_NUM_OF_CARDS)
#define HMULT   1
#define HDIFF   1

///-----------------------------------------------------------------------------
/// How to deal with root addresses on static variables
///-----------------------------------------------------------------------------

// define ROOT_SET_IN_USE, if you want to add each global or
// static variable to the root set yourself
// undefine it, when tracer should scan over all data
#define ROOT_SET_IN_USE
#define MAX_ROOT_SET_ENTRIES    8192    // size of rootset

///-----------------------------------------------------------------------------
/// Shall we test array-dimensions and function parameters ?
///-----------------------------------------------------------------------------

// it is recommended to define SECURITY_FIRST when you first
// test a new program, but the memory management works much
// faster without this (can be undefined in each single file)
//#define       SECURITY_FIRST

///-----------------------------------------------------------------------------
/// Shall we use optimized pointer tests ?
///-----------------------------------------------------------------------------

// Use this, when your favorite objects are small single_sized
// ones. Otherwise, optimized pointer tests have no positive
// effect, because normal pointer test will be done after this.
//#define       OPTIMIZED_POINTER_TESTS

///-----------------------------------------------------------------------------
/// Where shall we put our mark stack ?
///-----------------------------------------------------------------------------

// when putting mark stack below the stack pointer
//      - program will exit on stack overflow
//      - mark functions are forbidden to call other functions
// when putting it above the current break
//      - program will crash when heap is exhausted
//      - marking works a bit faster
#define STACK_ON_HEAP

/* we leave a gap of STACKGAP bytes below the machines
 * stack pointer when using stacksegment for our mark stack
 */
#define STACKGAP        1024

///-----------------------------------------------------------------------------
/// Usage of different kinds of cards
///-----------------------------------------------------------------------------

// undefine unused cardtypes here, memory management will be
// built to work as fast as possible by leaving conditions;
// but, of course, all sorts of cards can be used simultaneously

#define USE_STSS        // one card for any size/type-pair

#define USE_MTSS        // one card for different types but same size

#define USE_STMS        // one card for different sizes but same type

#define FAST_STMS       // prefer gc to extensive search in free lists on stms

// Alignment forcing on MTSS-cards, if there's nothing defined
// here we set it to 4 internally, that's enough for pointers
// and long integers; for doubles it should be increased to 8.
// Be sure not to set it to something else than a power of 2 !!
// #define MT_ALIGNMENT 8

// Undefine USE_LARGE, if any used object may be put onto one
// card of CARDSIZE byte. Note, that we actually need 32 byte
// on any card for header information, and one bit per long_word
// for the mark area. Here are some examples for maximum size
// of objects to be put on one single card:
//      CARDSIZE        HEAD+MARKBITS   Max. Objectsize
//      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//      8192            288             7904
//      4096            160             3936
//      2048             96             1952
//      1024             64              960
//       512             48              464
//       256             40              216
// Note, that we increase object sizes by 4 byte
// on MTSS and STMS-cards

#define USE_LARGE

// Are we interested in printing heapsize to stderr (when we increase heap)?
//#define PRINTSIZES

// Are we interested in gc-reports ?
//#define GCREPORTS

///-----------------------------------------------------------------------------
/// What to do on internal errors ?
///-----------------------------------------------------------------------------
#define error(m)        (fprintf(stderr,"%s Good bye!\n",m), exit(0))
#define error1(format,value) (fprintf(stderr,format,value), exit(0))

// skip sweeping phase in gc, sweep during allocs
#define SKIP_SWEEP

///-----------------------------------------------------------------------------
#endif  // XALLOC_CONF_H
///-----------------------------------------------------------------------------
