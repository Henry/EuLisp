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
///  Title:
///  Library: Runtime
///-----------------------------------------------------------------------------
#ifndef EU2C_SYS_H
#define EU2C_SYS_H

#include <setjmp.h> // to get basic things for continuations and unwind-protect
#include <signal.h>

// -----------------------------------------------------------------------------

#ifndef STRUCTURE_ALIGNMENT
#define STRUCTURE_ALIGNMENT
#endif

#define V_LITERAL(el_type, name, length)                                       \
    struct name{unsigned int L; void *C; el_type I[length];} name

#define S_LITERAL(type, name)                                                  \
    struct name{unsigned int unused; void *C; struct type I;} name

#define LITERAL(name)                                                          \
    struct name name STRUCTURE_ALIGNMENT
#define STAG(class)                                                            \
    0, class

#define VTAG(length, class)                                                    \
    length, class

// Define empty_list only in the most basic basic system
#ifdef BASIC_SYSTEM
extern struct empty_list{unsigned int unused; void *C; long I;} empty_list;
#else
struct empty_list{unsigned int unused; void *C; long I;} empty_list;
#endif
#define SET_CLASS_OF_NIL empty_list.C=&c__null__eulisp0.I;


#ifdef NIL_REGISTER
register long* NIL NIL_REGISTER;
#define SET_NIL NIL=&empty_list.I
#define XCALL(call) ({register typeof(call) AUX=call; SET_NIL; AUX;})
#define YCALL(call) (call, SET_NIL)
#else
#define NIL &empty_list.I
#define SET_NIL
#define XCALL(call) call
#define YCALL(call) call
#endif

#define SMALL_INT_SKIP

#ifndef LSHIFTR
extern int LSHIFTR(int i, int s);
#endif
#ifndef ASHIFTR
extern int ASHIFTR(int i, int s);
#endif

#define GET_JMPBUF(bufptr) *bufptr

typedef void* (*function)();
typedef void (*sighandler_t) (int);

///-----------------------------------------------------------------------------
#endif // EU2C_SYS_H
///-----------------------------------------------------------------------------
