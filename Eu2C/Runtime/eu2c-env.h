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
///  Title: Optimizations for the SPARC architecture
///  Library: Runtime
///-----------------------------------------------------------------------------
#ifndef EU2C_ENV_H
#define EU2C_ENV_H

///-----------------------------------------------------------------------------
/// NIL_REGISTER
///-----------------------------------------------------------------------------

// if set the value of NIL_REGISTER is used in
//     register t_Null NIL NIL_REGISTER;

#ifdef __GNUC__
#ifdef sparc
#define NIL_REGISTER asm("%g6")
#define STRUCTURE_ALIGNMENT __attribute__ ((aligned (8)))

///-----------------------------------------------------------------------------
/// Optimized shift operations
///-----------------------------------------------------------------------------

#define LSHIFTR(i,s)                                                           \
    ({register typeof(i) __value;                                              \
        if (__alignof__(__value)!=4)                                           \
            asm("sll %1,%3,%0;srl %0,%2,%0;srl %0,%3,%0"                       \
            : "=r,r" (__value)                                                 \
            : "r,r" (i),"i,r" (s), "i,i" (32-__alignof__(__value)*8));         \
        else                                                                   \
            asm("srl %1,%2,%0"                                                 \
            : "=r,r" (__value)                                                 \
            : "r,r" (i),"i,r" (s));                                            \
        __value;})

#define ASHIFTR(i,s)                                                           \
    ({register typeof(i) __value;                                              \
        if (__alignof__(__value)!=4)                                           \
            asm("sll %1,%3,%0;sra %0,%2,%0;sra %0,%3,%0"                       \
            : "=r,r" (__value)                                                 \
            : "r,r" (i),"i,r" (s), "i,i" (32-__alignof__(__value)*8));         \
        else                                                                   \
            asm("sra %1,%2,%0"                                                 \
            : "=r,r" (__value)                                                 \
            : "r,r" (i),"i,r" (s));                                            \
        __value;})

#endif
#endif

///-----------------------------------------------------------------------------
#endif // EU2C_ENV_H
///-----------------------------------------------------------------------------
