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
///  Title: characters
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#ifndef CHARACTER_H
#define CHARACTER_H

///-----------------------------------------------------------------------------
/// Initialization
///-----------------------------------------------------------------------------
extern void eul_initialize_char();

///-----------------------------------------------------------------------------
/// Character access
///-----------------------------------------------------------------------------
#define char_value(ref) (((ptrInt)ref)>>TAG_BITS)
#define eul_char_as_c_char(x) char_value(x)
#define c_char_as_eul_char(x) ((LispRef) ((((ptrInt)(x))<<TAG_BITS)|CHAR_TAG))

///-----------------------------------------------------------------------------
/// Character allocation
///-----------------------------------------------------------------------------
#define eul_allocate_char(loc, x) (loc) = c_char_as_eul_char(x)

///-----------------------------------------------------------------------------
#endif // CHARACTER_H
///-----------------------------------------------------------------------------
