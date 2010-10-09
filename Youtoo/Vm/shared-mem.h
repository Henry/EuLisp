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
///  Title: global variables
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Keith Playford, Andreas Kind
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------

#ifndef SHARED_MEM_H
#define SHARED_MEM_H

///-----------------------------------------------------------------------------
/// Global variable access and declaration
///-----------------------------------------------------------------------------

#define DEFINE_PGLOBAL(loc) LispRef loc
#define PGLOBAL_EXTERN(loc) extern LispRef loc
#define PGLOBAL_EXTERN_CLASS(loc) extern LispRef loc[]
#define PGLOBAL(loc) loc

///-----------------------------------------------------------------------------
#endif // SHARED_MEM_H
///-----------------------------------------------------------------------------
