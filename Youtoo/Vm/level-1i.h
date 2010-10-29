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
/// Title: Fast access to level-1 bindings
///  Library: eulvm (Bytecode Interpreter -- Eutopia)
///  Authors: Andreas Kind
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#ifndef LEVEL_1I_H
#define LEVEL_1I_H

extern LispRef eul_initialize_level_1_tables();
extern LispRef eul_dyn_level_1_binding_ref (LispRef, LispRef);
extern LispRef eul_dyn_level_1_binding_info (LispRef);
extern LispRef eul_dyn_level_1_syntax_binding_info (LispRef);

///-----------------------------------------------------------------------------
#endif // LEVEL_1I_H
///-----------------------------------------------------------------------------
