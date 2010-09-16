/// Copyright 1988 David Michael Betz
/// Copyright 1994 Russell Bradford
/// Copyright 2010 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                           EuLisp System 'EuXLisp'
///-----------------------------------------------------------------------------
//
//  EuXLisp is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
///-----------------------------------------------------------------------------
///  Title: Definitions for classes and objects
///-----------------------------------------------------------------------------
#ifndef XSOBJ_H
#define XSOBJ_H

// c.f. xloinit in csobj.c
#define CNAME      1
#define SUPERCLASS 2
#define CPL        3
#define SLOTS      4
#define KEYWORDS   5
#define SUBCLASSES 6
#define INSTSIZE   7
#define ABSTRACTP  8

#define CLASSSIZE  8

extern LVAL object, class, simple_class, class_vector;

///-----------------------------------------------------------------------------
#endif // XSOBJ_H
///-----------------------------------------------------------------------------
