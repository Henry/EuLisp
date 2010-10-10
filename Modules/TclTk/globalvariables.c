/// Copyright 1997 J. Garcia & University of Bath
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
/// Title: Global variables
///  Library: tcltk
///  Authors: J. Garcia
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#include "globalvariables.h"

// The main window for the application.
// If NULL then the application no longer exists.
Tk_Window mainWindow;

// Interpreter for this application.
Tcl_Interp *interp;

// Maximum length of script received by the tk-bind function
const uint maxScriptLen = 100;

///-----------------------------------------------------------------------------
