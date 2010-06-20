//  Copyright (c) 1988, by David Michael Betz.
//  Copyright (c) 1994, by Russell Bradford.
//  All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'EuXLisp'
///-----------------------------------------------------------------------------
///  File: osdefs.h
///  Description: extern declarations for machine specific functions
///-----------------------------------------------------------------------------
#ifndef OSDEFS_H
#define OSDEFS_H

#ifdef UNIX
extern LVAL xsystem();
#endif

///-----------------------------------------------------------------------------
#endif // OSDEFS_H
///-----------------------------------------------------------------------------
