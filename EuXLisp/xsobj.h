///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'EuXLisp'
///-----------------------------------------------------------------------------
///  File: xsobj.h
///  Description: definitions for classes and objects
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
