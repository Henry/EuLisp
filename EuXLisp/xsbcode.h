//  Copyright (c) 1988, by David Michael Betz.
//  Copyright (c) 1994, by Russell Bradford.
//  All rights reserved.
///-----------------------------------------------------------------------------
/// ---                 EuLisp System 'EuXLisp'
///-----------------------------------------------------------------------------
///  File: xsbcode.h
///  Description: xscheme compiler byte code definitions
///-----------------------------------------------------------------------------
#ifndef XSBCODE_H
#define XSBCODE_H

#define OP_BRT          0x01    // branch on true
#define OP_BRF          0x02    // branch on false
#define OP_BR           0x03    // branch unconditionally
#define OP_LIT          0x04    // load literal
#define OP_GREF         0x05    // global symbol value
#define OP_GSET         0x06    // set global symbol value
#define OP_EREF         0x09    // environment variable value
#define OP_ESET         0x0A    // set environment variable value
#define OP_SAVE         0x0B    // save a continuation
#define OP_CALL         0x0C    // call a function
#define OP_RETURN       0x0D    // return from a function
#define OP_T            0x0E    // load 'val' with t
#define OP_NIL          0x0F    // load 'val' with nil
#define OP_PUSH         0x10    // push the 'val' register
#define OP_CLOSE        0x11    // create a closure

#define OP_FRAME        0x12    // create a new enviroment frame
#define OP_MVARG        0x13    // move required argument to frame slot
#define OP_MVOARG       0x14    // move optional argument to frame slot
#define OP_MVRARG       0x15    // build rest argument and move to frame slot
#define OP_ADROP        0x19    // drop the rest of the arguments
#define OP_ALAST        0x1A    // make sure there are no more arguments
#define OP_DELAY        0x1B    // create a promise

#define OP_AREF         0x1C    // access a variable in an environment
#define OP_ASET         0x1D    // set a variable in an environment

#define OP_ATOM         0x1E    // atom predicate
#define OP_EQ           0x1F    // eq predicate
#define OP_NULL         0x20    // null? (or not) predicate
#define OP_CONS         0x21    // cons
#define OP_CAR          0x22    // car
#define OP_CDR          0x23    // cdr
#define OP_SETCAR       0x24    // set-car!
#define OP_SETCDR       0x25    // set-cdr!

// long offsets for large modules
#define L_OFF           0x30
#define OP_GREFL        0x35    // global symbol value
#define OP_GSETL        0x36    // set global symbol value
#define OP_EREFL        0x39    // environment variable value
#define OP_ESETL        0x3A    // set environment variable value
#define OP_LITL         0x34    // load literal

#define OP_ADD          0x40    // add two numeric expressions
#define OP_SUB          0x41    // subtract two numeric expressions
#define OP_MUL          0x42    // multiply two numeric expressions
#define OP_QUO          0x43    // divide two integer expressions
#define OP_LSS          0x44    // less than
#define OP_EQL          0x45    // equal to
#define OP_GTR          0x46    // greater than

#define OP_CLASSOF      0x47    // class-of
#define OP_CNM          0x48    // call-next-method
#define OP_ADDMETH      0x49    // add-method

#define OP_GETIVAR      0x4A    // get slot
#define OP_SETIVAR      0x4B    // set slot

#define OP_GET          0x4C    // symbol plist
#define OP_PUT          0x4D    // symbol plist
#define OP_CURMOD       0x50    // current-module
#define OP_CONSP        0x51    // cons?
#define OP_SYMBOLP      0x52    // symbol?
#define OP_VECTORP      0x53    // vector?
#define OP_APPEND       0x54    // append of two args
#define OP_LIST         0x55    // list of two args
#define OP_LENGTH       0x56    // length of a list
#define OP_REVERSE      0x57
#define OP_CAAR         0x5C
#define OP_CADR         0x5D
#define OP_CDAR         0x5E
#define OP_CDDR         0x5F

#define OP_GETSYNTAX    0x60    // symbol macro
#define OP_PUTSYNTAX    0x61

#define OP_DIV          0x62    // division

#ifndef NO_CHECK_REF
#define OP_CHECKREF     0x63    // check slot access
#endif

///-----------------------------------------------------------------------------
#endif // XSBCODE_H
///-----------------------------------------------------------------------------
