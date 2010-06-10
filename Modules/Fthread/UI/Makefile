### Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### EuLisp virtual-machine Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Output file names
###-----------------------------------------------------------------------------

TARGET_LIB = libfthread.a
TARGET_LIB_I = libfthread.i

###-----------------------------------------------------------------------------
### Files
###-----------------------------------------------------------------------------

LISP_MODULES = fthread.em flock.em fcsem.em
LISP_SOURCES = fthread.c flock.c fcsem.c
LISP_OBJECTS = fthread.o flock.o fcsem.o
FF_SOURCES = eul-thr.c eul-lock.c fthread.c flock.c fcsem.c
FF_OBJECTS = eul-thr.o eul-lock.o fthread.o flock.o fcsem.o
SOURCES = ${LISP_SOURCES} ${FF_SOURCES}
OBJECTS = ${LISP_OBJECTS} ${FF_OBJECTS}

###-----------------------------------------------------------------------------
### Library object file
###-----------------------------------------------------------------------------

.PHONY: lib
lib:	fthread.a ${FF_OBJECTS}
	${AR} ${TARGET_LIB} ${OBJECTS}
	${RANLIB} ${TARGET_LIB}
	mv -f ${TARGET_LIB_I} $(EUL_LIB_DIR)/${TARGET_LIB_I}
	ln -s $(EUL_LIB_DIR)/${TARGET_LIB_I} ${TARGET_LIB_I}
	mv -f ${TARGET_LIB} $(EUL_LIB_DIR)/${TARGET_LIB}
	ln -s $(EUL_LIB_DIR)/${TARGET_LIB} ${TARGET_LIB}

.SUFFIXES: .a .em
.SUFFIXES: .c .em

.em.a:
	${EUL_DIR}/youtoo ${U2FLAGS} -ar $* -l level1 -recompile
	rm -f ${*}.a
	ln -sf lib${*}.a ${*}.a

.em.c:
	${EUL_DIR}/youtoo ${U2FLAGS} $* -l level1 -l serial -no_recompile

###-----------------------------------------------------------------------------
### Misc
###-----------------------------------------------------------------------------

.PHONY: clean
clean:
	rm -f ${LISP_SOURCES} ${TEST_SOURCES} ${TARGET_LIB_I} \
		${TARGET_LIB} ${OBJECTS}

###-----------------------------------------------------------------------------
