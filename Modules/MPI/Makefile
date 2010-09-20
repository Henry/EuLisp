### Copyright 1997 A. Kind & University of Bath
### Copyright 2010 Henry G. Weller
###-----------------------------------------------------------------------------
##  This file is part of
### ---                         EuLisp System 'Youtoo'
###-----------------------------------------------------------------------------
##
##  Youtoo is free software: you can redistribute it and/or modify it under the
##  terms of the GNU General Public License version 2 as published by the Free
##  Software Foundation.
##
##  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
##  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
##  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
##  details.
##
##  You should have received a copy of the GNU General Public License along with
##  this program.  If not, see <http://www.gnu.org/licenses/>.
##
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### EuLisp MPI Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------

MPI_CC = mpicc
MPI_DIR = /home/dm2/henry/OpenFOAM/ThirdParty-dev/openmpi-1.4.1/platforms/linux64GccDPOpt
MPI_CFLAGS = -I${MPI_DIR}/include

###-----------------------------------------------------------------------------
### Output file names
###-----------------------------------------------------------------------------

TARGET_LIB = libmpis.a
TARGET_LIB_I = libmpis.i

###-----------------------------------------------------------------------------
### Files
###-----------------------------------------------------------------------------

LISP_MODULES = mpis.em
LISP_SOURCES = mpis.c
LISP_OBJECTS = mpis.o
FF_SOURCES = eul-mpi.c
FF_OBJECTS = eul-mpi.o
SOURCES = ${LISP_SOURCES} ${FF_SOURCES}
OBJECTS = ${LISP_OBJECTS} ${FF_OBJECTS}

TEST_MODULES = test1.em test2.em test3.em test4.em
TEST_SOURCES = test1.c test2.c test3.c test4.c

###-----------------------------------------------------------------------------
### Compiler + Interpreter
###-----------------------------------------------------------------------------

.PHONY: lib
lib:	mpis.a
	${MPI_CC} -c ${CFLAGS} ${MPI_CFLAGS} ${FF_SOURCES}
	${AR} ${TARGET_LIB} ${OBJECTS}
	${RANLIB} ${TARGET_LIB}
	mv -f ${TARGET_LIB_I} ${EUL_LIB_DIR}/${TARGET_LIB_I}
	ln -s ${EUL_LIB_DIR}/${TARGET_LIB_I} ${TARGET_LIB_I}
	mv -f ${TARGET_LIB} ${EUL_LIB_DIR}/${TARGET_LIB}
	ln -s ${EUL_LIB_DIR}/${TARGET_LIB} ${TARGET_LIB}

.PHONY: test
test:	${TEST_SOURCES}
	echo "#!/bin/sh" > Test1
	echo "exec mpirun -np 2 test1" >> Test1
	chmod ug+x Test1
	echo "#!/bin/sh" > Test2
	echo "exec mpirun -np 2 test2" >> Test2
	chmod ug+x Test2
	echo "#!/bin/sh" > Test3+4
	echo "exec mpirun -p4pg mpi-configure test4" >> Test3+4
	chmod ug+x Test3+4
	echo `hostname` 0 `pwd`/test4 > mpi-configure
	echo `hostname` 1 `pwd`/test3 >> mpi-configure

.SUFFIXES: .a .em
.SUFFIXES: .c .em

.em.a:
	${U2} ${U2FLAGS} -ar $* -l level1 -l serial -recompile
	rm -f ${*}.a
	ln -sf lib${*}.a ${*}.a

.em.c:
	${U2} ${U2FLAGS} $* -l level1 -l serial -l mpis -ld mpicc -static

###-----------------------------------------------------------------------------
### Misc
###-----------------------------------------------------------------------------

.PHONY: README
README: README.html

.PHONY: clean
clean:
	rm -f ${OBJECTS}

.PHONY: distclean
distclean: clean
	@rm -rf platforms
	@rm -f $(EUL_MODULE_LIB) $(EUL_MODULE_LIB_I)

###-----------------------------------------------------------------------------
