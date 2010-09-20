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
### EuLisp virtual-machine Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Module name
###-----------------------------------------------------------------------------

MODULE = scheme

###-----------------------------------------------------------------------------
### Library target files
###-----------------------------------------------------------------------------

MODULE_LIB = lib$(MODULE).a
MODULE_LIB_I = lib$(MODULE).i

EUL_MODULE_LIB = $(EUL_LIB_DIR)/$(MODULE_LIB)
EUL_MODULE_LIB_I = $(EUL_LIB_DIR)/$(MODULE_LIB_I)

EUL_BIGNUM_LIB = $(EUL_LIB_DIR)/libbignum.a
EUL_BIGNUM_LIB_I = $(EUL_LIB_DIR)/libbignum.i

###-----------------------------------------------------------------------------
### Tcl/Tk libraries
###-----------------------------------------------------------------------------

EUL_TK_LIBS = -lX11 -ltk$(EUL_TK_VERSION) -ltcl$(EUL_TK_VERSION)

###-----------------------------------------------------------------------------
### Files
###-----------------------------------------------------------------------------

MODULE_EM = $(MODULE).em
MODULE_C = $(MODULE).c
MODULE_O = $(ARCH_DIR)/$(MODULE).o

OTHER_EM = scheme0.em
OTHER_O = $(OTHER_EM:%.em=$(ARCH_DIR)/%.o)

SCMTOO_EM = scmtoo0.em scmtoo.em
SCMTOO_C = $(SCMTOO_EM:%.em=%.c)

SCM2TK_EM = scm2tk.em
SCM2TK_C = $(SCM2TK_EM:%.em=%.c)

TEST_EM = test1.em test2.em
TEST_EXE = $(TEST_EM:%.em=$(ARCH_DIR)/%)

###-----------------------------------------------------------------------------
### Build all
###-----------------------------------------------------------------------------
.PHONY: all
all: lib scmtoo

###-----------------------------------------------------------------------------
### Build library
###-----------------------------------------------------------------------------

.PHONY: lib
lib: $(EUL_MODULE_LIB)

$(MODULE_C) $(EUL_MODULE_LIB_I): \
	$(EUL_BIGNUM_LIB) $(EUL_BIGNUM_LIB_I) \
	$(MODULE_EM) $(OTHER_O) | $(ARCH_DIR)/.
	$(U2) $(U2FLAGS) -ar $(MODULE_EM) -od $(ARCH_DIR) \
		-l level1 -l math -l bignum -recompile
	mv -f $(MODULE_LIB_I) $(EUL_MODULE_LIB_I)

$(EUL_MODULE_LIB): $(EUL_MODULE_LIB_I) $(MODULE_O) $(OTHER_O)
	$(AR) $(EUL_MODULE_LIB) $(MODULE_O) $(OTHER_O)
	$(RANLIB) $(EUL_MODULE_LIB)
	rm -f $(MODULE_LIB)

$(EUL_BIGNUM_LIB) $(EUL_BIGNUM_LIB_I):
	$(MAKE) -C ../Bignum

###-----------------------------------------------------------------------------
### Build interpreters
###-----------------------------------------------------------------------------

.PHONY: scmtoo
scmtoo:	$(EUL_BIN_DIR)/scmtoo

$(EUL_BIN_DIR)/scmtoo: $(EUL_MODULE_LIB) $(EUL_MODULE_LIB_I) \
		scmtoo0.i scmtoo0.c Scmtoo_.c scheme0.i scheme0.c \
		| $(ARCH_DIR)/.
	$(U2) $(U2FLAGS) -c scmtoo -od $(ARCH_DIR) \
		-l level1 -l bignum -l math -l eval -l scheme -no_recompile
	./scmtooi2c $(ARCH)
	$(CC) $(CFLAGS) -o $(EUL_BIN_DIR)/scmtoo \
		$(SCMTOO_C) Scmtoo_.c scmtooi.c $(EUL_LIB_DIR)/eul-appl.o \
		-L$(EUL_LIB_DIR) -lscheme -lbignum -lgmp $(CLIBS) -lgc

.PHONY: scm2tk
scm2tk: $(EUL_BIN_DIR)/scm2tk

$(EUL_BIN_DIR)/scm2tk: $(EUL_MODULE_LIB) $(EUL_MODULE_LIB_I) \
		scmtoo0.i scmtoo0.c Scm2tk_.c scheme0.i scheme0.c \
		| $(ARCH_DIR)/.
	$(U2) $(U2FLAGS) -c scm2tk -od $(ARCH_DIR) \
		-l level1 -l bignum -l math -l eval -l scheme -l tcltk -no_recompile
	./scm2tki2c $(ARCH)
	$(CC) $(CFLAGS) -o $(EUL_BIN_DIR)/scm2tk \
		$(SCM2TK_C) Scm2tk_.c scmtoo0.c scm2tki.c \
		$(EUL_LIB_DIR)/eul-appl.o \
		-L$(EUL_LIB_DIR) -ltcltk -lscheme -lbignum -lgmp \
		$(CLIBS) $(EUL_TK_LIBS) -lgc

.SUFFIXES: .c .em .i .h
.em.i .em.c .em.h:
	$(U2) $(U2FLAGS) -c $* -l level1 -l eval -no_recompile

###-----------------------------------------------------------------------------
### Test
###-----------------------------------------------------------------------------

$(ARCH_DIR)/% : %.em $(EUL_MODULE_LIB) $(EUL_MODULE_LIB_I) | $(ARCH_DIR)/.
	$(U2) $(U2FLAGS) $< -od $(ARCH_DIR) \
		-l bignum -l scheme -l level1 -cflags -lgmp -no_recompile

.PHONY: test
test:	$(TEST_EXE)
	$(ARCH_DIR)/test1
	$(ARCH_DIR)/test2

###-----------------------------------------------------------------------------
### Misc
###-----------------------------------------------------------------------------

.PHONY: README
README: README.html

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)
	@rm -f *.{h,i}
	@rm -f $(MODULE_C) $(MODULE_EM:.em=_.c)
	@rm -f $(OTHER_EM:.em=.c)
	@rm -f $(SCMTOO_C) $(SCM2TK_C) $(SCMTOO_EM:.em=_.c) $(SCM2TK_EM:.em=_.c)
	@rm -f $(TEST_EM:.em=.c) $(TEST_EM:.em=_.c)
	@rm -f scmtooi.c scm2tki.c

.PHONY: distclean
distclean: clean
	@rm -rf platforms
	@rm -f $(EUL_MODULE_LIB) $(EUL_MODULE_LIB_I) \
		$(EUL_BIN_DIR)/scmtoo $(EUL_BIN_DIR)/scm2tk

###-----------------------------------------------------------------------------
