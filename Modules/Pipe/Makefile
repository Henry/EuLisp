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
### EuLisp Pipe library Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Module name
###-----------------------------------------------------------------------------

MODULE = pipe

###-----------------------------------------------------------------------------
### Library target files
###-----------------------------------------------------------------------------

MODULE_LIB = lib$(MODULE).a
MODULE_LIB_I = lib$(MODULE).i

EUL_MODULE_LIB = $(EUL_LIB_DIR)/$(MODULE_LIB)
EUL_MODULE_LIB_I = $(EUL_LIB_DIR)/$(MODULE_LIB_I)

###-----------------------------------------------------------------------------
### Source files
###-----------------------------------------------------------------------------

MODULE_EM = $(MODULE).em
MODULE_C = $(MODULE).c
MODULE_O = $(ARCH_DIR)/$(MODULE).o

FF_C = eul-pipe.c
FF_O = $(FF_C:%.c=$(ARCH_DIR)/%.o)

###-----------------------------------------------------------------------------
### Test source files
###-----------------------------------------------------------------------------

TEST_EM = test1.em
TEST_EXE = $(TEST_EM:%.em=$(ARCH_DIR)/%)

###-----------------------------------------------------------------------------
### Build library
###-----------------------------------------------------------------------------

.PHONY: lib
lib: $(EUL_MODULE_LIB)

$(MODULE_C): $(MODULE_EM) $(OTHER_EM) | $(ARCH_DIR)/.
	$(U2) $(U2FLAGS) -ar $(MODULE_EM) -od $(ARCH_DIR) -l level1 -recompile
	mv -f $(MODULE_LIB_I) $(EUL_MODULE_LIB_I)
	rm -f $(MODULE_LIB)

$(EUL_MODULE_LIB): $(MODULE_C) $(FF_O)
	$(AR) $(EUL_MODULE_LIB) $(MODULE_O) $(FF_O)
	$(RANLIB) $(EUL_MODULE_LIB)

###-----------------------------------------------------------------------------
### Test
###-----------------------------------------------------------------------------

$(ARCH_DIR)/% : %.em $(EUL_MODULE_LIB) | $(ARCH_DIR)/.
	$(U2) $(U2FLAGS) $< -od $(ARCH_DIR) -l level1 -l $(MODULE) -no_recompile

.PHONY: test
test:  $(TEST_EXE)
	$(TEST_EXE)

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------

.PHONY: README
README: README.html

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)
	@rm -f $(MODULE_C) $(MODULE).h
	@rm -f test*.{c,h}
	@rm -f *.i *_.{c,h}

.PHONY: distclean
distclean: clean
	@rm -rf platforms
	@rm -f $(EUL_MODULE_LIB) $(EUL_MODULE_LIB_I)

###-----------------------------------------------------------------------------
