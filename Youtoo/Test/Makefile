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
### Title: Makefile for Youtoo tests
###  Maintainer: Henry G. Weller
###  Description:
##    These specifically use $(U2) because they're used in
##    bootstrapping to check if everything bootstrapped ok.
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
TESTS = test1 test2 test3 test4
TESTS_EXE = $(TESTS:%=$(ARCH_DIR)/%)

###-----------------------------------------------------------------------------
all: $(TESTS_EXE)

$(ARCH_DIR)/test1: test1.em | $(ARCH_DIR)/.
	$(U2) ${U2FLAGS} -od $(ARCH_DIR) test1

$(ARCH_DIR)/test2: test2.em | $(ARCH_DIR)/.
	$(U2) ${U2FLAGS} -od $(ARCH_DIR) test2 -l boot

$(ARCH_DIR)/test3: test3.em | $(ARCH_DIR)/.
	$(U2) ${U2FLAGS} -od $(ARCH_DIR) test3 -l telos

$(ARCH_DIR)/test4: test4.em | $(ARCH_DIR)/.
	$(U2) ${U2FLAGS} -od $(ARCH_DIR) test4 -l level-1

test: $(TESTS_EXE)
	@$(foreach test, $(TESTS_EXE), \
		export LD_LIBRARY_PATH=$(EUL_LIB_DIR):$(LD_LIBRARY_PATH);\
		./$(test);)

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------
.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)
	@rm -f *.i *.c *.h

.PHONY: distclean
distclean: clean

###-----------------------------------------------------------------------------
