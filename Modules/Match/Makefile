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
### EuLisp Match library Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Module name
###-----------------------------------------------------------------------------

MODULE = match

###-----------------------------------------------------------------------------
### Library target files
###-----------------------------------------------------------------------------

MODULE_LIB = lib$(MODULE).a
MODULE_LIB_I = lib$(MODULE).i

###-----------------------------------------------------------------------------
### Source files
###-----------------------------------------------------------------------------

MODULE_EM = $(MODULE).em
MODULE_C = $(MODULE).c

OTHER_EM = match0.em match-support.em
EXTRA_EM = extras0.em extras.em

###-----------------------------------------------------------------------------
### Test source files
###-----------------------------------------------------------------------------

TEST_EM = test-match.em
TEST_EXE = $(TEST_EM:%.em=$(ARCH_DIR)/%)

TEST_MACROS_EM = test-match-macros.em

ALL_EM = $(MODULE_EM) $(OTHER_EM) $(EXTRA_EM) $(TEST_EM) $(TEST_MACROS_EM)

###-----------------------------------------------------------------------------
### Build library
###-----------------------------------------------------------------------------

.PHONY: $(MODULE)
$(MODULE): match.i
match.i:  match0.i match-support.i

%.i : %.em
	${U2} -c ${U2FLAGS} $< -l level1

###-----------------------------------------------------------------------------
### Test
###-----------------------------------------------------------------------------

$(ARCH_DIR)/% : %.em | $(ARCH_DIR)/.
	${U2} ${U2FLAGS} $< -od $(ARCH_DIR) -l level1

$(ARCH_DIR)/test-match: $(EXTRA_EM:.em=.i) match.i test-match-macros.i

.PHONY: test
test: $(ARCH_DIR)/test-match
	$(ARCH_DIR)/test-match

###-----------------------------------------------------------------------------

.PHONY: README
README: README.html

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)
	@rm -f $(ALL_EM:.em=.i) $(ALL_EM:.em=.h) $(ALL_EM:.em=.c) $(ALL_EM:.em=.o)
	@rm -f $(TEST_EM:.em=_.c) $(TEST_EM:.em=_.o) $(TEST_EM:.em=)

.PHONY: distclean
distclean: clean
	@rm -rf platforms

###-----------------------------------------------------------------------------
