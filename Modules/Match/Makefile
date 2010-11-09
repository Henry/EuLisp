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

###-----------------------------------------------------------------------------
### Source files
###-----------------------------------------------------------------------------
MODULE_EM = $(MODULE).em

OTHER_EM = match0.em match-support.em

###-----------------------------------------------------------------------------
### Test source files
###-----------------------------------------------------------------------------
TEST_EM = test-match.em
TEST_EXE = $(TEST_EM:%.em=$(ARCH_DIR)/%)

TEST_MACROS_EM = test-match-macros.em

ALL_EM = $(MODULE_EM) $(OTHER_EM) $(TEST_EM) $(TEST_MACROS_EM)

###-----------------------------------------------------------------------------
### Build library
###-----------------------------------------------------------------------------
.PHONY: $(MODULE)
$(MODULE): $(U2_C_DIR)/match.i
$(U2_C_DIR)/match.i: $(U2_C_DIR)/match0.i $(U2_C_DIR)/match-support.i

$(U2_C_DIR)/%.i : %.em
	${U2} -c ${U2FLAGS} $< -l level-0

###-----------------------------------------------------------------------------
### Test
###-----------------------------------------------------------------------------
$(ARCH_DIR)/% : %.em | $(ARCH_DIR)/.
	${U2} ${U2FLAGS} $< --object-dir $(ARCH_DIR) -l level-0

$(ARCH_DIR)/test-match: $(U2_C_DIR)/match.i $(U2_C_DIR)/test-match-macros.i

.PHONY: test
test: $(ARCH_DIR)/test-match
	$(ARCH_DIR)/test-match

###-----------------------------------------------------------------------------
.PHONY: README
README: README.html

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)

.PHONY: distclean
distclean: clean
	@rm -rf platforms $(U2_C_DIR)

###-----------------------------------------------------------------------------
