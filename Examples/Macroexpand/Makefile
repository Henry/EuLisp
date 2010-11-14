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
### EuLisp test-expand-macro Makefile
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Test source files
###-----------------------------------------------------------------------------
TEST_EM = test-expand-macro.em
TEST_EXE = $(TEST_EM:%.em=$(ARCH_DIR)/%)

###-----------------------------------------------------------------------------
### Test
###-----------------------------------------------------------------------------
$(ARCH_DIR)/% : %.em | $(ARCH_DIR)/.
	${U2} ${U2FLAGS} $< --object-dir $(ARCH_DIR) -l level-0 -l eval

.PHONY: test
test: $(ARCH_DIR)/test-expand-macro
	$(ARCH_DIR)/test-expand-macro

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
