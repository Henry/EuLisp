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
### EuLisp Smatch library Makefile
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Module name
###-----------------------------------------------------------------------------
MODULE = smatch

###-----------------------------------------------------------------------------
### Library target files
###-----------------------------------------------------------------------------
MODULE_LIB = lib$(MODULE).a

###-----------------------------------------------------------------------------
### Source files
###-----------------------------------------------------------------------------
MODULE_EM = $(MODULE).em
MODULE_ALL_EM = smatch-smatch0.em smatch0.em
MODULE_IHC = $(U2_C_DIR)/$(MODULE).i $(U2_C_DIR)/$(MODULE).c $(U2_C_DIR)/$(MODULE).h

###-----------------------------------------------------------------------------
### Test source files
###-----------------------------------------------------------------------------
TEST_EM = test-smatch.em
TEST_MAC_EM = test-smatch-macros.em

###-----------------------------------------------------------------------------
### Expand source files
###-----------------------------------------------------------------------------
EXPAND_EM = expand-match.em
EXPAND_MAC_EM = expand-match-macros.em

###-----------------------------------------------------------------------------
### Build library
###-----------------------------------------------------------------------------
.PHONY: $(MODULE)
$(MODULE): $(MODULE_IHC)

$(MODULE_IHC) : $(MODULE_EM)
	${U2} -c ${U2FLAGS} $< -l level-0

###-----------------------------------------------------------------------------
### Build test, execute and expand
###-----------------------------------------------------------------------------
.PHONY: test
test: $(ARCH_DIR)/test-smatch
	$(ARCH_DIR)/test-smatch

$(ARCH_DIR)/test-smatch : $(TEST_EM) $(TEST_MAC_EM) $(MODULE_EM)
	${U2} ${U2FLAGS} $< --object-dir $(ARCH_DIR) \
		-l level-0 -l eval --recompile

.PHONY: expand
expand: $(MODULE_EM)

$(MODULE_EM) : $(EXPAND_EM) $(EXPAND_MAC_EM) $(MODULE_ALL_EM)
	${U2} ${U2FLAGS} $< --object-dir $(ARCH_DIR) \
		--load-path ../Match -l level-0 -l eval --recompile

###-----------------------------------------------------------------------------
### Miscellaneous commands
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
