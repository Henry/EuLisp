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
### EuLisp eval-test Makefile
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Module name
###-----------------------------------------------------------------------------
EXAMPLES = eval-test
EXAMPLES_EXE = $(EXAMPLES:%=$(ARCH_DIR)/%)

makeExe = ${U2} ${U2FLAGS} $(1) --object-dir $(ARCH_DIR) -l eval -l level-1 \
		--load-path $(EUL_DIR)/Youtoo/Comptime2

###-----------------------------------------------------------------------------
### Build library
###-----------------------------------------------------------------------------
all: $(EXAMPLES_EXE)

$(ARCH_DIR)/eval-test: eval-test.em
	${U2} ${U2FLAGS} eval-test --object-dir $(ARCH_DIR) -l eval -l level-1 \
		--load-path $(EUL_DIR)/Youtoo/Comptime2

###-----------------------------------------------------------------------------
### Run
###-----------------------------------------------------------------------------
RUN = eval-test
RUN_EXE = $(RUN:%=$(ARCH_DIR)/%)

.PHONY: run
run: $(RUN_EXE)
	@$(foreach example, $(RUN_EXE), \
		export EUL_DIR=$(EUL_DIR); \
		export EUL_ARCH=$(ARCH); \
		export LD_LIBRARY_PATH=$(EUL_LIB_DIR):$(LD_LIBRARY_PATH); \
		export EUL_LOAD_PATH=.:$(EUL_DIR)/Lib.$(EUL_ARCH):$(EUL_DIR)/Youtoo/Comptime2:$(EUL_DIR)/Youtoo/Runtime:$(EUL_DIR)/Youtoo/Telos:$(EUL_DIR)/Youtoo/Comptime2/64bit; \
		echo '(let ((hello "hello")) (print hello nl)) (print locvar nl) (setq end "end")' | $(example) ;)

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------
.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)

.PHONY: distclean
distclean: clean
	@rm -rf platforms $(U2_C_DIR)

###-----------------------------------------------------------------------------
