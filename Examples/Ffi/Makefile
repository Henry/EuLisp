### Copyright 2010 Henry G. Weller
###-----------------------------------------------------------------------------
##  This file is part of
### ---                           EuLisp System 'Youtoo'
###-----------------------------------------------------------------------------
##
##  Eu2C is free software: you can redistribute it and/or modify it under the
##  terms of the GNU General Public License version 2 as published by the Free
##  Software Foundation.
##
##  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
##  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
##  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
##  details.
##
##  You should have received a copy of the GNU General Public License along with
##  this program.  If not, see <http://www.gnu.org/licenses/>.
##
###-----------------------------------------------------------------------------
### Title: Ffi example Makefile
###  Maintainer: Henry G. Weller
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
EXAMPLES = ffi ffi1 ffi2
EXAMPLES_EXE = $(EXAMPLES:%=$(ARCH_DIR)/%)

###-----------------------------------------------------------------------------
### Youtoo compile rules for all examples
###-----------------------------------------------------------------------------
all: $(EXAMPLES_EXE)

$(ARCH_DIR)/ffi: ffi.em $(ARCH_DIR)/ffi-ext.o | $(ARCH_DIR)/.
	${U2} ${U2FLAGS} ffi --object-dir $(ARCH_DIR) -l level-0 --fff ffi-ext

$(ARCH_DIR)/ffi1: ffi1.em
	${U2} ${U2FLAGS} ffi1 --object-dir $(ARCH_DIR) -l level-0

$(ARCH_DIR)/eul-ffi2.o: eul-ffi2.c
	$(CC) $(CFLAGS) -c eul-ffi2.c -o $(ARCH_DIR)/eul-ffi2.o

$(ARCH_DIR)/ffi2: ffi2.em $(ARCH_DIR)/eul-ffi2.o
	${U2} ${U2FLAGS} ffi2 --object-dir $(ARCH_DIR) -l level-0 --fff eul-ffi2

###-----------------------------------------------------------------------------
### Run the examples
###-----------------------------------------------------------------------------
RUN = ffi ffi2

.PHONY: run
run: $(EXAMPLES_EXE)
	@$(foreach example, $(RUN), \
		export LD_LIBRARY_PATH=$(EUL_LIB_DIR):$(LD_LIBRARY_PATH);\
		$(ARCH_DIR)/$(example);)
	$(ARCH_DIR)/ffi1 "20"

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
