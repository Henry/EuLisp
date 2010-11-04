### Copyright 2010 Henry G. Weller
###-----------------------------------------------------------------------------
##  This file is part of
### ---                                     EuLisp
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
### Title: String-perls Makefile
###  Maintainer: Henry G. Weller
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
EXAMPLES = string-perls
EXAMPLES_EXE = $(EXAMPLES:%=$(ARCH_DIR)/%)

makeExe = ${U2} ${U2FLAGS} $(1) --object-dir $(ARCH_DIR) -l level-0

###-----------------------------------------------------------------------------
all: $(EXAMPLES_EXE)

$(ARCH_DIR)/eul-string-perls.o: eul-string-perls.c | $(ARCH_DIR)/.
	$(CC) $(CFLAGS) -c eul-string-perls.c -o $(ARCH_DIR)/eul-string-perls.o

$(ARCH_DIR)/string-perls: string-perls.em $(ARCH_DIR)/eul-string-perls.o
	${U2} ${U2FLAGS} string-perls --object-dir $(ARCH_DIR) -l level-0 \
		--cflags -lpcre \
		--fff eul-string-perls

###-----------------------------------------------------------------------------
RUN = string-perls
RUN_EXE = $(RUN:%=$(ARCH_DIR)/%)

run: $(RUN_EXE)
	@$(foreach example, $(RUN_EXE), \
		export LD_LIBRARY_PATH=$(EUL_LIB_DIR):$(LD_LIBRARY_PATH);\
		./$(example);)

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
