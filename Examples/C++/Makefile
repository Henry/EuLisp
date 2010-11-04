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
### Title: C++ examples Makefile
###  Maintainer: Henry G. Weller
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
TEST = test1 test2
TEST_EM = $(TEST:%=%.em)

FF_CC = eul-cc1.cc eul-cc3.cc

FF2 = eul-cc2
FF2_C = $(FF2:%=%.c)
FF2_O = $(FF2_C:%.c=$(ARCH_DIR)/%.o)

###-----------------------------------------------------------------------------
all: $(ARCH_DIR)/test1 $(ARCH_DIR)/test2

$(ARCH_DIR)/eul-cc1.o: eul-cc1.cc | $(ARCH_DIR)/.
	$(CC) $(CFLAGS) -c eul-cc1.cc -o $(ARCH_DIR)/eul-cc1.o

$(ARCH_DIR)/test1: test1.em $(ARCH_DIR)/eul-cc1.o
	${U2} ${U2FLAGS} test1 --object-dir $(ARCH_DIR) \
		--cflags -lstdc++ -l level-0 --fff eul-cc1

$(U2_C_DIR)/test2.h: test2.em | $(ARCH_DIR)/.
	${U2} ${U2FLAGS} -c test2 --object-dir $(ARCH_DIR) -l level-0

$(FF2_O): $(FF2_C) $(U2_C_DIR)/test2.h | $(ARCH_DIR)/.
	$(CC) $(CFLAGS) -c $(FF2_C) -o $(FF2_O)

$(ARCH_DIR)/eul-cc3.o: eul-cc3.cc | $(ARCH_DIR)/.
	$(CC) $(CFLAGS) -c eul-cc3.cc -o $(ARCH_DIR)/eul-cc3.o

$(ARCH_DIR)/test2: test2.em $(FF2_O) $(ARCH_DIR)/eul-cc3.o
	${U2} ${U2FLAGS} test2 --object-dir $(ARCH_DIR) \
		--cflags -lstdc++ \
		-l level-0 --fff $(FF2) --fff eul-cc3 --recompile

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
