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
### Title: Youtoo executable Makefile
###  Maintainer: Henry G. Weller
###-----------------------------------------------------------------------------
ARCH = $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Output file names
###-----------------------------------------------------------------------------
YOUTOO = $(EUL_BIN_DIR)/youtoo
YOUTOO_NOGC = $(EUL_BIN_DIR)/youtoo_nogc

###-----------------------------------------------------------------------------
### Source and object files
###-----------------------------------------------------------------------------
U2_APPL_C = eul-appl.c
U2_APPL_O = $(U2_APPL_C:%.c=$(EUL_LIB_DIR)/%.o)

GC_FAKE_C = gc-fake.c
FAKE_GC_O = $(GC_FAKE_C:%.c=$(EUL_LIB_DIR)/%.o)

ALL_C = $(U2_APPL_C) $(GC_FAKE_C)

LIBS = libeulvm libboot libtelos liblevel-1 libmath libeval libmain
YOUTOO_LIBS := $(LIBS:%=$(EUL_LIB_DIR)/%.a)

ifeq ($(MAKECMDGOALS),shared)
    YOUTOO_LIBS := $(LIBS:%=$(EUL_LIB_DIR)/%.so)
else
    YOUTOO_LIBS := $(LIBS:%=$(EUL_LIB_DIR)/%.a)
endif

###-----------------------------------------------------------------------------
### Compiler + Interpreter
###-----------------------------------------------------------------------------
.PHONY: youtoo
youtoo:	$(YOUTOO)

.PHONY: shared
shared:	$(YOUTOO)

$(YOUTOO): $(U2_APPL_O) $(YOUTOO_LIBS)
	$(CC) $(CFLAGS) -o $(YOUTOO) \
		$(U2_APPL_O) -L$(EUL_LIB_DIR) \
		$(U2_LIBS) -lgc -lpthread

.PHONY: nogc
nogc: $(YOUTOO_NOGC)

$(YOUTOO_NOGC): $(U2_APPL_O) $(FAKE_GC_O)
	$(CC) $(CFLAGS) -o $(YOUTOO_NOGC) \
		$(U2_APPL_O) $(FAKE_GC_O) -L$(EUL_LIB_DIR) \
	 	$(U2_LIBS) -lpthread

$(EUL_LIB_DIR)/%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------
.PHONY: boot-clean
boot-clean:
	@rm -rf $(ARCH_DIR)

.PHONY: clean
clean:

.PHONY: distclean
distclean: clean
	@rm -rf platforms
	@rm -f $(U2_APPL_O) $(FAKE_GC_O)

###-----------------------------------------------------------------------------
