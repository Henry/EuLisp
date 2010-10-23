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
### Title: Virtual-machine Makefile
###  Maintainer: Henry G. Weller
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Output library files
###-----------------------------------------------------------------------------
VM_LIB = $(EUL_LIB_DIR)/libeulvm.a
VM_SHARED_LIB = $(EUL_LIB_DIR)/libeulvm.so

###-----------------------------------------------------------------------------
### Source and object files
###-----------------------------------------------------------------------------
VM_H = 	bytecode.h bytevec.h cache.h callback.h character.h class.h double.h \
	eul-string.h ff.h fpi.h handler.h init.h io.h keyword.h level1i.h \
	list.h notify.h object.h operator.h register.h shared-mem.h signals.h \
	stack.h state.h stdc.h symbol.h table.h tag.h util.h

VM_C = character.c fpi.c init.c rl_init.c interpret.c level1i.c list.c \
       register.c stack.c signals.c table.c util.c
VM_O = $(VM_C:%.c=$(ARCH_DIR)/%.o)

###-----------------------------------------------------------------------------
### Commands to build library object files
###-----------------------------------------------------------------------------
.PHONY: lib
lib: $(VM_LIB)

$(VM_LIB): $(VM_O)
	$(AR) $(VM_LIB) $(VM_O)
	@$(RANLIB) $(VM_LIB)

.PHONY: shared
shared:	$(VM_SHARED_LIB)

$(VM_SHARED_LIB): $(VM_O)
	$(U2_DIR)/Tools/makeso.$(OSTYPE) $(CC) $(VM_SHARED_LIB) $(VM_O)

###-----------------------------------------------------------------------------
### Dependency checking
###-----------------------------------------------------------------------------

# Simple dependency of .o files on all of the .h files
$(VM_O): $(VM_H)

.PHONY: depend
depend: | $(ARCH_DIR)/.
	@touch $(ARCH_DIR)/Makefile.dep
	makedepend -- $(CFLAGS) -- $(VM_C) \
		-p$(ARCH_DIR)/ -f $(ARCH_DIR)/Makefile.dep

-include $(ARCH_DIR)/Makefile.dep

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------
.PHONY: boot-clean
boot-clean: clean

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)

.PHONY: distclean
distclean:
	@rm -rf platforms

###-----------------------------------------------------------------------------
