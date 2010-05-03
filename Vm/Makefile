### Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### EuLisp virtual-machine Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Output library files
###-----------------------------------------------------------------------------

VM_LIB = $(EUL_LIB_DIR)/libeulvm.a
VM_SHARED_LIB = $(EUL_LIB_DIR)/libeulvm.so

###-----------------------------------------------------------------------------
### Source and object files
###-----------------------------------------------------------------------------

VM_C = character.c fpi.c init.c interpret.c level1i.c list.c \
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
	$(EUL_DIR)/Tools/makeso.$(OSTYPE) $(CC) $(VM_SHARED_LIB) $(VM_O)

###-----------------------------------------------------------------------------
### Dependency checking
###-----------------------------------------------------------------------------

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
