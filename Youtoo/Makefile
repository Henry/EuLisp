### Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### Top-level Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Source directories
###-----------------------------------------------------------------------------

SYS_DIRS = Vm Telos Runtime Comptime2 Youtoo

###-----------------------------------------------------------------------------
### Compiling youtoo with static or shared libraries
###-----------------------------------------------------------------------------

.PHONY: static
static:
	@echo "COMPILING youtoo ..."
	@$(call makeAll,$(SYS_DIRS),)
	@echo "DONE"

.PHONY: shared
shared:
	@echo "COMPILING youtoo with shared libraries ..."
	@$(call makeAll,$(SYS_DIRS),$@)
	@echo "DONE"

###-----------------------------------------------------------------------------
### Bootstrapping youtoo
###-----------------------------------------------------------------------------

.PHONY: boot
boot: $(U2_BOOT) $(EUL_BOOT_DIR)/Bin.$(ARCH)/b2h
	@echo "COMPILING youtoo modules ..."
	$(MAKE) -C Vm
	$(MAKE) -C Telos $@
	$(MAKE) -C Runtime $@
	@echo "-- CREATING preliminary youtoo executable source"
	$(MAKE) -C Comptime2 boot-preliminary
	@echo "-- CREATING statically linked level1 library interface file"
	$(U2_DIR)/Tools/i2c $(ARCH)
	${MAKE} -C Vm
	@echo "-- LINKING preliminary youtoo executable"
	@rm -f $(EUL_BIN_DIR)/youtoo
	$(MAKE) -C Comptime2 boot
	$(MAKE) -C Youtoo
	$(MAKE) -C Comptime2 boot-final
	@echo "-- CREATING final youtoo executable"
	@rm -f $(EUL_BIN_DIR)/youtoo
	$(MAKE) -C Comptime2 boot
	$(MAKE) -C Youtoo
	$(MAKE) -C Comptime2 bytecode2
	$(MAKE) -C $(EUL_BOOT_DIR)/Youtoo/Tools;
	mkdir -p Lib; cp $(EUL_LIB_DIR)/lib{boot,eval,level0,level1,math,telos}.i Lib
	rm -f {Telos,Runtime,Comptime2}/lib*.i
	@echo "DONE"

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------

.PHONY: README
README: README.html

.PHONY: test
test:
	@echo "TESTING youtoo installation ..."
	@$(MAKE) -C Test test
	@echo "DONE"

.PHONY: boot-clean
boot-clean:
	@echo "BOOT-CLEANING youtoo modules ..."
	@$(call makeAll,$(SYS_DIRS),$@)
	@rm -f $(EUL_LIB_DIR)/*.[aio]
	@echo "DONE"

.PHONY: clean
clean:
	@echo "CLEANING youtoo ..."
	@$(call makeAll,$(SYS_DIRS),$@)
	@$(MAKE) -C Test $@
	@echo "DONE"

.PHONY: gitclean
gitclean:
	@echo "GIT-CLEANING youtoo ..."
	@$(MAKE) -C Tools clean
	@$(MAKE) -C Test clean
	@echo "DONE"

.PHONY: distclean
distclean: clean
	@echo "DIST-CLEANING youtoo ..."
	@$(call makeAll,$(SYS_DIRS),$@)
	@$(MAKE) -C Test $@
	@echo "DONE"

###-----------------------------------------------------------------------------
