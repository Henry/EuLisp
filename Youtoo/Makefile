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
### Title: Top-level Makefile
###  Maintainer: Henry G. Weller
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
	@mkdir -p $(EUL_LIB_DIR)/$(U2_C_DIR)
	@cp Lib/lib*.i $(EUL_LIB_DIR)/$(U2_C_DIR)
	@$(call makeAll,$(SYS_DIRS),)
	@echo "DONE"

.PHONY: shared
shared:
	@echo "COMPILING youtoo with shared libraries ..."
	@mkdir -p $(EUL_LIB_DIR)/$(U2_C_DIR)
	@cp Lib/lib*.i $(EUL_LIB_DIR)/$(U2_C_DIR)
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
	@echo "-- CREATING statically linked level-1 library interface file"
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
	$(MAKE) -C $(EUL_DIR)/Youtoo/Tools
	mkdir -p Lib ; cp $(EUL_LIB_DIR)/$(U2_C_DIR)/lib{boot,eval,level-0,level-1,math,telos}.i Lib
	rm -f {Telos,Runtime,Comptime2}/lib*.i
	@echo "DONE"

###-----------------------------------------------------------------------------
###  Build both 32bit and 64bit on a 64bit machine
###-----------------------------------------------------------------------------
.PHONY: all
all:
	$(MAKE)
	$(MAKE) ARCH=i686

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
