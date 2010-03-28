## Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.

## ----------------------------------------------------------------------------
##                      EuLisp System 'youtoo'
## ----------------------------------------------------------------------------

## ----------------------------------------------------------------------------
## Top-level Makefile
## ----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include Lib.$(ARCH)/Makefile

## ----------------------------------------------------------------------------
## Source directories
## ----------------------------------------------------------------------------

SYS_DIRS = Vm Telos Runtime Comptime2 Youtoo Tools

## ----------------------------------------------------------------------------
## Compiling youtoo with static or shared libraries
## ----------------------------------------------------------------------------

.PHONY: static
static:
	@echo ".COMPILING youtoo ..."
	@$(call makeAll,$(SYS_DIRS),)
	@echo ".DONE."

.PHONY: shared
shared:
	@echo ".COMPILING youtoo with shared libraries ..."
	@$(call makeAll,$(SYS_DIRS),$@)
	@echo ".DONE."

## ----------------------------------------------------------------------------
## Bootstrapping youtoo
## ----------------------------------------------------------------------------

.PHONY: boot
boot:
	cd Tools; $(MAKE)
	@echo ".COMPILING youtoo modules ..."
	cd Vm; $(MAKE)
	cd Telos; $(MAKE) $@
	cd Runtime; $(MAKE) $@
	@echo ".CREATING preliminary youtoo executable"
	cd Comptime2; $(MAKE) preliminary
	@echo ".CREATING statically linked level1 library interface file"
	Tools/i2c $(ARCH)
	@echo ".COMPILING preliminary youtoo executable"
	@rm -f $(EUL_BIN_DIR)/youtoo
	$(MAKE)
	cd Comptime2; $(MAKE) final
	@echo ".COMPILING final youtoo executable"
	@rm -f $(EUL_BIN_DIR)/youtoo
	$(MAKE)
	cd Comptime2; $(MAKE) bytecode2
	@echo ".DONE."

## ----------------------------------------------------------------------------
## Miscellaneous commands
## ----------------------------------------------------------------------------

.PHONY: test
test:
	@cd Test; $(U2) hello1 -recompile; ./hello1
	@cd Test; $(U2) hello2 -l boot -recompile; ./hello2
	@cd Test; $(U2) hello3 -l telos -recompile; ./hello3
	@cd Test; $(U2) hello4 -l level1 -recompile; ./hello4
	@echo ".DONE."

.PHONY: README
README: index.html TODO.html
	@echo ".BUILDING all README.html ..."
	@cd Extras; $(MAKE) $@
	@echo ".DONE."

.PHONY: touch
touch:
	@echo ".TOUCHING youtoo ..."
	@$(call makeAll,$(SYS_DIRS),$@)
	@echo ".DONE."

.PHONY: boot-touch
boot-touch:
	@echo ".TOUCHING youtoo modules ..."
	@$(call makeAll,$(SYS_DIRS),$@)
	@echo ".DONE."

.PHONY: clean
clean:
	@echo ".CLEANING youtoo ..."
	@$(call makeAll,$(SYS_DIRS),$@)
	@cd Test; $(MAKE) $@
	@cd Extras; $(MAKE) $@
	@cd Examples; $(MAKE) $@
	@cd Doc; $(MAKE) $@
	@echo ".DONE."

.PHONY: distclean
distclean: clean
	@echo ".DIST-CLEANING youtoo ..."
	@$(call makeAll,$(SYS_DIRS),$@)
	@cd Test; $(MAKE) $@
	@cd Extras; $(MAKE) $@
	@cd Examples; $(MAKE) $@
	@cd Doc; $(MAKE) $@
	@rm -rf */platforms Bin.* Lib.*
	@rm -f .eulrc.*
	@echo ".DONE."

.PHONY: doc
doc: README
	@cd Doc; $(MAKE)

## ----------------------------------------------------------------------------
