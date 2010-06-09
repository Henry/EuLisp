### Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### Top-level Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include Lib.$(ARCH)/Makefile

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
boot: $(U2_BOOT) $(EUL_BOOT_DIR)/Tools/i2c
	@echo "COMPILING youtoo modules ..."
	cd Vm; $(MAKE)
	cd Telos; $(MAKE) $@
	cd Runtime; $(MAKE) $@
	@echo "-- CREATING preliminary youtoo executable source"
	cd Comptime2; $(MAKE) preliminary
	@echo "-- CREATING statically linked level1 library interface file"
	$(EUL_BOOT_DIR)/Tools/i2c $(ARCH)
	cd Vm; ${MAKE}
	@echo "-- LINKING preliminary youtoo executable"
	@rm -f $(EUL_BIN_DIR)/youtoo
	cd Comptime2; $(MAKE)
	cd Youtoo; $(MAKE)
	cd Comptime2; $(MAKE) final
	@echo "-- CREATING final youtoo executable"
	@rm -f $(EUL_BIN_DIR)/youtoo
	cd Comptime2; $(MAKE)
	cd Youtoo; $(MAKE)
	cd Comptime2; $(MAKE) bytecode2
	cd Tools; $(MAKE)
	mkdir -p Lib; cp $(EUL_LIB_DIR)/*.i Lib
	rm -f {Telos,Runtime,Comptime2}/lib*.i
	@echo "DONE"

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------

.PHONY: test
test:
	@echo "TESTING youtoo installation ..."
	@cd Test; $(U2) test1 -recompile; ./test1
	@cd Test; $(U2) test2 -l boot -recompile; ./test2
	@cd Test; $(U2) test3 -l telos -recompile; ./test3
	@cd Test; $(U2) test4 -l level1 -recompile; ./test4
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
	@cd Test; $(MAKE) $@
	@cd Extras; $(MAKE) $@
	@cd Examples; $(MAKE) $@
	@cd Doc; $(MAKE) $@
	@echo "DONE"

.PHONY: distclean
distclean: clean
	@echo "DIST-CLEANING youtoo ..."
	@$(call makeAll,$(SYS_DIRS),$@)
	@cd Test; $(MAKE) $@
	@cd Extras; $(MAKE) $@
	@cd Examples; $(MAKE) $@
	@cd Doc; $(MAKE) $@
	@rm -rf */platforms Bin.* Lib.*
	@rm -f .eulrc.*
	@echo "DONE"

README.org: index.org
	@sed 's%file:%http://henry.github.com/EuLisp/blob/master/%' $< > $@

.PHONY: README
README: index.html TODO.html README.org
	@echo "BUILDING all README.html ..."
	@cd Extras; $(MAKE) $@
	@echo "DONE"

.PHONY: doc
doc: README
	@cd Doc; $(MAKE)

###-----------------------------------------------------------------------------
