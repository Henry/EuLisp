## ----------------------------------------------------------------------------
##                      EuLisp System 'youtoo'
## ----------------------------------------------------------------------------

## ----------------------------------------------------------------------------
## Documentation Makefile
## ----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../Lib.$(ARCH)/Makefile

## ----------------------------------------------------------------------------
## Documentation directories
## ----------------------------------------------------------------------------

DOC_DIRS = EuLisp-0.991

## ----------------------------------------------------------------------------
## Commands
## ----------------------------------------------------------------------------

.PHONY: all
all: bindings
	@echo ".COMPILING DOCSs ..."
	@$(call makeAll,$(DOC_DIRS),)
	@echo ".DONE."

.PHONY: bindings
bindings: bindings.txt

bindings.txt: $(EUL_LIB_DIR)/liblevel1.i $(EUL_DIR)/Runtime/macros.i $(EUL_LIB_DIR)/libeval.i
	@cd $(EUL_DIR)/Tools; $(MAKE)
	$(EUL_BIN_DIR)/i2doc \
	$(EUL_LIB_DIR)/liblevel1.i $(EUL_DIR)/Runtime/macros.i $(EUL_LIB_DIR)/libeval.i \
	> bindings.txt

.PHONY: clean
clean:
	@echo ".CLEANING DOCs ..."
	@$(call makeAll,$(DOC_DIRS),$@)
	@echo ".DONE."

.PHONY: distclean
distclean:
	@echo ".CLEANING DOCs ..."
	@$(call makeAll,$(DOC_DIRS),$@)
	@echo ".DONE."

## ----------------------------------------------------------------------------
