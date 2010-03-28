## ----------------------------------------------------------------------------
##                      EuLisp System 'youtoo'
## ----------------------------------------------------------------------------

## ----------------------------------------------------------------------------
## Extras Makefile
## ----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../Lib.$(ARCH)/Makefile

## ----------------------------------------------------------------------------
## Extras directories
## ----------------------------------------------------------------------------

EXTRAS_DIRS = Bignum OS OPS5 Pipe Match Scheme TclTk
EXTRAS_DOC_DIRS = $(EXTRAS_DIRS) MPI

## ----------------------------------------------------------------------------
## Commands
## ----------------------------------------------------------------------------

.PHONY: all
all:
	@echo ".COMPILING EXTRASs ..."
	@$(call makeAll,$(EXTRAS_DIRS),)
	@echo ".DONE."

.PHONY: test
test:
	@echo ".COMPILING EXTRASs ..."
	@$(call makeAll,$(EXTRAS_DIRS),$@)
	@echo ".DONE."

.PHONY: README
README:
	@echo ".BUILDING README.html in EXTRASs ..."
	@$(call makeAll,$(EXTRAS_DOC_DIRS),$@)
	@echo ".DONE."

.PHONY: clean
clean:
	@echo ".CLEANING EXTRASs ..."
	@$(call makeAll,$(EXTRAS_DIRS),$@)
	@echo ".DONE."

.PHONY: distclean
distclean:
	@echo ".CLEANING EXTRASs ..."
	@$(call makeAll,$(EXTRAS_DIRS),$@)
	@echo ".DONE."

## ----------------------------------------------------------------------------
