###-----------------------------------------------------------------------------
### ---                  EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### Modules Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Modules directories
###-----------------------------------------------------------------------------

EXTRAS_DIRS = Bignum Case OS OPS5 Pipe Match Smatch Vmeta TclTk
EXTRAS_DOC_DIRS = $(EXTRAS_DIRS) MPI

###-----------------------------------------------------------------------------
### Commands
###-----------------------------------------------------------------------------

.PHONY: all
all:
	@echo "COMPILING Modules ..."
	@$(call makeAll,$(EXTRAS_DIRS),)
	@echo "DONE"

.PHONY: test
test:
	@echo "COMPILING Modules ..."
	@$(call makeAll,$(EXTRAS_DIRS),$@)
	@echo "DONE"

.PHONY: README
README:
	@echo "UPDATING README.html files in Modules ..."
	@$(call makeAll,$(EXTRAS_DOC_DIRS),$@)
	@echo "DONE"

.PHONY: clean
clean:
	@echo "CLEANING Modules ..."
	@$(call makeAll,$(EXTRAS_DIRS),$@)
	@echo "DONE"

.PHONY: gitclean
gitclean:
	@echo "Git-CLEANING Modules ..."
	@$(call makeAll,$(EXTRAS_DIRS),distclean)
	@echo "DONE"

.PHONY: distclean
distclean:
	@echo "CLEANING Modules ..."
	@$(call makeAll,$(EXTRAS_DIRS),$@)
	@echo "DONE."

###-----------------------------------------------------------------------------
