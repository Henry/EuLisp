###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### Documentation Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Documentation directories
###-----------------------------------------------------------------------------

DOC_DIRS = EuLisp-0.991

###-----------------------------------------------------------------------------
### Commands
###-----------------------------------------------------------------------------

.PHONY: all
all: euxlisp.1 euxlisp.html EuXLispNotes.html bindings
	@echo "COMPILING DOCSs ..."
	@$(call makeAll,$(DOC_DIRS),)
	@echo "DONE"

euxlisp.1: euxlisp.man
	nroff -man euxlisp.man > euxlisp.1

euxlisp.html: euxlisp.man
	rman -f HTML euxlisp.man > euxlisp.html

.PHONY: bindings
bindings: bindings.txt

bindings.txt: $(EUL_LIB_DIR)/liblevel1.i \
            $(EUL_DIR)/Youtoo/Runtime/macros.i \
            $(EUL_LIB_DIR)/libeval.i
	@$(MAKE) -C $(EUL_DIR)/Youtoo/Tools
	$(EUL_BIN_DIR)/i2doc \
		$(EUL_LIB_DIR)/liblevel1.i \
		$(EUL_DIR)/Youtoo/Runtime/macros.i \
		$(EUL_LIB_DIR)/libeval.i \
	> bindings.txt

.PHONY: clean
clean:
	@echo "CLEANING DOCs ..."
	@$(call makeAll,$(DOC_DIRS),$@)
	@echo "DONE"

.PHONY: gitclean
gitclean:
	@echo "GIT-CLEANING DOCs ..."
	@echo "DONE"

.PHONY: distclean
distclean:
	@echo "CLEANING DOCs ..."
	@$(call makeAll,$(DOC_DIRS),$@)
	@rm -f euxlisp.1
	@echo "DONE"

###-----------------------------------------------------------------------------
