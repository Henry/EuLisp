### Copyright 2010 Henry G. Weller
###-----------------------------------------------------------------------------
##  This file is part of
### ---                                     EuLisp
###-----------------------------------------------------------------------------
##
##  Eu2C is free software: you can redistribute it and/or modify it under the
##  terms of the GNU General Public License version 2 as published by the Free
##  Software Foundation.
##
##  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
##  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
##  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
##  details.
##
##  You should have received a copy of the GNU General Public License along with
##  this program.  If not, see <http://www.gnu.org/licenses/>.
##
###-----------------------------------------------------------------------------
### Title: Documentation Makefile
###  Maintainer: Henry G. Weller
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
all: euxlisp.1 euxlisp.html EuLispIntro.html EuLispIntroRef.html bindings
	@echo "COMPILING DOCSs ..."
	@$(call makeAll,$(DOC_DIRS),)
	@echo "DONE"

euxlisp.1: euxlisp.man
	nroff -man euxlisp.man > euxlisp.1

euxlisp.html: euxlisp.man
	rman -f HTML euxlisp.man > euxlisp.html

EuLispIntroRef.org: EuLispIntro.org
	@sed -f EuLisp-0.991/hypersed $< > $@

.PHONY: bindings
bindings: bindings.txt

bindings.txt: $(EUL_LIB_DIR)/u2/liblevel-1.i \
            $(EUL_DIR)/Youtoo/Runtime/u2/syntax-1.i \
            $(EUL_LIB_DIR)/u2/libeval.i
	@$(MAKE) -C $(EUL_DIR)/Youtoo/Tools
	$(EUL_BIN_DIR)/i2doc \
		$(EUL_LIB_DIR)/u2/liblevel-1.i \
		$(EUL_DIR)/Youtoo/Runtime/u2/syntax-1.i \
		$(EUL_LIB_DIR)/u2/libeval.i \
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
