### Copyright 2010 Henry G. Weller
###-----------------------------------------------------------------------------
##  This file is part of
### ---                           EuLisp System 'Youtoo'
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
### Title: Tools Makefile
###  Maintainer: Henry G. Weller
###  Description:
##    Building these with a version of Youtoo without dynamic linking makes
##    bootstrapping simpler.
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Source files
###-----------------------------------------------------------------------------
B2H_EM = b2h.em b2h-aux.em
B2H_C = $(B2H_EM:%.em=%.c) b2h_.c

I2DOC_EM = i2doc.em
I2DOC_C = $(I2DOC_EM:%.em=%.c) i2doc_.c

###-----------------------------------------------------------------------------
### Targets
###-----------------------------------------------------------------------------
all: b2h i2doc

###-----------------------------------------------------------------------------
### Compile b2h directly from the EuLisp source.
###-----------------------------------------------------------------------------
.PHONY: b2h
b2h: $(EUL_BIN_DIR)/b2h

$(EUL_BIN_DIR)/b2h: $(B2H_EM) | $(ARCH_DIR)/.
	$(MAKE) -C $(EUL_DIR)/Modules/Vmeta
	${U2} ${U2FLAGS} b2h \
		--object-dir $(ARCH_DIR) \
		-l level-1 \
		--load-path $(EUL_DIR)/Modules/Vmeta
	mv $(ARCH_DIR)/b2h $(EUL_BIN_DIR)/b2h

###-----------------------------------------------------------------------------
### Compile i2doc directly from the EuLisp source.
###-----------------------------------------------------------------------------
.PHONY: i2doc
i2doc: $(EUL_BIN_DIR)/i2doc

$(EUL_BIN_DIR)/i2doc: $(I2DOC_EM) $(ARCH_DIR)/i2doc-ext.o | $(ARCH_DIR)/.
	${U2} ${U2FLAGS} i2doc --object-dir $(ARCH_DIR) -l level-1 --fff i2doc-ext
	mv $(ARCH_DIR)/i2doc $(EUL_BIN_DIR)/i2doc

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------
.PHONY: boot-clean
boot-clean: clean

clean:
	@rm -rf $(ARCH_DIR)

distclean: clean
	@rm -rf platforms $(U2_C_DIR)
	@rm -f $(EUL_BIN_DIR)/b2h $(EUL_BIN_DIR)/i2doc

###-----------------------------------------------------------------------------
