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
### Title: Telos object system Makefile
###  Maintainer: Henry G. Weller
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Reset U2 to use the bootstrap youtoo
###-----------------------------------------------------------------------------
U2_DEV := $(U2)
U2 = $(U2_BOOT)

###-----------------------------------------------------------------------------
### Output file names
###-----------------------------------------------------------------------------
BOOT_LIB = $(EUL_LIB_DIR)/libboot.a
BOOT_LIB_I = $(EUL_LIB_DIR)/$(U2_C_DIR)/libboot.i
BOOT_SHARED_LIB = $(EUL_LIB_DIR)/libboot.so

TELOS_LIB = $(EUL_LIB_DIR)/libtelos.a
TELOS_LIB_I = $(EUL_LIB_DIR)/$(U2_C_DIR)/libtelos.i
TELOS_SHARED_LIB = $(EUL_LIB_DIR)/libtelos.so

###-----------------------------------------------------------------------------
### Source and object files
###-----------------------------------------------------------------------------
BOOT_EM = boot.em boot1.em
BOOT_C = $(BOOT_EM:%.em=$(U2_C_DIR)/%.c)
BOOT_O = $(BOOT_C:%.c=$(ARCH_DIR)/%.o)

BOOT_MAC_EM = boot0.em
BOOT_MAC_I = $(U2_C_DIR)/boot0.i
BOOT_MAC_C = $(U2_C_DIR)/boot0.c

TELOS_EM = mop-access.em mop-alloc.em mop-class.em mop-defcl.em \
	mop-gf.em mop-init.em mop-inspect.em mop-key.em mop-meth.em mop-prim.em \
	telos.em
TELOS_C = $(BOOT_C) $(TELOS_EM:%.em=$(U2_C_DIR)/%.c)
TELOS_O = $(TELOS_C:%.c=$(ARCH_DIR)/%.o)

TELOS_MAC_EM = mop-gf0.em mop-meth0.em mop-defcl0.em telos0.em
TELOS_MAC_C = $(BOOT_MAC_C) $(TELOS_MAC_EM:%.em=$(U2_C_DIR)/%.c)
TELOS_MAC_O = $(TELOS_MAC_C:%.c=$(ARCH_DIR)/%.o)

###-----------------------------------------------------------------------------
### Commands to build library object files
###-----------------------------------------------------------------------------
.PHONY: lib
lib: $(BOOT_LIB) $(TELOS_LIB)

$(BOOT_LIB): $(BOOT_O)
	$(AR) $(BOOT_LIB) $(BOOT_O)
	@$(RANLIB) $(BOOT_LIB)

$(TELOS_LIB): $(TELOS_MAC_O) $(TELOS_O)
	$(AR) $(TELOS_LIB) $(TELOS_MAC_O) $(TELOS_O)
	@$(RANLIB) $(TELOS_LIB)

.PHONY: shared
shared:	$(BOOT_SHARED_LIB) $(TELOS_SHARED_LIB)

$(BOOT_SHARED_LIB): $(BOOT_O)
	$(U2_DIR)/Tools/makeso.$(OSTYPE) \
		$(CC) $(BOOT_SHARED_LIB) $(BOOT_O)

$(TELOS_SHARED_LIB): $(TELOS_MAC_O) $(TELOS_O)
	$(U2_DIR)/Tools/makeso.$(OSTYPE) \
		$(CC) $(TELOS_SHARED_LIB) $(TELOS_MAC_O) $(TELOS_O)

###-----------------------------------------------------------------------------
### Bootstrapping
###-----------------------------------------------------------------------------
.PHONY: boot
boot:	$(U2_C_DIR)/boot0.i $(U2_C_DIR)/_telos0.i libboot.i libtelos.i

# --- Called from ../Runtime/Makefile:boot during bootstrap building
# --- Should not be called directly
.PHONY: boot-syntax
boot-syntax: liblevel-1.i
	$(U2) $(U2FLAGS) -c telos0 \
		--object-dir $(ARCH_DIR) \
		--load-path $(U2_DIR)/Telos -l level-1
	@rm -f liblevel-1.i
	$(MAKE) boot-all

.PHONY: boot-all
boot-all: $(BOOT_C) $(BOOT_MAC_C) $(TELOS_MAC_C) $(TELOS_C)
	$(MAKE)

$(BOOT_LIB_I): $(BOOT_EM) $(BOOT_MAC_EM)
$(TELOS_LIB_I): $(BOOT_EM) $(BOOT_MAC_EM) $(TELOS_EM) $(TELOS_MAC_EM)

$(EUL_LIB_DIR)/$(U2_C_DIR)/lib%.i : %.em
	$(U2) $(U2FLAGS) --archive $* \
		--object-dir $(ARCH_DIR) \
		--load-path $(U2_DIR)/Telos
	@mv $(U2_C_DIR)/$(@F) $@

lib%.i : $(EUL_LIB_DIR)/$(U2_C_DIR)/lib%.i
	@ln -s $< $@

ifeq ($(findstring boot,$(MAKECMDGOALS)),boot)
.PRECIOUS: $(U2_C_DIR)/%.i $(U2_C_DIR)/%.c $(U2_C_DIR)/%.h
$(U2_C_DIR)/%.i $(U2_C_DIR)/%.c $(U2_C_DIR)/%.h : %.em
	$(U2) $(U2FLAGS) -c $< \
		--object-dir $(ARCH_DIR) \
		--load-path $(U2_DIR)/Telos -l level-1
endif

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------
.PHONY: boot-clean
boot-clean: clean
	@rm -rf $(U2_C_DIR)

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)

.PHONY: distclean
distclean:
	@rm -rf platforms

###-----------------------------------------------------------------------------
