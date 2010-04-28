## Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.

## ----------------------------------------------------------------------------
##                      EuLisp System 'youtoo'
## ----------------------------------------------------------------------------

## ----------------------------------------------------------------------------
## EuLisp Telos object system Makefile
## ----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../Lib.$(ARCH)/Makefile

## ----------------------------------------------------------------------------
## Output file names
## ----------------------------------------------------------------------------

BOOT_LIB = $(EUL_LIB_DIR)/libboot.a
BOOT_LIB_I = $(EUL_LIB_DIR)/libboot.i
BOOT_SHARED_LIB = $(EUL_LIB_DIR)/libboot.so

TELOS_LIB = $(EUL_LIB_DIR)/libtelos.a
TELOS_LIB_I = $(EUL_LIB_DIR)/libtelos.i
TELOS_SHARED_LIB = $(EUL_LIB_DIR)/libtelos.so

## ----------------------------------------------------------------------------
## Source and object files
## ----------------------------------------------------------------------------

BOOT_EM = boot.em boot1.em
BOOT_C = $(BOOT_EM:%.em=%.c)
BOOT_O = $(BOOT_C:%.c=$(ARCH_DIR)/%.o)

BOOT_MAC_EM = boot0.em
BOOT_MAC_C = boot0.c

TELOS_EM = mop-access.em mop-alloc.em mop-class.em mop-defcl.em \
	mop-gf.em mop-init.em mop-inspect.em mop-key.em mop-meth.em mop-prim.em \
	telos.em

TELOS_C = $(BOOT_C) $(TELOS_EM:%.em=%.c)
TELOS_O = $(TELOS_C:%.c=$(ARCH_DIR)/%.o)

TELOS_MAC_EM = mop-gf0.em mop-meth0.em mop-defcl0.em telos0.em
TELOS_MAC_C = $(BOOT_MAC_C) $(TELOS_MAC_EM:%.em=%.c)
TELOS_MAC_O = $(TELOS_MAC_C:%.c=$(ARCH_DIR)/%.o)

## ----------------------------------------------------------------------------
## Commands to build library object files
## ----------------------------------------------------------------------------

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
	$(EUL_DIR)/Tools/makeso.$(OSTYPE) \
		$(CC) $(BOOT_SHARED_LIB) $(BOOT_O)

$(TELOS_SHARED_LIB): $(TELOS_MAC_O) $(TELOS_O)
	$(EUL_DIR)/Tools/makeso.$(OSTYPE) \
		$(CC) $(TELOS_SHARED_LIB) $(TELOS_MAC_O) $(TELOS_O)

## ----------------------------------------------------------------------------
## Bootstrapping
## ----------------------------------------------------------------------------

.PHONY: boot
boot:	boot0.i _telos0.i $(BOOT_LIB_I) $(TELOS_LIB_I) $(BOOT_LIB) $(TELOS_LIB)

.PHONY: boot-syntax
boot-syntax:
	$(U2) $(U2FLAGS) -c telos0 -od $(ARCH_DIR) \
	       -load_path $(EUL_DIR)/Telos -l level1
	$(MAKE)

$(BOOT_LIB_I): $(BOOT_EM) $(BOOT_MAC_EM)
$(TELOS_LIB_I): $(BOOT_EM) $(BOOT_MAC_EM) $(TELOS_EM) $(TELOS_MAC_EM)

$(EUL_LIB_DIR)/lib%.i : %.em
	$(U2) $(U2FLAGS) -ar $* -od $(ARCH_DIR) -load_path $(EUL_DIR)/Telos
	mv $(@F) $@

.PRECIOUS: %.i %.c %.h
%.i %.c %.h : %.em
	$(U2) $(U2FLAGS) -c $* -od $(ARCH_DIR) \
		-load_path $(EUL_DIR)/Telos -l level1

## ----------------------------------------------------------------------------
## Miscellaneous commands
## ----------------------------------------------------------------------------

.PHONY: touch
touch:
	@touch $(TELOS_C) $(TELOS_MAC_C)

.PHONY: boot-touch
boot-touch:
	@touch *.em

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)

.PHONY: distclean
distclean:
	@rm -rf platforms

## ----------------------------------------------------------------------------
