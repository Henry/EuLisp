### Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### EuLisp Telos object system Makefile
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
BOOT_LIB_I = $(EUL_LIB_DIR)/libboot.i
BOOT_SHARED_LIB = $(EUL_LIB_DIR)/libboot.so

TELOS_LIB = $(EUL_LIB_DIR)/libtelos.a
TELOS_LIB_I = $(EUL_LIB_DIR)/libtelos.i
TELOS_SHARED_LIB = $(EUL_LIB_DIR)/libtelos.so

###-----------------------------------------------------------------------------
### Library dependencies
###-----------------------------------------------------------------------------

LEVEL1_LIB_I = $(EUL_LIB_DIR)/liblevel1.i

###-----------------------------------------------------------------------------
### Source and object files
###-----------------------------------------------------------------------------

BOOT_EM = boot.em boot1.em
BOOT_C = $(BOOT_EM:%.em=%.c)
BOOT_O = $(BOOT_C:%.c=$(ARCH_DIR)/%.o)

BOOT_MAC_EM = boot0.em
BOOT_MAC_I = boot0.i
BOOT_MAC_C = boot0.c

TELOS_EM = mop-access.em mop-alloc.em mop-class.em mop-defcl.em \
	mop-gf.em mop-init.em mop-inspect.em mop-key.em mop-meth.em mop-prim.em \
	telos.em
TELOS_C = $(BOOT_C) $(TELOS_EM:%.em=%.c)
TELOS_O = $(TELOS_C:%.c=$(ARCH_DIR)/%.o)

TELOS_MAC_EM = mop-gf0.em mop-meth0.em mop-defcl0.em telos0.em
TELOS_MAC_C = $(BOOT_MAC_C) $(TELOS_MAC_EM:%.em=%.c)
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
boot:	boot0.i _telos0.i libboot.i libtelos.i

# --- Called from ../Runtime/Makefile:boot during bootstrap building
# --- Should not be called directly
.PHONY: boot-syntax
boot-syntax: liblevel1.i
	$(U2) $(U2FLAGS) -c telos0 -od $(ARCH_DIR) \
		-load_path $(U2_DIR)/Telos -l level1
	@rm -f liblevel1.i
	$(MAKE)

$(BOOT_LIB_I): $(BOOT_EM) $(BOOT_MAC_EM)
$(TELOS_LIB_I): $(BOOT_EM) $(BOOT_MAC_EM) $(TELOS_EM) $(TELOS_MAC_EM)

$(EUL_LIB_DIR)/lib%.i : %.em
	$(U2) $(U2FLAGS) -ar $* -od $(ARCH_DIR) \
		-load_path $(U2_DIR)/Telos
	@mv $(@F) $@

lib%.i : $(EUL_LIB_DIR)/lib%.i
	@ln -s $(EUL_LIB_DIR)/$@ $@

ifeq ($(MAKECMDGOALS),boot)
.PRECIOUS: %.i %.c %.h
%.i %.c %.h : %.em
	$(U2) $(U2FLAGS) -c $* -od $(ARCH_DIR) \
		-load_path $(U2_DIR)/Telos -l level1
endif

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------

.PHONY: boot-clean
boot-clean: clean
	@rm -f *.[ihc]

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)

.PHONY: distclean
distclean:
	@rm -rf platforms

###-----------------------------------------------------------------------------
