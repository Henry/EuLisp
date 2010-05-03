### Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### EuLisp OPS5 rule-based system Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Module name
###-----------------------------------------------------------------------------

MODULE = ops5

###-----------------------------------------------------------------------------
### Library target files
###-----------------------------------------------------------------------------

MODULE_LIB = lib$(MODULE).a
MODULE_LIB_I = lib$(MODULE).i

EUL_MODULE_LIB = $(EUL_LIB_DIR)/$(MODULE_LIB)
EUL_MODULE_LIB_I = $(EUL_LIB_DIR)/$(MODULE_LIB_I)

###-----------------------------------------------------------------------------
### Source files
###-----------------------------------------------------------------------------

MODULE_EM = $(MODULE).em
MODULE_C = $(MODULE).c
MODULE_O = $(ARCH_DIR)/$(MODULE).o

###-----------------------------------------------------------------------------
### Test source files
###-----------------------------------------------------------------------------

TEST_EM = test1.em
TEST_EXE = $(TEST_EM:%.em=$(ARCH_DIR)/%)

###-----------------------------------------------------------------------------
### Build library
###-----------------------------------------------------------------------------

.PHONY: lib
lib: $(EUL_MODULE_LIB)

$(EUL_MODULE_LIB) $(EUL_MODULE_LIB_I): $(MODULE_EM) $(OTHER_EM) | $(ARCH_DIR)/.
	$(U2) $(U2FLAGS) -ar $(MODULE_EM) -od $(ARCH_DIR) -l level1 -recompile
	mv -f $(ARCH_DIR)/$(MODULE_LIB) $(EUL_MODULE_LIB)
	mv -f $(MODULE_LIB_I) $(EUL_MODULE_LIB_I)

###-----------------------------------------------------------------------------
### Test
###-----------------------------------------------------------------------------

$(ARCH_DIR)/% : %.em $(EUL_MODULE_LIB) $(EUL_MODULE_LIB_I) | $(ARCH_DIR)/.
	$(U2) $(U2FLAGS) $< -od $(ARCH_DIR) -l level1 -l $(MODULE) -no_recompile

.PHONY: test
test:  $(TEST_EXE)
	$(TEST_EXE) Mab.l

###-----------------------------------------------------------------------------
### Misc
###-----------------------------------------------------------------------------

.PHONY: README
README: README.html

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)
	@rm -f *.{c,h,i}

.PHONY: distclean
distclean: clean
	@rm -rf platforms
	@rm -f $(EUL_MODULE_LIB) $(EUL_MODULE_LIB_I)

###-----------------------------------------------------------------------------
