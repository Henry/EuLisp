### Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### Makefile for Youtoo tests
##
##  These specifically use $(U2) because they're used in
##  bootstrapping to check if everything bootstrapped ok.
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------

TESTS = test1 test2 test3 test4
TESTS_EXE = $(TESTS:%=$(ARCH_DIR)/%)

###-----------------------------------------------------------------------------

all: $(TESTS_EXE)

$(ARCH_DIR)/test1: test1.em | $(ARCH_DIR)/.
	$(U2) ${U2FLAGS} -od $(ARCH_DIR) test1

$(ARCH_DIR)/test2: test2.em | $(ARCH_DIR)/.
	$(U2) ${U2FLAGS} -od $(ARCH_DIR) test2 -l boot

$(ARCH_DIR)/test3: test3.em | $(ARCH_DIR)/.
	$(U2) ${U2FLAGS} -od $(ARCH_DIR) test3 -l telos

$(ARCH_DIR)/test4: test4.em | $(ARCH_DIR)/.
	$(U2) ${U2FLAGS} -od $(ARCH_DIR) test4 -l level1

test: $(TESTS_EXE)
	@$(foreach test, $(TESTS_EXE), \
		export LD_LIBRARY_PATH=$(EUL_LIB_DIR):$(LD_LIBRARY_PATH);\
		./$(test);)

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)
	@rm -f *.i *.c *.h

.PHONY: distclean
distclean: clean

###-----------------------------------------------------------------------------
