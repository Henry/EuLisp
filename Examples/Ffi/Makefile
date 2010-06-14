###-----------------------------------------------------------------------------
### ---                    EuLisp Ffi Examples
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### EuLisp Ffi example Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------

EXAMPLES = ffi ffi1 ffi2
EXAMPLES_EXE = $(EXAMPLES:%=$(ARCH_DIR)/%)

###-----------------------------------------------------------------------------
### Youtoo compile rules for all examples
###-----------------------------------------------------------------------------
all: $(EXAMPLES_EXE)

$(ARCH_DIR)/ffi: ffi.em $(ARCH_DIR)/ffi-ext.o | $(ARCH_DIR)/.
	${U2} ${U2FLAGS} ffi -od $(ARCH_DIR) -l level1 -fff ffi-ext

$(ARCH_DIR)/ffi1: ffi1.em
	${U2} ${U2FLAGS} ffi1 -od $(ARCH_DIR) -l level1

$(ARCH_DIR)/eul-ffi2.o: eul-ffi2.c
	$(CC) $(CFLAGS) -c eul-ffi2.c -o $(ARCH_DIR)/eul-ffi2.o

$(ARCH_DIR)/ffi2: ffi2.em $(ARCH_DIR)/eul-ffi2.o
	${U2} ${U2FLAGS} ffi2 -od $(ARCH_DIR) -l level1 -fff eul-ffi2

###-----------------------------------------------------------------------------
###  Run the examples
###-----------------------------------------------------------------------------
RUN = ffi ffi2

.PHONY: run
run: $(EXAMPLES_EXE)
	@$(foreach example, $(RUN), \
		export LD_LIBRARY_PATH=$(EUL_LIB_DIR):$(LD_LIBRARY_PATH);\
		$(ARCH_DIR)/$(example);)
	$(ARCH_DIR)/ffi1 "20"

###-----------------------------------------------------------------------------
.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)
	@rm -f ffi[0-9]*.c ffi[0-9]*.h *.i *_.c

.PHONY: distclean
distclean: clean
	@rm -rf platforms

###-----------------------------------------------------------------------------
