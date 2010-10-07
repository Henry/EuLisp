###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### EuLisp string-perls Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------

EXAMPLES = string-perls
EXAMPLES_EXE = $(EXAMPLES:%=$(ARCH_DIR)/%)

makeExe = ${U2} ${U2FLAGS} $(1) -od $(ARCH_DIR) -l level-0

###-----------------------------------------------------------------------------

all: $(EXAMPLES_EXE)

$(ARCH_DIR)/eul-string-perls.o: eul-string-perls.c | $(ARCH_DIR)/.
	$(CC) $(CFLAGS) -c eul-string-perls.c -o $(ARCH_DIR)/eul-string-perls.o

$(ARCH_DIR)/string-perls: string-perls.em $(ARCH_DIR)/eul-string-perls.o
	${U2} ${U2FLAGS} string-perls -od $(ARCH_DIR) -l level-0 \
		-cflags -lpcre \
		-fff eul-string-perls

###-----------------------------------------------------------------------------

RUN = string-perls
RUN_EXE = $(RUN:%=$(ARCH_DIR)/%)

run: $(RUN_EXE)
	@$(foreach example, $(RUN_EXE), \
		export LD_LIBRARY_PATH=$(EUL_LIB_DIR):$(LD_LIBRARY_PATH);\
		./$(example);)

###-----------------------------------------------------------------------------

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)
	@rm -f $(EXAMPLES:%=%.c) $(EXAMPLES:%=%_.c) $(EXAMPLES:%=%.h) *.i

.PHONY: distclean
distclean: clean
	@rm -rf platforms

###-----------------------------------------------------------------------------
