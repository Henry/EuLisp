###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### EuLisp C++ examples Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------

TEST = test1 test2
TEST_EM = $(TEST:%=%.em)

FF_CC = eul-cc1.cc eul-cc3.cc

FF2 = eul-cc2
FF2_C = $(FF2:%=%.c)
FF2_O = $(FF2_C:%.c=$(ARCH_DIR)/%.o)

###-----------------------------------------------------------------------------

all: $(ARCH_DIR)/test1 $(ARCH_DIR)/test2

$(ARCH_DIR)/eul-cc1.o: eul-cc1.cc | $(ARCH_DIR)/.
	c++ -c eul-cc1.cc -o $(ARCH_DIR)/eul-cc1.o

$(ARCH_DIR)/test1: test1.em $(ARCH_DIR)/eul-cc1.o
	${U2} ${U2FLAGS} test1 -od $(ARCH_DIR) \
		-cflags -lstdc++ -l level1 -fff eul-cc1

test2.h: test2.em | $(ARCH_DIR)/.
	${U2} ${U2FLAGS} -c test2 -od $(ARCH_DIR) -l level1

$(FF2_O): $(FF2_C) test2.h | $(ARCH_DIR)/.
	$(CC) $(CFLAGS) -c $(FF2_C) -o $(FF2_O)

$(ARCH_DIR)/eul-cc3.o: eul-cc3.cc | $(ARCH_DIR)/.
	c++ -c eul-cc3.cc -o $(ARCH_DIR)/eul-cc3.o

$(ARCH_DIR)/test2: test2.em $(FF2_O) $(ARCH_DIR)/eul-cc3.o
	${U2} ${U2FLAGS} test2 -od $(ARCH_DIR) \
		-cflags -lstdc++ \
		-l level1 -fff $(FF2) -fff eul-cc3 -recompile

###-----------------------------------------------------------------------------

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)
	@rm -f $(TEST_EM:.em=.c) $(TEST_EM:.em=.h) $(TEST_EM:.em=_.c) *.i

.PHONY: distclean
distclean: clean
	@rm -rf platforms

###-----------------------------------------------------------------------------
