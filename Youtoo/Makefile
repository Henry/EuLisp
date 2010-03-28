## Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.

## ----------------------------------------------------------------------------
##                      EuLisp System 'youtoo'
## ----------------------------------------------------------------------------

## ----------------------------------------------------------------------------
## EuLisp youtoo executable Makefile
## ----------------------------------------------------------------------------

ARCH = $(shell uname -m)
include ../Lib.$(ARCH)/Makefile

## ----------------------------------------------------------------------------
## Output file names
## ----------------------------------------------------------------------------

YOUTOO = $(EUL_BIN_DIR)/youtoo
YOUTOO_NOGC = $(EUL_BIN_DIR)/youtoo_nogc

## ----------------------------------------------------------------------------
## Source and object files
## ----------------------------------------------------------------------------

EUL_APPL_C = eul-appl.c
EUL_APPL_O = $(EUL_APPL_C:%.c=$(EUL_LIB_DIR)/%.o)

GC_FAKE_C = gc-fake.c
FAKE_GC_O = $(GC_FAKE_C:%.c=$(EUL_LIB_DIR)/%.o)

ALL_C = $(EUL_APPL_C) $(GC_FAKE_C)

LIBS = libeulvm libboot libtelos liblevel1 libmath libeval
YOUTOO_LIBS := $(LIBS:%=$(EUL_LIB_DIR)/%.a)

ifeq ($(MAKECMDGOALS),shared)
    YOUTOO_LIBS := $(LIBS:%=$(EUL_LIB_DIR)/%.so)
else
    YOUTOO_LIBS := $(LIBS:%=$(EUL_LIB_DIR)/%.a)
endif

## ----------------------------------------------------------------------------
## Compiler + Interpreter
## ----------------------------------------------------------------------------

.PHONY: youtoo
youtoo:	$(YOUTOO)

.PHONY: shared
shared:	$(YOUTOO)

$(YOUTOO): $(EUL_APPL_O) $(YOUTOO_LIBS)
	$(CC) $(CFLAGS) -o $(YOUTOO) \
		$(EUL_APPL_O) -L$(EUL_LIB_DIR) \
		$(CLIBS) -lgc -lpthread

.PHONY: nogc
nogc: $(YOUTOO_NOGC)

$(YOUTOO_NOGC): $(EUL_APPL_O) $(FAKE_GC_O)
	$(CC) $(CFLAGS) -o $(YOUTOO_NOGC) \
		$(EUL_APPL_O) $(FAKE_GC_O) -L$(EUL_LIB_DIR) \
	 	$(CLIBS) -lpthread

$(EUL_LIB_DIR)/%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

## ----------------------------------------------------------------------------
## Miscellaneous commands
## ----------------------------------------------------------------------------

.PHONY: touch
touch:
	touch $(ALL_C)

.PHONY: boot-touch
boot-touch:

.PHONY: clean
clean:

.PHONY: distclean
distclean: clean
	@rm -rf platforms
	@rm -f $(EUL_APPL_O) $(FAKE_GC_O)

## ----------------------------------------------------------------------------
