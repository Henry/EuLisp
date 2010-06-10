### Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### EuLisp virtual-machine Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Module name
###-----------------------------------------------------------------------------

MODULE = tcltk

###-----------------------------------------------------------------------------
### Library target files
###-----------------------------------------------------------------------------

MODULE_LIB = lib$(MODULE).a
MODULE_LIB_I = lib$(MODULE).i

EUL_MODULE_LIB = $(EUL_LIB_DIR)/$(MODULE_LIB)
EUL_MODULE_LIB_I = $(EUL_LIB_DIR)/$(MODULE_LIB_I)

###-----------------------------------------------------------------------------
### Tcl/Tk libraries
###-----------------------------------------------------------------------------

EUL_TK_LIBS = -lX11 -ltk$(EUL_TK_VERSION) -ltcl$(EUL_TK_VERSION)

###-----------------------------------------------------------------------------
### Files
###-----------------------------------------------------------------------------

MODULE_EM = $(MODULE).em
MODULE_C = $(MODULE).c
MODULE_O = $(ARCH_DIR)/$(MODULE).o

OTHER_EM = \
	tk_class2.em    tk_general.em \
	tk_utils.em	tk_class.em      tk_commands.em \
	tk_images.em
OTHER_O = $(OTHER_EM:%.em=$(ARCH_DIR)/%.o)

FF_C = \
	StrOperations.c globalvariables.c tk_init.c FnCwidgets.c \
	FnCcommands.c FnCimages.c
FF_O = $(FF_C:%.c=$(ARCH_DIR)/%.o)

###-----------------------------------------------------------------------------
### Build library
###-----------------------------------------------------------------------------

.PHONY: lib
lib: $(EUL_MODULE_LIB)

$(MODULE_C) $(EUL_MODULE_LIB_I): $(MODULE_EM) $(OTHER_EM) | $(ARCH_DIR)/.
	$(U2) $(U2FLAGS) -ar $(MODULE_EM) -od $(ARCH_DIR) -l level1 -recompile
	mv -f $(MODULE_LIB_I) $(EUL_MODULE_LIB_I)

$(EUL_MODULE_LIB): $(EUL_MODULE_LIB_I) $(MODULE_O) $(OTHER_O) $(FF_O)
	$(AR) $(EUL_MODULE_LIB) $(MODULE_O) $(OTHER_O) $(FF_O)
	$(RANLIB) $(EUL_MODULE_LIB)
	rm -f $(MODULE_LIB)

###-----------------------------------------------------------------------------
### Build interpreter
###-----------------------------------------------------------------------------

.PHONY: you2tk
you2tk:	$(EUL_BIN_DIR)/you2tk

$(EUL_BIN_DIR)/you2tk: $(EUL_MODULE_LIB) you2tk.em You2tk_.c | $(ARCH_DIR)/.
	$(U2) $(U2FLAGS) -c you2tk -od $(ARCH_DIR) \
		-l level1 -l math -l eval -l tcltk -no_recompile
	./you2tki2c $(ARCH)
	$(CC) $(CFLAGS) $(EUL_TK_FLAGS) -o $(EUL_BIN_DIR)/you2tk \
		you2tk.c You2tk_.c you2tki.c \
	  	$(EUL_LIB_DIR)/eul-appl.o \
		-L$(EUL_LIB_DIR) -ltcltk $(CLIBS) $(EUL_TK_LIBS) -lgc

###-----------------------------------------------------------------------------
### Test
###-----------------------------------------------------------------------------

.PHONY: test
test:
	@echo "COMPILING EXTRASs ..."
	$(MAKE) -C Test all
	@echo "DONE"

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------

.PHONY: README
README: README.html

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)
	@rm -f $(MODULE_C) $(MODULE).h \
		$(OTHER_EM:%.em=%.c) $(OTHER_EM:%.em=%.h)
	@rm -f tcltk_.c you2tki.c you2tk.h you2tk.c
	@rm -f *.i
	$(MAKE) -C Test clean

.PHONY: distclean
distclean: clean
	@rm -rf platforms
	@rm -f $(EUL_MODULE_LIB) $(EUL_MODULE_LIB_I)
	$(MAKE) -C Test distclean

###-----------------------------------------------------------------------------
