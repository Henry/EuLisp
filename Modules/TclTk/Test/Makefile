### Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
## EuLisp TckTk/Test Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Test executable names
###-----------------------------------------------------------------------------

TESTS = \
	tk_browser tk_button \
	tk_callback tk_canvas tk_class_hierchy tk_graph_editor tk_image \
	tk_menu tk_misc tk_scale tk_scrollbar1 tk_scrollbar2 tk_text
TESTS_EXE = $(TESTS:%=$(ARCH_DIR)/%)

###-----------------------------------------------------------------------------

U2TKFLAGS = -cflags -ltk$(EUL_TK_VERSION) -cflags -ltcl$(EUL_TK_VERSION)

$(ARCH_DIR)/% : %.em $(EUL_LIB_DIR)/libtcltk.i | $(ARCH_DIR)/.
	${U2} $(U2FLAGS) $< -od $(ARCH_DIR) ${U2TKFLAGS} -l level1 -l tcltk

all: $(TESTS_EXE)

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)
	@rm -f *.{c,h,i} *_.{c,h,i}

.PHONY: distclean
distclean: clean
	@rm -rf platforms

###-----------------------------------------------------------------------------
