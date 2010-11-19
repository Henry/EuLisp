### Copyright 1997 A. Kind & University of Bath
### Copyright 2010 Henry G. Weller
###-----------------------------------------------------------------------------
##  This file is part of
### ---                         EuLisp System 'Youtoo'
###-----------------------------------------------------------------------------
##
##  Youtoo is free software: you can redistribute it and/or modify it under the
##  terms of the GNU General Public License version 2 as published by the Free
##  Software Foundation.
##
##  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
##  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
##  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
##  details.
##
##  You should have received a copy of the GNU General Public License along with
##  this program.  If not, see <http://www.gnu.org/licenses/>.
##
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
U2TKFLAGS = --cflags -ltk$(EUL_TK_VERSION) --cflags -ltcl$(EUL_TK_VERSION)

$(ARCH_DIR)/% : %.em $(EUL_LIB_DIR)/$(U2_C_DIR)/libtcltk.i | $(ARCH_DIR)/.
	${U2} $(U2FLAGS) $< --object-dir $(ARCH_DIR) ${U2TKFLAGS} -l level-1 -l tcltk

all: $(TESTS_EXE)

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------
.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)

.PHONY: distclean
distclean: clean
	@rm -rf platforms $(U2_C_DIR)

###-----------------------------------------------------------------------------
