### Copyright 2010 Henry G. Weller
###-----------------------------------------------------------------------------
##  This file is part of
### ---                           EuLisp System 'Eu2C'
###-----------------------------------------------------------------------------
##
##  Eu2C is free software: you can redistribute it and/or modify it under the
##  terms of the GNU General Public License version 2 as published by the Free
##  Software Foundation.
##
##  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
##  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
##  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
##  details.
##
##  You should have received a copy of the GNU General Public License along with
##  this program.  If not, see <http://www.gnu.org/licenses/>.
##
###-----------------------------------------------------------------------------
###  Title: Examples Makefile
###  Author: Henry G. Weller
###  Maintainer: Henry G. Weller
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Basic system and optimisation switch
###-----------------------------------------------------------------------------
BS0 = -bs level-0
BS0X = -bs level-0x

OPT =
# If optimisation is set to "ti" (type-inference) unset the basic system
ifeq ($(OPT),ti)
    BS0 =
    BS0X =
endif

###-----------------------------------------------------------------------------
### All examples
###-----------------------------------------------------------------------------
EXAMPLES_0 = gtakl2 gtakl hanoi hello tak takl \
             test-class test-double apropos test-number

EXAMPLES_0X = command-line gtakl2t taklt takt umdrehen

EXAMPLES_0_EXE = $(EXAMPLES_0:%=$(ARCH_DIR)/%)
EXAMPLES_0X_EXE = $(EXAMPLES_0X:%=$(ARCH_DIR)/%)

EXAMPLES = $(EXAMPLES_0) $(EXAMPLES_0X)
EXAMPLES_EXE = $(EXAMPLES_0_EXE) $(EXAMPLES_0X_EXE)

###-----------------------------------------------------------------------------
### Eu2C compile rules for all examples
###-----------------------------------------------------------------------------
all: all0 all0x

all0: $(EXAMPLES_0_EXE)
all0x: $(EXAMPLES_0X_EXE)

$(EXAMPLES_0_EXE): $(ARCH_DIR)/%: %.em \
		$(EUL_DIR)/Eu2C/EuLispModules/$(ARCH_DIR)/level-0.def
	../Bin/eu2c -arch $(ARCH) $(BS0) $<

$(EXAMPLES_0X_EXE): $(ARCH_DIR)/%: %.em \
		$(EUL_DIR)/Eu2C/EuLispModules/$(ARCH_DIR)/level-0x.def
	../Bin/eu2c -arch $(ARCH) $(BS0X) $<

###-----------------------------------------------------------------------------
###  Run the examples
###-----------------------------------------------------------------------------
.PHONY: run
run: $(EXAMPLES_EXE)
	@$(foreach example, $(EXAMPLES_EXE), \
		echo "Running " $(example); \
		$(example);)

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------
.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)

.PHONY: distclean
distclean: clean
	@rm -rf platforms

###-----------------------------------------------------------------------------
