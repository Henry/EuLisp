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
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

# Set the default machine word-length to 32
# but override the default for x86_84 architectures
# May also be overridden on the command-line
WL = m32
ifeq ($(ARCH),x86_64)
    WL = m64
endif

ARCH := $(ARCH)$(WL)

###-----------------------------------------------------------------------------
### All examples
###-----------------------------------------------------------------------------
EXAMPLES_EM = $(wildcard *.em)
EXAMPLES = $(EXAMPLES_EM:%.em=%)

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
