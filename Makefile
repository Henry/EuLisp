### Copyright 1997 A. Kind & University of Bath
### Copyright 2010 Henry G. Weller
###-----------------------------------------------------------------------------
##  This file is part of
### ---                                     EuLisp
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
### Title: EuLisp Top-level Makefile
###  Maintainer: Henry G. Weller
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------
.PHONY: default
default: euxlisp youtoo

.PHONY: all
all: default doc

.PHONY: euxlisp
euxlisp:
	@echo "BUILDING EuXLisp ..."
	@$(MAKE) -C EuXLisp
	@echo "DONE"

.PHONY: youtoo
youtoo:
	@echo "BUILDING Youtoo ..."
	@$(MAKE) -C Youtoo
	@echo "DONE"

.PHONY: test
test:
	@echo "TESTING installation ..."
	@$(MAKE) -C Youtoo test
	@echo "DONE"

README.org: index.org
	@sed 's%file:%http://henry.github.com/EuLisp/%' $< > $@

.PHONY: README
README: index.html TODO.html README.org
	@echo "UPDATING all README.html files ..."
	@$(MAKE) -C Youtoo $@
	@$(MAKE) -C Eu2C $@
	@$(MAKE) -C Modules $@
	@echo "DONE"

.PHONY: doc
doc: README
	@$(MAKE) -C Doc

.PHONY: clean
clean:
	@echo "CLEANING ..."
	@$(MAKE) -C Youtoo $@
	@$(MAKE) -C Modules $@
	@$(MAKE) -C Examples $@
	@$(MAKE) -C Doc $@
	@echo "DONE"

.PHONY: gitclean
gitclean:
	@echo "GIT-CLEANING ..."
	@$(MAKE) -C Youtoo $@
	@$(MAKE) -C Modules $@
	@$(MAKE) -C Examples $@
	@$(MAKE) -C Doc $@
	@echo "DONE"

.PHONY: distclean
distclean: clean
	@echo "DIST-CLEANING ..."
	@$(MAKE) -C Youtoo $@
	@$(MAKE) -C Modules $@
	@$(MAKE) -C Examples $@
	@$(MAKE) -C Doc $@
	@rm -rf */platforms Bin.* Lib.*
	@rm -f .eulrc.*
	@echo "DONE"

###-----------------------------------------------------------------------------
