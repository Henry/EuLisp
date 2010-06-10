### Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### Top-level Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------

.PHONY: all
all: youtoo doc

.PHONY: youtoo
youtoo:
	@echo "BUILDING Youtoo ..."
	@cd Youtoo; $(MAKE)
	@echo "DONE"

README.org: index.org
	@sed 's%file:%http://henry.github.com/EuLisp/%' $< > $@

.PHONY: README
README: index.html TODO.html README.org
	@echo "UPDATING all README.html files ..."
	@cd Modules; $(MAKE) $@
	@echo "DONE"

.PHONY: doc
doc: README
	@cd Doc; $(MAKE)

.PHONY: clean
clean:
	@echo "CLEANING ..."
	@cd Youtoo; $(MAKE) $@
	@cd Modules; $(MAKE) $@
	@cd Examples; $(MAKE) $@
	@cd Doc; $(MAKE) $@
	@echo "DONE"

.PHONY: distclean
distclean: clean
	@echo "DIST-CLEANING ..."
	@cd Youtoo; $(MAKE) $@
	@cd Modules; $(MAKE) $@
	@cd Examples; $(MAKE) $@
	@cd Doc; $(MAKE) $@
	@rm -rf */platforms Bin.* Lib.*
	@rm -f .eulrc.*
	@echo "DONE"

###-----------------------------------------------------------------------------
