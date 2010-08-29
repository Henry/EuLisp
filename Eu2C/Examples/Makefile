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

EXAMPLES_EM = $(wildcard *.em)
EXAMPLES = $(EXAMPLES_EM:%.em=%)

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------

.PHONY: clean
clean:
	@rm -f $(EXAMPLES) \
		$(EXAMPLES:%=%.c) $(EXAMPLES:%=%.h) $(EXAMPLES:%=%.o) \
		$(EXAMPLES:%=%.inst) $(EXAMPLES:%=%.log) *~

.PHONY: distclean
distclean: clean

###-----------------------------------------------------------------------------
