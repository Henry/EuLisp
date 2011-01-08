### Copyright 2010 Henry G. Weller
###-----------------------------------------------------------------------------
##  This file is part of
### ---                                     EuLisp
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
### Title: Examples Makefile
###  Maintainer: Henry G. Weller
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###  EuLisp Level-0 examples
EXAMPLES_0 = hanoi div append fact empty null nfib sieve sumTo tak takl triang \
	random2 rec

###  EuLisp Level-1 examples
EXAMPLES_1 = dphil echo echod padd url url2 url3

OTHERS_EM = csem

###  All examples
EXAMPLES = $(EXAMPLES_0) macro-dispatch-char $(EXAMPLES_1)
EXAMPLES_EXE = $(EXAMPLES:%=$(ARCH_DIR)/%)

###  Youtoo default compile rules for Level-0 and Level-1
makeExe0 = ${U2} ${U2FLAGS} $(1) --object-dir $(ARCH_DIR) -l level-0 -l math
makeExe1 = ${U2} ${U2FLAGS} $(1) --object-dir $(ARCH_DIR) -l level-1 -l math

###-----------------------------------------------------------------------------
### Youtoo compile rules for all examples
###-----------------------------------------------------------------------------
all: $(EXAMPLES_EXE)

# A stress test
$(ARCH_DIR)/append: append.em
	$(call makeExe0,append)

$(ARCH_DIR)/div: div.em
	$(call makeExe0,div)

$(ARCH_DIR)/dphil: dphil.em
	$(call makeExe1,dphil)

$(ARCH_DIR)/echo: echo.em
	$(call makeExe1,echo)

$(ARCH_DIR)/echod: echod.em
	$(call makeExe1,echod)

$(ARCH_DIR)/empty: empty.em
	$(call makeExe0,empty)

$(ARCH_DIR)/fact: fact.em
	$(call makeExe0,fact)

$(ARCH_DIR)/hanoi: hanoi.em
	$(call makeExe0,hanoi)

$(ARCH_DIR)/macro-dispatch-char: macro-dispatch-char.em
	$(call makeExe0,macro-dispatch-char)

$(ARCH_DIR)/nfib: nfib.em
	$(call makeExe0,nfib)

$(ARCH_DIR)/null: null.em
	$(call makeExe0,null)

$(ARCH_DIR)/padd: padd.em
	$(call makeExe1,padd)

$(ARCH_DIR)/random2: random2.em
	$(call makeExe0,random2)

$(ARCH_DIR)/rec: rec.em
	$(call makeExe0,rec)

$(ARCH_DIR)/sieve: sieve.em
	$(call makeExe0,sieve)

$(ARCH_DIR)/sumTo: sumTo.em
	$(call makeExe0,sumTo)

$(ARCH_DIR)/tak: tak.em
	$(call makeExe0,tak)

$(ARCH_DIR)/takl: takl.em
	$(call makeExe0,takl)

$(ARCH_DIR)/triang: triang.em
	$(call makeExe0,triang)

$(ARCH_DIR)/url: url.em
	$(call makeExe1,url)

$(ARCH_DIR)/url2: url2.em
	$(call makeExe1,url2)

$(ARCH_DIR)/url3: url3.em
	$(call makeExe1,url3)

###-----------------------------------------------------------------------------
### Run the examples
###-----------------------------------------------------------------------------
.PHONY: runeuxlisp
runeuxlisp:
	@$(foreach example, $(EXAMPLES_0), \
		echo "EuXLisp running " $(example); \
		$(EUL_BIN_DIR)/euxlisp -m $(example);)

# Currently echo, echod and padd are not run because they are interactive
RUN = fact append div dphil empty hanoi macro-dispatch-char nfib null \
	sieve sumTo tak takl triang url2

.PHONY: runyoutoo
runyoutoo: $(EXAMPLES_EXE)
	@$(foreach example, $(RUN), \
		echo "Youtoo running " $(example); \
		export LD_LIBRARY_PATH=$(EUL_LIB_DIR):$(LD_LIBRARY_PATH);\
		$(ARCH_DIR)/$(example);)
	$(ARCH_DIR)/url www.cs.bath.ac.uk /~jap/ak1/youtoo/home.html
	$(ARCH_DIR)/url3 http://www.cs.bath.ac.uk/~jap/ak1/youtoo/home.html

.PHONY: run
run: runeuxlisp runyoutoo

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
