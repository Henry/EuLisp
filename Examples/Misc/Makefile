### Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
###-----------------------------------------------------------------------------
### ---                             EuLisp Examples
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### EuLisp miscellaneous examples Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### EuLisp Level-0 examples
###-----------------------------------------------------------------------------
EXAMPLES_0 = fact hanoi sieve
EXAMPLES_0_EM = $(EXAMPLES_0:%=%.em)

###-----------------------------------------------------------------------------
### EuLisp Level-1 examples
###-----------------------------------------------------------------------------
EXAMPLES_1 = append div dphil echo echod empty \
	macro-dispatch-char nfib null padd sumTo tak takl triang \
	url url2 url3

OTHERS_EM = csem

###-----------------------------------------------------------------------------
### All examples
###-----------------------------------------------------------------------------
EXAMPLES = $(EXAMPLES_0) $(EXAMPLES_1)
EXAMPLES_EXE = $(EXAMPLES:%=$(ARCH_DIR)/%)

###-----------------------------------------------------------------------------
### Youtoo default compile rules for Level-0 and Level-1
###-----------------------------------------------------------------------------
makeExe0 = ${U2} ${U2FLAGS} $(1) -od $(ARCH_DIR) -l level0
makeExe1 = ${U2} ${U2FLAGS} $(1) -od $(ARCH_DIR) -l level1

###-----------------------------------------------------------------------------
### Youtoo compile rules for all examples
###-----------------------------------------------------------------------------
all: $(EXAMPLES_EXE)

# A stress test
$(ARCH_DIR)/append: append.em | $(ARCH_DIR)/.
	$(call makeExe1,append)

$(ARCH_DIR)/div: div.em
	$(call makeExe1,div)

$(ARCH_DIR)/dphil: dphil.em
	$(call makeExe1,dphil)

$(ARCH_DIR)/echo: echo.em
	$(call makeExe1,echo)

$(ARCH_DIR)/echod: echod.em
	$(call makeExe1,echod)

$(ARCH_DIR)/empty: empty.em
	$(call makeExe1,empty)

$(ARCH_DIR)/fact: fact.em
	$(call makeExe0,fact)

$(ARCH_DIR)/hanoi: hanoi.em
	$(call makeExe0,hanoi)

$(ARCH_DIR)/macro-dispatch-char: macro-dispatch-char.em macro-dispatch-char.dat
	$(call makeExe1,macro-dispatch-char)

$(ARCH_DIR)/nfib: nfib.em
	$(call makeExe1,nfib)

$(ARCH_DIR)/null: null.em
	$(call makeExe1,null)

$(ARCH_DIR)/padd: padd.em
	$(call makeExe1,padd)

$(ARCH_DIR)/random2.c: random2.em
	${U2} ${U2FLAGS} -c random2 -od $(ARCH_DIR) -l level1 -l math

$(ARCH_DIR)/sieve: sieve.em
	$(call makeExe0,sieve)

$(ARCH_DIR)/sumTo: sumTo.em
	${U2} ${U2FLAGS} sumTo -od $(ARCH_DIR) -l boot

$(ARCH_DIR)/tak: tak.em
	$(call makeExe1,tak)

$(ARCH_DIR)/takl: takl.em
	$(call makeExe1,takl)

$(ARCH_DIR)/triang: triang.em
	$(call makeExe1,triang)

$(ARCH_DIR)/url: url.em
	$(call makeExe1,url)

$(ARCH_DIR)/url2: url2.em
	$(call makeExe1,url2)

$(ARCH_DIR)/url3: url3.em
	$(call makeExe1,url3)

###-----------------------------------------------------------------------------
###  Run the examples
###-----------------------------------------------------------------------------
.PHONY: runeuxlisp
runeuxlisp: $(EXAMPLES_0_EM)
	@$(foreach example, $(EXAMPLES_0_EM), \
		$(EUL_BIN_DIR)/euxlisp -m $(example);)

# Currently echo, echod and padd are not run because they are interactive
RUN = fact append div dphil empty hanoi macro-dispatch-char nfib null \
	sieve sumTo tak takl triang url2

.PHONY: runyoutoo
runyoutoo: $(EXAMPLES_EXE)
	@$(foreach example, $(RUN), \
		export LD_LIBRARY_PATH=$(EUL_LIB_DIR):$(LD_LIBRARY_PATH);\
		$(ARCH_DIR)/$(example);)
	$(ARCH_DIR)/url www.cs.bath.ac.uk /~jap/ak1/youtoo/home.html
	$(ARCH_DIR)/url3 http://www.cs.bath.ac.uk/~jap/ak1/youtoo/home.html

.PHONY: run
run: runeuxlisp runyoutoo

###-----------------------------------------------------------------------------
.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)
	@rm -f $(EXAMPLES:%=%.c) $(EXAMPLES:%=%_.c) $(EXAMPLES:%=%.h) *.i
	@rm -f $(OTHERS_EM:%=%.c) $(OTHERS_EM:%=%_.c) $(OTHERS_EM:%=%.h)

.PHONY: distclean
distclean: clean
	@rm -rf platforms

###-----------------------------------------------------------------------------
