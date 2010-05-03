### Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### EuLisp miscellaneous examples Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------

EXAMPLES = append div dphil echo echod empty fact ffi1 ffi2 hanoi \
	macro-dispatch-char nfib null padd sieve sumTo tak takl triang \
	url url2 url3
EXAMPLES_EXE = $(EXAMPLES:%=$(ARCH_DIR)/%)

OTHERS_EM = csem

SCHEME_EXAMPLES = puzzle

makeExe = ${U2} ${U2FLAGS} $(1) -od $(ARCH_DIR) -l level1

###-----------------------------------------------------------------------------

all: $(EXAMPLES_EXE)

# A stress test
$(ARCH_DIR)/append: append.em | $(ARCH_DIR)/.
	$(call makeExe,append)

$(ARCH_DIR)/div: div.em
	$(call makeExe,div)

$(ARCH_DIR)/dphil: dphil.em
	$(call makeExe,dphil)

$(ARCH_DIR)/echo: echo.em
	$(call makeExe,echo)

$(ARCH_DIR)/echod: echod.em
	$(call makeExe,echod)

$(ARCH_DIR)/empty: empty.em
	$(call makeExe,empty)

$(ARCH_DIR)/fact: fact.em
	$(call makeExe,fact)

$(ARCH_DIR)/ffi1: ffi1.em
	$(call makeExe,ffi1)

$(ARCH_DIR)/eul-ffi2.o: eul-ffi2.c
	$(CC) -c eul-ffi2.c -o $(ARCH_DIR)/eul-ffi2.o

$(ARCH_DIR)/ffi2: ffi2.em $(ARCH_DIR)/eul-ffi2.o
	${U2} ${U2FLAGS} ffi2 -od $(ARCH_DIR) -l level1 -fff eul-ffi2

$(ARCH_DIR)/hanoi: hanoi.em
	$(call makeExe,hanoi)

$(ARCH_DIR)/macro-dispatch-char: macro-dispatch-char.em macro-dispatch-char.dat
	$(call makeExe,macro-dispatch-char)

$(ARCH_DIR)/nfib: nfib.em
	$(call makeExe,nfib)

$(ARCH_DIR)/null: null.em
	$(call makeExe,null)

$(ARCH_DIR)/padd: padd.em
	$(call makeExe,padd)

# Needs ../Scheme in EUL_LOAD_PATH
$(ARCH_DIR)/puzzle: puzzle.em
	$(call makeExe,puzzle)

$(ARCH_DIR)/random2.c: random2.em
	${U2} ${U2FLAGS} -c random2 -od $(ARCH_DIR) -l level1 -l math

$(ARCH_DIR)/sieve: sieve.em
	$(call makeExe,sieve)

$(ARCH_DIR)/sumTo: sumTo.em
	${U2} ${U2FLAGS} sumTo -od $(ARCH_DIR) -l boot

$(ARCH_DIR)/tak: tak.em
	$(call makeExe,tak)

$(ARCH_DIR)/takl: takl.em
	$(call makeExe,takl)

$(ARCH_DIR)/triang: triang.em
	$(call makeExe,triang)

$(ARCH_DIR)/url: url.em
	$(call makeExe,url)

$(ARCH_DIR)/url2: url2.em
	$(call makeExe,url2)

$(ARCH_DIR)/url3: url3.em
	$(call makeExe,url3)

###-----------------------------------------------------------------------------

# Currently echo, echod and padd are not run because they are interactive
RUN = append div dphil empty fact ffi2 hanoi macro-dispatch-char nfib null \
	sieve sumTo tak takl triang url2
RUN_EXE = $(RUN:%=$(ARCH_DIR)/%)

run: $(RUN_EXE)
	@$(foreach example, $(RUN_EXE), \
		export LD_LIBRARY_PATH=$(EUL_LIB_DIR):$(LD_LIBRARY_PATH);\
		./$(example);)
	$(ARCH_DIR)/ffi1 "20"
	$(ARCH_DIR)/url www.cs.bath.ac.uk /~jap/ak1/youtoo/home.html
	$(ARCH_DIR)/url3 http://www.cs.bath.ac.uk/~jap/ak1/youtoo/home.html

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
