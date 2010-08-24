### Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### EuLisp compile-time Makefile
###-----------------------------------------------------------------------------

ARCH = $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Reset U2 to use the bootstrap executable
###-----------------------------------------------------------------------------

U2_DEV := $(U2)
U2 = $(U2_BOOT)

###-----------------------------------------------------------------------------
### Output file names
###-----------------------------------------------------------------------------

EVAL_LIB = $(EUL_LIB_DIR)/libeval.a
EVAL_LIB_I = $(EUL_LIB_DIR)/libeval.i
EVAL_SHARED_LIB = $(EUL_LIB_DIR)/libeval.so

###-----------------------------------------------------------------------------
### Source and object files
###-----------------------------------------------------------------------------

EVAL_EM := \
    an-side.em aux-table.em cg-asm.em cg-bycode.em cg-bycode1.em cg-bycode2.em \
    cg-dld.em cg-exec.em cg-gen.em cg-interf.em cg-link.em cg-stack.em cg-state.em eval.em \
    ex-body.em ex-direct.em ex-expose.em ex-expr.em ex-import.em ex-module.em \
    ex-syntax.em i-all.em i-args.em i-compile.em i-error.em i-ffi.em \
    i-level1.em i-modify.em i-notify.em i-param.em i-rep.em op-peep-r.em op-peep.em \
    p-env.em p-parse.em p-read.em sx-node.em sx-obj.em sx-obj1.em sx-obj2.em \
    sx-write.em youtoo.em i-aux0.em
EVAL_C := $(EVAL_EM:%.em=%.c)
EVAL_O := $(EVAL_C:%.c=$(ARCH_DIR)/%.o)

EVAL_FF_C =  \
    $(WORD_LENGTH)bit/cg-exec-word-length.c \
    Ff/eul-prompt-string.c Ff/eul-dld.c Ff/Youtoo_.c

EVAL_C += $(EVAL_FF_C)
EVAL_O += $(EVAL_FF_C:%.c=$(ARCH_DIR)/%.o)

###-----------------------------------------------------------------------------
### Link level1 libraries
###-----------------------------------------------------------------------------

.PHONY: static
static:	$(EVAL_LIB)

$(EVAL_LIB): $(EVAL_O)
	$(AR) $(EVAL_LIB) $(EVAL_O)
	@$(RANLIB) $(EVAL_LIB)

.PHONY: shared
shared:	$(EVAL_SHARED_LIB)

$(EVAL_SHARED_LIB): $(EVAL_O)
	$(U2_DIR)/Tools/makeso.$(OSTYPE) \
		$(CC) $(EVAL_SHARED_LIB) $(EVAL_O)

###-----------------------------------------------------------------------------
### Bootstrapping
###-----------------------------------------------------------------------------

.PHONY: boot
boot: static

.PHONY: boot-preliminary
boot-preliminary: _ex-aux0.i _op-peep0.i _cg-bycode0.i _i-aux0.i _sx-obj0.i \
		libboot.i liblevel1.i libmath.i \
		$(ARCH_DIR)/cg-exec-word-length.o
	$(U2) $(U2FLAGS) -od $(ARCH_DIR) -c ex-aux0 op-peep0 cg-bycode0 i-aux0 sx-obj0 \
	       -load_path $(U2_DIR)/Comptime2 \
	       -load_path $(U2_DIR)/Comptime2/$(WORD_LENGTH)bit \
	       -l level1
	$(U2) $(U2FLAGS) -c ex-expr -od $(ARCH_DIR) \
	       -load_path $(U2_DIR)/Comptime2 \
	       -load_path $(U2_DIR)/Comptime2/$(WORD_LENGTH)bit \
	       -load_path $(U2_DIR)/Runtime \
	       -load_path $(U2_DIR)/Telos \
	       -l boot -l level1 -l math
	$(U2) $(U2FLAGS) -ar eval -od $(ARCH_DIR) \
	       -load_path $(U2_DIR)/Comptime2 \
	       -load_path $(U2_DIR)/Comptime2/$(WORD_LENGTH)bit \
	       -load_path $(U2_DIR)/Runtime \
	       -load_path $(U2_DIR)/Telos \
	       -l boot -l level1 -l math
	@echo "---- CREATING youtoo.em module for preliminary youtoo executable"
	@sed 	-e 's@#modules#@level1 math eval@' \
		-e 's@#which#@Preliminary executable!@' \
		<youtoo.em.in >youtoo.em
	@echo "---- CREATING preliminary youtoo executable files"
	$(U2) $(U2FLAGS) -c youtoo -od $(ARCH_DIR) \
	       -load_path $(U2_DIR)/Comptime2 \
	       -load_path $(U2_DIR)/Comptime2/$(WORD_LENGTH)bit \
	       -l level1 -l math -l eval -recompile
	@rm -f libboot.i liblevel1.i libmath.i
	@mv libeval.i $(EUL_LIB_DIR)
	@rm -f $(ARCH_DIR)/libeval.a $(EUL_LIB_DIR)/libeval.a $(ARCH_DIR)/youtoo.o
	@echo "---- DONE."

lib%.i : $(EUL_LIB_DIR)/lib%.i
	@ln -s $(EUL_LIB_DIR)/$@ $@

.PHONY: boot-final
boot-final:
	@echo "---- CREATING youtoo.em module for final, proper youtoo executable"
	@sed 	-e 's@#modules#@level0 level1 math eval i-aux0 macros@' \
		-e 's@#which#@Final, proper executable!@' \
		<youtoo.em.in >youtoo.em
	@echo "---- CREATING final youtoo executable"
	$(U2_DEV) $(U2FLAGS) -c youtoo -od $(ARCH_DIR) -l level1 -l math -l eval -no_recompile
	rm -f $(ARCH_DIR)/libeval.a $(EUL_LIB_DIR)/libeval.a $(ARCH_DIR)/youtoo.o
	@echo "---- DONE."

.PHONY: bytecode2
bytecode2:
	@echo "---- CREATING include/bytecode2.h which contains instruction information"
	$(EUL_BOOT_DIR)/Bin.$(ARCH)/b2h cg-bycode1.em cg-bycode2.em > ../include/bytecode2.h
	@echo "---- DONE."

ifeq ($(findstring boot,$(MAKECMDGOALS)),boot)
.PRECIOUS: %.i %.c %.h
%.i %.c %.h : %.em
	$(U2) $(U2FLAGS) -c $* -od $(ARCH_DIR) \
		-load_path $(U2_DIR)/Comptime2 \
		-load_path $(U2_DIR)/Comptime2/$(WORD_LENGTH)bit \
		-load_path $(U2_DIR)/Runtime \
		-load_path $(U2_DIR)/Telos \
		-l level1
endif

$(ARCH_DIR)/cg-exec-word-length.o: \
		$(ARCH_DIR)/$(WORD_LENGTH)bit/cg-exec-word-length.o
	@cp $< $@

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------

.PHONY: boot-clean
boot-clean: clean
	@rm -f *.[ihc]

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)

.PHONY: distclean
distclean:
	@rm -rf platforms 64bit/platforms 32bit/platforms

###-----------------------------------------------------------------------------
