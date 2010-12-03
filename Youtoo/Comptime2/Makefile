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
### Title: Compile-time Makefile
###  Maintainer: Henry G. Weller
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
EVAL_LIB_I = $(EUL_LIB_DIR)/$(U2_C_DIR)/libeval.i
EVAL_SHARED_LIB = $(EUL_LIB_DIR)/libeval.so

MAIN_LIB = $(EUL_LIB_DIR)/libmain.a
MAIN_LIB_I = $(EUL_LIB_DIR)/$(U2_C_DIR)/libmain.i
MAIN_SHARED_LIB = $(EUL_LIB_DIR)/libmain.so

###-----------------------------------------------------------------------------
### Source and object files
###-----------------------------------------------------------------------------
EVAL_EM := \
    an-side.em aux-table.em cg-asm.em cg-bycode.em cg-bycode1.em cg-bycode2.em \
    cg-dld.em cg-exec.em cg-gen.em cg-interf.em cg-link.em cg-stack.em cg-state.em eval.em \
    ex-body.em ex-direct.em ex-expose.em ex-expr.em ex-import.em ex-module.em \
    ex-syntax.em i-all.em i-args.em i-compile.em i-error.em i-ffi.em \
    i-level-1.em i-modify.em i-notify.em i-param.em i-rep.em op-peep-r.em op-peep.em \
    p-env.em p-parse.em p-read.em sx-node.em sx-obj.em sx-obj1.em sx-obj2.em \
    sx-write.em i-aux0.em
EVAL_C := $(EVAL_EM:%.em=$(U2_C_DIR)/%.c)
EVAL_O := $(EVAL_C:%.c=$(ARCH_DIR)/%.o)

EVAL_FF_C =  \
    $(WORD_LENGTH)bit/$(U2_C_DIR)/cg-exec-word-length.c \
    Ff/eul-prompt-string.c Ff/eul-dld.c

EVAL_C += $(EVAL_FF_C)
EVAL_O += $(EVAL_FF_C:%.c=$(ARCH_DIR)/%.o)

MAIN_EM := main.em youtoo.em
MAIN_C := $(MAIN_EM:%.em=$(U2_C_DIR)/%.c)
MAIN_O := $(MAIN_C:%.c=$(ARCH_DIR)/%.o)

MAIN_FF_C =  Ff/Youtoo_.c

MAIN_C += $(MAIN_FF_C)
MAIN_O += $(MAIN_FF_C:%.c=$(ARCH_DIR)/%.o)

###-----------------------------------------------------------------------------
### Link level-1 libraries
###-----------------------------------------------------------------------------
.PHONY: static
static:	$(EVAL_LIB) $(MAIN_LIB)

$(EVAL_LIB): $(EVAL_O)
	$(AR) $(EVAL_LIB) $(EVAL_O)
	@$(RANLIB) $(EVAL_LIB)

$(MAIN_LIB): $(MAIN_O)
	$(AR) $(MAIN_LIB) $(MAIN_O)
	@$(RANLIB) $(MAIN_LIB)

.PHONY: shared
shared:	$(EVAL_SHARED_LIB) $(MAIN_SHARED_LIB)

$(EVAL_SHARED_LIB): $(EVAL_O)
	$(U2_DIR)/Tools/makeso.$(OSTYPE) \
		$(CC) $(EVAL_SHARED_LIB) $(EVAL_O)

$(MAIN_SHARED_LIB): $(MAIN_O)
	$(U2_DIR)/Tools/makeso.$(OSTYPE) \
		$(CC) $(MAIN_SHARED_LIB) $(MAIN_O)

###-----------------------------------------------------------------------------
### Bootstrapping
###-----------------------------------------------------------------------------
.PHONY: boot
boot: static

.PHONY: boot-preliminary
boot-preliminary: $(U2_C_DIR)/_ex-aux0.i $(U2_C_DIR)/_op-peep0.i $(U2_C_DIR)/_cg-bycode0.i $(U2_C_DIR)/_i-aux0.i $(U2_C_DIR)/_sx-obj0.i \
		$(U2_C_DIR)/libboot.i $(U2_C_DIR)/libmath.i $(U2_C_DIR)/liblevel-1.i | $(ARCH_DIR)/.
	$(U2) $(U2FLAGS) --object-dir $(ARCH_DIR) \
		-c ex-aux0 op-peep0 cg-bycode0 i-aux0 sx-obj0 \
		--load-path $(U2_DIR)/Comptime2 \
		--load-path $(U2_DIR)/Comptime2/$(WORD_LENGTH)bit \
		-l level-1
	$(U2) $(U2FLAGS) -c ex-expr --object-dir $(ARCH_DIR) \
		--load-path $(U2_DIR)/Comptime2 \
		--load-path $(U2_DIR)/Comptime2/$(WORD_LENGTH)bit \
		--load-path $(U2_DIR)/Runtime \
		--load-path $(U2_DIR)/Telos \
		-l boot -l level-1 -l math
	@mkdir -p $(WORD_LENGTH)bit/$(U2_C_DIR)
	@mkdir -p $(ARCH_DIR)/$(U2_C_DIR)
	@ln -fs ../../../$(WORD_LENGTH)bit/$(ARCH_DIR)/$(U2_C_DIR)/cg-exec-word-length.o \
		$(ARCH_DIR)/$(U2_C_DIR)/cg-exec-word-length.o
	$(U2) $(U2FLAGS) --archive eval --object-dir $(ARCH_DIR) \
		--load-path $(U2_DIR)/Comptime2 \
		--load-path $(U2_DIR)/Comptime2/$(WORD_LENGTH)bit \
		--load-path $(U2_DIR)/Runtime \
		--load-path $(U2_DIR)/Telos \
		-l boot -l level-1 -l math
	@mv $(U2_C_DIR)/libeval.i $(EUL_LIB_DIR)/$(U2_C_DIR)
	@rm -f $(ARCH_DIR)/libeval.a $(EUL_LIB_DIR)/libeval.a
	$(U2) $(U2FLAGS) --archive main --object-dir $(ARCH_DIR) \
		--load-path $(U2_DIR)/Comptime2 \
		--load-path $(U2_DIR)/Comptime2/$(WORD_LENGTH)bit \
		--load-path $(U2_DIR)/Runtime \
		--load-path $(U2_DIR)/Telos \
		-l boot -l level-1 -l math
	@mv $(U2_C_DIR)/libmain.i $(EUL_LIB_DIR)/$(U2_C_DIR)
	@rm -f $(ARCH_DIR)/libmain.a $(EUL_LIB_DIR)/libmain.a
	@echo "---- CREATING youtoo.em module for preliminary youtoo executable"
	@sed 	-e 's@#modules#@level-1 math eval main@' \
		-e 's@#which#@Preliminary executable!@' \
		<youtoo.em.in >youtoo.em
	@echo "---- CREATING preliminary youtoo executable files"
	$(U2) $(U2FLAGS) -c youtoo --object-dir $(ARCH_DIR) \
		--load-path $(U2_DIR)/Comptime2 \
		--load-path $(U2_DIR)/Comptime2/$(WORD_LENGTH)bit \
		-l level-1 -l math -l eval -l main --recompile
	@rm -f $(U2_C_DIR)/libboot.i $(U2_C_DIR)/libmath.i $(U2_C_DIR)/liblevel-1.i
	@rm -f $(ARCH_DIR)/$(U2_C_DIR)/youtoo.o
	@echo "---- DONE."

$(U2_C_DIR)/lib%.i : $(EUL_LIB_DIR)/$(U2_C_DIR)/lib%.i
	@mkdir -p $(U2_C_DIR)
	@cp -p $< $@

.PHONY: boot-final
boot-final:
	@echo "---- CREATING youtoo.em module for final, proper youtoo executable"
	@sed 	-e 's@#modules#@level-0 level-1 math eval main i-aux0 macros syntax-0@' \
		-e 's@#which#@Final, proper executable!@' \
		<youtoo.em.in >youtoo.em
	@echo "---- CREATING final youtoo executable"
	$(U2_DEV) $(U2FLAGS) -c youtoo --object-dir $(ARCH_DIR) \
		-l level-1 -l math -l eval -l main -no_recompile
	rm -f $(ARCH_DIR)/libmain.a $(EUL_LIB_DIR)/libmain.a $(ARCH_DIR)/$(U2_C_DIR)/youtoo.o
	@echo "---- DONE."

.PHONY: bytecode2
bytecode2:
	@echo "---- CREATING include/bytecode2.h which contains instruction information"
	$(EUL_BOOT_DIR)/Bin.$(ARCH)/b2h cg-bycode1.em cg-bycode2.em \
		> ../include/bytecode2.h
	@echo "---- DONE."

ifeq ($(findstring boot,$(MAKECMDGOALS)),boot)
.PRECIOUS: $(U2_C_DIR)/%.i $(U2_C_DIR)/%.c $(U2_C_DIR)/%.h
$(U2_C_DIR)/%.i $(U2_C_DIR)/%.c $(U2_C_DIR)/%.h : %.em
	$(U2) $(U2FLAGS) -c $< \
		--object-dir $(ARCH_DIR) \
		--load-path $(U2_DIR)/Comptime2 \
		--load-path $(U2_DIR)/Comptime2/$(WORD_LENGTH)bit \
		--load-path $(U2_DIR)/Runtime \
		--load-path $(U2_DIR)/Telos \
		-l level-1
endif

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------
.PHONY: boot-clean
boot-clean: clean
	@rm -rf $(U2_C_DIR) $(WORD_LENGTH)bit/$(U2_C_DIR)

.PHONY: clean
clean:
	@rm -f $(U2_C_DIR)/lib*.i
	@rm -rf $(ARCH_DIR) $(WORD_LENGTH)bit/$(ARCH_DIR)

.PHONY: distclean
distclean: clean
	@rm -rf platforms 64bit/platforms 32bit/platforms

###-----------------------------------------------------------------------------
