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
### Title: Level-1 runtime Makefile
###  Maintainer: Henry G. Weller
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Reset U2 to use the bootstrap youtoo
###-----------------------------------------------------------------------------
U2_DEV := $(U2)
U2 = $(U2_BOOT)

###-----------------------------------------------------------------------------
### Output file names
###-----------------------------------------------------------------------------
LEVEL_0_LIB = $(EUL_LIB_DIR)/liblevel-0.a
LEVEL_0_LIB_I = $(EUL_LIB_DIR)/$(U2_C_DIR)/liblevel-0.i
LEVEL_0_SHARED_LIB = $(EUL_LIB_DIR)/liblevel-0.so

LEVEL_1_LIB = $(EUL_LIB_DIR)/liblevel-1.a
LEVEL_1_LIB_I = $(EUL_LIB_DIR)/$(U2_C_DIR)/liblevel-1.i
LEVEL_1_SHARED_LIB = $(EUL_LIB_DIR)/liblevel-1.so

MATH_LIB = $(EUL_LIB_DIR)/libmath.a
MATH_LIB_I = $(EUL_LIB_DIR)/$(U2_C_DIR)/libmath.i
MATH_SHARED_LIB = $(EUL_LIB_DIR)/libmath.so

###-----------------------------------------------------------------------------
### Library dependencies
###-----------------------------------------------------------------------------
TELOS_LIB_I = $(EUL_LIB_DIR)/$(U2_C_DIR)/libtelos.i

###-----------------------------------------------------------------------------
### Source and object files
###-----------------------------------------------------------------------------
LEVELN_EM = bit.em callback.em character.em collect.em compare.em condition.em \
	convert.em convert1.em copy.em double.em double1.em dynamic.em event.em \
	float.em format.em fpi.em \
	handler.em integer.em list.em lock.em math.em number.em \
	random.em read.em socket.em stream.em stream1.em stream2.em stream3.em \
	string.em symbol.em table.em table1.em let-cc.em thread.em vector.em

LEVEL_0_EM = $(LEVELN_EM) level-0.em
LEVEL_0_C := $(LEVEL_0_EM:%.em=$(U2_C_DIR)/%.c)
LEVEL_0_O := ${LEVEL_0_C:%.c=$(ARCH_DIR)/%.o}

LEVEL_0_FF_C = Ff/eul-ext.c Ff/eul-ntok.c Ff/eul-read.c Ff/eul-sock.c
LEVEL_0_C += $(LEVEL_0_FF_C)
LEVEL_0_O += ${LEVEL_0_FF_C:%.c=$(ARCH_DIR)/%.o}

LEVEL_0_MAC_EM = stream0.em macros.em syntax-0.em
LEVEL_0_MAC_C = $(LEVEL_0_MAC_EM:%.em=$(U2_C_DIR)/%.c)
LEVEL_0_MAC_O = ${LEVEL_0_MAC_C:%.c=$(ARCH_DIR)/%.o}

LEVEL_1_EM = $(LEVELN_EM) level-1.em
LEVEL_1_C := $(LEVEL_1_EM:%.em=$(U2_C_DIR)/%.c)
LEVEL_1_O := ${LEVEL_1_C:%.c=$(ARCH_DIR)/%.o}

LEVEL_1_FF_C = $(LEVEL_0_FF_C)
LEVEL_1_C += $(LEVEL_1_FF_C)
LEVEL_1_O += ${LEVEL_1_FF_C:%.c=$(ARCH_DIR)/%.o}

LEVEL_1_MAC_EM = $(LEVEL_0_MAC_EM)
LEVEL_1_MAC_C = $(LEVEL_1_MAC_EM:%.em=$(U2_C_DIR)/%.c)
LEVEL_1_MAC_O = ${LEVEL_1_MAC_C:%.c=$(ARCH_DIR)/%.o}

MATH_EM := double1.em double.em math.em
MATH_C := $(MATH_EM:%.em=$(U2_C_DIR)/%.c)
MATH_O := ${MATH_C:%.c=$(ARCH_DIR)/%.o}

MATH_FF_C = Ff/eul-dbl.c
MATH_C += $(MATH_FF_C)
MATH_O += $(ARCH_DIR)/Ff/eul-dbl.o

###-----------------------------------------------------------------------------
### Commands to build library object files
###-----------------------------------------------------------------------------
.PHONY: lib
lib:	$(LEVEL_0_LIB) $(LEVEL_1_LIB) $(MATH_LIB)

$(LEVEL_0_LIB): $(LEVEL_0_MAC_O) $(LEVEL_0_O)
	$(AR) $(LEVEL_0_LIB) $(LEVEL_0_MAC_O) $(LEVEL_0_O)
	@$(RANLIB) $(LEVEL_0_LIB)

$(LEVEL_1_LIB): $(LEVEL_1_MAC_O) $(LEVEL_1_O)
	$(AR) $(LEVEL_1_LIB) $(LEVEL_1_MAC_O) $(LEVEL_1_O)
	@$(RANLIB) $(LEVEL_1_LIB)

$(MATH_LIB): $(MATH_O)
	$(AR) $(MATH_LIB) $(MATH_O)
	@$(RANLIB) $(MATH_LIB)

.PHONY: shared
shared:	$(LEVEL_0_SHARED_LIB) $(LEVEL_1_SHARED_LIB) $(MATH_SHARED_LIB)

$(LEVEL_0_SHARED_LIB): $(LEVEL_0_MAC_O) $(LEVEL_0_O)
	$(U2_DIR)/Tools/makeso.$(OSTYPE) \
		$(CC) $(LEVEL_0_SHARED_LIB) \
		$(LEVEL_0_MAC_O) $(LEVEL_0_O)

$(LEVEL_1_SHARED_LIB): $(LEVEL_1_MAC_O) $(LEVEL_1_O)
	$(U2_DIR)/Tools/makeso.$(OSTYPE) \
		$(CC) $(LEVEL_1_SHARED_LIB) \
		$(LEVEL_1_MAC_O) $(LEVEL_1_O)

$(MATH_SHARED_LIB): $(MATH_O)
	$(U2_DIR)/Tools/makeso.$(OSTYPE) \
		$(CC) $(MATH_SHARED_LIB) \
	        $(MATH_O)

###-----------------------------------------------------------------------------
### Bootstrapping
###-----------------------------------------------------------------------------
.PHONY: boot
boot:	$(U2_C_DIR)/libtelos.i $(U2_C_DIR)/_macros.i $(U2_C_DIR)/liblevel-0.i $(U2_C_DIR)/liblevel-1.i $(U2_C_DIR)/libmath.i
	@rm -f $(U2_C_DIR)/libtelos.i
	$(MAKE) -C ../Telos boot-syntax
	$(MAKE) boot-syntax

.PHONY: boot-syntax
boot-syntax:
	$(U2) $(U2FLAGS) -c macros \
		--object-dir $(ARCH_DIR) \
		--load-path $(U2_DIR)/Runtime \
		--load-path $(U2_DIR)/Telos \
		-l level-1
	$(U2) $(U2FLAGS) -c syntax-0 \
		--object-dir $(ARCH_DIR) \
		--load-path $(U2_DIR)/Runtime \
		--load-path $(U2_DIR)/Telos \
		-l level-1
	$(MAKE) boot-all

.PHONY: boot-all
boot-all: $(LEVEL_0_MAC_C) $(LEVEL_0_C) $(LEVEL_1_MAC_C) $(LEVEL_1_C) $(MATH_C)
	$(MAKE)

$(LEVEL_0_LIB_I): $(LEVEL_0_MAC_EM) $(LEVEL_0_EM)
$(LEVEL_1_LIB_I): $(LEVEL_0_MAC_EM) $(LEVEL_0_EM)
$(MATH_LIB_I): $(MATH_EM)

$(U2_C_DIR)/lib%.i : $(EUL_LIB_DIR)/$(U2_C_DIR)/lib%.i
	@mkdir -p $(U2_C_DIR)
	@cp -p $< $@

$(EUL_LIB_DIR)/$(U2_C_DIR)/lib%.i : %.em
	$(U2) $(U2FLAGS) --archive $* \
		--object-dir $(ARCH_DIR) \
		--load-path $(U2_DIR)/Telos \
		--load-path $(U2_DIR)/Runtime \
		-l telos
	@mv $(U2_C_DIR)/$(@F) $@

ifeq ($(findstring boot,$(MAKECMDGOALS)),boot)
.PRECIOUS: $(U2_C_DIR)/%.i $(U2_C_DIR)/%.c $(U2_C_DIR)/%.h
$(U2_C_DIR)/%.i $(U2_C_DIR)/%.c $(U2_C_DIR)/%.h : %.em
	$(U2) $(U2FLAGS) -c $< \
		--object-dir $(ARCH_DIR) \
		--load-path $(U2_DIR)/Runtime \
		--load-path $(U2_DIR)/Telos \
		-l level-1
endif

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------
.PHONY: boot-clean
boot-clean: clean
	@rm -rf $(U2_C_DIR)

.PHONY: clean
clean:
	@rm -f $(U2_C_DIR)/lib*.i
	@rm -rf $(ARCH_DIR)

.PHONY: distclean
distclean: clean
	@rm -rf platforms

###-----------------------------------------------------------------------------
