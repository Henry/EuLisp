## Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.

## ----------------------------------------------------------------------------
##                      EuLisp System 'youtoo'
## ----------------------------------------------------------------------------

## ----------------------------------------------------------------------------
## EuLisp level-1 runtime Makefile
## ----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../Lib.$(ARCH)/Makefile

## ----------------------------------------------------------------------------
## Output file names
## ----------------------------------------------------------------------------

LEVEL1_LIB = $(EUL_LIB_DIR)/liblevel1.a
LEVEL1_LIB_I = $(EUL_LIB_DIR)/liblevel1.i
LEVEL1_SHARED_LIB = $(EUL_LIB_DIR)/liblevel1.so

MATH_LIB = $(EUL_LIB_DIR)/libmath.a
MATH_LIB_I = $(EUL_LIB_DIR)/libmath.i
MATH_SHARED_LIB = $(EUL_LIB_DIR)/libmath.so

## ----------------------------------------------------------------------------
## Source and object files
## ----------------------------------------------------------------------------

LEVEL1_EM = bit.em callback.em character.em collect.em compare.em condition.em \
	convert.em convert1.em copy.em double.em double1.em dynamic.em event.em \
	float.em format.em fpi.em \
	handler.em integer.em level1.em list.em lock.em math.em number.em \
	random.em read.em socket.em stream.em stream1.em stream2.em stream3.em \
	string.em symbol.em table.em table1.em let-cc.em thread.em vector.em
LEVEL1_C := $(LEVEL1_EM:%.em=%.c)
LEVEL1_O := ${LEVEL1_C:%.c=$(ARCH_DIR)/%.o}

LEVEL1_FF_C = Ff/eul-ext.c Ff/eul-ntok.c Ff/eul-sock.c
LEVEL1_C += $(LEVEL1_FF_C)
LEVEL1_O += ${LEVEL1_FF_C:%.c=$(ARCH_DIR)/%.o}

LEVEL1_MAC_EM = stream0.em macros.em
LEVEL1_MAC_C = $(LEVEL1_MAC_EM:%.em=%.c)
LEVEL1_MAC_O = ${LEVEL1_MAC_C:%.c=$(ARCH_DIR)/%.o}

MATH_C := double1.c double.c math.c
MATH_O := ${MATH_C:%.c=$(ARCH_DIR)/%.o}

MATH_FF_C = Ff/eul-dbl.c
MATH_C += $(MATH_FF_C)
MATH_O += $(ARCH_DIR)/Ff/eul-dbl.o

## ----------------------------------------------------------------------------
## Commands to build library object files
## ----------------------------------------------------------------------------

.PHONY: lib
lib:	$(LEVEL1_LIB) $(MATH_LIB)

$(LEVEL1_LIB): $(LEVEL1_MAC_O) $(LEVEL1_O)
	$(AR) $(LEVEL1_LIB) $(LEVEL1_MAC_O) $(LEVEL1_O)
	@$(RANLIB) $(LEVEL1_LIB)

$(MATH_LIB): $(MATH_O)
	$(AR) $(MATH_LIB) $(MATH_O)
	@$(RANLIB) $(MATH_LIB)

.PHONY: shared
shared:	$(LEVEL1_SHARED_LIB) $(MATH_SHARED_LIB)

$(LEVEL1_SHARED_LIB): $(LEVEL1_MAC_O) $(LEVEL1_O)
	$(EUL_DIR)/Tools/makeso.$(OSTYPE) \
		$(CC) $(LEVEL1_SHARED_LIB) \
		$(LEVEL1_MAC_O) $(LEVEL1_O)

$(MATH_SHARED_LIB): $(MATH_O)
	$(EUL_DIR)/Tools/makeso.$(OSTYPE) \
		$(CC) $(MATH_SHARED_LIB) \
	        $(MATH_O)

## ----------------------------------------------------------------------------
## Bootstrapping
## ----------------------------------------------------------------------------

.PHONY: boot
boot:	_stream0.i _macros.i $(LEVEL1_LIB_I) $(MATH_LIB_I) $(LEVEL1_LIB) $(MATH_LIB)
	cd ../Telos; $(MAKE) boot-syntax
	$(MAKE) boot-syntax

.PHONY: boot-syntax
boot-syntax:
	$(U2) $(U2FLAGS) -c macros -od $(ARCH_DIR) \
		-load_path $(EUL_DIR)/Runtime \
		-load_path $(EUL_DIR)/Telos \
		-l level1
	$(MAKE)

$(LEVEL1_LIB_I): $(LEVEL1_EM) $(LEVEL1_MAC_EM)
$(MATH_LIB_I): $(MATH_EM)

# Note that we try to build the library interface files twice
# because the first time building liblevel1.i fails but it is
# successful the second time
$(EUL_LIB_DIR)/lib%.i : %.em
	-$(U2) $(U2FLAGS) -ar $* -od $(ARCH_DIR) \
		-load_path $(EUL_DIR)/Runtime \
		-load_path $(EUL_DIR)/Telos \
		-l telos
	$(U2) $(U2FLAGS) -ar $* -od $(ARCH_DIR) \
		-load_path $(EUL_DIR)/Runtime \
		-load_path $(EUL_DIR)/Telos \
		-l telos
	mv $(@F) $@

%.i : %.em
	$(U2) $(U2FLAGS) -c $* -od $(ARCH_DIR) \
		-load_path $(EUL_DIR)/Runtime \
		-load_path $(EUL_DIR)/Telos \
		-l level1

## ----------------------------------------------------------------------------
## Miscellaneous commands
## ----------------------------------------------------------------------------

.PHONY: touch
touch:
	@touch $(LEVEL1_C) $(LEVEL1_MAC_C) $(MATH_C)

.PHONY: boot-touch
boot-touch:
	@touch *.em $(LEVEL1_FF_C) $(MATH_FF_C)

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)

.PHONY: distclean
distclean:
	@rm -rf platforms

## ----------------------------------------------------------------------------
