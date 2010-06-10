### Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
###-----------------------------------------------------------------------------
### ---                  EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### EuLisp level-1 runtime Makefile
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

LEVEL1_LIB = $(EUL_LIB_DIR)/liblevel1.a
LEVEL1_LIB_I = $(EUL_LIB_DIR)/liblevel1.i
LEVEL1_SHARED_LIB = $(EUL_LIB_DIR)/liblevel1.so

MATH_LIB = $(EUL_LIB_DIR)/libmath.a
MATH_LIB_I = $(EUL_LIB_DIR)/libmath.i
MATH_SHARED_LIB = $(EUL_LIB_DIR)/libmath.so

###-----------------------------------------------------------------------------
### Library dependencies
###-----------------------------------------------------------------------------

TELOS_LIB_I = $(EUL_LIB_DIR)/libtelos.i

###-----------------------------------------------------------------------------
### Source and object files
###-----------------------------------------------------------------------------

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

MATH_EM := double1.em double.em math.em
MATH_C := $(MATH_EM:%.em=%.c)
MATH_O := ${MATH_C:%.c=$(ARCH_DIR)/%.o}

MATH_FF_C = Ff/eul-dbl.c
MATH_C += $(MATH_FF_C)
MATH_O += $(ARCH_DIR)/Ff/eul-dbl.o

###-----------------------------------------------------------------------------
### Commands to build library object files
###-----------------------------------------------------------------------------

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
	$(U2_DIR)/Tools/makeso.$(OSTYPE) \
		$(CC) $(LEVEL1_SHARED_LIB) \
		$(LEVEL1_MAC_O) $(LEVEL1_O)

$(MATH_SHARED_LIB): $(MATH_O)
	$(U2_DIR)/Tools/makeso.$(OSTYPE) \
		$(CC) $(MATH_SHARED_LIB) \
	        $(MATH_O)

###-----------------------------------------------------------------------------
### Bootstrapping
###-----------------------------------------------------------------------------

.PHONY: boot
boot:	libtelos.i _macros.i liblevel1.i libmath.i
	@rm -f libtelos.i
	$(MAKE) -C ../Telos boot-syntax
	$(MAKE) boot-syntax

.PHONY: boot-syntax
boot-syntax:
	$(U2) $(U2FLAGS) -c macros -od $(ARCH_DIR) \
		-load_path $(U2_DIR)/Runtime \
		-load_path $(U2_DIR)/Telos \
		-l level1
	$(MAKE)

$(LEVEL1_LIB_I): $(LEVEL1_MAC_EM) $(LEVEL1_EM)
$(MATH_LIB_I): $(MATH_EM)

lib%.i : $(EUL_LIB_DIR)/lib%.i
	@ln -s $(EUL_LIB_DIR)/$@ $@

$(EUL_LIB_DIR)/lib%.i : %.em
	$(U2) $(U2FLAGS) -ar $* -od $(ARCH_DIR) \
		-load_path $(U2_DIR)/Telos \
		-load_path $(U2_DIR)/Runtime \
		-l telos
	@mv $(@F) $@

.PRECIOUS: %.i %.c %.h
%.i %.c %.h : %.em
	$(U2) $(U2FLAGS) -c $* -od $(ARCH_DIR) \
		-load_path $(U2_DIR)/Runtime \
		-load_path $(U2_DIR)/Telos \
		-l level1

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------

.PHONY: boot-clean
boot-clean: clean
	@rm -f *.[ihc]

.PHONY: clean
clean:
	@rm -f libtelos.i
	@rm -rf $(ARCH_DIR)

.PHONY: distclean
distclean: clean
	@rm -rf platforms

###-----------------------------------------------------------------------------
