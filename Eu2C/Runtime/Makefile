### Copyright 1994-2010 Fraunhofer ISST
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
###  Title: Runtime Makefile
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

# Set default word-length option
WL = m32
ifeq ($(ARCH),x86_64)
    WL = m64
endif

ARCH := $(ARCH)$(WL)

CC = gcc
GCCFLAGS = -std=gnu99 -Wall -O2
EU2C_FLAGS = -$(WL)
CFLAGS = $(GCCFLAGS) $(EU2C_FLAGS)

.PRECIOUS: %/.
%/.:
	mkdir -p $(@)
# Reset the default goal.
.DEFAULT_GOAL :=
$(ARCH_DIR)/%.o : %.[cs] | $(ARCH_DIR)/.
	mkdir -p $(@D)
	$(CC) -c $(CFLAGS) $< -o $@

###-----------------------------------------------------------------------------
### Output library files
###-----------------------------------------------------------------------------

ARCHIVES = $(ARCH_DIR)/eu2c.a $(ARCH_DIR)/eu2c-s.a

###-----------------------------------------------------------------------------
### Source and object files
###-----------------------------------------------------------------------------

RUNTIME_H = \
	xalloc_arch.h xalloc_conf.h xalloc_user.h xalloc.h \
	c-runtime.h timing.h eu2c-sys.h

RUNTIME_C = \
	card.c heap.c heap-init.c trace.c xalloc.c timing.c \
	c-runtime.c eu2c-sys.c

RUNTIME_O = $(RUNTIME_C:%.c=$(ARCH_DIR)/%.o)

RUNTIME_HC = $(RUNTIME_H) $(RUNTIME_C)

THREAD_H = thread.h
THREAD_C = thread.c $(ARCH)-swch.s
THREAD_O = $(ARCH_DIR)/thread.o $(ARCH_DIR)/$(ARCH)-swch.o

THREAD_HC = $(THREAD_H) $(THREAD_C)

ifeq ($(ARCH),i686)
    ARCHIVES += $(ARCH_DIR)/eu2c-threads.a $(ARCH_DIR)/eu2c-threads-s.a
    RUNTIME_O += $(THREAD_O)
endif

###-----------------------------------------------------------------------------
### Commands to build library object files
###-----------------------------------------------------------------------------

all: $(ARCHIVES)

$(ARCH_DIR)/eu2c.a: $(RUNTIME_HC)
	$(MAKE) makea ARCHIVE=$@ EU2C_FLAGS="-$(WL) -DNOTHREADS"
	$(MAKE) clean

$(ARCH_DIR)/eu2c-s.a: $(RUNTIME_HC)
	$(MAKE) makea ARCHIVE=$@ EU2C_FLAGS="-$(WL) -DESCURITY_FIRST -DNOTHREADS"
	$(MAKE) clean

$(ARCH_DIR)/eu2c-threads.a: $(RUNTIME_HC) $(THREAD_HC)
	$(MAKE) makea ARCHIVE=$@ EU2C_FLAGS=-$(WL)
	$(MAKE) clean

$(ARCH_DIR)/eu2c-threads-s.a: $(RUNTIME_HC) $(THREAD_HC)
	$(MAKE) makea ARCHIVE=$@ EU2C_FLAGS="-$(WL) -DESCURITY_FIRST"
	$(MAKE) clean

.PHONY: makea
makea:  $(RUNTIME_O)
	rm -f $(ARCHIVE)
	ar rv $(ARCHIVE) $(RUNTIME_O)
	ranlib $(ARCHIVE)

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR)/*.o

.PHONY: distclean
distclean: clean
	@rm -rf platforms

###-----------------------------------------------------------------------------
