### Copyright 1994 Russell Bradford
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
### Title: EuXLisp Top-level Makefile
###  Note: set Eu2CROOT e.g. export Eu2CROOT=`pwd`
### Usage: make
###  Notes:
##    Originally tested on sunos, aix, solaris, Linux, ultrix, stardent, irix,
##    but this version only on Linux.
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include ../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Options:
###  -DSIGNAL when you don't have sigset
###  -DRESTORE_SIGNAL when signals are not automatically reset
###  -DSOCK to include socket code, but beware: it is possibly buggy
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### Recall this Makefile for the particular achitecture
###-----------------------------------------------------------------------------

it:
	@$(MAKE) `uname`

# Generic Linux
Linux:
	@$(MAKE) `uname`-$(ARCH)

# Linux 32bit
Linux-i686:
	$(MAKE) all \
	"CFLAGS = -std=gnu99 -Wall -O3 -DWORD_LENGTH=32 -DUNIX -DSIGNAL -DSTRINGH -DREADLINE" \
        "LIBS = -lm -lreadline" "CC = gcc"

# Linux 64bit
Linux-x86_64:
	$(MAKE) all \
        "CFLAGS = -std=gnu99 -Wall -O3 -DWORD_LENGTH=64 -DUNIX -DSIGNAL -DSTRINGH -DREADLINE" \
        "LIBS = -lm -lreadline" "CC = gcc"

# Generic SunOS
SunOS:
	@$(MAKE) SunOS-`uname -r | awk -F. -e '{print $$1}'`

# Solaris
# Use -lsocket -lnsl for sockets
SunOS-5:
	$(MAKE) all "CFLAGS = -std=gnu99 -DUNIX -g " "LDFLAGS = -g" "LIBS = -lm" "CC = gcc"

# IBM AIX
AIX:
	$(MAKE) all "CFLAGS = -std=gnu99 -DUNIX -D_ALL_SOURCE -DSOCK" "LIBS = -lm" "CC = gcc"

###-----------------------------------------------------------------------------
### Search and installation paths
###-----------------------------------------------------------------------------

###  Search path for modules
# look first in environment variable EU_MODULE_PATH
# then the builtin equivalent.
# EU_MODULE_PATH is a colon separated list of directory names
MODULE_SEARCH_PATH = ".", "Modules", "$(PWD)/../Modules"

###  Search path for the EuXLisp image
# look first in environment variable EU_IMAGE_PATH
# then the builtin equivalent
IMAGE_SEARCH_PATH = "$(EUL_LIB_DIR)"

###  Name and location of the EuXLisp image
IMAGE = euxlisp.wks
IMAGE_PLATFORM = $(EUL_LIB_DIR)/$(IMAGE)

###  Name and location of the EuXLisp image for executing modules
IMAGE_MOD = euxlisp_mod.wks
IMAGE_MOD_PLATFORM = $(EUL_LIB_DIR)/$(IMAGE_MOD)

###  Installation location
DESTBIN = /usr/local/bin
DESTLIB = /usr/local/share/Euxlisp

###  Transfer paths to the compiler
# Note: CPPFLAGS is used here because it is already on the used in the
# $(ARCH_DIR)/%.o : %.c rule
CPPFLAGS = -DIMAGE='"$(IMAGE)"' -DIMAGE_MOD='"$(IMAGE_MOD)"' \
         -DMODULE_SEARCH_PATH='$(MODULE_SEARCH_PATH)' \
         -DIMAGE_SEARCH_PATH='$(IMAGE_SEARCH_PATH)'

###-----------------------------------------------------------------------------
### Source and object files
###-----------------------------------------------------------------------------

EUXL_C = osspecific.c xsdmem.c xsfun2.c xsint.c xsobj.c xssym.c \
         xscheme.c xsftab.c xsimage.c xsio.c xsprint.c \
         xscom.c xsfun1.c xsinit.c xsmath.c xsread.c \
         xsmodule.c xstable.c xsocket.c

EUXL_H = osdefs.h osptrs.h xsbcode.h xscheme.h xsproto.h xssymbols.h \
         xsbanner.h xsobj.h

EULX_O = $(EUXL_C:%.c=$(ARCH_DIR)/%.o)

LISP = Boot/*.em

OTHERS = Makefile saveimage README Modules/*.em euxlisp.man euxlisp.1 \
         eunotes.html

DISTFILES = $(EUXL_C) $(EUXL_H) $(LISP) $(OTHERS)

###-----------------------------------------------------------------------------
### Commands to build EuXLisp
###-----------------------------------------------------------------------------

all: $(EUL_BIN_DIR)/euxlisp $(IMAGE_PLATFORM) $(IMAGE_MOD_PLATFORM)

$(EULX_O) : $(EUXL_H)

$(EUL_BIN_DIR)/euxlisp: $(EULX_O)
	$(CC) $(LDFLAGS) $(EULX_O) $(LIBS) -o $@

$(IMAGE_PLATFORM): $(EUL_BIN_DIR)/euxlisp $(LISP) saveimage
	EU_MODULE_PATH=Boot $(EUL_BIN_DIR)/euxlisp -n < saveimage
	mv euxlisp.wks $(IMAGE_PLATFORM)

$(IMAGE_MOD_PLATFORM): $(EUL_BIN_DIR)/euxlisp $(LISP) savemodimage
	EU_MODULE_PATH=Boot $(EUL_BIN_DIR)/euxlisp -n < savemodimage
	mv euxlisp_mod.wks $(IMAGE_MOD_PLATFORM)

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------

version: xsbanner.h
	awk '/define BANNER/ {print $$8}' < xsbanner.h | sed 's/[."]//g' > version

TAGS: $(EUXL_C) $(EUXL_H) $(LISP)
	etags $(EUXL_C) $(EUXL_H) $(LISP)

.PHONY: tags
tags: TAGS

.PHONY: install
install: install-bin install-img install-mod

.PHONY: install-bin
install-bin: $(ARCH_DIR)/euxlisp
	cp $(ARCH_DIR)/euxlisp $(DESTBIN)

.PHONY: install-img
install-img: $(IMAGE_PLATFORM)
	cp $(IMAGE_PLATFORM) $(DESTLIB)/Image

.PHONY: install-mod
install-mod: Modules/*.em
	mkdir $(DESTLIB)/Modules
	cp ../Modules/*.em $(DESTLIB)/Modules

.PHONY: clean
clean:
	@rm -rf $(ARCH_DIR) TAGS

###-----------------------------------------------------------------------------
