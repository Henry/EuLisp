###-----------------------------------------------------------------------------
### ---                   EuLisp System 'EuXLisp'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### EuXLisp executable Makefile
###  Tested on sunos, aix, solaris, linux, ultrix, stardent, irix.
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### Options:
###  -DSIGNAL when you don't have sigset
###  -DRESTORE_SIGNAL when signals are not automatically reset
###  -DSOCK to include socket code, but beware: it is possibly buggy
###-----------------------------------------------------------------------------

SHELL = /bin/sh

ARCH := $(shell uname -m)
ARCH_DIR = platforms/$(ARCH)

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
	$(MAKE) all "CFLAGS = -std=gnu99 -DUNIX -DSIGNAL -DSTRINGH -DWORD_LENGTH=32 -Wall" \
                "LIBS = -lm" "CC = gcc"

# Linux 64bit
Linux-x86_64:
	$(MAKE) all "CFLAGS = -std=gnu99 -DUNIX -DSIGNAL -DSTRINGH -DWORD_LENGTH=64 -Wall" \
                "LIBS = -lm" "CC = gcc"

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
IMAGE_SEARCH_PATH = "$(PWD)/$(ARCH_DIR)"

###  Name and location of the EuXLisp image
IMAGE = euxlisp.wks
IMAGE_PLATFORM = $(ARCH_DIR)/$(IMAGE)

###  Installation location
DESTBIN = /usr/local/bin
DESTLIB = /usr/local/share/Euxlisp

###  Transfer paths to the compiler
DFLAGS = -DIMAGE='"$(IMAGE)"' \
         -DMODULE_SEARCH_PATH='$(MODULE_SEARCH_PATH)' \
         -DIMAGE_SEARCH_PATH='$(IMAGE_SEARCH_PATH)'

###-----------------------------------------------------------------------------
### Source and object files
###-----------------------------------------------------------------------------

EUXL_C = msstuff.c xsdmem.c xsfun2.c xsint.c xsobj.c xssym.c \
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

all: $(IMAGE_PLATFORM)

.PRECIOUS: %/.
%/.:
	mkdir -p $(@)

$(ARCH_DIR)/%.o : %.c $(EUXL_H) Makefile | $(ARCH_DIR)/.
	mkdir -p $(@D)
	$(CC) -c $(CFLAGS) $(DFLAGS) $< -o $@

$(ARCH_DIR)/euxlisp: $(EULX_O)
	$(CC) $(LDFLAGS) $(EULX_O) $(LIBS) -o $@

$(IMAGE_PLATFORM): $(ARCH_DIR)/euxlisp $(LISP) saveimage
	EU_MODULE_PATH=Boot $(ARCH_DIR)/euxlisp -n < saveimage
	mv euxlisp.wks $(IMAGE_PLATFORM)

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
