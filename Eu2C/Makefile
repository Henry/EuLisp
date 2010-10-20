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
### Title: Eu2C Top-level Makefile
###  Maintainer: Henry G. Weller
### Usage: make [options]
### Options:
###   CL=<Lisp Compiler: cmu, sbcl>
###   ARCH=<Machine architecture: i686, x86_64...>
###   eu2c=<Eu2C compiler image name>
###   module=<module name>
###   eu2c, runtime, modlib, modeu2c, clean, distclean
### Defaults:
###   CL=cmu
###   ARCH=[`uname -m` i.e. the default architecture on the machine]
###   eu2c=eu2c
###   module=level-0
###-----------------------------------------------------------------------------
ARCH := $(shell uname -m)
include ../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Set the Eu2C top-level environment variable
###-----------------------------------------------------------------------------
export Eu2CROOT=$(EUL_DIR)/Eu2C

###-----------------------------------------------------------------------------
### Set Common Lisp compiler
###-----------------------------------------------------------------------------
CL = cmu
ifeq ($(CL),cmu)
    image_ext = cmu
    ECL2Eu2C = lisp $(ARCH) $(1) -load $(2)
    Eu2C = lisp $(2) -core $(1)
else
    ifeq ($(CL),sbcl)
        image_ext = sbcl
    else
        $(error CL should be CL=cmu or CL=sbcl)
    endif
endif

###-----------------------------------------------------------------------------
### Set C compiler
###-----------------------------------------------------------------------------
CC = gcc
CFLAGS = -m$(WORD_LENGTH) -O2 -Wall -Wno-unused
#CFLAGS = -ggdb3

###-----------------------------------------------------------------------------
### Eu2C compiler image names
###-----------------------------------------------------------------------------
eu2c = eu2c
eu2c_image = Lib/$(ARCH)/$(eu2c).$(image_ext)

module = level-0
module_image = Lib/$(ARCH)/eu2c.$(module).$(image_ext)

###-----------------------------------------------------------------------------
### Source files
###-----------------------------------------------------------------------------

Apply = Apply/*.lisp Apply/*.ecl
EclModules = EclModules/*.lisp EclModules/*.ecl
EuLispModules = EuLispModules/*.em EuLispModules/*.config

###-----------------------------------------------------------------------------
### Commands to build library object files
###-----------------------------------------------------------------------------

# Create the eu2c compiler, C runtime library, module library
# and level compiler
eu2c_level: eu2c runtime modlib modeu2c

###-----------------------------------------------------------------------------
###  Create the eu2c compiler image
###-----------------------------------------------------------------------------
.PHONY: eu2c
eu2c: $(eu2c_image)
$(eu2c_image): $(Apply) $(EclModules)
	@echo "===> Create the basic eu2c compiler image " $@
	@mkdir -p Lib/$(ARCH)
	@$(call ECL2Eu2C,,Apply/boot1.lisp)
	@$(call ECL2Eu2C,$(eu2c),Apply/boot2.lisp)

###-----------------------------------------------------------------------------
###  Compile the C runtime/GC library
###-----------------------------------------------------------------------------
.PHONY: runtime
runtime:
	@echo "===> Compile the C runtime library"
	@$(MAKE) -C Runtime

###-----------------------------------------------------------------------------
###  Compile the module library
###-----------------------------------------------------------------------------
.PHONY: modlib
modlib: EuLispModules/$(ARCH_DIR)/$(module).a
EuLispModules/$(ARCH_DIR)/$(module).a: $(eu2c_image) $(EuLispModules)
	@echo "===> Compile the module library " $@
	@cd EuLispModules && \
	mkdir -p $(ARCH_DIR) && \
	cp ti-sys-signatures.em $(ARCH_DIR)/ti-sys-signatures.def && \
	echo "(apply-compiler::compile-basic-system (cadr ext:*command-line-strings*)) (quit)" | \
	$(call Eu2C,${Eu2CROOT}/$(eu2c_image),$(module)) && \
	$(CC) $(CFLAGS) -I${Eu2CROOT}/Runtime -I$(ARCH_DIR) \
		-c $(ARCH_DIR)/$(module).c -o $(ARCH_DIR)/$(module).o && \
	ar rcv $(ARCH_DIR)/$(module).a $(ARCH_DIR)/$(module).o && \
	ranlib $(ARCH_DIR)/$(module).a

###-----------------------------------------------------------------------------
###  Create the eu2c compiler image including the module
###-----------------------------------------------------------------------------
.PHONY: modeu2c
modeu2c: $(module_image)
$(module_image): $(eu2c_image) EuLispModules/$(ARCH_DIR)/$(module).a
	@echo "===> Create the module compiler image " $@
	@echo "(let ((name (cadr ext:*command-line-strings*)))(apply-compiler::load-basic-system :application name) (dump-apply (concatenate 'string (string (quote |$(eu2c).|)) name))) (quit)" | \
		$(call Eu2C,$(eu2c_image),$(module))

###-----------------------------------------------------------------------------
###  Build both 32bit and 64bit on a 64bit machine
###-----------------------------------------------------------------------------
.PHONY: all
all:
	$(MAKE)
	$(MAKE) module=level-0x
	$(MAKE) ARCH=i686
	$(MAKE) ARCH=i686 module=level-0x

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------
.PHONY: README
README: README.html

clean_images:
	@rm -f Lib/$(ARCH)/*.$(image_ext)

distclean_images:
	rm -rf Lib/*

clean_module:
	@rm -f EuLispModules/ti-sys-signatures.def
	@rm -rf EuLispModules/$(ARCH_DIR)

distclean_module: clean_module
	@rm -rf EuLispModules/platforms

clean_fasl:
	@rm -f {.,Apply,EclModules}/*.{fasl,bin,sse2f}

clean: clean_fasl
	@$(MAKE) -C Runtime clean
	@$(MAKE) -C Examples clean

distclean: clean_fasl distclean_images distclean_module
	@$(MAKE) -C Runtime distclean
	@$(MAKE) -C Examples distclean

###-----------------------------------------------------------------------------
