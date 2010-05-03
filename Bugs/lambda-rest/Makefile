### Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### EuLisp lambda-rest bug test Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include ../../Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------

all: lambda-rest simple

lambda-rest: lambda-rest.em
	$(U2) $(U2FLAGS) lambda-rest -l level1

simple: simple.em
	$(U2) $(U2FLAGS) simple -l level1

clean:
	rm *.i *.h *.c *.o lambda-rest simple

###-----------------------------------------------------------------------------
