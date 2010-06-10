#!/usr/bin/env bash
# usage: cd .../EuLysses  && ./Tools/delete-gen-from-em.sh
#
# Deletes the .c, .h, .i, and _.c files that were generated from .em files.
#
# Assumes that if there is a .em file the matching .c, .h, .i, and _.c files
# were generated from it.

# So we can override for use when exporting.
PROJECT=${PROJECT:-youtoo}

for i in $(find . -name \*.em)
do 
    base=$(dirname $i)/$(basename $i .em)
    echo base: $base
    for j in .c .h .i _.c
    do
	file=${base}$j
	echo file: $file
	if [ -r "$file" ]
	then
	    prcs depopulate $PROJECT $file
	else 
	    echo not readable: $file
	fi
    done
done
