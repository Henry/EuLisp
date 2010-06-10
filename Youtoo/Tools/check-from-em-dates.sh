#!/bin/sh
find=${FIND:-find}

do_c=1
do_h=1
do_i=1
do_u=

errors=0
while getopts "chiu" opt
do
  if [ "$opt" = "?" ]
  then
    errors=$((errors+1))
    continue
  fi

  case $opt in
  c) if [ -z "$do_c" ]; then do_c=1; else do_c=; fi;;
  h) if [ -z "$do_h" ]; then do_h=1; else do_h=; fi;;
  i) if [ -z "$do_i" ]; then do_i=1; else do_i=; fi;;
  u) if [ -z "$do_u" ]; then do_u=1; else do_u=; fi;;
  esac
done

# echo do_c: $do_c do_h: $do_h do_i: $do_i do_u: $do_u

timeinfo () {
  if [ -r "$1" ]
  then
    ${find} $1 -printf "%TY/%Tm/%Td-%TH:%TM:%TS %p\n"
  else
    echo "                 no $1"
  fi
}

for i in $(${find} Telos Runtime Comptime2 -name \*.em)
do
  base=$(dirname $i)/$(basename $i .em)
  timeinfo $i
  [ -n "$do_c" ] && timeinfo ${base}.c
  [ -n "$do_h" ] && timeinfo ${base}.h
  [ -n "$do_i" ] && timeinfo ${base}.i
  [ -n "$do_u" ] && timeinfo ${base}_.c
  echo
done
