#!/bin/sh

name="$(basename $1 .meld)"
echo "$name"
sbcl --eval "(load \"meld/meld-compiler/setup\")" \
     --eval "(ql:quickload \"cl-meld\")" \
     --eval "(cl-meld:meld-compile \"$1\" \"$name\")" \
     --eval "(quit)"

if [ $? != 0 ]; then
   echo "Failed to compile file $1"
   exit 1
fi
if [ ! -f $name.m ]; then
   echo "Failed to compile file $1"
   exit 1
fi

echo "Compilation done"
