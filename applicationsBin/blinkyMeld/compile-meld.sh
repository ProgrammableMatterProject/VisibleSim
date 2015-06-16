#!/bin/sh

name="$(basename $1 .meld)"
echo "$name"

#sbcl --eval "(load \"meld/meld-compiler/setup\")" \
#     --eval "(ql:quickload \"cl-meld\")" \
#     --eval "(cl-meld:meld-compile \"$1\" \"$name\")" \
#     --eval "(quit)"

compile_file=`mktemp -t compileXXXX`
sbcl --eval "(load \"meld/meld-compiler/setup\")" \
     --eval "(ql:quickload \"cl-meld\")" \
     --eval  "(cl-meld:meld-compile \"$1\" \"$name\")" \
     --no-userinit --non-interactive --noinform --noprint \
     --no-sysinit --dynamic-space-size 4Gb 2>&1 | tee $compile_file

if [ $? != 0 ]; then
    rm -f $compile_file
    echo "Failed to compile file $1"
    exit 1
fi

if [ ! -f $name.m ]; then
    rm -f $compile_file
    echo "Failed to compile file $1"
    exit 1
fi

rm -f $compile_file
echo "Compilation done!"

(
cd meld/LMParser
make
)

meld/LMParser $name.m

echo "Parsing done!"

