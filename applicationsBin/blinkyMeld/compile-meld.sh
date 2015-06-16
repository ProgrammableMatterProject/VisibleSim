#!/bin/sh

usage(){
    echo "Usage: $0 <path-to-meld-file>"
    echo "Example: $0 meld/programs/rainbow.meld"
    exit 1
}

[ $# -eq 0 ] && usage

name="$(basename $1 .meld)"
echo "$name"

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

if grep -q "WARNING" $compile_file; then
   echo "Compiler returned warnings!"
   read -p "Continue? y/n " yn
   if [ "$yn" != "y" ]; then
      rm -f $compile_file
      exit 1
   fi
fi

rm -f $compile_file
echo "Compilation done!"

if [ ! -f meld/LMParser/LMParser ]; then
   echo "Compiling LMParser ..."
   (
       cd meld/LMParser/
       make
   )
   echo "Done."
fi

echo "Generating .bb file"
meld/LMParser/LMParser $name.m

if [ $? != 0 ]; then
   echo "Failed to parse byte-code file $name.m"
   exit 1
fi

rm -f $name.m

echo "Done."

