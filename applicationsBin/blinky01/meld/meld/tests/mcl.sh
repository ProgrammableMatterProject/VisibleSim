#!/bin/sh

# compile a meld program into a .m file
# I think this may require bash (on my system sh is bash)

# test that input file exists
if [ ! -f $1 ]; then
    echo "I cannot find the meld source file '$1'"
    exit -1
fi

# test that compiler exists
x=${MELDCL:-bad}
if [ $x == bad ]; then
    echo "I cannot find env variable MELDCL defining path to meld compiler"
    exit -1
fi
if [ ! -d $x ]; then
    echo "I cannot find the directory containing the meld compiler: $x"
    exit -1
fi

# get src and dest paths
src=`readlink -f $1`
sdir=${src%/*}
sfile=${src##*/}
dest=${src%.meld}

if [ $# == 2 ]; then
    # user specified output file, so don't use default
    dest=`readlink -f $2`
    # make sure path to dest is writable
    pdir=${dest%/*}
    if [ ! -d $pdir ]; then
	echo "Specifed target is not a directory ($pdir)"
    fi
    base=${dest##*/}
    ext=${base##*.m}
    if [ ${ext:-nothing} == nothing ]; then
	echo "Did you really mean to include the .m in your destination file?"
    fi
fi
if [ ${dest:-nothing-to-write-to} == nothing-to-write-to ]; then
    echo "Error with output specification ($dest).  Bad path?"
    exit 1
fi
echo "Will compile $src into $dest.m"

/bin/rm -f /tmp/$$.script
#echo "(require :sb-posix)" > /tmp/$$.script
echo "(load \"load\")" >> /tmp/$$.script
#echo "(sb-posix:chdir \"$sdir\")"  >> /tmp/$$.script
#echo "(cl-meld:meld-compile \"$sfile\" \"$dest\")" >> /tmp/$$.script
echo "(cl-meld:meld-compile \"$src\" \"$dest\")" >> /tmp/$$.script
echo "(exit)"  >> /tmp/$$.script
cd $MELDCL
#echo "========= Starting lisp environment"
#cat /tmp/$$.script
#echo "========="
sbcl < /tmp/$$.script >& /tmp/$$.log
#echo "========= Done"
x=`grep -v "STYLE-WARNING" /tmp/$$.log | grep "^\*\|^All\|^T$" | wc -l`
if [ $x != 6 ]; then 
    echo "==== Some error in compilation.  Log follows"
    cat /tmp/$$.log
    echo "==== Some error in compilation.  Log is above"
    exit 1
else
    echo "Compiled succesfully"
    /bin/rm /tmp/$$.log
fi
/bin/rm -f /tmp/$$.script
exit 
