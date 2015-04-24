#!/bin/sh

#mpiexec -n 1 ../meld -d -c sl -f code/$progundertest.m  >& $$.output
#ref=files/$progundertest.test

cmd=$1
ref=files/$2.test
progundertest=$2;

$cmd >& /tmp/y$$.output
if [ $? != 0 ]; then
   echo "Failed to execute vm for $progundertest (See /tmp/y$$.output)"
   exit 1
fi
diff -q $ref /tmp/y$$.output >& /tmp/$$.diffoutput
do=$?
/bin/rm -f /tmp/$$.diffoutput
if [ $do != 0 ]; then
   echo "Failed on diff to reference output for $progundertest (see /tmp/y$$.output)"
   exit 2
else
    rm /tmp/y$$.output
    echo "$progundertest passed"
fi
exit 0

