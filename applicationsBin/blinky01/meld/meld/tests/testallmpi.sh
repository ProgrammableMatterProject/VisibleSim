#!/bin/sh
if [ ${MELDCL:-unset} == unset ]; then
  echo "You need to set MELDCL to point to directory with meld compiler"
fi
../meld-mpi -a mpi
if [ $? != 0 ]; then
    echo "vm is not compiled to use mpi"
    exit 1
fi
for i in progs/*.meld; do make -f test.make FILE=$i TARGET=mpi NODES=1; done
exit