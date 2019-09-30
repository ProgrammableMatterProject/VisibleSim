#!/bin/sh

for i in `seq 2 9`;
do
    ../meshAssembly -c ../config_"$i"x"$i".xml -t -R -l;
    cat simulation.log | grep -e "mvmt: " > mvmt.log;
    python mvmt.py | cut -d ' ' -f2 > mvmt_"$i".dat;
    rm -rf mvmt.log simulation.log;
    gnuplot -e "filename='mvmt_"$i".dat'; dimension='"$i"x"$i"'" mvmt.plot;
done
