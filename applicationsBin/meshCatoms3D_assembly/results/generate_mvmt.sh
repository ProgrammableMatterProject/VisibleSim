#!/bin/sh

for i in `seq 2 9`;
do
    /meshAssembly -c b6/config_"$i"x"$i"_cf_b6.xml -t -R -l;
    cat simulation.log | grep -e "mvmt: " > mvmt.log;
    python parallelism.py | cut -d ' ' -f2 > mvmt_"$i".dat;
    gnuplot -e "filename='mvmt_"$i".dat'; dimension='"$i"x"$i"'" mvmt.plot;
done
