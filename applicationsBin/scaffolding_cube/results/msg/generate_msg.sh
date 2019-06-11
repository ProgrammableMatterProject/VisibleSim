#!/bin/sh

echo "" > msg_size.dat
for i in `seq 2 9`;
do
    ../../meshAssembly -c ../../b6/config_"$i"x"$i"_cf_b6.xml -t -R -l;
    cat simulation.log | grep "nbMessages:" | cut -d$'\t' -f1 --complement > msg_"$i".dat;
    rm simulation.log;
    echo -n "$i\t" >> msg_size.dat;
    cat msg_"$i".dat | tail -n1 | cut -d$'\t' -f2 >> msg_size.dat;
    echo "" >> msg_size.dat;
done

gnuplot messages_cube_all.plot
