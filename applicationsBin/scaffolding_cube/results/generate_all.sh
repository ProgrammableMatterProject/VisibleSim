#!/bin/bash

generate_logs_if_required() {
    for i in `seq 1 9`; do
        if [[ ! -f simulation_cube_"$i".log ]]; then
            ../meshAssembly -c ../b6/config_"$i"x"$i"_cf_b6.xml -t -R -l;
            mv simulation.log simulation_cube_"$i".log
        fi
    done
}

generate_msg() {
    if [[ ! -f msg/msg_size.dat ]]; then
        echo -e "" > msg/msg_size.dat
        for i in `seq 1 9`; do
            cat simulation_cube_"$i".log | grep "nbMessages:" | \
                cut -d$'\t' -f1 --complement > msg/msg_"$i".dat;
            echo -n "$i"$'\t' >> msg/msg_size.dat;
            cat msg/msg_"$i".dat | tail -n1 | cut -d$'\t' -f2 >> msg/msg_size.dat
        done
    fi

    (cd msg \
         && gnuplot msg_cube_all.plot \
         && read -p "add module count to msg_size and press a key to continue..." \
         && gnuplot msg_cube_size.plot)
}

generate_mvmt() {
    for i in `seq 1 9`; do
        cat simulation_cube_"$i".log | grep -e "mvmt: " > mvmt/mvmt.log;
        (cd mvmt \
             && python mvmt.py | cut -d ' ' -f2 > mvmt_"$i".dat \
             && gnuplot -e "filename='mvmt_"$i".dat'; dimension='"$i"x"$i"'" \
                        mvmt_cube.plot)
    done
}

generate_main() {
    echo -n "" > main.dat
    for i in `seq 1 9`; do
        cat simulation_cube_"$i".log | grep -e "main: " | \
            cut -d$'\t' -f1 --complement >> main.dat;
    done

    (cd time && gnuplot timestep.plot)
}

generate_logs_if_required
generate_main
generate_msg
generate_mvmt
