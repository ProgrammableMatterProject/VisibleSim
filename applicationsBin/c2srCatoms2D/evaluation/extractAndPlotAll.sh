#! /bin/bash

rm -rf graphs/*.pdf

(
    cd scripts
    ./extract.sh
    ./extract-rate.sh
)

(
    cd graphs
    ./replot.sh
)
