#! /bin/sh

for gpl in $(ls *.gpl); do
    gnuplot "$gpl"
done
