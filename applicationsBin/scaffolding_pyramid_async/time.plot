#!/usr/bin/gnuplot
reset
#set terminal png

set xlabel "Size h of the pyramid"
set ylabel "Reconfiguration Time (Time Steps)"

#set grid

#set samples 1000
set datafile separator whitespace
set style fill solid 1 border
set style data boxes
set boxwidth 0.5 relative
#set output 'numberOfMessages.png'
set xtics (0,1,2,3,4,5,6,7,8,9)
#set ytics (0, 5000, 10000, 15000, 20000, 25000, 30000, 35000)
#Shadecolor = "#80E0A080"
#set logscale y
#set logscale x
# set format y "%g %%"
set yrange[0:]
#set yrange[0:3000]
set xrange[0:10]
set key top right
#set grid

#linear fit

f(x) = a*x + b
fit f(x) 'time.dat' using 1:2 via a, b
ti = sprintf("y(x) = %.2fx+%.1f", a, b)
set label ti at 5,360 rotate by 33

# set arrow from 0,40729 to 2910,35000 nohead
# set arrow from 1415,0 to 1415,35000 nohead lt 3
# set arrow from 2910,0 to 2910,35000 nohead lt 4

# set x2range[0:7923]
# set x2tics 8 1000
# set xtics nomirror

set terminal pngcairo size 1000,600
pngName = "pyramid_time_h.png"
set output pngName

linewidth = 2
plot 'time.dat' u 1:2 t 'Reconfiguration time of the h-pyramid relative to h' lt 3 lw linewidth,\
     f(x) lt 4 lw 2

pause -1
