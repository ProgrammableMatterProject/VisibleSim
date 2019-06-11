#!/usr/bin/gnuplot
reset
#set terminal png

set xlabel "Time Steps"
set ylabel "Number of Modules in Motion"

#set grid

#set samples 1000
set datafile separator whitespace
#set style data lines
set style data histograms
set boxwidth 0.1
#set output 'numberOfMessages.png'
#set xtics (0, 500, 1000, 1500, 2000, 2500, 3000, 3500)
#set ytics (0, 5000, 10000, 15000, 20000, 25000, 30000, 35000)
#Shadecolor = "#80E0A080"
#set logscale y
#set logscale x
# set format y "%g %%"
# set yrange[0:100]
#set yrange[0:3000]
set xrange[0:900]
set key top right
#set grid

#linear fit

#f(x) = a*x + b
#fit f(x) 'floorbyfloor-simplified.csv' using 5:3 via a, b
#ti = sprintf("y(x) = %.2fx+%.1f", a, b)
#set label ti at 1500,13000 rotate by 35

#set arrow from 0,40729 to 2910,35000 nohead
#set arrow from 1415,0 to 1415,35000 nohead lt 3
#set arrow from 2910,0 to 2910,35000 nohead lt 4

set terminal pdf
set output "mvmt_compare_9.pdf"

set samples 1000
linewidth = 3
plot  "mvmt_9.dat" u  1:2 smooth bezier t "9x9 Pyramid b=6 (Sync)" lt 3 lw linewidth with lines, \
      "mvmt_9_async.dat" u  1:2 smooth bezier t "9x9 Pyramid b=6 (Async)" lt 7 lw linewidth with lines

     # "" smooth sbezier t "Bezier Smooth" lw 3 lt 6

# pause -1
