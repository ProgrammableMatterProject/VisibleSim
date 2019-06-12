#!/usr/bin/gnuplot
reset
#set terminal png

set xlabel "Time Steps"
set ylabel "# Sent Messages"

#set grid

#set samples 1000
set datafile separator whitespace
set style data lines
#set output 'numberOfMessages.png'
#set xtics (0, 500, 1000, 1500, 2000, 2500, 3000, 3500)
#set ytics (0, 5000, 10000, 15000, 20000, 25000, 30000, 35000)
#Shadecolor = "#80E0A080"
#set logscale y
#set logscale x
# set format y "%g %%"
# set yrange[0:100]
#set yrange[0:3000]
# set xrange[0:784917248]
set key bottom right
#set grid

#linear fit

#f(x) = a*x + b
#fit f(x) 'floorbyfloor-simplified.csv' using 5:3 via a, b
#ti = sprintf("y(x) = %.2fx+%.1f", a, b)
#set label ti at 1500,13000 rotate by 35

#set arrow from 0,40729 to 2910,35000 nohead
#set arrow from 1415,0 to 1415,35000 nohead lt 3
#set arrow from 2910,0 to 2910,35000 nohead lt 4

linewidth = 2
plot 'msg4.dat' u  1:2  t '4x4 Pyramid' lt 8 lw linewidth,\
# 'msg_p3_b6.dat' u  1:2  t '3x3 Pyramid' lt 7 lw linewidth,\
# 'msg_p4_b6.dat' u  1:2  t '4x4 Pyramid' lt 3 lw linewidth,\
# 'msg_p5_b6.dat' u  1:2  t '5x5 Pyramid' lt 4 lw linewidth,\
# 'msg_p6_b6.dat' u  1:2  t '6x6 Pyramid' lt 5 lw linewidth,\
# 'msg_p7_b6.dat' u  1:2  t '7x7 Pyramid' lt 6 lw linewidth,\
# 'msg_p8_b6.dat' u  1:2  t '8x8 Pyramid' lt 1 lw linewidth,\
# 'msg_p9_b6.dat' u  1:2  t '9x9 Pyramid' lt 2  lw linewidth

pause -1
