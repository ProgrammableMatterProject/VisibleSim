#!/usr/bin/gnuplot
reset
#set terminal png

set ylabel "Time Steps"
set xlabel "Dimension of Target Cube"

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
#set format y "%g %%"
set yrange[0:]
set xrange[2:]
# set xrange[0:784917248]
set key bottom right
#set grid

#linear fit

f(x) = a*x + b
fit f(x) '../main.dat' using 1:2 via a, b
ti = sprintf("y(x) = %.2fx+%.1f", a, b)
set label ti at 1000,1000 rotate by 35

set terminal pdf
pdfName = "cube_time.pdf"
set output pdfName

linewidth = 3
plot 'main.dat' u  1:2  t 'Reconfiguration Time into a x-Cube' lt 1 lw linewidth,\
     f(x) t 'fit' lt 2 lw linewidth

#pause -1
