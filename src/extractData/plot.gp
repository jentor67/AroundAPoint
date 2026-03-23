#!/usr/bin/gnuplot
# Set terminal type for output (e.g., png)
set terminal pngcairo

# Set the output file name
set output 'my_graph.png'

# Set plot title and axis labels
set title 'My Data Plot'
set xlabel 'X Axis'
set ylabel 'Y Axis'
set zlabel 'Z Axis'
set grid
lineCount=3000000

# Plot data from a file named 'data.txt' using columns 1 and 2 with lines
# plot 'data.txt' using 1:2 with lines title 'Dataset 1'
#splot 'obj1.txt' every ::0::'$lineCount' with lines #, \
splot 'obj1.txt' with lines 
#, \
#'obj2.txt' every ::0::'$lineCount' with lines, \
#'obj3.txt' every ::0::'$lineCount' with lines, \
#'obj4.txt' every ::0::'$lineCount' with lines, \
#'obj5.txt' every ::0::'$lineCount' with lines, \
#'obj0.txt' every ::0::'$lineCount' with lines

