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
lineCount=315000

# Plot data from a file named 'data.txt' using columns 1 and 2 with lines
# plot 'data.txt' using 1:2 with lines title 'Dataset 1'

# this works
#plot '/mnt/kdrive/file_00000004.dat' using 1:2 with lines title 'Dataset 1'


#splot 'obj1.txt' every ::0::'$lineCount' with lines #, \

splot '/mnt/kdrive/file_00000001.dat' every ::0::lineCount with lines title "Sun" , \
'/mnt/kdrive/file_00000002.dat' every ::0::lineCount with lines title "Earth"

