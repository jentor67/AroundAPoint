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

i = 1
while (i <= 277) {
    print i

    lineCount=3600*i
    
    padded_val = sprintf("%09d", lineCount)
    
    set output '10_Objects/my10Objects_' . padded_val . '.png'

    set terminal png size 1280,960 enhanced
    
    set xrange [-10:10]
    set yrange [-10:10]
    set zrange [-10:10]
    
    
    # Plot data from a file named 'data.txt' using columns 1 and 2 with lines
    # plot 'data.txt' using 1:2 with lines title 'Dataset 1'
    
    # this works
    #plot '/mnt/kdrive/file_00000004.dat' using 1:2 with lines title 'Dataset 1'
    
    
    #splot 'obj1.txt' every ::0::'$lineCount' with lines #, \
    
    splot '/mnt/kdrive/file_00000001.dat' every ::0::lineCount with lines title "C" , \
    '/mnt/kdrive/file_00000002.dat' every ::0::lineCount with lines title "", \
    '/mnt/kdrive/file_00000003.dat' every ::0::lineCount with lines title "", \
    '/mnt/kdrive/file_00000004.dat' every ::0::lineCount with lines title "", \
    '/mnt/kdrive/file_00000005.dat' every ::0::lineCount with lines title "", \
    '/mnt/kdrive/file_00000006.dat' every ::0::lineCount with lines title "", \
    '/mnt/kdrive/file_00000007.dat' every ::0::lineCount with lines title "", \
    '/mnt/kdrive/file_00000008.dat' every ::0::lineCount with lines title "", \
    '/mnt/kdrive/file_00000009.dat' every ::0::lineCount with lines title "", \
    '/mnt/kdrive/file_00000010.dat' every ::0::lineCount with lines title ""

    i = i + 1
}
