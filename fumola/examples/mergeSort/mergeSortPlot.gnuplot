# Convert natural log to log2
log2(x) = log(x)/log(2)

set terminal pngcairo size 1000,700
set output "mergeSortPlot.png"
set title "Comparisons for Demand-Driven MergeSort"
set key left top

plot \
    "comparisonCount.dat" using 1:4 with linespoints title "Fumola: Demand All", \
    "comparisonCount.dat" using 1:(lgamma($1+1)/log(2)) with linespoints title "Optimal Demand All", \
    "comparisonCount.dat" using 1:3 with linespoints title "Fumola: Demand Half", \
    "comparisonCount.dat" using 1:2 with linespoints title "Fumola: Demand One", \
 