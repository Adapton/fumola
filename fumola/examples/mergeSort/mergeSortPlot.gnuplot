# Convert natural log to log2
log2(x) = log(x)/log(2)

set terminal pngcairo size 1000,700
set output "mergeSortPlot.png"
set title "Comparisons for Demand-Driven MergeSort (Demand All)"

plot \
    "comparisonCount.dat" using 1:2 with linespoints title "Measured: Cache On/Off", \
    "comparisonCount.dat" using 1:(lgamma($1+1)/log(2)) with linespoints title "Optimal"
