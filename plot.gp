set xlabel "t" 
set ylabel "y"
set xzeroaxis
set yzeroaxis
set size ratio -1
set yrange [-2:2]
set terminal svg font "IBM Plex Mono, 8" size 800, 400 dynamic
set output "fourier.svg"

plot "plot-data.txt" using 1:2 with lines linewidth 3 title "f(t)", \
     "plot-data.txt" using 1:3 with lines title "term1", \
     "plot-data.txt" using 1:4 with lines title "term2", \
     "plot-data.txt" using 1:5 with lines title "term3", \
     "plot-data.txt" using 1:6 with lines title "term4", \
