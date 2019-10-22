# formato y nombre de la imagen
set term png
set output "P3-1920-fig1.png"

# muestra los ejes
set xzeroaxis
set yzeroaxis

# define el título
set title "D(E),D'(E)"

# define el rango de valores de los ejes que se muestra en pantalla
#set xrange[-0.5:3]
#set yrange[-6:3]

# define los títulos de los ejes
set xlabel "E"
set ylabel "D(E),D'(E) (U.A.)"

# format dels nombres de l'eix y: 2 decimals
set format y '%.2f'
set format x '%.2f'

set key outside

# plot 
plot "P3-1920-res.dat" index 0 using 1:2 with lines t "D(E)", \
"P3-1920-res.dat" index 0 using 1:3 with lines t "D'(E)"
#pause -1
