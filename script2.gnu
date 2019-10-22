# formato y nombre de la imagen
set term png
set output "P3-1920-fig2.png"

# muestra los ejes
set xzeroaxis
set yzeroaxis

# define el título
set title "Trajectòria"

# define el rango de valores de los ejes que se muestra en pantalla
#set xrange[-0.5:3]
#set yrange[-6:3]

# define los títulos de los ejes
set xlabel "x (U.A)"
set ylabel "y (U.A)"

# format dels nombres de l'eix y: 2 decimals
set format y '%.2f'
set format x '%.2f'

set key outside

# plot 
plot "P3-1920-res.dat" index 2 using 3:4 with lines t "y(x)"
#pause -1
