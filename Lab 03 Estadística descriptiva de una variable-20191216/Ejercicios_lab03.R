# 1. En 200 intervalos de tiempo de 5 segundos, se cuenta el número de mensajes, 
# infectados de algún virus, que llegan a un servidor: 

library(DescTools)
library(moments)
library(ggplot2) 

# Mensajes por periodos de 5 segundos
datos = c(1,1, 1, 2, 0, 2, 0, 0, 2, 2, 0, 1, 0, 2, 2, 0, 0, 1, 2, 2, 1, 2, 3, 0, 0, 1, 4, 0, 1, 0, 0, 3, 3, 2, 0, 1 ,0 ,0 ,0 ,3, 2, 0, 3, 3, 1, 1, 3, 2, 0, 1, 2, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 3, 2, 0, 0, 2, 1, 0, 3, 1, 0, 2, 1, 3, 2, 1, 1, 0, 3, 1, 0, 0, 0, 2, 0, 1, 1, 3, 1, 0, 0, 2, 3, 2, 1, 1, 1, 0, 4, 1, 4, 0, 1, 1, 2, 0, 3, 3, 1, 2, 3, 2 ,1, 0, 1, 1, 1, 2, 3, 0, 2, 2, 1, 3, 6, 2, 0, 2, 2, 0, 1, 0, 2, 0, 1, 3, 0, 2, 0, 0, 1, 0, 3, 1, 1, 1, 1, 3, 1, 1, 2, 0, 1, 4, 4, 1, 2, 0, 3, 0, 1, 0, 0, 1, 2, 3, 0, 2, 3, 2, 1, 2, 3, 2, 0, 1, 1, 1, 1, 1, 2, 1, 2, 2, 1, 1, 1, 0, 2, 0, 0, 3, 0, 2, 2, 0, 1, 2, 3, 0)

# a) Determinar la correspondiente tabla de frecuencias
Freq(datos)
# Freq Frecuencias absolutas
# Perc Es la frecuencia relativa
# cumfreq Frecuencias absolutas acumuladas
# cumperc Frecuencia relativas acumuladas

# b) Hallar media, mediana, moda, varianza y coeficiente de variación.

# Media:
mean(datos)

# Mediana:
median(datos)

# Moda: 
Mode(datos)

# Varianza. Como no encuentro la funcion pues la creo
variance <- function (x) sum((x-mean(x))^2)/(length(x)-1)
variance(datos)

# Varianza de modulo
var(datos)

# Coeficiente de variacion
CoeficienteVariacion = function(x) var(x) / abs(mean(x))
CoeficienteVariacion(datos)

# c) Dibujar un diagrama de barras de frecuencias absolutas y un polígono de frecuencias relativas acumuladas. 

freqAbs = Freq(datos)[,"freq"]
qplot(freqAbs, geom = "bar")

hist(datos, freq = FALSE)
#freq True: Frecuencias absolutas, false: relativas, 
