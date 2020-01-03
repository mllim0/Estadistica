setwd("C:/Users/anton/Documents/Programacion/Estadistica/Lab 05 Estadística descriptiva bidimensional-20191216")
load("HIPER200.RDATA")
attach(HIPER200)

tablaCafeFisi = table(cafe, act_fisi)
tablaCafeFisi

#  La distribución de Actividad Física condicionada a la toma de Café. 
print(prop.table(tablaCafeFisi,1), digit=2) # X/Y

# Representaciones graficas apropiadas para estas dos variables. 
barplot(tablaCafeFisi,beside=TRUE,col=c(2,3,4),legend =rownames(tablaCafeFisi), sub="Cafe")

library(lattice)
histogram(~cafe|act_fisi)

# ¿Son variables independientes? Con este ultimo histograma podemos apreciar que: 
# No hay mucha diferencia entre la cantidad de cafe ingerida y la actividad fisica

# Tomar la variable Peso agrupada en intervalos y la variable Actividad Física. 
# Construir su tabla de frecuencias conjuntas. 

library(DescTools)
# Miramos el valor min, med y max para hacer una estimacion de los intervalos
summary(peso)

# Agrupamos la variable peso en intervalos
tablaPesoFisi = table(peso=factor(cut(peso, breaks = 4)), act_fisi)
tablaPesoFisi

#Distribucion del peso para cada nivel de actividad fisica
distribucion = prop.table(tablaPesoFisi,1)
print(distribucion, digits = 2)

# Mostrar graficamente
barplot(distribucion,beside=FALSE,col=c(2,3,4),legend =rownames(distribucion), sub="Act_fisica")

# ¿Son variables independiente? No son variables independientes. Pues se puede apreciar
# Donde más concentracion de peso alto hay es en actividad fisica escasa. Los pesos normales entran dentro de
# actividades fisicas moderadas y los pesos menores con mayor indice de actividad fisica intensa

