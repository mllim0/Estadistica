# =Convertir dos vectores en tablas=
library(DescTools)
nucleoTiempo = as.table(setNames(tiempos, nucleos))
nucleoTiempo
# !==

# Ejercicio 1
# Cuantos más núcleos más rápido
# y = Tiempo de respuesta en ms
# x = Numero de nucleos

# Creamos los datos y
tiempos = c(11,12,10,13,11,9,10,7,12,8,7,3,6,5,5)
nucleos = c(2,3,4,4,5,5,6,7,7,9,9,10,11,11,12)

# Es verdad que cuan más núcleos más rápido? Es verdad. Si vemos la recta de regresión afirma el modelo. A cuan mas numero de nucleos menos tiempo de computo para realizar una tarea
# Diagrama de puntos (Diagrama de dispersión)
plot(tiempos, nucleos)

# Para modelos lineales
# Ejemplos:
# y=a+bx | y= a+bx +cx^2
# reg1<-lm(y~x) summary(reg1) reg2<-lm(y~x+I(x^2)) summary(reg2) reg3<-lm(y~x+I(x^2)+I(x^3)) summary(reg3) reg41<-lm(y~I(1/x))
#
library(stats)
a = lm(tiempos~nucleos)
# Dibujar la recta
abline(a)


# Prediciendo el tiempo de computo para un prototipo con 13 nucleos || El timepo es de 3.78ms
predict (a ,data.frame(nucleos=c(13,14,15)),interval ="prediction")
x1<-c(13,14,15) ; y1<-c(3.78,2.98,2.17)
points(x1,y1,col="blue",pch=20)
