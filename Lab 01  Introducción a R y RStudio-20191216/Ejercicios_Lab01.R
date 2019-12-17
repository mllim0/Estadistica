# 1- Proponer cinco vectores de dimensión 5, llamémosles a, b, c, d y e, respectivamente. 
#    a) Sumar los dos primeros vectores a+b, Realizar el producto escalar entre a y b. Calcular 2a-5b+3d 
#    b) Crear una matriz de 5x5 con dichos vectores como columnas. Calcular el determinante de A. Calcular 
#    sus raíces y vectores característicos. Su inversa, si existe. 
#    c) Combinar los 25 datos en un vector x. Ordenarlos de menor a mayor. Determinar su valor mínimo. 
#    Determinar su valor máximo. La suma de todos los datos. La media aritmética de todos los datos. 

#Creamos lo vectores
a = c(10,11,12,14,15)
b = c(1:5)
c = c(1:5)
d = c(1:5)
e = c(1,2,3,4,5)

#Sumamos a+b
a+b

#Producto escalar
prod(a*b)

#Calcular 2a-5b+3d
2*a-5*b+3*d

#Crear una matriz 5*5 con los vectores como columnas
matriz = matrix(ncol = 5, nrow = 5)
matriz[,1] = a
matriz[,2] = b
matriz[,3] = c
matriz[,4] = d
matriz[,5] = e

#calcular el determinante de la matriz
det(matriz)

#Calcular sus raices
sqrt(matriz)

#Calcular su inversa Si se puede (No se puede con esta matriz)
solve(matriz)

#Combinar los 25 datos en un vector x
x = as.vector(matriz)

#Ordenarlos de menor a mayor
ordenados = sort(x, FALSE)

#Determinar su valor minimo
min(ordenados)

#Determinar su valor maximo
max(ordenados)

#La suma de todos los datos
sum(ordenados)

#La media de los elementos
mean(ordenados)

#=================================================
# Ejercicio 2: Resolver el sistema de ecuaciones
# 7a+6b+3c+2d=10
# 5a+3d=5
# 4a-2b+5c+4d=15
# 6b-2c+d=12
#=================================================

# Declaramos los datos
soluciones = matrix(c(10,5,15,12))
sistema = matrix(c(7,5,4,0,6,0,-2,0,3,0,5,-2,2,3,4,1), ncol = 4, nrow = 4)

#Mostramos los datos
sistema
soluciones

# Usamos la formula Z = (Xt * X)^-1 * Xt * Y

resultado = solve(t(sistema) %*% sistema) %*% t(sistema) %*% soluciones 
