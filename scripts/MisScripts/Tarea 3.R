# 1. Crea un vector llamado Harry formado por la sucesión de números 
# consecutivos entre el -10 y 27. Pídele a R que devuelva el elemento 
# de índice 7. Escribe el resultado.

Harry = seq(-10,27, by=1)
Harry
Harry[7] #-4

# 2. Da el máximo de la sucesión $100\cdot 2^n -7\cdot3^n$ con $n=0,
# \dots,200$.

n = 0:200
SucMax = max(sapply(n, FUN = function(n){(100*2^n)-(7*3^n)}))
SucMax # 1499

# 3. Crea la sucesión de números consecutivos entre 0 y 40. A continuación, 
# crea el vector 3 * 5 ^ n - 1 con n = 0, ..., 40. Ponle como nombre x. 
# Ahora, da el subvector de los elementos que son estrictamente mayores a 3.5.

n = 0:40
x = sapply(n, FUN = function(n){3*(5^{n-1})})
subVecXIndex = which(x > 3.5)
subVecX = x[which(x > 3.5)]
subVecX

# [1]  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41
# [1] 1.500000e+01 7.500000e+01 3.750000e+02 1.875000e+03 9.375000e+03 4.687500e+04 2.343750e+05 1.171875e+06 5.859375e+06 2.929688e+07 1.464844e+08 7.324219e+08 3.662109e+09
# [14] 1.831055e+10 9.155273e+10 4.577637e+11 2.288818e+12 1.144409e+13 5.722046e+13 2.861023e+14 1.430511e+15 7.152557e+15 3.576279e+16 1.788139e+17 8.940697e+17 4.470348e+18
# [27] 2.235174e+19 1.117587e+20 5.587935e+20 2.793968e+21 1.396984e+22 6.984919e+22 3.492460e+23 1.746230e+24 8.731149e+24 4.365575e+25 2.182787e+26 1.091394e+27 5.456968e+27

# 4. Crea una función que devuelva la parte real, la imaginaria, el módulo, 
# el argumento y el conjugado de un número, 
# mostrando solo 2 cifras significativas.


complejo = function(x){
  print("La parte real del número introducido es: ")
  print(Re(x),2)
  print("La parte imaginaria del número introducido es: ")
  print(Im(x),2)
  print("El módulo del número introducido es: ")
  print(Mod(x),2)
  print("El argumento del número introducido es: ")
  print(Arg(x),2)
  print("El conjugado del número introducido es: ")
  print(Conj(x),2)
}


complejo(c(4+3i, 1+2i, 5+4i))


# 5. Crea una función que resuelva ecuaciones de segundo grado 
# (de la forma $Ax^2+Bx+C=0$). No importa, por ahora, que tengas en cuenta 
# las ecuaciones de segundo grado que no tienen solución real.

SegGrad = function(a,b,c){
  print("Tomando la ecuación positiva: ")
  print((-b+sqrt(   as.complex(b^2-4*a*c)     ))/2*a)
  print("Tomando la ecuación negativa: ")
  print((-b-sqrt(   as.complex(b^2-4*a*c)     ))/2*a)
}

SegGrad(1,2,3)


# 6. Da 3 opciones diferentes para calcular el subvector c(9,19,20,16): 

vec = c(0,9,98,2,6,7,5,19,88,20,16,0)

# 1ra opcion
vec[c(2,8,10,11)]

# 2da opcion
which(vec>=9 & vec<=20)
vec[which(vec>=9 & vec<=20)]

#3ra opcion
vec[-c(1,3,4,5,6,7,9,12)]

# Tomando el vector vec definido en el apartado anterior, busca:

# - qué entradas son pares

vec[which(vec%%2 == 0)]

# - qué entradas no son pares y mayores que 20

vec[which(vec%%2 == 1 & vec > 20)]

# - dónde toma vec su valor máximo

which.max(vec)

# - dónde toma vec sus valores mínimos

which(vec==min(vec))


# 7. Da la entrada (2,2) de A\cdot (A+A )\cdot A, con:
# A = [ [ 1  3 ] [ 2  4 ] ]

A = rbind(c(1,3),c(2,4))
B = A%*%(A+A)%*%A
B[2,2] # 236


# 8. Da los valores propios de la matriz
# B = [ [ 2  4  -6 ] [ 0  0  3 ] [ 0 -2  5 ] ]

B = rbind(c(2, 4, -6),c(0, 0, 3),c(0, -2, 5))
B
eigen(B)$values # 3 2 2

# 9. Da, redondeando a 3 cifras decimales, los vectores propios de la matriz
# C = [ [ -48  35  -12 ] [ -134  95  -32 ] [ -194  133  -44 ] ]

C = rbind(c(-48,  35,  -12),c(-134,  95,  -32),c(-194,  133,  -44))

round(eigen(C)$vectors,3)

#       [,1]  [,2]   [,3]
# [1,] 0.371 0.169  0.098
# [2,] 0.743 0.507 -0.195
# [3,] 0.557 0.845 -0.976


# 10. Da el rango de la matriz
# D =[ [ -2  -8  -2  3 ] [ -3  -6  -1  2 ] [ -9  -22  -3  7 ] [ -18  -44  -8  15 ] ]

D = rbind(c(-2,  -8,  -2,  3),c(-3,  -6,  -1,  2),c(-9,  -22,  -3,  7), c(-18,  -44,  -8,  15))
D

qr(D)$rank

# 3
