# Pregunta 1

# Conversión de unidades
min = 60 # seg
hora = 60*min # seg
dia = 24*hora # seg

# Cálculo de días enteros y horas sobrantes en 250 millones de segundos
diasEnt = 250e6%/%dia # 2893 días
horasRest = 250e6%%dia # 44800 horas

# Conversión horas sobrantes a días enteros
horasRestDias = horasRest%/%24 # 1866 días
horasRestDiasRest = horasRest%%24 # 16 horas

# Conversión horas restantes a días

Conv = 16/24

Dias = diasEnt + horasRestDias + Conv

# Cuántos años han pasado desde el 2018

AñosPasados = Dias%/%365
DiasPasadosRestantes = Dias%%365

# 14 enero a las 4 pm
# Pregunta 2 

# Ax + B = C
fun = function(A,B,C){(C-B)/A}
fun(2,4,0)  # -2
fun(5,3,0)  # -0.6
fun(7,4,18) # 2
fun(1,1,1)  # 0

# Pregunta 3
round(3*exp(1)-pi,3) # 5.013

# Pregunta 4
round(Mod(((2+3i)^2)/(5+8i)),3) # 1.378