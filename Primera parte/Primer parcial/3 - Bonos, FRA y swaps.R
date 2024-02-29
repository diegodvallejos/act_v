# Bonos, FRA y swaps-----------------------------------------------------------
library(tidyverse)
Tabla_ej_3 <- tibble(mes=seq(1, 12),
                     TNA=c(0.0405, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.0650, 0.0675, 0.07))


#Inciso a)---- # Solo modificar los parametros de arriba

Interes_cupon <- 0.05 # <- TNA con capitalizacion mensual
Frecuencia_capitalizacion <- 1/12
Valor_nominal <- 100
#pagos menuales
#Vencimiento a un año

#Sabemos que el precio del bono es flujo de fondos por factor de descuento


Tabla_ej_3$FD <- (1 + Tabla_ej_3$TNA * Tabla_ej_3$mes * Frecuencia_capitalizacion) ^ -1

Tabla_ej_3$FF <- c(rep(Interes_cupon * Frecuencia_capitalizacion * Valor_nominal, length(Tabla_ej_3$mes) - 1 ), (Interes_cupon * Frecuencia_capitalizacion * Valor_nominal) + Valor_nominal)

Tabla_ej_3$Precio <- Tabla_ej_3$FD * Tabla_ej_3$FF

Precio_bono <- sum(Tabla_ej_3$Precio)


# Inciso b) ----   #Solo cambiar parametros de arriba y La tasa LIBOR+bps

prestamo <- 60000000

# i)
mes_pago <- 8   #Porque se paga al mes 8
mes_comienzo <- 2  #Por ser pagos bimestrales
Tasa_forward <- (Tabla_ej_3$FD[mes_comienzo] / Tabla_ej_3$FD[mes_pago+mes_comienzo] - 1) * ((mes_pago)^(-1))*(Frecuencia_capitalizacion)^(-1)


#ii)

demostacion_jua <- Tasa_forward + 0.0125 # <- XD

#Opero para obtener el costo efectivo del prestamo

# Momento t = 2/12

#Defino LIbor + bps

LIBOR <- 0.0925
bps <- 0.0105

liquidacion_FRA <- (LIBOR - Tasa_forward) * prestamo * mes_pago * Frecuencia_capitalizacion * (1 + LIBOR* mes_pago * Frecuencia_capitalizacion)^(-1)

#Entonces pido un prestamo por menos del valor original

Prestamo_punto_3 <- prestamo - liquidacion_FRA

#Ahora vamos al momento t = 10/12

pago_prestamo_punto_3 <- Prestamo_punto_3 * (1+ (LIBOR+bps)*mes_pago*Frecuencia_capitalizacion)

#Entonces el costo financiero seria

Costo_financiero <- ((pago_prestamo_punto_3 / prestamo) - 1) * ((mes_pago)^(-1)) * (Frecuencia_capitalizacion)^(-1)

# Inciso c) ----


plazo_liquidacion <- 3   #Por ser trimestral

suma <- 0
for (i in seq(plazo_liquidacion, nrow(Tabla_ej_3), by=plazo_liquidacion)){
  suma <- suma + Tabla_ej_3$FD[i]
}

Tasa_swap <- ((1 - Tabla_ej_3$FD[nrow(Tabla_ej_3)]) / suma) * ((Frecuencia_capitalizacion)^(-1)) * (plazo_liquidacion)^(-1)
