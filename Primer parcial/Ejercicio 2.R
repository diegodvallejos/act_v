#Diego Demian Vallejos - num de registro:899536
library(tidyverse)

Interes_cupon <- 0.05 # <- TNA con capitalizacion mensual
Frecuencia_capitalizacion <- 1/12 # <- el codigo fue modificado varias veces durante el examen y esta variable quedo con este nombre poco correcto que no me da tiempo a cambiar
Valor_nominal <- 100
#pagos menuales
#Vencimiento a un año

#Sabemos que el precio del bono es flujo de fondos por factor de descuento

# Inciso a) --------------------------------------------------------------------

Tabla_ej_2 <- tibble(mes=seq(1, 12),
                     TNA=c(0.0405, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.0650, 0.0675, 0.07))



Tabla_ej_2$FD <- (1 + Tabla_ej_2$TNA * Tabla_ej_2$mes * Frecuencia_capitalizacion) ^ -1

Tabla_ej_2$FF <- c(rep(Interes_cupon * Frecuencia_capitalizacion * Valor_nominal, length(Tabla_ej_2$mes) - 1 ), (Interes_cupon * Frecuencia_capitalizacion * Valor_nominal) + Valor_nominal)

Tabla_ej_2$Precio <- Tabla_ej_2$FD * Tabla_ej_2$FF

Precio_bono <- sum(Tabla_ej_2$Precio)

Precio_bono


# Inciso b) --------------------------------------------------------------------

prestamo <- 60000000

# i)
mes_pago <- 8   #Porque se paga al mes 8
mes_comienzo <- 2  #Por ser pagos bimestrales
Tasa_forward <- (Tabla_ej_2$FD[mes_comienzo] / Tabla_ej_2$FD[mes_pago+mes_comienzo] - 1) * ((mes_pago)^(-1))*(Frecuencia_capitalizacion)^(-1)

Tasa_forward

#ii)

#Opero para obtener el costo efectivo del prestamo

# Momento t = 2/12

#Defino LIbor + bps

LIBOR <- 0.0925
bps <- 0.0105

liquidacion_FRA <- (LIBOR - Tasa_forward) * prestamo * mes_pago * Frecuencia_capitalizacion * (1 + LIBOR* mes_pago * Frecuencia_capitalizacion)^(-1)

#Entonces pido un prestamo por menos del valor original

Prestamo_punto_2 <- prestamo - liquidacion_FRA

#Ahora vamos al momento t = 10/12

pago_prestamo_punto_2 <- Prestamo_punto_2 * (1+ (LIBOR+bps)*mes_pago*Frecuencia_capitalizacion)

#Entonces el costo financiero seria

Costo_financiero <- ((pago_prestamo_punto_2 / prestamo) - 1) * ((mes_pago)^(-1)) * (Frecuencia_capitalizacion)^(-1)

# Inciso c) ----


plazo_liquidacion <- 2   #Por ser bimestrales

suma <- 0
for (i in seq(plazo_liquidacion, nrow(Tabla_ej_2), by=plazo_liquidacion)){
  suma <- suma + Tabla_ej_2$FD[i]
}

(Tasa_swap <- ((1 - Tabla_ej_2$FD[nrow(Tabla_ej_2)]) / suma) * ((Frecuencia_capitalizacion)^(-1)) * (plazo_liquidacion)^(-1))
