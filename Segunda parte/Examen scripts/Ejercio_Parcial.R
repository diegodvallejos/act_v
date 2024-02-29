library(tidyverse)
rm(list())
Capital <- 2000

#Activos
tasa_fija_activo <- 0.19
ActivoTF <- 6000
ActivoTV <- 5000
Activo<- ActivoTF + ActivoTV

#Pasivos
tasa_fija_pasivo <- 0.16
PasivoTV <- 3500 #plazo fijo
PasivoTF <- 5500 #prestamo
Pasivo <- PasivoTV + PasivoTF

tasa_referencia <- 0.15
spread_activo <- 0.04
spread_pasivo <- 0.01
confianza <- 0.99
volatilidad_tasas <- 0.015

#a) EaR <- valor absoluto(Gap_TV * variacion_ tasas) ---------------------

Zc <- qnorm(confianza)

Gap_TV <- ActivoTV - PasivoTV
variacion_tasas <- Zc*volatilidad_tasas

EaR <- Gap_TV*variacion_tasas
EaR


#b) EVaR(aprox) <- Duration_Gap * variacion_tasas -------------------
t <- 1:3
#Activo
CF_ActivoTF <- c(ActivoTF * tasa_fija_activo + ActivoTF/3,
                 ActivoTF/3*2 * tasa_fija_activo + ActivoTF/3,
                 ActivoTF/3 * tasa_fija_activo + ActivoTF/3)
FD_ActivoTF <- (1 + tasa_fija_activo)^-t
VA_ActivoTF <- CF_ActivoTF * FD_ActivoTF

Duration_ActivoTF <- sum(t*(VA_ActivoTF)/ActivoTF)
Duration_ActivoTV <- 1
Duration_Activo <- Duration_ActivoTF + Duration_ActivoTV

#Pasivo
CF_PasivoTF <- PasivoTF * c(rep(tasa_fija_pasivo,2),(1+tasa_fija_pasivo))
FD_PasivoTF <- (1 + tasa_fija_pasivo)^-t
VA_PasivoTF <- CF_PasivoTF * FD_PasivoTF

Duration_PasivoTF <- sum(t*(VA_PasivoTF)/PasivoTF)
Duration_PasivoTV <- 1
Duration_Pasivo <- Duration_PasivoTF + Duration_PasivoTV

#Duration_Gap
tasa_variable_activo <- tasa_referencia + spread_activo
tasa_variable_pasivo <- tasa_referencia + spread_pasivo

Duration_ActivoTF_mod <- Duration_ActivoTF / (1+tasa_fija_activo)
Duration_ActivoTV_mod <- Duration_ActivoTV / (1+tasa_variable_activo)
Duration_PasivoTF_mod <- Duration_PasivoTF / (1+tasa_fija_pasivo)
Duration_PasivoTV_mod <- Duration_PasivoTV / (1+tasa_variable_pasivo)


Duration_Gap <- -Duration_ActivoTF_mod*ActivoTF - Duration_ActivoTV_mod*ActivoTV + Duration_PasivoTF_mod*PasivoTF + Duration_PasivoTV_mod*PasivoTV

EVaR_aproximado <- Duration_Gap * variacion_tasas

#c) ---------------
#Como el GAP tv es > 0, para eliminar el riesgo de la tasa tengo que salir a 
#fondear por los 1500 a tasa variable a la tasa vigente en ese momento
#(PasivoTV)

#d) Gap_Liquidez <- Activo - Pasivo ------------------
Cuota <- 2000
Tabla <- tibble(A=c(Activo, Activo-Cuota, Activo-2*Cuota, 0),
                P=c(Pasivo, Pasivo, Pasivo, 0),
                PN=c(rep(Capital,4)),
                Gap_Liquidez=c(A-P-PN),
                Gap_Liquidez_marg=c(0, diff(Gap_Liquidez)))
#En los momentos 1,2 y 3 es necesario fondear