#Diego Demian Vallejos
#Numero de registro : 899536

library(tidyverse)

ATF=700
ATV=300

PTF=800
PTV=200

A_spread=0.02
P_spread=-0.01


Tabla <- tibble(concepto <- c("Activos tasa fija", "Activo tasa variable", "Activo total", "pasivo tasa fija", "pasivo tasa variable", "Pasivo total"),
                saldos <- c(700, 300, 1000, 800, 200, 1000),
                margen <- c(0.02, 0.02, NA, -0.01, -0.01, NA))
Tabla

#Gap de tasas

Gap_TI = ATV - PTV
Gap_TI

calculo_dif_margenes <- function(ATV, PTV, dTI){
  "Calcula diferencia de margenes.
  
  Parametros
  ----------
  ATV : int
    Activo a tasa variable
  PTV : int
    Pasivo a tasa variable
  dTI : float
    Volatilidad de tasas o diferencia de tasas
  
  Return
  ------
  calculo : float
    Diferencia de margenes entre ambos escenarios de calculo de GAP de liquidez"
  calculo_intermedio = ATV - PTV
  calculo = calculo_intermedio*dTI
  
  return(calculo)
}

Caso_caida_2_por_ciento <- calculo_dif_margenes(ATV, PTV, -0.02)
Caso_caida_2_por_ciento

print("El gap era positivo, asi que nos perjudicó una baja de la tasa")
#b) ----------------------------------------------------------------------------
#utilizo escenario inicial
tasa_referencia = 0.12

I_ATF = ATF*(tasa_referencia + A_spread)
I_ATV = ATV*(tasa_referencia + A_spread)

I_A = I_ATF + I_ATV

I_PTF=PTF*(tasa_referencia + P_spread)
I_PTV=PTV*(tasa_referencia + P_spread)

I_P = I_PTF + I_PTV

(margen=I_A - I_P)


#c) ----------------------------------------------------------------------------
dTI=0.02 #tasa de referencia nueva 11%

tasa_referencia_n = tasa_referencia + dTI
#solo actualizo tasa variable

I_ATV_n = ATV*(tasa_referencia_n + A_spread)

I_A_n = I_ATF + I_ATV_n

I_PTV_n = PTV*(tasa_referencia_n + P_spread)

I_P_n = I_PTF + I_PTV_n


margen_n = I_A_n - I_P_n

dMargen= margen_n - margen
dMargen

print("Vemos que ahora la diferencia de margen es positiva por el mismo valor que antes pero con signo opuesto. Esto es porque si nuestro gap es positivo, me beneficia una suba de tasa y si es negativo al contrario")







