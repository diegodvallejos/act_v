library(tidyverse)

ATF=700
ATV=300

PTF=800
PTV=200

A_spread=0.02
P_spread=-0.04
tasa_referencia = 0.1

Tabla <- tibble(concepto <- c("Activos tasa fija", "Activo tasa variable", "Activo total", "pasivo tasa fija", "pasivo tasa variable", "Pasivo total"),
                 saldos <- c(700, 300, 1000, 800, 200, 1000),
                 margen <- c(0.02, 0.02, NA, -0.04, -0.04, NA))
Tabla
#intereses cobrados

I_ATF = ATF*(tasa_referencia + A_spread)
I_ATV = ATV*(tasa_referencia + A_spread)

I_A = I_ATF + I_ATV

I_PTF=PTF*(tasa_referencia + P_spread)
I_PTV=PTV*(tasa_referencia + P_spread)

I_P = I_PTF + I_PTV

(margen=I_A - I_P)

#sube tasa 1%
dTI=0.01 #tasa de referencia nueva 11%

tasa_referencia_n = tasa_referencia + dTI
#solo actualizo tasa variable

I_ATV_n = ATV*(tasa_referencia_n + A_spread)

I_A_n = I_ATF + I_ATV_n

I_PTV_n = PTV*(tasa_referencia_n + P_spread)

I_P_n = I_PTF + I_PTV_n


margen_n = I_A_n - I_P_n

dMargen= margen_n - margen

#Gap de tasa

Gap_TI = ATV - PTV

dmargen_v2 = Gap_TI * dTI


#si el gap de tasa de interes es positivo, una suba de tasa beneficia a la entidad, si es negativo al contrario