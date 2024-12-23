library(tidyverse)

rm(list = ls())
graphics.off()

# Ejemplo de las diapositivas 28 y 29 de riesgo de tasa de interes
Balance <- tibble(
  Concepto = c("Activo", "Pasivo", "Capital"),
  valor_mercado = c(1000, 900, 100),
  Contable = c(1000, 900, 100)
)

Balance

#Escenario inicial -------------------------------------------------------------
 #prestamos a tasa Fija por 5 años, 10%
t = 1:5
tasa_A = 0.1
CF_Activo = Balance$Contable[1] * c(rep(tasa_A, 4), (1+tasa_A))
CF_Activo

FD_A <- (1+tasa_A)^-t
VA_A <- CF_Activo*FD_A
sum(VA_A)

#Plazo Fijo anual Repactable, tasa 9%
tasa_P <- 0.09
CF_Pasivo <- Balance$Contable[2] * c(rep(tasa_P, 4), (1+tasa_P))
CF_Pasivo

FD_P <- (1 + tasa_P)^-t
VA_P <- CF_Pasivo * FD_P
sum(VA_P)

#Capital
CF_Capital <- CF_Activo - CF_Pasivo
CF_Capital
VA_C <- VA_A - VA_P
sum(VA_C)

#Escenario suba 200bps ---------------------------------------------------------
 #Activo
tasa_A_n <- 0.12
CF_Activo_n <-Balance$Contable[1] * c(rep(tasa_A, 4), (1 + tasa_A)) #OJO, no cambia
CF_Activo_n

FD_A_n <- (1 + tasa_A_n)^-t
VA_A_n <- CF_Activo_n * FD_A_n
sum(VA_A_n)

#Pasivo
tasa_P_n <- 0.11
CF_Pasivo_n <- Balance$Contable[2] * c(rep(tasa_P_n, 4), (1 + tasa_P_n))
CF_Pasivo_n

FD_P_n <- (1 + tasa_P_n)^-t
VA_P_n <- CF_Pasivo_n * FD_P_n
sum(VA_P_n)

#Capital
CF_Capital_n <- CF_Activo_n - CF_Pasivo_n
CF_Capital_n
VA_C_n <- VA_A_n - VA_P_n
sum(VA_C_n)

#Una suba de 2% en las tasas de interes de activos y pasivos tiene un impacto en
#valor economico de 55.9 (el capital pasa de 100 a 44.1)














