library(tidyverse)

rm(list=ls())

Balance <- tibble(
  Concepto = c("Activo", "Pasivo", "Capital"),
  Valor_mercado = c(1000, 900, 100),
  Contable = c(1000, 900, 100)
)
Balance

# Escenario Inicial -----------------------------------------------------------
  #Prestamos a Tasa Fija por 5 años, 10%
  t = 1:5
  tasa_A <- 0.10
  CF_Activo <- Balance$Contable[1] * c( rep(tasa_A, 4), (1 + tasa_A))
  CF_Activo
  FD_A <- (1 + tasa_A)^-t
  VA_A <- CF_Activo * FD_A
  sum(VA_A)
  
  #Plazo Fijo Anual Repactable, tasa 9%
  tasa_P <- 0.09
  CF_Pasivo <- Balance$Contable[2] * c( rep(tasa_P, 4), (1 + tasa_P))
  CF_Pasivo
  FD_P <- (1 + tasa_P)^-t
  VA_P <- CF_Pasivo * FD_P
  sum(VA_P)
  
  
  #Capital
  CF_Capital <- CF_Activo - CF_Pasivo
  CF_Pasivo
  VA_C <- VA_A - VA_P
  sum(VA_C)
  
#Escenario suba 200bp-----------------------------------------------------------
  #Activo
  tasa_A_n <- 0.12
  CF_Activo_n <- Balance$Contable[1] * c( rep(tasa_A, 4), (1 + tasa_A))
  CF_Activo_n
  FD_A_n <- (1 + tasa_A_n)^-t
  VA_A_n <- CF_Activo_n * FD_A_n
  sum(VA_A_n)
  
  #Pasivo
  tasa_P_n <- 0.11
  CF_Pasivo_n <- Balance$Contable[2] * c( tasa_P, rep(tasa_P_n, 3), (1 + tasa_P_n))
  CF_Pasivo_n
  FD_P_n <- (1 + tasa_P_n)^-t
  VA_P_n <- CF_Pasivo_n * FD_P_n
  sum(VA_P_n)
  
  #Capital
  CF_Capital_n <- CF_Activo_n - CF_Pasivo_n
  CF_Capital_n
  VA_C_n <- VA_A_n - VA_P_n
  sum(VA_C_n)
  
#Una suba del 2% en las tasas de interes de activos y pasivos tiene un impacto en el
#valor economico de 55.9 (El capital pasa de 100 a 44.1)
  
  
  
  
  
  
  
  
  
  
  
  
  
  