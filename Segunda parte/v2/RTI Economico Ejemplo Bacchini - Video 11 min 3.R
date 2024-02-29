library(tidyverse)

#Diapositivas 28 y 29 RTI

Balance <- tibble(
  Concepto = c("Activo", "Pasivo", "Capital"),
  Valor_Mercado = c(1000,900,100),
  Contable = c(1000,900,100)
)

#valor de mercado y contable coinciden en el momento inicial. 

#Escenario Inicial---------
  #Activo:Prestamo a Tasa Fija por 5 años SISTEMA DE AMORT FRANCES
t = 1:5
  Tasa_A <- 0.1
  CF_Activo <- Balance$Contable[1] * c(rep(Tasa_A,4), (1+Tasa_A))#en el ultimo perido se repaga el capital + intereses (los primero 4 periodos solo intereses)
  FD_A <- (1 + Tasa_A) ^-t #lo necesito para calcular el valor actual
  VA_A <- CF_Activo * FD_A
  sum(VA_A) #chequeo

  #Pasivo:Plazo Fijo Anual Repactable, tasa = 9%
  tasa_P <- 0.09
  CF_Pasivo <- Balance$Contable[2] * c(rep(tasa_P,4), (1+tasa_P))
  FD_P <- (1+ tasa_P)^-t
  VA_P <- CF_Pasivo * FD_P
  sum(VA_P)

  #Capital
  CF_Capital <- CF_Activo - CF_Pasivo
  VA_C <- VA_A - VA_P
  sum(VA_C)

  
#Escenario suba de 200 bps (basic points) o 2%-----------
  #Activo
  tasa_A_n <- 0.12 #_n = nuevo (2%+ que el inicial)
  
#opcion caro 
  bps=0.02
  tasa_A_nn <- Tasa_A+bps
  
  CF_Activo_n <- Balance$Contable[1] * c(rep(Tasa_A,4), (1+Tasa_A)) #NO SE MODIFICA SON ACTIVOS A TASA FIJA. SE CALCULA CON LA TASA ORIGINAL
  FD_A_n <- (1+tasa_A_n)^-t #ESTO SI SE MODIFICA
  VA_A_n <- CF_Activo_n * FD_A_n
  sum(VA_A_n) #subio 2% la tasa de interes y bajo el valor del activo 
  
  #Pasivo
  tasa_P_n <- 0.11
  CF_Pasivo_n <- Balance$Contable[2] * c( tasa_P, rep(tasa_P_n,3), (1+tasa_P_n)) #SE MODIFICA (a partir del segundo periodo, porque para el primer periodo ya habia pactado una tasa del 9%), EL PASIVO ES A TASA VARIABLE REPACTABLE 
  FD_P_n <- (1+tasa_P_n)^-t
  VA_P_n <- CF_Pasivo_n * FD_P_n
  sum(VA_P_n) #el valor cayo menos porque los flujos siguientes en el caso pasivo se actualizan a la nueva tasa entonces el valor no cae tanto
  
  #Capital
  CF_Capital_n <- CF_Activo_n - CF_Pasivo_n
  VA_C_n <- VA_A_n - VA_P_n
  sum(VA_C_n)
  
# Una suba del 2% en las tasas de interes de activos y pasivos tiene un impacto en el 
# Valor Economico de 55.9 (El capital pasa de 100 a 44.1)
# El valor contable no se modifica
# El activo cae mucho mas que el pasivo, ya que el pasivo es a tasa variable y al repactar 
# la tasa, el valor economico no se sufre tanto ante movimientos en la tasa de interes.
# Mientras eñ activo al ser a tasa fija por 5 años, el movimiento en la tasa de interes
# tiene un fuerte impacto en su valor economico.
  
  
