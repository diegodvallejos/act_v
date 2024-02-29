library(tidyverse)
rm(list = ls())

#Ejemplo Gap de Liquidez -----------------
Activos <- c(1000,900,700,650,500,300)
A_Amortizacion <- c(0, -diff(Activos))

Pasivos <- c(1000,800,500,400,350,100)
P_Amortiazcion <- c(0, -diff(Pasivos))

Gap <- Activos - Pasivos
GapMarginal <- c(0, diff(Gap))
GapMarginal == (A_Amortizacion - P_Amortiazcion)


  #Gestion Gap de Liquidez: Ejemplo 1 ------------
rm(list=ls())
Gap_Liq <- tibble(Activos= c(1000,1000,750,500),
                  A_Amortizacion= c(0, -diff(Activos)),
                  Pasivos= c(1000,800,650,450),
                  P_Amortizacion= c(0, -diff(Pasivos)),
                  Gap= Activos - Pasivos,
                  GapMArginal= c(0, diff(Gap)))
Gap_Liq
    #Como fondeo el Gap?
    #Pasivo 1: 33 x 3 periodos
    Gap_Liq <- Gap_Liq %>%
    mutate(P1 = c(0, 50, 50, 50),
          Gap_P1 = Gap - P1)
    Gap_Liq

    #Pasivo2: 50 x 2 periodos
    Gap_Liq <- Gap_Liq %>%
      mutate(P2 = c(0,50,50,0),
             Gap_P2= Gap_P1 - P2)
    Gap_Liq
    
    #Pasivo3: 100 x 1 periodo
    Gap_Liq <- Gap_Liq %>%
      mutate(P3 = c(0,100,0,0),
             Gap_P3 = Gap_P2 - P3)
    Gap_Liq
    
#Gestion Gap de Liquidez: Ejemplo 2 ----------------------
    rm(list=ls())
    Gap_Liq <- tibble(Activos= c(1000,750,500),
                      A_Amortizacion= c(0, -diff(Activos)),
                      Pasivos= c(800,400,400),
                      P_Amortizacion= c(0,-diff(Pasivos)),
                      Gap= Activos - Pasivos)
    Gap_Liq
    
    #Como fondeo el Gap?
    #Pasivo 1: 100 x 3 periodos
    Gap_Liq <- Gap_Liq %>%
      mutate(P1 = c(100,100,100),
             Gap_P1 = Gap - P1)
    Gap_Liq
    
    #Pasivo 2: 100 x 2 periodos
    Gap_Liq <- Gap_Liq %>%
      mutate(P2 = c(100,100,0),
             Gap_P2 = Gap_P1 - P2)
    Gap_Liq
    
    #Pasivo 3 : 150 x 1 periodo
    Gap_Liq <- Gap_Liq %>%
      mutate(P3 = c(0, 150, 0),
             Gap_P3 = Gap_P2 - P3)
    
    Gap_Liq