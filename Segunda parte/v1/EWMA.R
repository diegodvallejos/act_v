#Cargar paquetes
library(tidyverse)
library(quantmod)

rm(list = ls())
graphics.off()

#EWMA (Exponentially Weighted Moving Average): la volatilidad para cada uno de los períodos
#de tiempo es ponderada, teniendo mayor peso las últimas observaciones

#Obtener datos de precios de acciones utilizando el simbolo de ticker de Yahoo Finance
ticker1="AAPL"
ticker2="IBM"
desde="2023-01-01"
hasta="2023-06-30"
getSymbols(c(ticker1,ticker2), from = desde , to= hasta)

#Crear un conjunto de datos con los precios de cierre
sum(!(index(AAPL)==index(IBM))) #Chequeo que las fechas coincidan
stock_data = tibble(date=index(AAPL),
                    AAPL=as.double(AAPL$AAPL.Close),
                    IBM=as.double(IBM$IBM.Close))
#Rendimientos
R_AAPL=log(stock_data$AAPL[2:nrow(stock_data)]/stock_data$AAPL[1:(nrow(stock_data)-1)])
R_IBM=log(stock_data$IBM[2:nrow(stock_data)]/stock_data$IBM[1:(nrow(stock_data)-1)])

calcular_ewma <- function(retornos, lambda = 0.94) {
  r_promedio <- 0
  sumatoria <- 0
  
  for (i in length(retornos):1) {
    sumatoria <- sumatoria + lambda^(length(retornos) - i) * (retornos[i] - r_promedio)^2
  }
  
  return((sqrt((1 - lambda) * sumatoria))*sqrt(252)) #anualizada
}

# Ejemplo de uso con los retornos de AAPL
(sigma_AAPL_EWMA = calcular_ewma(R_AAPL))

(sigma_IBM_EWMA=calcular_ewma(R_IBM))
