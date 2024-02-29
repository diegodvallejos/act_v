library(tidyverse)
library(quantmod)

rm(list = ls())
graphics.off()

#Obtener datos de precios de acciones utilizazndo el simbolo de ticker de Yahoo Finance
ticker1="AAPL"
ticker2="IBM"
desde="2023-01-01"
hasta="2023-06-30"
getSymbols(c(ticker1,ticker2), from = desde , to= hasta)


#Crear un conjunto de datos con los precois de cierre
#Los datos los cambio manualmente
sum(!(index(AAPL)==index(IBM))) #Chequeo que las fechas coincidan
stock_data = tibble(date=index(AAPL),
                    AAPL=as.double(AAPL$AAPL.Close),
                    IBM=as.double(IBM$IBM.Close))
#Rendimientos
R_AAPL_E=log(stock_data$AAPL[2:nrow(stock_data)]/stock_data$AAPL[1:(nrow(stock_data)-1)])
R_IBM_E=log(stock_data$IBM[2:nrow(stock_data)]/stock_data$IBM[1:(nrow(stock_data)-1)])

lambda=0.94#Dato
r=R_AAPL_E#retornos
r_promedio=0#retorno promedio, en corto plazo lo asumo =0 
sumatoria=0
for(i in length(r):1){
  sumatoria= sumatoria + lambda^(length(r)-i) * (r[i]-r_promedio)^2
}
sigma_AAPL_EWMA=sqrt((1-lambda)* sumatoria) 


