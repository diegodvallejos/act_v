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

#Crear grafico de lineas

#Cambio los tickers manualmente

ggplot(stock_data, aes(x=date))+
  geom_line(aes(y=AAPL, color = "AAPL"))+ 
  geom_line(aes(y=IBM, color = "IBM"))+ 
  scale_color_manual(values = c("AAPL" = "blue", "IBM"="red"))+
  labs(title = "Precios de cierre de acciones",
       x = "fecha",
       y= "Precio de cierre",
       color = "Acciones")+ 
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")+
  theme(axis.text.x = element_text(angle = 90))

#Rendimientos
R_AAPL=log(stock_data$AAPL[2:123]/stock_data$AAPL[1:122])
sigma_AAPL = sd(R_AAPL)*sqrt(252)
w0_AAPL= 100 * stock_data$AAPL[123]#supongo que tengo 100 acciones, 123 depende del numero filas de stock_data

#calculo VaR con volatilidad diaria
VaR=sd(R_AAPL)*sqrt(5)*w0_AAPL #sqrt(5) es porque en este caso tomamos de horizonte 5 dias

#calculo VaR con volatilidad anualizada
VaR=sigma_AAPL*sqrt(5/252)*w0

R_IBM=log(stock_data$IBM[2:123]/stock_data$IBM[1:122])
sigma_IBM = sd(R_IBM)*sqrt(252)

w0_IBM= 100 * stock_data$IBM[123]#supongo que tengo 100 acciones, 123 depende del numero filas de stock_data

#calculo VaR con volatilidad diaria
VaR=sd(R_IBM)*sqrt(5)*w0_IBM #sqrt(5) es porque en este caso tomamos de horizonte 5 dias

#calculo VaR con volatilidad anualizada
VaR=sigma_IBM*sqrt(5/252)*w0_IBM

#Para cartera puedo usar 

cor(R_AAPL,R_IBM)

