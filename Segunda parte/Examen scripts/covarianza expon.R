#formula covarianza

library(tidyverse)
library(quantmod)

rm(list = ls())
graphics.off()

#Obtener datos de precios de accioens utilizando el simbolo de ticker Yahoo finance

getSymbols(c("AAPL", "IBM"), from = "2023-01-01", to = "2023-06-30")

#Crear un conjunto de datos con los precios de cierre
sum(!(index(AAPL) == index(IBM))) #Chequeo que las fechas coincidan

stock_data = tibble(date = index(AAPL),
                    AAPL = as.double(AAPL$AAPL.Close),
                    IBM = as.double(IBM$IBM.Close))

#Rendimientos -----------------------------------------------------------------

R_AAPL = log(stock_data$AAPL[2:nrow(stock_data)] / stock_data$AAPL[2:nrow(stock_data)-1])
sigma_AAPL = sd(R_AAPL) * sqrt(252)

R_IBM = log(stock_data$IBM[2:nrow(stock_data)] / stock_data$IBM[2:nrow(stock_data)-1])
sigma_IBM = sd(R_IBM) * sqrt(252)

stock_data <- stock_data %>%
  mutate(R_AAPL = c(NA, R_AAPL), 
         R_IBM = c(NA, R_IBM))

Rho = cor(R_AAPL, R_IBM)
Rho_matrix = matrix(c(1, Rho,
                      Rho, 1), nrow=2)

varcov = matrix(c(sigma_AAPL^2, sigma_AAPL * sigma_IBM * Rho,
                  sigma_AAPL * sigma_IBM * Rho, sigma_IBM^2), nrow=2)

#covarianza ponderada para 2 acciones

covarianza_exponencial <- function(lambda = 0.94, data1, data2) {
  first_step <- 0
  for (j in 1:(length(data1)-1)) {
    first_step <- first_step + lambda^(j-1) * data1[length(data1)-j+1] * data2[length(data2)-j+1]
  }
  cov_exp <- 252 * (1 - lambda) / (1 - lambda^(length(data1)-1)) * first_step
  return(cov_exp)
}

# Calcular rendimientos
R_AAPL <- log(stock_data$AAPL[2:nrow(stock_data)] / stock_data$AAPL[1:(nrow(stock_data)-1)])
R_IBM <- log(stock_data$IBM[2:nrow(stock_data)] / stock_data$IBM[1:(nrow(stock_data)-1)])

# Calcular covarianza exponencial
covarianza_exponencial(0.94, R_AAPL, R_IBM)



