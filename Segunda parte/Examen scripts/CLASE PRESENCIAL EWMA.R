
library(tidyverse)
library(quantmod)

rm(list = ls())
graphics.off()

#Obtener datos de precios de acciones utilizando el simbolo de ticker de Yahho Finance
getSymbols(c('AAPL', 'IBM'), from = "2023-01-01", to = '2023-04-01')

#Crear un conjunto de datos con los precios de cierre
sum(!(index(AAPL) == index(IBM))) #Chequeo que las fechas coincidan. Si me da 0 es que todas las fechas son iguales
stock_data = tibble(date = index(AAPL),
                    AAPL = as.double (AAPL$AAPL.Close), #trabajo con los precios de cierre, pero puedo trabajar con cualquiera
                    IBM = as.double(IBM$IBM.Close)) #as.double me da solo el numero

#Rendimientos --------------------------------------------------------------
R_AAPL = log(stock_data$AAPL[2:nrow(stock_data)]/stock_data$AAPL[1:nrow(stock_data) - 1]) #Retornos
(sigma_AAPL = sd(R_AAPL)*sqrt(252)) #El año tiene aproximadamente 252 dias habiles. Como aca estoy calculando los retornos diarios, los anualizo multiplicando por la raiz de 252
R_IBM = log(stock_data$IBM[2:nrow(stock_data)]/stock_data$IBM[1:nrow(stock_data) - 1])
(sigma_IBM = sd(R_IBM)*sqrt(252))
stock_data <- stock_data%>%
  mutate(R_AAPL = c(NA, R_AAPL), #NA pq Apple tiene solo 122 valores de retorno contra 123 precios
         R_IBM = c(NA, R_IBM)) #Vector de retorno
(Rho = cor(R_AAPL, R_IBM)) #Correlacion
(Rho_matrix = matrix(c(1, Rho,
                       Rho, 1), nrow = 2))
(varcov = matrix(c(sigma_AAPL^2, sigma_AAPL*sigma_IBM*Rho, 
                   sigma_AAPL*sigma_IBM*Rho, sigma_IBM^2), nrow = 2))
sig <- c(sigma_AAPL, sigma_IBM)

 #EWMA (como esta)----------------
lambda = 0.94
n = (nrow(stock_data)-1)

  #AAPL
  aux = 0
  for(t in 1:(nrow(stock_data)-1)){
    aux <- aux + lambda^(t-1)*stock_data$R_AAPL[nrow(stock_data)-t+1]^2
  };rm(t)

  sigma_AAPL_EWMA <- sqrt(252*(1-lambda)*aux)
  sigma_AAPL_EWMA
  rm(aux)
  
  #IBM
  aux=0
  for(t in 1:(nrow(stock_data)-1)){
    aux <- aux + lambda^(t-1)*stock_data$R_IBM[nrow(stock_data)-t+1]^2
  };rm(t)
  sigma_IBM_EWMA <- sqrt(252*(1-lambda)*aux)
  sigma_IBM_EWMA
  rm(aux)
  
  #Ambas
  sig_EWMA <- c(AAPL=sigma_AAPL_EWMA, IBM=sigma_IBM_EWMA)
  sig_EWMA
  sig
  
  #EWMA Corregida ---------------------
  #Analizo ponderadores
  n <- (nrow(stock_data)-1)
  t <- 1:n
  ponderadores_EWMA <- (1-lambda)*lambda^(t-1)
  plot(ponderadores_EWMA)
  sum(ponderadores_EWMA)

#Como corrijo que la suma sea menor a 1?
#la suma de los lambda^(t-1) es: (1- lambda^n)/(1-lambda). PAra garantizar que
#la suma de los ponderadores sea uno, debo dividir a cada ponderador por esa suma.
#si "n" es lo suficientemente grande, (1-lambda^n) tiende a 1 y por lo tanto
#en la formula aparece (1-lambda) multiplicando. Si "n" no es tan grande, no
#debo despreciar el factor (1-lambda^n)
  
  ponderadores_EWMA_v2 <- (1- lambda)/(1-lambda^n) * lambda^(t-1)
  points(ponderadores_EWMA_v2, col="orange")
  sum(ponderadores_EWMA_v2)
  

  #Recaluclo Desvios
   lambda= 0.94
   n<- (nrow(stock_data)-1)
  #AAPL
   aux=0
   for(t in 1:(nrow(stock_data)-1)){
     aux <- aux + lambda^(t-1)*stock_data$R_AAPL[nrow(stock_data)-t+1]^2
   };rm(t)
   
   sigma_AAPL_EWMA_v2 <- sqrt(252*(1-lambda)/(1-lambda^n)*aux)
   sigma_AAPL_EWMA_v2
   rm(aux)
   #IBM
   aux=0
   for(t in 1:(nrow(stock_data)-1)){
     aux <- aux + lambda^(t-1)*stock_data$R_IBM[nrow(stock_data)-t+1]^2
   };rm(t)
   
   sigma_IBM_EWMA_v2 <- sqrt(252*(1-lambda)/(1-lambda^n)*aux)
   sigma_IBM_EWMA_v2
   rm(aux)
   
   #Resumen
   sig_EWMA_v2 <- c(AAPL=sigma_AAPL_EWMA_v2, IBM=sigma_IBM_EWMA_v2)
   sig_EWMA_v2
   sig_EWMA
   sig
   
   
#Correlaciones y varianzas-covarianzas -------------------
   #Ponderaciones iguales
   (Rho= cor(R_AAPL, R_IBM))
   (Rho_matrix = matrix(c(1,Rho,
                          Rho,1), nrow=2))
   (varcov = sig * t(sig*Rho_matrix)) #cada fila y cada columna las multiplica por la siguiente
   #varcov=matrix(c(sigma_AAPL^2, sigma_AAPL*sigma_IBM*Rho,
   #                sigma_AAPL*sigma_IBM*Rho, sigma_IBM^2), nrow=2)

   #Ponderaciones exponenciales
   aux=0
   for(t in 1:n){
     stock_data$R_AAPL[nrow(stock_data)-t+1]*stock_data$R_IBM[nrow(stock_data)-t+1]
   };rm(t)
   covar_EWMA <- 252*(1-lambda)/(1-lambda^n)*aux
   rm(aux)
   varcov_EWMA <- matrix(c(sig_EWMA_v2["AAPL"]^2, covar_EWMA,
                         covar_EWMA, sig_EWMA_v2["IBM"]^2),nrow=2)
   varcov_EWMA
   varcov
   
   varcov_EWMA[1,2] / (sig_EWMA_v2[1]*sig_EWMA_v2[2])
