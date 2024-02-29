#CLASE RIESGO MERCADO

library(tidyverse)
library(quantmod)

rm(list = ls())
graphics.off()

#Obtener datos de precios de acciones utilizando el simbolo de ticker de Yahho Finance
  getSymbols(c('AAPL', 'IBM'), from = "2023-01-01", to = '2023-06-30')
  
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

  #Crear el grafico de lineas de correlacion---------------------------------------------------
  ggplot(stock_data, aes(x = date)) + 
    geom_line(aes(y = AAPL, color = "AAPL")) +
    geom_line(aes(y = IBM, color = "IBM")) +
    scale_color_manual(values = c("AAPL" = "blue", "IBM" = "red")) +
    labs(title = "Precios de cierre de acciones",
         x = "Fecha",
         y = "Precio de cierre",
         color = "Acciones") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b - %y") +
    theme(axis.text.x = element_text(angle = 90))
  
#Crear el grafico de lineas rendimiento---------------------------------------------------
  ggplot(stock_data, aes(x = date)) + 
    (geom_line(aes(y = R_AAPL, color = "AAPL"))) +
    (geom_line(aes(y = R_IBM, color = "IBM"))) +
    scale_color_manual(values = c("AAPL" = "lightblue", "IBM" = "pink")) +
    labs(title = "Rendimiento diario",
         x = "Fecha",
         y = "Precio de cierre",
         color = "Acciones") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b - %y") +
    theme(axis.text.x = element_text(angle = 90))
  
  
#Analisis de Var: Cartera con 70 acciones AAPL y 100 de IBM--------------------
  c <- 0.99 #Nivel de confianza
  z <- qnorm(c) #Percentil de la distribucion normal
  h <- 5/252 #5 dias, 252 dias habiles --> años
## Valuacion -------------------------------------------------------------
    (W_A = 70 * stock_data$AAPL[nrow(stock_data)]) #Valor de la posicion en apple. nrow --> ultima fila
    (W_I = 100 * stock_data$IBM[nrow(stock_data)]) #Valor de la posicion en IBM
    (W = W_A + W_I) #Total de mi posicion
    (w <- c(W_A, W_I)/W) #Proporcion invertida 
    (sigma_P <- sqrt(t(w)%*% varcov %*% w) %>% as.double()) #Desvio estandar de mi portafolio - Multiplicacion de matrices
    (sqrt(w[1]^2*sigma_AAPL^2 + w[2]^2*sigma_IBM^2 + 2*w[1]*w[2]*sigma_AAPL*sigma_IBM*Rho)) #Desvio estandar de mi portafolio - Formula

  ## VaR No Paramétrico -----------------------------------------------------
  #Ultimos 6 meses --> decidir tiempo a tomar
  
  stock_data  <- stock_data %>%
   mutate( P =  w[1]*stock_data$AAPL + w[2]*stock_data$IBM) #P hace ref al portafolio
  R_P = log(stock_data$P[2:nrow(stock_data)]/stock_data$P[1 : (nrow(stock_data) -1)])   #R_P rendimiento del portafolio (desde el segundo hasta el ultimo / el primero hasta el anteultimo)
  
  plot(R_P, type = "l")
  
  (quantile(R_P, 1-c))    #Mis retornos diarios tienen una prob de 1% de tener una perdida mayor al 1,89%
  
  (VarABS_NoParam_1d <- -quantile(R_P, 1-c)*W) #VaR estimado de mi portafolio a un dia. Si quiero horizonte mas largo por ej. 5 dias
  (VarRel_NoParam_1d <- (mean(R_P)-quantile(R_P, 1-c))*W)
  
  ## VaR Normal: Individual y Cartera ---------------------------------------
  (VaR_A_Normal = z*sigma_AAPL*sqrt(h)*W_A) #VaR si solo tuviera mis 70 acciones de apple
  (z*sd(R_AAPL)*sqrt(5)*W_A)
  VaR_A_Normal/W_A #Son un 6.7% de la cartera. Hay un 1% de probabiidad de que la perdida en un horizonte de 5 dias, sea superior a 892 dolares. 
  
  (VaR_I_Normal = z*sigma_IBM*sqrt(h)*W_I)
  (z*sd(R_IBM)*sqrt(5)*W_I)
  VaR_I_Normal/W_I #Si solo tuviera iBM en el portafolio, en un horizonte de 5 dias, hay un 1% de probabilidad de que la perdida sea mayor a 766 dolares. Representa un 5.7% del portafolio
  
  (VaR_P_Normal_NoDiv <- VaR_A_Normal + VaR_I_Normal) #Suponiendo correlacion de 1 --> no hay beneficios de diversificacion
  VaR_P_Normal_NoDiv/(W) #6.2% del portafolio, mitad de camino de los W_A y W_I
  
  (VaR_P_Normal <- z*sigma_P*sqrt(h)*W)
  VaR_P_Normal/W #Menor que si no hay benef por diversif
  (Benef_Diver <- VaR_P_Normal_NoDiv - VaR_P_Normal)
  
  #VaR Marginal---------------------------------------------------------
  (Betas <- (varcov %*% w) / sigma_P^2)
  sum(t(w) %*% Betas) #chequeo
  (VaR_Marginal <- VaR_P_Normal/W*Betas)
  
  #Component VaR (caretera original) ---------------------------------------------------------
  (VaR_Component <- VaR_P_Normal*w*Betas)
  sum(VaR_Component) - VaR_P_Normal #Chequeo
  VaR_Component / VaR_P_Normal
  
  
  