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


#Crear grafico de lineas

ggplot(stock_data, aes(x=date)) + 
  geom_line(aes(y = AAPL, color = "AAPL")) + 
  geom_line(aes(y = IBM, color = "IBM")) + 
  scale_color_manual(values = c("AAPL" = "blue", "IBM" = "red")) + 
  labs(title = "Precios de cierre de las acciones", 
       x = "Fecha",
       y = "Precio de cierre",
       color = "Acciones") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")
  #theme(axis.test.x = element_text(angle=90))

ggplot(stock_data, aes(x=date)) + 
  geom_line(aes(y = R_AAPL, color = "AAPL")) + 
  geom_line(aes(y = R_IBM, color = "IBM")) + 
  scale_color_manual(values = c("AAPL" = "blue", "IBM" = "red")) + 
  labs(title = "Rendimiento diario", 
       x = "Fecha",
       y = "Precio de cierre",
       color = "Acciones") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")

#Analisis de VaR : Cartera con 70 accioens AAPL y 100 de IBM--------------------

C <- 0.99
z <- qnorm(C)
h <- 5/252

##Valuacion --------------------------------------------
W_A = 70 * stock_data$AAPL[nrow(stock_data)]
W_I = 100 * stock_data$IBM[nrow(stock_data)]

W = W_A + W_I
w <- c(W_A, W_I)/W

sigma_P <- sqrt(t(w)%*% varcov %*% w) %>% as.double()
sqrt(w[1]^2*sigma_AAPL^2 + w[2]^2*sigma_IBM^2 + 2*w[1]*w[2]*sigma_AAPL*sigma_IBM*Rho)

##VaR No Parametrico------------------------------------------------------------------
stock_data <- stock_data %>%
  mutate(P = w[1]*stock_data$AAPL + w[2]*stock_data$IBM) #Accion hibrida que es mitad y mitad de cada accion

R_P = log(stock_data$P[2:nrow(stock_data)] / stock_data$P[1:nrow(stock_data)-1]) 
quantile(R_P, 1-C)
vaR_NoParam_1d <- -quantile(R_P, 1-C)*W
vaR_NoParam_1d_rel = (mean(R_P)-quantile(R_P, 1-C)*W)

plot(R_P, type="l")



##Var Normal: Individual y cartera
VaR_A_Normal = z*sigma_AAPL*sqrt(h)*W_A
z*sd(R_AAPL)*sqrt(5)*W_A
VaR_A_Normal / W_A

VaR_I_Normal = z*sigma_IBM*sqrt(h)*W_I
z*sd(R_IBM)*sqrt(5)*W_I
VaR_I_Normal / W_I

VaR_P_Normal_NoDiv <- VaR_A_Normal + VaR_I_Normal
VaR_P_Normal_NoDiv / W

VaR_P_Normal = z*sigma_P*sqrt(h)*W
VaR_P_Normal/W
Benef_Diver = VaR_P_Normal_NoDiv - VaR_P_Normal

##VaR Marginal-------------------------------------------------------------
Betas <- (varcov %*% w) / sigma_P^2
sum(t(w) %*% Betas) #chequeo
VaR_Marginal = VaR_P_Normal / W*Betas

## VaR Incremental: 100 acciones mas de AAPl------------
W_A_n <- 170 * stock_data$AAPL[nrow(stock_data)]
W_n = W_A_n + W_I
w_n = c(W_A_n, W_I)/W

sigma_P_n = sqrt(t(w_n) %*% varcov %*% w_n) %>% as.double()
sqrt(w_n[1]^2 * sigma_AAPL^2 + w_n[2]*sigma_IBM^2 + 2*sigma_AAPL*sigma_IBM*Rho)

VaR_P_Normal_n = z*sigma_P_n*sqrt(h)*W_n
VaR_P_Normal_incremental = VaR_P_Normal_n - VaR_P_Normal


## Component VaR (cartera original)-------------------------------------
VaR_Component = VaR_P_Normal * w * Betas
sum(VaR_Component) - VaR_P_Normal #chequeo
VaR_Component / VaR_P_Normal




