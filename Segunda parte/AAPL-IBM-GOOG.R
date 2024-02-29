#Ejercicio VAR 3 variables

library(flextable)
library(tidyverse)
library(ggplot2)
library(quantmod)

#cantidades dadas

#Datos
Tabla <- tibble(Acciones = c("AAPL","IBM","GOOG"),
                Cantidades = c(200,300,350),
                Precio = c(190,134,120),
                Volatilidad = c(0.22,0.18,0.24))

Tabla

c<- 0.995
Zc <- qnorm(c)
h<- 5/252

rho_AAPL_IBM <- 0.7
rho_AAPL_GOOG <- 0.2
rho_IBM_GOOG <- 0.24

W_AAPL <- Tabla$Cantidades[1]*Tabla$Precio[1]
W_IBM <- Tabla$Cantidades[2]*Tabla$Precio[2]
W_GOOG <- Tabla$Cantidades[3]*Tabla$Precio[3]

W <- W_AAPL+W_IBM+W_GOOG

w<- c(W_AAPL, W_IBM, W_GOOG)/W
w

#Calculo el VaR Normal (parametrico) Individual y Cartera
VaR_AAPL_Indiv <- Zc*Tabla$Volatilidad[1]*sqrt(h)*W_AAPL
VaR_AAPL_Indiv

VaR_IBM_Indiv <- Zc*Tabla$Volatilidad[2]*sqrt(h)*W_IBM
VaR_IBM_Indiv

VaR_GOOG_Indiv <- Zc*Tabla$Volatilidad[3]*sqrt(h)*W_GOOG
VaR_GOOG_Indiv

VaR_P_Indiv_nd <- VaR_AAPL_Indiv+VaR_IBM_Indiv+VaR_GOOG_Indiv
VaR_P_Indiv_nd

#VaR Diversificado
matriz_correlaciones = matrix(c(1, rho_AAPL_IBM,rho_AAPL_GOOG,
                      rho_AAPL_IBM,1,rho_IBM_GOOG,
                      rho_AAPL_GOOG,rho_IBM_GOOG,1), nrow=3)
matriz_correlaciones

matriz_varcov = matrix(c(Tabla$Volatilidad[1]^2, Tabla$Volatilidad[1]*Tabla$Volatilidad[2]*rho_AAPL_IBM, Tabla$Volatilidad[1]*Tabla$Volatilidad[3]*rho_AAPL_GOOG,
                         Tabla$Volatilidad[1]*Tabla$Volatilidad[2]*rho_AAPL_IBM, Tabla$Volatilidad[2]^2 , Tabla$Volatilidad[2]*Tabla$Volatilidad[3]*rho_IBM_GOOG,
                         Tabla$Volatilidad[1]*Tabla$Volatilidad[3]*rho_AAPL_GOOG, Tabla$Volatilidad[2]*Tabla$Volatilidad[3]*rho_IBM_GOOG, Tabla$Volatilidad[3]^2), nrow=3)
matriz_varcov

matriz_varcov_2 <- diag(Volatilidad) %*% matriz_correlaciones %*% diag(Volatilidad) #USAR ESTA QUE ES MAS CORTA

volatilidad_P <- sqrt(t(w)%*% matriz_varcov %*% w) %>% as.double()#volatilidad anual del portafolio/desvio estandar del portafolio
volatilidad_P

varianza_anual <- volatilidad_P^2
varianza_anual

VaR_P_div <- Zc*volatilidad_P*sqrt(h)*W
VaR_P_div

#VaR Marginal
beta <- (matriz_varcov%*%w)/varianza_anual
beta

sum(t(w)%*% beta) #chequeo que tiene que dar 1

VaR_Marginal<- VaR_P_div/W*beta
VaR_Marginal

#Component VaR
Component_VaR <- VaR_P_div*w*beta
Component_VaR

sum(Component_VaR)-VaR_P_div #chequeo que suma de component var me tiene que dar lo mismo, osea la resta =0 no coincide por 0.00jkJZBk

#%VaR
(Component_VaR/VaR_P_div)


#en base a yahoo finance
