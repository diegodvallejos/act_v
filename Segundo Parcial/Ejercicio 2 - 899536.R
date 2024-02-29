#Diego Demian Vallejos
#Numero de registro: 899536

library(flextable)
library(tidyverse)
library(ggplot2)
library(quantmod)

#cantidades dadas

#Datos
Tabla <- tibble(Acciones = c("AAPL","IBM","GOOG"),
                Cantidades = c(250,300,300),
                Precio = c(175,135,125),
                Volatilidad = c(0.23,0.21,0.20))

Tabla

#a) VaR individual --------------------------------------------------------------

c<- 0.99
Zc <- qnorm(c)
h<- 10/252

rho_AAPL_IBM <- 0.25
rho_AAPL_GOOG <- 0.1
rho_IBM_GOOG <- 0.90

W_AAPL <- Tabla$Cantidades[1]*Tabla$Precio[1]
W_IBM <- Tabla$Cantidades[2]*Tabla$Precio[2]
W_GOOG <- Tabla$Cantidades[3]*Tabla$Precio[3]

W <- W_AAPL+W_IBM+W_GOOG

w<- c(W_AAPL, W_IBM, W_GOOG)/W
w

VaR_AAPL_Indiv <- Zc*Tabla$Volatilidad[1]*sqrt(h)*W_AAPL
VaR_AAPL_Indiv

VaR_IBM_Indiv <- Zc*Tabla$Volatilidad[2]*sqrt(h)*W_IBM
VaR_IBM_Indiv

VaR_GOOG_Indiv <- Zc*Tabla$Volatilidad[3]*sqrt(h)*W_GOOG
VaR_GOOG_Indiv

VaR_P_Indiv_nd <- VaR_AAPL_Indiv+VaR_IBM_Indiv+VaR_GOOG_Indiv
VaR_P_Indiv_nd

#b) VaR diversificado ----------------------------------------------------------

matriz_correlaciones = matrix(c(1, rho_AAPL_IBM,rho_AAPL_GOOG,
                                rho_AAPL_IBM,1,rho_IBM_GOOG,
                                rho_AAPL_GOOG,rho_IBM_GOOG,1), nrow=3)
matriz_correlaciones

matriz_varcov <- diag(Tabla$Volatilidad) %*% matriz_correlaciones %*% diag(Tabla$Volatilidad) #USAR ESTA QUE ES MAS CORTA

volatilidad_P <- sqrt(t(w)%*% matriz_varcov %*% w) %>% as.double()#volatilidad anual del portafolio/desvio estandar del portafolio
volatilidad_P

varianza_anual <- volatilidad_P^2
varianza_anual

VaR_P_div <- Zc*volatilidad_P*sqrt(h)*W
VaR_P_div

#c) Component VaR y marginal VaR --------------------------------------------------------------

beta <- (matriz_varcov%*%w)/varianza_anual
beta

sum(t(w)%*% beta)

VaR_Marginal<- VaR_P_div/W*beta
VaR_Marginal

Component_VaR <- VaR_P_div*w*beta
Component_VaR

sum(Component_VaR)-VaR_P_div 

Benef_diver <-VaR_P_Indiv_nd - VaR_P_div
Benef_diver

print("EL component var determina el Valor a riesgo de cada activo considerado en su conjunto como portafolio, vemos que IBM es la que mas aporta y esto tiene que ver con las correlaciones entre acciones")

#d) Verdadero o Falso ----------------------------------------------------------

print("Es Falso, si bien es el que mas riesgo tiene de forma indiviadual visto desde el punto de vista de la cartera, no es la mas alta.")

