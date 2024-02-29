#Analisi VaR ppt diapo 24

library(flextable)
library(tidyverse)
library(ggplot2)
library(quantmod)

c<- 0.99
Zc <- qnorm(c)
h<- 10/252

Tabla <- tibble(Acciones = c("A","B","C"),
                Cantidades = c(300,200,250),
                Precio = c(10,40,20),
                Volatilidad = c(0.25,0.15,0.2))

Tabla

W_A <- Tabla$Cantidades[1]*Tabla$Precio[1]
W_B <- Tabla$Cantidades[2]*Tabla$Precio[2]
W_C <- Tabla$Cantidades[3]*Tabla$Precio[3]

W <- W_A+W_B+W_C

w<- c(W_A, W_B, W_C)/W
w

#Calculo el VaR Individual/Normal

VaR_A_Indiv <- Zc*Tabla$Volatilidad[1]*sqrt(h)*W_A
VaR_A_Indiv

VaR_B_Indiv <- Zc*Tabla$Volatilidad[2]*sqrt(h)*W_B
VaR_B_Indiv

VaR_C_Indiv <- Zc*Tabla$Volatilidad[3]*sqrt(h)*W_C
VaR_C_Indiv

VaR_P_Indiv_nd <- VaR_A_Indiv+VaR_B_Indiv+VaR_C_Indiv
VaR_P_Indiv_nd

rho_ab <- 0.7
rho_ac <- 0.5
rho_bc <- 0.6
volatilidad_a <- 0.25
volatilidad_b <- 0.15
volatilidad_c <- 0.2
matriz_correlaciones = matrix(c(1, rho_ab,rho_ac,
                      rho_ab,1,rho_bc,
                      rho_ac,rho_bc,1), nrow=3)
matriz_correlaciones

#Matriz varianxas y covarianzas
matriz_varcov <- diag(Tabla$Volatilidad) %*% matriz_correlaciones %*% diag(Tabla$Volatilidad)
matriz_varcov

(volatilidad_P <- sqrt(t(w)%*% varcov %*% w) %>% as.double()) #volatilidad anual del portafolio/desvio estandar del portafolio

(varianza_anual <- volatilidad_P^2)

#volatilidad a 10 dias ???.

#VaR Diversificado
VaR_P_Indiv_d <- Zc*volatilidad_P*sqrt(h)*W
VaR_P_Indiv_d

#VaR Marginal
beta <- (varcov%*%w)/varianza_anual
beta

sum(t(w)%*% beta) #chequeo que tiene que dar 1

VaR_Marginal<- VaR_P_Indiv_d/W*beta
VaR_Marginal

#Component VaR
Component_VaR <- VaR_P_Indiv_d*w*beta
Component_VaR

sum(Component_VaR)-VaR_P_Indiv_d #chequeo que suma de component var me tiene que dar lo mismo, osea la resta =0 no coincide por 0.00jkJZBk

#%VaR
(Component_VaR/VaR_P_Indiv_d)

#Practicar VaR Incremental: Supongo que tengo 100 acciones + del activo A
incremento<- 100

W_A_n <- W_A + incremento*10 #porque 10 es el precio del activo A y no lo tengo metido en una variable
W_A_n
#si me dijesen: compro 80.000$ de accion A W_A_n <- 80.000

W_n <- W_A_n + W_B + W_C
w_n <- c(W_A_n, W_B, W_C)/W_n
w_n

(volatilidad_P_n <- sqrt(t(w_n)%*% varcov %*% w_n) %>% as.double())

#nuevo var no diversificado
VaR_P_Indiv_d_n <- Zc*volatilidad_P_n*sqrt(h)*W_n

VaR_Incremental <- VaR_P_Indiv_d_n - VaR_P_Indiv_d
VaR_Incremental #por comprar 100 acciones + de A, me aumenta el VAR 1274,1934$

