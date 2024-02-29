library(tidyverse)
rm(list = ls())
graphics.off()

#Parametros
r = 0.08
S_0 = 75
mu = 0.2
sigma = 0.3

#Considere una opcion de compra (call) sobre mil acciones con vencimiento en dos meses. 
#(tiempo al vencimiento = 2/12) y un precio de ejercicio de $78. Calcule la prima del contrato usando el modelo de Black y Scholes

K = 78
Vto_0 = 2/12
Cant_S = 1000

d1 = (log(S_0/K) + (r + 0.5*sigma^2)*Vto_0) / (sigma * sqrt(Vto_0))
d2 = d1 - sigma*sqrt(Vto_0)
primacall = S_0 * pnorm(d1) - K * exp(-r * Vto_0) * pnorm(d2)

(contratocall = primacall * Cant_S)








