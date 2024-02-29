library(tidyverse)
rm(list = ls())

r <- 0.05
K <- 50
Vto_0 <- 20/52
mu <- 0.08
sigma <- 0.2
Cant_S <- 100000

Week <- seq(0, 20)
t <- seq(0, Vto_0, by = 1/52)
Vto <- Vto_0 - t
S = c(49,
      48.12,
      50.25,
      51.75,
      53.12,
      53,
      51.87,
      51.38,
      49.88,
      50.37,
      52.13,
      51.88,
      52.87,
      54.87,
      54.62,
      55.87,
      57.25)


d1 <- (log(S/K) + (r + 0.5*sigma^2) * Vto) / (sigma*sqrt(Vto))
delta <- pnorm(d1)
Posicion <- round(delta, 3) * Cant_S
Compra_venta <- c(Posicion[1], diff(Posicion))
Costo_S <- Compra_venta * S
Valor_S <- Posicion * S


Deuda <- rep(NA, 21)
Interes <- rep(NA, 21)
Deuda[1] <- Costo_S[1]
for(i in 2:21){
  Interes[i] = Deuda[i-1]*(r*1/52)
  Deuda[i] = Deuda[i-1] + Interes[i] + Costo_S[i]
}

Tabla <- tibble(Week, t, Vto, d1, delta, Posicion, Compra_venta, Costo_S, Deuda, Interes, Valor_S)

#Liquidacion Final

(Call_Entrega = ifelse(S[21] > K, K*Cant_S, 0))
(LiquidoDeuda = -Deuda[21])
(Flujo_Final = Call_Entrega + LiquidoDeuda)
(VP_costo = Flujo_Final * exp(-r * Vto_0))

#Costo teorico del call






