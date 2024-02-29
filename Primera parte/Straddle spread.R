rm(list = ls())
library(tidyverse)
library(flextable)

K_C = 35  # Precio de ejercicio de las opciones de compra
C = 3     # Prima de las opciones de compra
K_P = 35  # Precio de ejercicio de las opciones de venta
P = 2.5   # Prima de las opciones de venta

S <- seq(25, 45, by = 1)

# Función de pago de las opciones de compra
PayoutCall <- pmax(S - K_C, 0)
RdoCall = PayoutCall - C

# Función de pago de las opciones de venta
PayoutPut <- pmax(K_P - S, 0)
RdoPut = P - PayoutPut

# Función de pago total del Box Spread
box_spread_payoff <- RdoCall + RdoCall - RdoPut - RdoPut

df_box_spread <- tibble(S = S, payoff = box_spread_payoff)

ggplot(df_box_spread, aes(x = S, y = payoff)) + 
  geom_line(lwd = 1.5) + 
  geom_hline(yintercept = 0, col = 'green') + 
  geom_line(aes(x = S, y = RdoCall), col = 'red', linetype = "dashed") + # Opciones de Compra
  geom_line(aes(x = S, y = -RdoPut), col = 'blue', linetype = "dashed") + # Opciones de Venta
  labs(title = "Box Spread Payoff",
       subtitle = paste("K_C =", K_C, ", C =", C, ", K_P =", K_P, ", P =", P))
