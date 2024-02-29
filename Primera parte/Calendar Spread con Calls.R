rm(list = ls())
library(tidyverse)
library(flextable)

K1_C = 35    # Precio de ejercicio de la opción de compra con el primer vencimiento
C1 = 3       # Prima de la opción de compra con el primer vencimiento
K2_C = 40    # Precio de ejercicio de la opción de compra con el segundo vencimiento
C2 = 2.5     # Prima de la opción de compra con el segundo vencimiento

S <- seq(25, 45, by = 1)

# Función de pago de la opción de compra con el primer vencimiento
PayoutCall1 <- pmax(S - K1_C, 0)
RdoCall1 = PayoutCall1 - C1

# Función de pago de la opción de compra con el segundo vencimiento
PayoutCall2 <- pmax(S - K2_C, 0)
RdoCall2 = PayoutCall2 - C2

# Función de pago del Calendar Spread
calendar_spread_payoff <- RdoCall1 - RdoCall2

df_calendar_spread <- tibble(S = S, payoff = calendar_spread_payoff)

ggplot(df_calendar_spread, aes(x = S, y = payoff)) + 
  geom_line(lwd = 1.5) + 
  geom_hline(yintercept = 0, col = 'green') + 
  geom_line(aes(x = S, y = RdoCall1), col = 'red', linetype = "dashed") + # Opción de compra con el primer vencimiento
  geom_line(aes(x = S, y = -RdoCall2), col = 'blue', linetype = "dashed") + # Opción de compra con el segundo vencimiento
  labs(title = "Calendar Spread Payoff",
       subtitle = paste("K1_C =", K1_C, ", C1 =", C1, ", K2_C =", K2_C, ", C2 =", C2))
