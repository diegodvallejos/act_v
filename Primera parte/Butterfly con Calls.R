
rm(list = ls())
library(tidyverse)
library(flextable)

K1_C = 30
C1 = 3
K2_C = 35
C2 = 1.5
K3_C = 40
C3 = 0.75

K1_P = 30
p1 = 1.75
K2_P = 35
p2 = 2.5
K3_P = 40
p3 = 5


tibble(Variable = c('Strike', 'Prima'),
       Call_1 = c(K1_C, C1),
       Call_2 = c(K2_C, C2),
       Call_3 = c(K3_C, C3),
       Put_1 = c(K1_P, p1),
       Put_2 = c(K2_P, p2),
       Put_3 = c(K3_P, p3),
) %>%
  flextable() %>%
  colformat_double(digits = 2)

S <- seq (25, 45, by= 1)

# Función de pago de la call 1 (K1_C = 30, C1 = 3)
PayoutCall_1 <- pmax(S - K1_C, 0)
RdoCall_1 = PayoutCall_1 - C1

# Función de pago de la call 2 (K2_C = 35, C2 = 1.5)
PayoutCall_2 <- pmax(S - K2_C, 0)
RdoCall_2 =  C2 - PayoutCall_2 

PayoutCall_3 <- pmax(S - K3_C, 0)
RdoCall_3 = PayoutCall_3 - C3

butterfly_call_payoff <- RdoCall_1 + 2 * RdoCall_2 + RdoCall_3

df_butterfly = tibble(S =S, payoff = butterfly_call_payoff)

ggplot(df_butterfly, aes(x=S, y= payoff)) + 
  geom_line(lwd = 1.5) + 
  geom_hline(yintercept = 0, col='green') + 
  geom_line(aes(x=S, y=RdoCall_1), col='pink') + #Call 1
  geom_line(aes(x=S, y=RdoCall_2), col='lightblue') + #Call 2
  geom_line(aes(x=S, y=RdoCall_3), col='yellow')  #Call 2
  subtitle = "K1_C = 30, C1 = 3, K2_C = 35, C2 = 1.5, K3_C = 40, C3 = 0.75"
  
  
  
