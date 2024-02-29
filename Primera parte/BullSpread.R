rm(list = ls())
library(tidyverse)
library(flextable)

K1_C <- 30
C1 <- 3
K2_C <- 35
C2 <- 1.5
K3_C <- 40
C3 <- 0.75

K1_P <- 30
p1 <- 1.75
K2_P <- 35
p2 <- 2.5
K3_P <- 40
p3 <- 5


tibble(Variable = c('Strike', 'Prima'),
       Call_1 = c(K1_C, C1),
       Call_2 = c(K2_C, C2),
       Call_3 = c(K3_C, C3),
       
       Put_1 = c(K1_P, p1),
       Put_2 = c(K2_P, p2),
       Put_3 = c(K3_P, p3),
       ) %>%
  flextable() %>%
  colformat_double(digits=2)

# Bull C1 y C2-----------------------
S = seq(20, 45)
max(S - K1_C, 0)

PayoutCall_1 = unlist(map(S, function(x)max(x - K1_C, 0)))
RdoCall_1 = PayoutCall_1 - C1

PayoutCall_2 = unlist(map(S, function(x)max(x - K2_C, 0)))
RdoCall_2 = PayoutCall_2 - C2

df = tibble(S = S,
            C1 = RdoCall_1,
            C2 = RdoCall_2,
            Bull_c1_c2 = RdoCall_1 - RdoCall_2)

ggplot(df, aes(x=S, y=Bull_c1_c2)) + 
  geom_line(lwd = 2) + 
  geom_abline(slope=0, intercept=0, col='red') + 
  geom_line(aes(x=S, y=RdoCall_1)) + 
  geom_line(aes(x=S, y=RdoCall_2)) + 
  ggtitle(paste("Bull Spread con Calls: K1=", K1_C, "C1 -", C1,"; K2-", K2_C,", C2-", C2))

# Bear P1 y P2------------------------------------
S = seq(20, 45)
PayoutPut_1 = unlist(map(S, function(x)max(K1_P - x, 0)))
RdoPut_1 = PayoutPut_1 - p1

PayoutPut_2 = unlist(map(S, function(x)max(K2_P - x, 0)))
RdoPut_2 = PayoutPut_2 - p2

df_bear = tibble(S =S,
                 P1 = RdoPut_1,
                 P2 = RdoPut_2,
                 Bear_p1_p2 = RdoPut_2 - RdoPut_1)

ggplot(df_bear, aes(x=S, y=Bear_p1_p2)) + 
  geom_line(lwd = 2) + 
  geom_abline(slope = 0, intercept = 0, col='red') + 
  geom_line(aes(x=S, y=RdoPut_1)) + 
  geom_line(aes(x=S, y=-RdoPut_2)) +
  ggtitle(paste("Bear spread con Puts: K1=", K1_P, ",P1=",p1, ", K2=", K2_P, ",P2=", p2))

#Limite al precio de opciones

r = 0.12
vto = 3/12
S = 33
C1 >= S - K1_C * exp(-r*vto)
C2 >= S - K2_C * exp(-r*vto)
C3 >= S - K3_C * exp(-r*vto)

p1 >= K1_P * exp(-r*vto) - S
p2 >= K2_P * exp(-r*vto) - S
p3 >= K3_P * exp(-r*vto) - S

#Compro Cartera "A" y vendo "Cartera B"
S - K1_C * exp(-r*vto) - C1

-p3 - S + K3_P * exp(-r*vto)

#Paridad call-put

K1_C * exp(-r*vto) + C1==S + p1
K1_C * exp(-r*vto) + C1
S + p1 - (K1_C * exp(-r*vto) + C1)













