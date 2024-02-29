#Estategia con opciones y limites de prima----------------------------------------

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

# Bull C2 y C3-----------------------
S = seq(25, 50) # <- Margen de 10 para cada lado
max(S - K2_C, 0)

PayoutCall_2 = unlist(map(S, function(x)max(x - K2_C, 0)))
RdoCall_2 = PayoutCall_2 - C2

PayoutCall_3 = unlist(map(S, function(x)max(x - K3_C, 0)))
RdoCall_3 = PayoutCall_3 - C3

df = tibble(S = S,
            C2 = RdoCall_2,
            C3 = RdoCall_3,
            Bull_c2_c3 = RdoCall_2 - RdoCall_3) # Notar que en Bear es al reves la resta

ggplot(df, aes(x=S, y=Bull_c2_c3)) + 
  geom_line(lwd = 2) + 
  geom_abline(slope=0, intercept=0, col='blue') + 
  geom_line(aes(x=S, y=RdoCall_2)) + 
  geom_line(aes(x=S, y=RdoCall_3)) + 
  ggtitle(paste("Bull Spread con Calls: K2=", K2_C, "C2 -", C2,"; K3-", K3_C,", C3-", C3))

# Bear P2 y P3------------------------------------
S = seq(25, 50)
PayoutPut_2 = unlist(map(S, function(x)max(K2_P - x, 0)))
RdoPut_2 = PayoutPut_2 - p2

PayoutPut_3 = unlist(map(S, function(x)max(K3_P - x, 0)))
RdoPut_3 = PayoutPut_3 - p3

df_bear = tibble(S =S,
                 P2 = RdoPut_2,
                 P3 = RdoPut_3,
                 Bear_p2_p3 = RdoPut_3 - RdoPut_2)  # <- Notar que en bull es al reves la resta

ggplot(df_bear, aes(x=S, y=Bear_p2_p3)) + 
  geom_line(lwd = 2) + 
  geom_abline(slope = 0, intercept = 0, col='blue') + 
  geom_line(aes(x=S, y=RdoPut_2)) + 
  geom_line(aes(x=S, y=-RdoPut_3)) +
  ggtitle(paste("Bear spread con Puts: K2=", K2_P, ",P2=",p2, ", K3=", K3_P, ",P3=", p3))

#Limite al precio de opciones

r = 0.08
vto = 2/12
S = 34
C1 >= S - K1_C * exp(-r*vto)
C2 >= S - K2_C * exp(-r*vto)
C3 >= S - K3_C * exp(-r*vto)

p1 >= K1_P * exp(-r*vto) - S
p2 >= K2_P * exp(-r*vto) - S        # <- chequeo los limites inferiores a ver si es posible arbitrar
p3 >= K3_P * exp(-r*vto) - S

#Compro Cartera "A" y vendo "Cartera B"
S - K1_C * exp(-r*vto) - C1   #Call <- Compro el valor presente del precio del ejercicio y el call, y vendo el subyacente

#al final ST - K - max(ST - K, 0)

-p3 - S + K3_P * exp(-r*vto)   #Put <- Compro un put y subyacente y vendo dinero en efectivo igual al precio de ejercicio

#Al final - max(K-ST, 0) -ST + K

#Paridad call-put

K1_C * exp(-r*vto) + C1==S + p1
K1_C * exp(-r*vto) + C1
S + p1 - (K1_C * exp(-r*vto) + C1)









