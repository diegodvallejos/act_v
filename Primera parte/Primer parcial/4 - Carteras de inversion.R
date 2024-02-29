#Ejercicio 4 Modelo ~ Cartera de Inversion---------------
#a) Grafique la curva riesgo-rendimiento---------------
library(tidyverse)
ER = c(0.20, 0.12)
DE = c(0.18, 0.15)

Rho=c(-1, -0.5, 0, 0.5, 1)

w1 =seq(0, 1, by = 0.001)
w2 = 1 - w1

ER_C=w1*ER[1]+w2*ER[2]
DE_C=map(Rho,
         function(x)sqrt(w1^2*DE[1]^2+w2^2*DE[2]^2+2*w1*w2*DE[1]*DE[2]*x))
df=tibble(w1,
          w2,
          ER=ER_C,                       #para los diferentes coeficientes
          DE_1=unlist(DE_C[1]),
          DE_2=unlist(DE_C[2]),
          DE_3=unlist(DE_C[3]),
          DE_4=unlist(DE_C[4]),
          DE_5=unlist(DE_C[5]))

ggplot(df) +
  geom_point(aes(x = DE_1, y = ER), color = "blue") +
  geom_point(aes(x = DE_2, y = ER), color = "green") +
  geom_point(aes(x = DE_3, y = ER), color = "orange") +
  geom_point(aes(x = DE_4, y = ER), color = "gold") +
  geom_point(aes(x = DE_5, y = ER), color = "red") +
  geom_hline(yintercept = 0, lwd = 1.1) +
  geom_vline(xintercept = 0, lwd = 1.1) +
  scale_y_continuous("Rend. Esperado", limits = c(0.08, 0.25), breaks = seq(0.08, 0.25, by = 0.01)) +
  scale_x_continuous("Riesgo", limits = c(0.00, 0.30), breaks = seq(0.00, 0.30, by = 0.01))

#b) CAL----------------------------
rf=0.06  # <- tasa libre de riesgo. Se trabaja con coef de correlacion de 0.5 <- en este caso es Rho[4]
ratiosharpe=(w1*ER[1]+w2*ER[2]-rf)/
  sqrt(w1^2*DE[1]^2+w2^2*DE[2]^2+2*w1*w2*DE[1]*DE[2]*Rho[4])
ratiosharpe
plot(w1,ratiosharpe)

w1opt=w1[ratiosharpe==max(ratiosharpe)]
w2opt=1-w1opt

ERopt=w1opt*ER[1]+(w2opt)*ER[2]
DEopt=sqrt(w1opt^2*DE[1]^2+w2opt^2*DE[2]^2+2*w1opt*w2opt*DE[1]*DE[2]*Rho[4])   #Aca tambien cambiaria el Rho si no es coef de correlacion 0.5

ggplot(df)+
  geom_point(aes(x = DE_4, y = ER), col = "blue") +
  geom_abline(slope=(ERopt-rf)/DEopt,intercept=rf,lwd=1.1,col="green") + #CAL
  geom_point(aes(x = DEopt, y = ERopt), col = "red",shape=15) +
  geom_hline(yintercept = 0, lwd = 1.1) +
  geom_vline(xintercept = 0, lwd = 1.1) +
  scale_y_continuous("Rend. Esperado", limits = c(0.05, 0.25), breaks = seq(0.05, 0.25, by = 0.01)) +
  scale_x_continuous("Riesgo", limits = c(0.00, 0.25), breaks = seq(0.00, 0.20, by = 0.01))  #Por las dudas no tocar el riesgo

