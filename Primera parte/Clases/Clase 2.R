### Ejercicio 1 ###

rend_libre_riesgo <- 0.07
porcentaje_riesgoso <- 0.75
esperanza_riesgoso <- 0.15
desvio_riesgoso <- 0.22

(Esperanza_de_cartera <- esperanza_riesgoso*porcentaje_riesgoso + (1-porcentaje_riesgoso)*rend_libre_riesgo)

(Desvio_de_cartera <- porcentaje_riesgoso * desvio_riesgoso)


### Ejemplo clase ###

ER <- c(0.08, 0.13)
DE <- c(0.12, 0.20)

rho <- c(-1, 0, 0.8, 1)

w1 <- seq(0, 1, by = 0.001)
w2 <- 1-w1

#Curva en espacio riesgo-rendimiento
ER_C <- w1*ER[1] + w2*ER[2]

DE_C <- map(rho, function(x) sqrt(w1^2*DE[1]^2 + w2^2*DE[2]^2 + 2*w1*w2*DE[1]*DE[2]*x))

df <- tibble(ER=ER_C,
             DE_1 = unlist(DE_C[1]),
             DE_2 = unlist(DE_C[2]),
             DE_3 = unlist(DE_C[3]),
             DE_4 = unlist(DE_C[4]))

                          
ggplot(df) + 
  geom_point(aes(x = DE_1, y = ER), col = "blue")+
  geom_point(aes(x = DE_2, y = ER), col = "forestgreen")+
  geom_point(aes(x = DE_3, y = ER), col = "orange")+
  geom_point(aes(x = DE_4, y = ER), col = "gold")+
  geom_hline(yintercept = 0, lwd = 1.1)+
  geom_vline(xintercept = 0, lwd = 1.1)+
  scale_y_continuous("Rend. Esperado", limits = c(0.00, 0.25), breaks = seq(0.00, 0.25, by = 0.001))+
  scale_x_continuous("Riesgo", limits = c(0.00, 0.20), breaks = seq(0.00, 0.20, by = 0.001))



