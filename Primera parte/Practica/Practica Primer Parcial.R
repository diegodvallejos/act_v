# ----Practica 1: Bonos -----------------------------------

#Resolvemos ejercicios de las diapositivas
#Primero planteamos la tabla inicial

Tabla_bonos <- data.frame(periodo=seq(1,4), precio_cupon_cero=c(945, 890, 835, 785))

#Completamos las columnas: Factor de descuento, Tasa efectiva anual, Tasa nominal continua
precio_bono <- 1000
Tabla_bonos$FD <- Tabla_bonos$precio_cupon_cero/precio_bono
Tabla_bonos$TEA <- (Tabla_bonos$FD ^ (-1/Tabla_bonos$periodo))-1 # <- Todas estas formulas estan en los apuntes de clase Actuarial V
Tabla_bonos$TNA_cont <- -log(Tabla_bonos$FD)/Tabla_bonos$periodo

#Asi queda la tabla despues de este primer paso
Tabla_bonos

#Ahora agregaremos las columnas: Plazo forward, tasa efectiva anual forward y tasa nominal anual forward
Tabla_bonos$plazo_fwd <- c('0-1', '1-2', '2-3','3-4')
Tabla_bonos$TEA_fwd <- NA
Tabla_bonos$TEA_fwd[1] <- Tabla_bonos$TEA[1]
for (i in 2:length(Tabla_bonos$periodo)){
  Tabla_bonos$TEA_fwd[i] = (((1 + Tabla_bonos$TEA[i])^i) / (1 + Tabla_bonos$TEA[i - 1])^(i - 1)) ^ (1/(i-(i-1))) - 1
} # <- For loop para completar la columna con formula de los apuntes Actuarial V
Tabla_bonos$TNA_cont_fwd <- NA
Tabla_bonos$TNA_cont_fwd[1] <- Tabla_bonos$TNA_cont[1]
for (i in 2:length(Tabla_bonos$periodo)){
  Tabla_bonos$TNA_cont_fwd[i] <- Tabla_bonos$TNA_cont[i] + (Tabla_bonos$TNA_cont[i] - Tabla_bonos$TNA_cont[i - 1]) * (( i - 1 ) / ( i - ( i - 1)))
} # <- For loop para completar la columna con su respectiva formula

#Asi queda la tabla despues de este segundo paso
Tabla_bonos

#Ahora vamos en contrar el paryield
Paryield <- (1- Tabla_bonos$FD[4]) / sum(Tabla_bonos$FD)*100

#Supongamos que queremos graficar la curva de tasas de interes:


ggplot(Tabla_bonos, aes(x=periodo, y=TEA_fwd)) + geom_line()


#Calculo del precio de un bono ----
#Teniendo un bono que paga un cupon del 7% anual, valor nominal de 100 y amortiza el total del capital al vencimiento en 4 años
#Calculamos el precio del bono

Bono1 <- data.frame(plazo=Tabla_bonos$periodo, FF=c(7,7,7,107), FD=Tabla_bonos$FD)

#Entonces el precio del bono se calcula
(precio <- sum(Bono1$FF*Bono1$FD))

#Ejemplo de calculo de Duration ----
#Definimos un tabla en base al ejercicio 2 del Hull Ed7 que aparece en las diapositivas
#Vamos a usar tibble porque permite contruir columnas de forma secuencial

Tabla_ej2Hull <- tibble(t=seq(0.5, 3, by=0.5),
                        FD=exp(-t*12/100),
                        CF=rep(10, 6)/2 + c(rep(0,5), 100),
                        PV=FD*CF,
                        peso=PV / sum(PV),
                        t_x_peso=t*peso)

#Asi se ve la tabla
Tabla_ej2Hull

#Ahora vamos a hacer el calculo del precio del bono, la duration, la variacion del precio y el nuevo precio

(precio_bono_hull <- sum(Tabla_ej2Hull$PV))

(duration_hull <- sum(Tabla_ej2Hull$t_x_peso))

(delta_precio <- -0.001* precio_bono_hull * duration_hull)

(nuevo_precio_hull <- precio_bono_hull + delta_precio)


#---- Practica 2: Mercado de capitales -------------------
#Se va a empezar haciendo un ejercicio en el que vamos a calcular la esperanza y el desvio de una cartera
#Calculo de esperanza y desvio de cartera ----
rend_libre_riesgo <- 0.07
porcentaje_riesgoso <- 0.75
esperanza_riesgoso <- 0.15
desvio_riesgoso <- 0.22

(Esperanza_de_cartera <- esperanza_riesgoso*porcentaje_riesgoso + (1-porcentaje_riesgoso)*rend_libre_riesgo)

(Desvio_de_cartera <- porcentaje_riesgoso * desvio_riesgoso)

#Construccion de frontera eficiente de inversion----
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
  scale_y_continuous("Rend. Esperado", limits = c(0.00, 0.25), breaks = seq(0.00, 0.25, by = 0.01))+
  scale_x_continuous("Riesgo", limits = c(0.00, 0.20), breaks = seq(0.00, 0.20, by = 0.01))

#----Practica 3: Fra & Swaps----

