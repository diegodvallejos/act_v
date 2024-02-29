#Valuacion de opciones-----------------------------------------------------------
library(tidyverse)
#Ejercicio 1 parcial 1C2023

Volatilidad <- 0.25
Rend_esperado <- 0.12
Tasa_libre_riesgo <- 0.05 # <- Anual continua
t <- 2 # <- Cantidad de meses
n <- 2 # <- cantidad de pasos
delta_t <- t/n
#Composicion continua

#Inciso a)----

#Contruimos UP y DOWN porque siempre es el mismo

UP <- 1.2# <- 3/12 es porque seria el intervalo de tiempo. En este caso es 3/12 porque cada paso es cada 3 meses y siempre se hace anual
DOWN <- 0.8


# Momento Inicial : paso 0

S0 <- 50

# Momento 3 meses : Paso 1

(S1u <- S0*UP)

(S1d <- S0*DOWN)

# Momento 6 meses : Paso 2

S2uu <- S1u*UP

S2ud <- S1u*DOWN

S2du <- S1d*UP

S2dd <- S1d*DOWN

#EL arbol queda asi:
Arbol <- tibble(indice=c("S0_UU", "S0_U", "S0", "S0_D", "S0_DD"),
                Cero=c(NA,NA,S0, NA, NA),
                Dos_meses=c(NA, S1u, NA, S1d, NA),
                Cuatro_meses=c(S2uu, NA, S2ud, NA, S2dd))

Arbol


#Inciso b)----------------------------------------------------------------------
#put europeo

K <- 52
#Arrancamos de atras para adelante

f2uu <- max(K - S2uu, 0)

f2ud <- max(K - S2ud, 0)

f2du <- max(K - S2du, 0)

f2dd <- max(K - S2dd, 0)

#Ahora momento 1

Prob_riesgo_neutro <- (S0 * exp(Tasa_libre_riesgo * delta_t) - S1d) / (S1u - S1d)

f1u <- (Prob_riesgo_neutro * f2uu + f2ud * (1 - Prob_riesgo_neutro)) * exp(-Tasa_libre_riesgo * delta_t)

f1d <- (Prob_riesgo_neutro * f2du + f2dd * (1 - Prob_riesgo_neutro)) * exp(-Tasa_libre_riesgo * delta_t)

#Ahora momento 0

f0 <- (Prob_riesgo_neutro * f1u + f1d * (1 - Prob_riesgo_neutro)) * exp(-Tasa_libre_riesgo * delta_t)


Arbol_b <- tibble(indice=c("f0_UU", "f0_U", "f0", "f0_D", "f0_DD"),
                  Momento_0=c(NA,NA,f0, NA, NA),
                  Momento_1=c(NA, f1u, NA, f1d, NA),
                  Momento_2=c(f2uu, NA, f2ud, NA, f2dd))


Arbol_b

#Put Americano
#Parto de inciso b
Prob_riesgo_neutro <- (S0 * exp(Tasa_libre_riesgo * delta_t) - S1d) / (S1u - S1d)

f1u <- max((K-S1u),(Prob_riesgo_neutro * f2uu + f2ud * (1 - Prob_riesgo_neutro)) * exp(-Tasa_libre_riesgo * delta_t))

f1d <- pmax((K - S1d),(Prob_riesgo_neutro * f2du + f2dd * (1 - Prob_riesgo_neutro)) * exp(-Tasa_libre_riesgo * delta_t))

#Ahora momento 0

f0 <- max(K-S0,(Prob_riesgo_neutro * f1u + f1d * (1 - Prob_riesgo_neutro))) * exp(-Tasa_libre_riesgo * delta_t)


Arbol_b <- tibble(indice=c("f0_UU", "f0_U", "f0", "f0_D", "f0_DD"),
                  Momento_0=c(NA,NA,f0, NA, NA),
                  Momento_1=c(NA, f1u, NA, f1d, NA),
                  Momento_2=c(f2uu, NA, f2ud, NA, f2dd))


Arbol_b