# Ejercicio 1 ------------------------------------------------------------------
#Diego Demian Vallejos, num registro 899536
library(tidyverse)

#Parametros

Volatilidad <- 0.25
Rendimiento_esperado <- 0.12
Tasa_libre_de_riesgo <- 0.06 # <- Anual continua
t <- 6/12 # <- Cantidad de meses
n <- 2 # <- cantidad de pasos
delta_t <- t/n


#Contruimos las prob de UP y DOWN

UP <- exp(Volatilidad * sqrt(delta_t))
DOWN <- 1/UP

#Construimos el arbol

# Momento Inicial : paso 0

S0 <- 75

# Momento 3 meses : Paso 1

(S1_up <- S0*UP)

(S1_down <- S0*DOWN)

# Momento 6 meses : Paso 2

S2_up_up <- S1_up*UP

S2_up_down <- S1_up*DOWN

S2_down_up <- S1_down*UP

S2_down_down <- S1_down*DOWN

#EL arbol queda asi:
Arbol <- tibble(indice=c("S0_UU", "S0_U", "S0", "S0_D", "S0_DD"),
                Cero=c(NA,NA,S0, NA, NA),
                tres_meses=c(NA, S1_up, NA, S1_down, NA),
                seis_meses=c(S2_up_up, NA, S2_up_down, NA, S2_down_down))

Arbol

#Ahora construimos el arbol del derivado

#put europeo

K <- 78 
#Arrancamos de atras para adelante

S2_up_up_b <- max(K - S2_up_up, 0)

S2_up_down_b <- max(K - S2_up_down, 0)

S2_down_up_b <- max(K - S2_down_up, 0)

S2_down_down_b <- max(K - S2_down_down, 0)

#Ahora momento 1

Prob_de_riesgo_neutro <- (S0 * exp(Tasa_libre_de_riesgo * delta_t) - S1_down) / (S1_up - S1_down)

S1_up_b <- (Prob_de_riesgo_neutro * S2_up_up_b + S2_up_down_b * (1 - Prob_de_riesgo_neutro)) * exp(-Tasa_libre_de_riesgo * delta_t)

S1_down_b <- (Prob_de_riesgo_neutro * S2_down_up_b + S2_down_down_b * (1 - Prob_de_riesgo_neutro)) * exp(-Tasa_libre_de_riesgo * delta_t)

#Ahora momento 0

S0_b <- (Prob_de_riesgo_neutro * S1_up_b + S1_down_b * (1 - Prob_de_riesgo_neutro)) * exp(-Tasa_libre_de_riesgo * delta_t)


Arbol_b <- tibble(indice=c("S0_UU", "S0_U", "S0", "S0_D", "S0_DD"),
                  Momento_0=c(NA,NA,S0_b, NA, NA),
                  Momento_1=c(NA, S1_up_b, NA, S1_down_b, NA),
                  Momento_2=c(S2_up_up_b, NA, S2_up_down_b, NA, S2_down_down_b))


Arbol_b # <- Arbol del derivado

(Prima_consigna <- Arbol_b$Momento_0[3])














