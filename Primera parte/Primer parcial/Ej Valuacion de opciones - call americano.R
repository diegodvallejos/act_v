library(tidyverse)
Volatilidad <- 0.2
Rend_esperado <- 0.1
Tasa_libre_riesgo <- 0.05
t <- 6/12
n <- 2 # <- cantidad de pasos
delta_t <- t/n
#Composicion continua

#Inciso a)----

#Contruimos UP y DOWN porque siempre es el mismo

UP <- exp(Volatilidad * sqrt(delta_t)) # <- 3/12 es porque seria el intervalo de tiempo. En este caso es 3/12 porque cada paso es cada 3 meses y siempre se hace anual
DOWN <- 1/UP


# Momento Inicial : paso 0

S0 <- 70

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
                Momento_0=c(NA,NA,S0, NA, NA),
                Momento_1=c(NA, S1u, NA, S1d, NA),
                Momento_2=c(S2uu, NA, S2ud, NA, S2dd))

Arbol


#Inciso b)----

K <- 68 
#Arrancamos de atras para adelante

S2uu_b <- max(S2uu - K, 0)

S2ud_b <- max(S2ud - K, 0)

S2du_b <- max(S2du - K, 0)

S2dd_b <- max(S2dd - K, 0)

#Ahora momento 1

Prob_riesgo_neutro <- (S0 * exp(Tasa_libre_riesgo * delta_t) - S1d) / (S1u - S1d)

S1u_b <- pmax((S1u - K),(Prob_riesgo_neutro * S2uu_b + S2ud_b * (1 - Prob_riesgo_neutro)) * exp(-Tasa_libre_riesgo * delta_t))

S1d_b <- pmax((S1d - K),(Prob_riesgo_neutro * S2du_b + S2dd_b * (1 - Prob_riesgo_neutro)) * exp(-Tasa_libre_riesgo * delta_t))

#Ahora momento 0

S0_b <- pmax((S0 - K),(Prob_riesgo_neutro * S1u_b + S1d_b * (1 - Prob_riesgo_neutro)) * exp(-Tasa_libre_riesgo * delta_t))


Arbol_b <- tibble(indice=c("S0_UU", "S0_U", "S0", "S0_D", "S0_DD"),
                  Momento_0=c("","",S0_b, "", ""),
                  Momento_1=c("", S1u_b, "", S1d_b, ""),
                  Momento_2=c(S2uu_b, "", S2ud_b, "", S2dd_b))

