#Valuacion de opciones-----------------------------------------------------------
library(tidyverse)
#Ejercicio 1 parcial 1C2023

Volatilidad <- 0.25
Rendimiento_esperado <- 0.12
Tasa_libre_de_riesgo <- 0.06 # <- Anual continua
t <- 4/12 # <- Cantidad de meses
n <- 2 # <- cantidad de pasos
delta_t <- t/n
#Composicion continua

#Inciso a)----

#Contruimos UP y DOWN porque siempre es el mismo

UP <- exp(Volatilidad * sqrt(delta_t)) # <- 3/12 es porque seria el intervalo de tiempo. En este caso es 3/12 porque cada paso es cada 3 meses y siempre se hace anual
DOWN <- 1/UP


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
                Dos_meses=c(NA, S1_up, NA, S1_down, NA),
                Cuatro_meses=c(S2_up_up, NA, S2_up_down, NA, S2_down_down))

Arbol


#Inciso b)----------------------------------------------------------------------
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


Arbol_b


#Inciso c)----------------------------------------------------------------------

#Arbitraje - camino down down

put_mercado <- 6
put_teorico <- S0_b


##Arbol delta, voy de atras para adelante <- queda igual si es put o call

delta_1_up <- (S2_up_up_b - S2_up_down_b) / (S2_up_up - S2_up_down)

delta_1_down <- (S2_down_up_b - S2_down_down_b) / (S2_down_up - S2_down_down)

delta_0 <- (S1_up_b - S1_down_b) / (S1_up - S1_down)

#Ahora hacemos el arbitraje

# t=0

venta_put <- put_mercado #Como esta caro me comprometo a venderlo caro y comprarlo barato
Compra_subyecente <- -delta_0*S0
Prestamo_0 <- -Compra_subyecente-venta_put
FF_0 <- venta_put + Compra_subyecente + Prestamo_0

# t=1 : 2 meses

Compra_subyecente_en_1 <- -(delta_1_down - delta_0)*S1_down #porque estamos viendo el caso down - down
Prestamo_1 <- -Compra_subyecente_en_1
FF_1 <- Compra_subyecente_en_1 + Prestamo_1

# t=2 : 4 meses

vendo_put_2 <- -S2_down_down_b
Pago_prestamo_0 <- -Prestamo_0*exp(Tasa_libre_de_riesgo*t)
Pago_prestamo_1 <- -Prestamo_1*exp(Tasa_libre_de_riesgo*delta_t)
Liquidacion_subyacente <- -S2_down_down  # <- Como es un put, va negativo

FF_2 <- vendo_put_2 + Pago_prestamo_0 + Pago_prestamo_1 + Liquidacion_subyacente

Check <- (put_mercado - S0_b)*exp(Tasa_libre_de_riesgo*t)














