#Valuacion de opciones---------------------------
library(tidyverse)
#Ejercicio 1 1P1C2023

rm(list = ls()) #Para limpiar los datos anteriores
graphics.off()  #Para limpiar los datos anteriores

#El precio de una accion es $75, su volatilidad es 25% y el rendimiento esperado es 12%. La tasa libre de riesgo es 6% anual con composicion continua.
# a) Constuya un arbol binomial de dos pasos, con plazo total de cuatro meses.
# b) Con el arbol del punto a), calcula la prima de un call europeo con precio de ejercicio $68 y vencimiento de seis meses.
# c) Realice un arbitraje si el precio del punto b) es $6,5. Considere solamente el camino "up-up" del precio subyacente para ilustrar la estrategia.

#Datos de mercado
S = 75
Volatilidad = 0.25
Rendimiento_esperado = 0.12
Tasa_libre_de_riesgo = 0.06 #Anual continua

#Inciso a)----
#Construimos el arbol
t <- 4/12 # Cantidad de meses
n <- 2 # <- cantidad de pasos
delta_t <- t/n


#Contruimos UP y DOWN porque siempre es el mismo

u <- exp(Volatilidad * sqrt(delta_t)) # <- 2/12 es porque seria el intervalo de tiempo. En este caso es 3/12 porque cada paso es cada 3 meses y siempre se hace anual
d<- 1/u

# Momento Inicial : paso 0

S0 <- S

# Momento 3 meses : Paso 1

(S1_up <- S0*u)

(S1_down <- S0*d)

# Momento 6 meses : Paso 2

S2_up_up <- S1_up*u

S2_up_down <- S1_up*d

S2_down_up <- S1_down*u

S2_down_down <- S1_down*d

#EL arbol queda asi:
Arbol <- tibble(indice=c("S0_UU", "S0_U", "S0", "S0_D", "S0_DD"),
                t0=c(NA,NA,S0, NA, NA),
                t1=c(NA, S1_up, NA, S1_down, NA),
                t2=c(S2_up_up, NA, S2_up_down, NA, S2_down_down))

Arbol

#Inciso b)----
#Put Europeo
K = 78 
#Arrancamos de atras para adelante

f2_up_up <- max(K - S2_up_up, 0)

f2_up_down <- max(K - S2_up_down, 0)

f2_down_up <- max(K - S2_down_up, 0)

f2_down_down <- max(K - S2_down_down, 0)

#Ahora momento 1

PRN <- (S0 * exp(Tasa_libre_de_riesgo * delta_t) - S1_down) / (S1_up - S1_down)  #No se modifica - Estrategia delta que replica el derivado (no son las verdaderas probabilidades del mercado)

f1_up <- (PRN * f2_up_up + f2_up_down * (1 - PRN)) * exp(-Tasa_libre_de_riesgo * delta_t)

f1_down <- (PRN* f2_down_up + f2_down_down * (1 - PRN)) * exp(-Tasa_libre_de_riesgo * delta_t)

#Ahora momento 0

f0 <- (PRN * f1_up + f1_down * (1 - PRN)) * exp(-Tasa_libre_de_riesgo * delta_t)


Arbol_f <- tibble(indice=c("S0_UU", "S0_U", "S0", "S0_D", "S0_DD"),
                  t0=c(NA, NA, f0 , NA, NA),
                  t1=c(NA, f1_up, NA, f1_down, NA),
                  t2=c(f2_up_up, NA, f2_up_down, NA, f2_down_down))

Arbol_f

#Inciso c)---- ARBITRAJE
#Precio de mercado > Precio teorico --> Vendo lo caro, compro lo barato

Put_mercado <- 5
Put_teorico <- f0


##Arbol delta, voy de atras para adelante (queda igual P o C)

delta_1_up <- (f2_up_up - f2_up_down) / (S2_up_up - S2_up_down)

delta_1_down <- (f2_down_up - f2_down_down) / (S2_down_up - S2_down_down)

delta_0 <- (f1_up - f1_down) / (S1_up - S1_down)

#Ahora hacemos el arbitraje

# t=0

Compro_put <- -Put_mercado #Como esta caro me comprometo a venderlo caro y comprarlo barato
Vendo_subyecente <- delta_0*S0
Inversion_0 <- -(Vendo_subyecente + Compro_put)
FF_0 <- Compro_put + Vendo_subyecente + Inversion_0 

# t=1 : 2 meses

Vendo_subyecente_en_1 <- (delta_1_down - delta_0)*S1_down #porque estamos viendo el caso down-down
Inversion_1 <- -Vendo_subyecente_en_1
FF_1 <- Vendo_subyecente_en_1 + Inversion_1

# t=2 : 4 meses

Compro_subyacente <- S2_down_down
Pago_Inversion_0 <- -Inversion_0*exp(Tasa_libre_de_riesgo*t)
Pago_Inversion_1 <- -Inversion_1*exp(Tasa_libre_de_riesgo*delta_t)
Ejerzo_Put <- f2_down_down # Si es call va sin el menos

FF_2 <- Compro_subyacente + Pago_Inversion_0 + Pago_Inversion_1 + Ejerzo_Put 

Check <- (Put_teorico - Put_mercado)*exp(Tasa_libre_de_riesgo*t)