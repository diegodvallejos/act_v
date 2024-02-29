#Modelo de primer parcial

#Ejercicio 1: Valuacion de opciones----

#El precio de una accion es $70, su volatilidad es 20% y el rendimiento esperado es 10%. La tasa libre de riesgo es 5% anual con composicion continua.
# a) Constuya un arbol binomial de dos pasos, con plazo total de seis meses.
# b) Con el arbol del punto a), calcula la prima de un call europeo con precio de ejercicio $68 y vencimiento de seis meses.
# c) Realice un arbitraje si el precio del punto b) es $6,5. Considere solamente el camino "up-up" del precio subyacente para ilustrar la estrategia.


#Punto b)
Volatilidad <- 0.2
Rendimiento_esperado <- 0.1
Tasa_libre_de_riesgo <- 0.05
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

(S1_up <- S0*UP)

(S1_down <- S0*DOWN)

# Momento 6 meses : Paso 2

S2_up_up <- S1_up*UP
  
S2_up_down <- S1_up*DOWN
  
S2_down_up <- S1_down*UP

S2_down_down <- S1_down*DOWN

#EL arbol queda asi:
Arbol <- tibble(indice=c("S0_UU", "S0_U", "S0", "S0_D", "S0_DD"),
                Momento_0=c(NA,NA,S0, NA, NA),
                Momento_1=c(NA, S1_up, NA, S1_down, NA),
                Momento_2=c(S2_up_up, NA, S2_up_down, NA, S2_down_down))

Arbol


#Inciso b)----

K <- 68 
#Arrancamos de atras para adelante

S2_up_up_b <- max(S2_up_up - K, 0)

S2_up_down_b <- max(S2_up_down - K, 0)

S2_down_up_b <- max(S2_down_up - K, 0)

S2_down_down_b <- max(S2_down_down - K, 0)

#Ahora momento 1

Prob_de_riesgo_neutro <- (S0 * exp(Tasa_libre_de_riesgo * delta_t) - S1_down) / (S1_up - S1_down)

S1_up_b <- (Prob_de_riesgo_neutro * S2_up_up_b + S2_up_down_b * (1 - Prob_de_riesgo_neutro)) * exp(-Tasa_libre_de_riesgo * delta_t)

S1_down_b <- (Prob_de_riesgo_neutro * S2_down_up_b + S2_down_down_b * (1 - Prob_de_riesgo_neutro)) * exp(-Tasa_libre_de_riesgo * delta_t)

#Ahora momento 0

S0_b <- (Prob_de_riesgo_neutro * S1_up_b + S1_down_b * (1 - Prob_de_riesgo_neutro)) * exp(-Tasa_libre_de_riesgo * delta_t)


Arbol_b <- tibble(indice=c("S0_UU", "S0_U", "S0", "S0_D", "S0_DD"),
                Momento_0=c("","",S0_b, "", ""),
                Momento_1=c("", S1_up_b, "", S1_down_b, ""),
                Momento_2=c(S2_up_up_b, "", S2_up_down_b, "", S2_down_down_b))


#Inciso c)----

#Si en el mercado esta caro, tenemos que vender para un call, para un put es al reves. (?)

Call_mercado <- 6.5
Call_teorico <- S0_b


##Arbol delta, voy de atras para adelante

delta_1_up <- (S2_up_up_b - S2_up_down_b) / (S2_up_up - S2_up_down)

delta_1_down <- (S2_down_up_b - S2_down_down_b) / (S2_down_up - S2_down_down)

delta_0 <- (S1_up_b - S1_down_b) / (S1_up - S1_down)

#Ahora hacemos el arbitraje

# t=0

venta_call <- Call_mercado #Como esta caro me comprometo a venderlo caro y comprarlo barato
Compra_subyecente <- -delta_0*S0
Prestamo_0 <- -Compra_subyecente-venta_call
FF_0 <- venta_call + Compra_subyecente + Prestamo_0

# t=1 : 3 meses

Compra_subyecente_en_1 <- -(delta_1_up - delta_0)*S1_up #porque estamos viendo el caso up-ip
Prestamo_1 <- -Compra_subyecente_en_1
FF_1 <- Compra_subyecente_en_1 + Prestamo_1

# t=2 : 6 meses

vendo_call_2 <- -S2_up_up_b
Pago_prestamo_0 <- -Prestamo_0*exp(Tasa_libre_de_riesgo*t)
Pago_prestamo_1 <- -Prestamo_1*exp(Tasa_libre_de_riesgo*delta_t)
Liquidacion_subyacente <- S2_up_up

FF_2 <- vendo_call_2 + Pago_prestamo_0 + Pago_prestamo_1 + Liquidacion_subyacente

Check <- (Call_mercado - S0_b)*exp(Tasa_libre_de_riesgo*t)



#Ejercicio 2: Estrategia con opciones y limites de las primas

Tabla_ej_2 <- tibble(Variable=c("Strike", "Prima"),
                     Call_1=c(30, 3),
                     Call_2=c(35, 1.5),
                     Call_3=c(40, 0.75),
                     Put_1=c(30, 1.75),
                     Put_2=c(35, 2.5),
                     Put_3=c(40,5))














#Ejercicio 3: Bonos, FRA, Swaps----

Tabla_ej_3 <- tibble(mes=seq(1, 12),
                     TNA=c(0.0405, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.0650, 0.0675, 0.07))


#Inciso a)----

Interes_cupon <- 0.12 # <- TNA con capitalizacion mensual
Frecuencia_capitalizacion <- 1/12
Valor_nominal <- 100
#pagos menuales
#Vencimiento a un año

#Sabemos que el precio del bono es flujo de fondos por factor de descuento


Tabla_ej_3$FD <- (1 + Tabla_ej_3$TNA * Tabla_ej_3$mes * Frecuencia_capitalizacion) ^ -1

Tabla_ej_3$FF <- c(rep(Interes_cupon * Frecuencia_capitalizacion * Valor_nominal, length(Tabla_ej_3$mes) - 1 ), (Interes_cupon * Frecuencia_capitalizacion * Valor_nominal) + Valor_nominal)

Tabla_ej_3$Precio <- Tabla_ej_3$FD * Tabla_ej_3$FF

Precio_bono <- sum(Tabla_ej_3$Precio)


# Inciso b) ----

prestamo <- 50000000

# i)
mes_pago <- 9
mes_comienzo <- 1
Tasa_forward <- (Tabla_ej_3$FD[mese_comienzo] / Tabla_ej_3$FD[mes_pago+mese_comienzo] - 1) * ((mes_pago)^(-1))*(Frecuencia_capitalizacion)^(-1)


#ii)

demostacion_jua <- Tasa_forward + 0.0125 # <- XD

#Opero para obtener el costo efectivo del prestamo

# Momento t = 1/12

#Defino LIbor + bps

LIBOR <- 0.0825
bps <- 0.0125

liquidacion_FRA <- (LIBOR - Tasa_forward) * prestamo * mes_pago * Frecuencia_capitalizacion * (1 + LIBOR* mes_pago * Frecuencia_capitalizacion)^(-1)

#Entonces pido un prestamo por menos del valor original

Prestamo_punto_3 <- prestamo - liquidacion_FRA

#Ahora vamos al momento t = 10/12

pago_prestamo_punto_3 <- Prestamo_punto_3 * (1+ (LIBOR+bps)*mes_pago*Frecuencia_capitalizacion)

#Entonces el costo financiero seria

Costo_financiero <- ((pago_prestamo_punto_3 / prestamo) - 1) * ((mes_pago)^(-1)) * (Frecuencia_capitalizacion)^(-1)

# Inciso c) ----


plazo_liquidacion <- 3

suma <- 0
for (i in seq(plazo_liquidacion, nrow(Tabla_ej_3), by=plazo_liquidacion)){
  suma <- suma + Tabla_ej_3$FD[i]
}

Tasa_swap <- ((1 - Tabla_ej_3$FD[nrow(Tabla_ej_3)]) / suma) * ((Frecuencia_capitalizacion)^(-1)) * (plazo_liquidacion)^(-1)


# Ejercicio 4: Cartera de inversion----











