## Ejemplo 1 ##

## Calcular factores de descuento, tasas spor, tasas forward
#paga un nominal de 1000 al finalizar

#Dataframe
tabla_ejemplo_1 <- data.frame(periodo = 1:4, precio_cupon_cero = c(945, 890, 835, 785))
tabla_ejemplo_1



#FD
tabla_ejemplo_1$FD <- tabla_ejemplo_1$precio_cupon_cero/1000
tabla_ejemplo_1



#Tasa efectiva anual = tasa spot efectiva
tabla_ejemplo_1$TEA <-(tabla_ejemplo_1$FD ^ (-1/tabla_ejemplo_1$periodo))-1
tabla_ejemplo_1



## TNA_continua = tasa sport continua

tabla_ejemplo_1$TNA_cont <- -(log(tabla_ejemplo_1$FD))/tabla_ejemplo_1$periodo
tabla_ejemplo_1

tabla_ejemplo_1$plazo_fwd <-c("0-1", "1-2", "2-3", "3-4")
tabla_ejemplo_1



## TEA forward
tabla_ejemplo_1$TEA_fwd <- NA

tabla_ejemplo_1$TEA_fwd [1] <- tabla_ejemplo_1$TEA[1]

for(i in 2:length(tabla_ejemplo_1$precio_cupon_cero)){
  tabla_ejemplo_1$TEA_fwd[i] <- (((1 + tabla_ejemplo_1$TEA[i])^i) / (1 + tabla_ejemplo_1$TEA[i - 1])^(i - 1)) ^ (1/(i-(i-1))) - 1
}
  

## TEA continua forward
tabla_ejemplo_1$TNA_cont_fwd <- NA

tabla_ejemplo_1$TNA_cont_fwd[1] <- tabla_ejemplo_1$TNA_cont[1]

for (i in 2:length(tabla_ejemplo_1$periodo)) {
  tabla_ejemplo_1$TNA_cont_fwd[i] <- tabla_ejemplo_1$TNA_cont[i] + (tabla_ejemplo_1$TNA_cont[i] - tabla_ejemplo_1$TNA_cont[i - 1]) * (( i - 1 ) / ( i - ( i - 1)))
}


#¿Tasa par a 4 años?
par_yield <- (1 - tabla_ejemplo_1$FD[4]) / sum(tabla_ejemplo_1$FD)*100



#### Ejercicio 2 ####

bono <- data.frame(plazo=1:4, FD = c(0.945, 0.89, 0.835, 0.785))

bono$FF <- c(rep(0.07*100, 4)) + c(rep(0, 3), 100)


precio_bono <- sum(bono$FF * bono$FD)
precio_bono


### Ejercicio 3 ###

tasa <- 0.12
VN <- 100
pago <- 5

tabla_ej3 <- tibble(tiempo = seq(0.5, 3, by=0.5),
                        FD = exp(-tiempo*tasa),
                        pago = rep(VN*0.1, 6) / 2 + c(rep(0,5),100),
                        valor_presente = FD*pago,
                        peso = valor_presente / sum(valor_presente),
                        t_x_peso = tiempo*peso)


(Duration <- sum(tabla_ej3$t_x_peso))

(precio <- sum(tabla_ej3$pago * tabla_ej3$FD))

(delta_precio <- -0.001 * precio *Duration) ## preguntar por que la variacion de tasa es esa

(precio_nuevo <-precio + delta_precio)


### Ejercicio 4 ###
#preguntar que onda con fp y xp
