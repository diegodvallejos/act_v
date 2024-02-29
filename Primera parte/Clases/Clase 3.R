### Ejercicio 1 ###

tasa <- 0.06
principal <- 1000000
tabla <- data.frame(year=1:5, zero_rate=c(3.0, 4.0, 4.6, 5.0, 5.3), forward_rate=c(NA, 5.0, 5.8, 6.2, 6.5))
tabla

forward <- exp(0.05)-1

# Fra es el principal por la diferencia de tasas entra la actual y la forward por el factor de descuento

fra <- principal*(tasa-forward)*exp((-0.04)*2)

### Ejercicio 2 - ejemplo 1###

tabla_2 <- data.frame(periodo=1:4, precio_cupon_zero=c(945, 890, 835, 785))
nominal <- 1000

tabla_2$FD <- tabla_2$precio_cupon_zero/1000

tasa_swap <- (1-tabla_2$FD[4])/sum(tabla_2$FD)

### Ejercicio 2 -ejemplo 2 ###

principal <- 100

bono_variable <- (100+(0.118/4)*100)*((1+(0.12/4))^(-2/3))

sum_FD <- 0
for(i in seq(0,4,1)){
  sum_FD <- sum_FD + (1+(0.12/4))^(-(2+i*3)/3)
}

bono_fijo <- 100*(0.1/4)*sum_FD + 100*(1+(0.12/4))^(-14/3)

swap <- bono_variable - bono_fijo


### Ejercicio 3 ### pagina 19 3.1
#1)

forward_soja <- (14260+300)*(1+(0.3/12))^6

#2)

forward_accion <- 150*((1+(0.3/12))^8)/(1+(0.05/12))

#3)

forward_rend_cierto <- (500-20*(1+(0.3/12))^-2)*((1+(0.3/12))^6)


### Ejercicio 4 ### pagina 23 3.1

#1)

forward_tipo_cambio <- ((1/(1+0.0371)*61.98)*(1+0.32))

#2)

forward_tipo_cambio_2 <- ((1/(1+0.012)*66.97)*(1+0.5)) 


### Clase 3.2

num_contratos <- function(beta_objetivo, beta_cartera, valor_actual, fxq){
  cant_contratos <- (beta_objetivo - beta_cartera)*valor_actual/fxq
  
  return(cant_contratos)
}

beta_objetivo <- 1.1
valor_actual <- 38500000
beta_cartera <- 0.9
fxq <- 0.95*275000

ejemplo_1 <- num_contratos(beta_objetivo, beta_cartera, valor_actual, fxq)
ejemplo_1

#sacamos el beneficio
profit <- (40103000/38500000)-1

#añadimos al beneficio el beneficio de los futuros
profit_2 <- ((40103000+338937.5)/38500000)-1

#el beta efectivo

effective_beta <- profit_2/profit
