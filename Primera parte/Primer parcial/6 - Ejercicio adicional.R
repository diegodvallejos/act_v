
rm(list = ls())
library(tidyverse)

#Ejercicio 1

#Datos


r=0.08
sigma=0.30
S0=75
mu=0.20
Cant_Periodos = 2
Periodos_Totales_Por_Año = 12
Vto_0=Cant_Periodos/Periodos_Totales_Por_Año
Cant_S=1000
K=78

# Ejercicio 1.a

d1_0_a = (log(S0/K) + (r + 0.5*sigma^2)*Vto_0) / (sigma*sqrt(Vto_0))
d2_0_a = d1_0_a - sigma*sqrt(Vto_0)

#En caso de ser Call

prima_call_a = S0*pnorm(d1_0_a) - K*exp(-r*Vto_0)*pnorm(d2_0_a)
Costo_Teorico=prima_call_a * Cant_S
print(prima_call_a)

#En caso de ser Put

prima_put_a= K * exp(-r*Vto_0) * pnorm(-d2_0_a) - S0 * pnorm(-d1_0_a)
Costo_Teorico=prima_put_a * Cant_S

#Comprobacion con Paridad Call-Put
prima_put_a+S0
prima_call_a+K*exp(-r*Vto_0)

# Ejercicio 1.b----------------------------------------------------------------

#En caso de ser Call
N_Sim_b=10000
ST_b=S0 * exp( (r - 1/2 * sigma^2)*Vto_0 + sigma * rnorm(N_Sim_b)*sqrt(Vto_0) )
Call_T_b = sapply(ST_b, function(x) max( x - K ,0))
call_sim_b=mean(Call_T_b) * exp(-r*Vto_0)
print(call_sim_b)

#En caso de ser Put
N_Sim_b=10000
ST_b=S0 * exp( (r - 1/2 * sigma^2)*Vto_0 + sigma * rnorm(N_Sim_b)*sqrt(Vto_0) )
Put_T_b = sapply(ST_b, function(x) max(K-x,0))
put_sim_b=mean(Put_T_b) * exp(-r*Vto_0)
print(put_sim_b)

#Comentarios
# Al comparar los resultados obtenidos en el punto a y b podemos ver que son 
# numeros "parecidos", la diferencia radica en que en el punto b al ser una 
# simulacion el resultado obtenido tiene un desvio, el cual podria reducirse 
# aumentando el numero de simulaciones realizadas.

#Ejercicio 1.c

#Calculo Delta

d1_0_c = (log(S0/K) + (r + 0.5*sigma^2)*Vto_0) / (sigma*sqrt(Vto_0))
d2_0_c = d1_0_c - sigma*sqrt(Vto_0)

delta_0_c= pnorm(d1_0_c)
Posicion_0_c= round(delta_0_c*Cant_S,0)
Costo_0_c= S0*Posicion_0_c
print(Posicion_0_c)

RtaC=paste("Por lo tanto en el momento inicial para construir una cartera delta-neutral sedeben comprar",Posicion_0_c,"acciones. 
           Las operaciones realizadas son, se calcula eldelta el cual nos da un valor de",delta_0_c,"con este mas la cantidad de acciones que forman parte de la opcion de compra vendida se calcula
            la cantidad de acciones,que se deben comprar para que la cartera quede delta-neutral. Por lo tanto en la 
           cartera quedan: La venta del call, las",Posicion_0_c,"acciones compradas, el dinero
           de la venta de la opcion y el prestamo pedido (a la tasa libre de riesgo) para comprar las acciones")

#Ejercicio 1.d

dt = 1/48
S1 = 72
Vto_1_d = Vto_0 - dt 
d1_1_d = (log(S1/K) + (r + 0.5*sigma^2)*Vto_1_d) / (sigma*sqrt(Vto_1_d))
delta_1_d = pnorm(d1_1_d)
Posicion_1_d = round(delta_1_d*Cant_S,0)
Compra_1_d = Posicion_1_d - Posicion_0_c #Si da negativo se vende
Costo_1_d = Compra_1_d * S1
Intereses_t0_d=Costo_0_c*(r*dt)
Costo_Total_d = Costo_0_c + Costo_1_d+Intereses_t0_d
Situacion_1_d=tibble(S1,delta_1_d,Posicion_1_d,Compra_1_d,Costo_1_d,Intereses_t0_d,Costo_Total_d,)
RtaD=paste("Primero se calcula el d1 correspondiente a el nuevo precio de la accion (S1) 
           y el nuevo tiempo restante (Vto_1), con esos datos se calcula el nuevo delta =",delta_1_d,
           "de la cartera y junto con este las cantidades necesarias de la accion =",Posicion_1_d,"
           para que la cartera sea delta neutral. Se venden las acciones que se tienen de m?s 
           al precio del momento.")

#Ejercicio 1.e

Cant_Periodos_Rebalanceo = 8
Periodos_Rebalanceo_Totales_Por_Año = 48
Week=seq(0,Cant_Periodos_Rebalanceo) #Varia segun sea el periodo en el cual rebalanceamos la cartera
t=seq(0,Vto_0,by =1/Periodos_Rebalanceo_Totales_Por_Año ) # Esto es porque se rebalancea semanalmente. Diarios 1/252 o 1/365
Vto_e = Vto_0 - t #Vector de tiempo al Vto
S_e = c(75,
      72,
      70,
      72,
      74,
      75,
      77,
      79,
      80)


d1_e = (log(S_e/K) + (r + 0.5*sigma^2)*Vto_e) / (sigma*sqrt(Vto_e))
delta_e = pnorm(d1_e)
Posicion_e = round(delta_e*Cant_S,0)
Compra_Venta_e = c(Posicion_e[1], diff(Posicion_e))
Costo_S_e = Compra_Venta_e*S_e
Valor_S_e = Posicion_e*S_e

Deuda_e = rep(NA,Cant_Periodos_Rebalanceo+1)
Interes_e = rep(NA,Cant_Periodos_Rebalanceo+1)
Deuda_e[1] = Costo_S_e[1]
for(i in 2:(Cant_Periodos_Rebalanceo+1)){
  Interes_e[i] = Deuda_e[i-1]*(r*(1/Periodos_Rebalanceo_Totales_Por_Año)) #Aproximacion burda, 
  # en realidad las tasas son continuas. Preguntar!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  Deuda_e[i] = Deuda_e[i-1] + Interes_e[i] + Costo_S_e[i]
  
}

Simulacion_e = tibble(Week, t, Vto_e, d1_e, delta_e, Posicion_e, Compra_Venta_e, 
               Costo_S_e, Deuda_e, Interes_e, Valor_S_e)

#Liquidacion Final-------------------------------------------------------------

Call_Entrega_e = ifelse(S_e[Cant_Periodos_Rebalanceo+1]>K,K*Cant_S,0)
Liquido_Deuda_e = -Deuda_e[Cant_Periodos_Rebalanceo+1]
Flujo_Final_e = Call_Entrega_e + Liquido_Deuda_e
VP_Costo_e = Flujo_Final_e*exp(-r*Vto_0) #Llevo Flujo_Final a la semana 0

#Comparacion-------------------------------------------------------------------

VP_Costo_e + Costo_Teorico

#Ejercicio 1.f

Cant_Periodos_Rebalanceo = 8
Periodos_Rebalanceo_Totales_Por_Año = 48
Week=seq(0,Cant_Periodos_Rebalanceo) #Varia segun sea el periodo en el cual rebalanceamos la cartera
t=seq(0,Vto_0,by =1/Periodos_Rebalanceo_Totales_Por_Año ) # Esto es porque se rebalancea semanalmente. Diarios 1/252 o 1/365
Vto_f = Vto_0 - t #Vector de tiempo al Vto

N_Sim_f=10000
Call_Entrega_Sim_f = rep(NA,N_Sim_f)
Liquido_Deuda_Sim_f = rep(NA,N_Sim_f)
Flujo_Final_Sim_f = rep(NA,N_Sim_f)
VP_Costo_Sim_f = rep(NA,N_Sim_f)
dt=1/Periodos_Rebalanceo_Totales_Por_Año
a=0 #Varia segun el instrumento y sus condiciones.En este casi Accion sin dividendos.
tendencia = (mu - 0.5 * sigma^2)*dt #Se usa si tenemos como dato Mu
volatilidad = sigma * sqrt(dt)

for(j in 1:N_Sim_f){
 
  S_f = rep(NA, (Cant_Periodos_Rebalanceo+1))
  S_f[1] = S0
  for(i in 2:(Cant_Periodos_Rebalanceo+1)){
    S_f[i] = S_f[i-1] * exp(tendencia + volatilidad * rnorm(1))
  }
  
  
  d1_f = (log(S_f/K) + (r + 0.5*sigma^2)*Vto_f) / (sigma*sqrt(Vto_f))
  delta_f = exp(-a*Vto_0) * pnorm(d1_f)
  Posicion_f = round(delta_f*Cant_S,0)
  Compra_Venta_f = c(Posicion_f[1], diff(Posicion_f))
  Costo_S_f = Compra_Venta_f*S_f
  Valor_S_f = Posicion_f*S_f
  
  Deuda_f = rep(NA,Cant_Periodos_Rebalanceo+1)
  Interes_f = rep(NA,Cant_Periodos_Rebalanceo+1)
  Deuda_f[1] = Costo_S_f[1]
  for(i in 2:(Cant_Periodos_Rebalanceo+1)){
    Interes_f[i] = Deuda_f[i-1]*(r*(1/Periodos_Rebalanceo_Totales_Por_Año)) #Aproximacion burda, en realidad         las tasas son continuas.
    Deuda_f[i] = Deuda_f[i-1] + Interes_f[i] + Costo_S_f[i]
  }
  
  
  #Liquidacion Final
  
  Call_Entrega_Sim_f[j] = ifelse(S_f[(Cant_Periodos_Rebalanceo+1)]>K,K*Cant_S,0)
  Liquido_Deuda_Sim_f[j] = -Deuda_f[(Cant_Periodos_Rebalanceo+1)]
  Flujo_Final_Sim_f[j] = Call_Entrega_Sim_f[j] + Liquido_Deuda_Sim_f[j]
  VP_Costo_Sim_f[j] = Flujo_Final_Sim_f[j]*exp(-r*Vto_0) #Llevo Flujo_Final a la semana 0
  
}
Costo_Promedio_f=mean(VP_Costo_Sim_f)
  #Analisis de Resultados
Prima_Cobrada_f = Costo_Teorico #Se peude vender a un precio distinto al del costo teorico.
Resultado_f = VP_Costo_Sim_f + Prima_Cobrada_f
Promedio_f = mean(Resultado_f)
Desvio_f = sd(Resultado_f)

Desempeño_f = sd(Resultado_f)/Costo_Teorico #Especie de Coeficiente de Variacion.

Rtados_Neg_f=sum(Resultado_f < 0)
Prop_Rtados_Neg_f=Rtados_Neg_f/N_Sim_f
hist(Resultado_f)

#Intervalo de confianza

Conf=c(0.025,0.975)
Ranking= N_Sim_f*Conf
IC=sort(Resultado_f)[Ranking]
print(IC)

#Ejercicio 1.g

prima_g=4175
Resultado_g=VP_Costo_Sim_f+prima_g
Promedio_g=mean(Resultado_g)
sum(Resultado_g<0)/N_Sim_f
hist(Resultado_g)

#Ejercicio 1.h

f=function(x){
  d1_h=(log(S0/K)+(r+0.5*x^2)*Vto_0) / (x*sqrt(Vto_0))
  d2_h=d1_h - x*sqrt(Vto_0)
  return(S0*pnorm(d1_h)-K*exp(-r*Vto_0)*pnorm(d2_h))
}
f(0.3) #Pruebo me de lo mismo 

f1=function(x){
  d1_hh=(log(S0/K)+(r+0.5*x^2)*Vto_0) / (x*sqrt(Vto_0))
  return(S0*sqrt(Vto_0)*dnorm(d1_hh))
}
sigma_0_h=0.3
n_iteraciones_h=20
sigma_h=matrix(rep(NA,n_iteraciones_h))
sigma_h[1]=sigma_0_h
for(i in 2:n_iteraciones_h){
  sigma_h[i]=sigma_h[i-1]-(f(sigma_h[i-1])-4.175)/f1(sigma_h[i-1])
}

#RTA La volatilidad implicita para ese valor de prima es 0.4124764

#Ejercicio 1.i
